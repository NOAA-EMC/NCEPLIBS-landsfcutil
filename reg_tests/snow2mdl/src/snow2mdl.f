 module snow2mdl
!$$$  module documentation block
!
! module:    snow2mdl
!   prgmmr: gayno         org: w/np2     date: 2005-dec-16
!
! abstract: interpolate snow data to model grid and grib the result
!
! program history log:
!   2005-DEC-16  gayno   - initial version
!   2007-SEP-20  gayno   - tested for b-grids. added improved
!                          thinning for gfs grid.
!   2008-feb-04  gayno   - added autosnow data
!
! usage: use snow2mdl
!
! remarks: some variable definitions
!   snow_cvr_mdl  - snow cover on model grid in percent
!   snow_dep_mdl  - snow depth on model grid in meters 
!                              
! attributes:
!   language: fortran 90
!   machine:  ibm sp
!
!$$$

 use program_setup,        only   : lat_threshold,          &
                                    model_snow_file,        &
                                    min_snow_depth,         &
                                    snow_cvr_threshold,     &
                                    grib_day,               &
                                    grib_century,           &
                                    grib_hour,              &
                                    grib_month,             &
                                    grib_year

 use model_grid,           only   : resol_mdl,       &
                                    imdl,            &
                                    jmdl,            &
                                    ijmdl,           &
                                    ipts_mdl,        &
                                    jpts_mdl,        &
                                    lsmask_mdl,      &
                                    lsmask_mdl_sav,  &
                                    lats_mdl,        &
                                    lons_mdl,        &
                                    kgds_mdl,        &
                                    grid_id_mdl,     &
                                    thinned,         &
                                    lonsperlat_mdl

 use snowdat,              only   : nesdis_res,        &                                 
                                    afwa_res,          &
                                    autosnow_res,      &
                                    inesdis,           &
                                    jnesdis,           &
                                    mesh_nesdis,       &
                                    snow_cvr_nesdis,   &
                                    sea_ice_nesdis,    &
                                    bitmap_nesdis,     &
                                    iafwa,             &
                                    jafwa,             &
                                    snow_dep_afwa_nh,  &
                                    snow_dep_afwa_sh,  &
                                    bitmap_afwa_nh,    &
                                    bitmap_afwa_sh,    &
                                    iautosnow,         &
                                    jautosnow,         &
                                    snow_cvr_autosnow, &
                                    bitmap_autosnow,   &
                                    use_nh_afwa,       &
                                    use_sh_afwa,       &
                                    use_nesdis,        &
                                    use_autosnow,      &
                                    kgds_nesdis,       &
                                    kgds_afwa_nh,      &
                                    kgds_afwa_sh,      &
                                    kgds_autosnow,     &
                                    bad_afwa_nh, bad_afwa_sh 

 use read_write_utils,     only   : uninterpred

 real, allocatable               :: snow_cvr_mdl(:,:)  ! cover in % on mdl grid                                   
 real, allocatable               :: snow_dep_mdl(:,:)  ! depth on model grid

 contains

 subroutine interp
!$$$  subprogram documentation block
!
! subprogram:   interp
!   prgmmr: gayno          org: w/np2     date: 2005-dec-16
!
! abstract:  interpolate snow data to model grid.
!
! program history log:
! 2005-dec-16  gayno    - initial version
! 2007-sep-20  gayno    - tested for b-grids. added improved
!                         thinning for gfs grid.
! 2008-feb-04  gayno    - add use of autosnow data
!
! usage: call interp
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! remarks: the determination of cover and depth on the model
!   grid is as follows:
!
!   user may select nesdis snow cover data,
!   autosnow snow cover data, afwa snow depth data,
!   or all three data sources.  nesdis data
!   is valid in the nh only.  autosnow data is valid
!   in the sh only.  afwa is available for both hemispheres.
!
!   if nesdis/autosnow data is selected, but afwa data
!   is not selected, then the depth is set to a user-defined
!   nominal value where the cover exceeds the user-defined threshold
!   (typically 50%).
!
!   if afwa data is selected, but nesdis/autosnow data is
!   not selected, the depth is set to the afwa value.  the
!   cover is set to 100% at points with non-zero depth.
!
!   if all data sources are selected, the snow cover is
!   determined from the nesdis data (northern
!   hemipshere) and autosnow data (southern hemisphere).  if
!   the cover exceeds the user-defined threshold at
!   a model point, then the depth is set to the afwa value or
!   a user-defined minimum depth is, whichever is greater. if
!   the cover is less than the threshold, the depth is set
!   to zero regardless of the afwa value.
!
!      
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 implicit none

 integer                   :: i, j, ii, jj, ij
 integer                   :: ijmdl2, istart, iend, imid, iii
 integer, allocatable      :: idum(:,:)
 integer                   :: int_opt, ipopt(20)
 integer                   :: kgds_mdl_tmp(200)
 integer                   :: no, ibo, iret, nret

 logical*1, allocatable    :: bitmap_mdl(:)

 real                      :: dum
 real                      :: gridi
 real                      :: gridj
 real, allocatable         :: lsmask_1d(:)
 real, allocatable         :: snow_cvr_mdl_1d(:)
 real, allocatable         :: snow_dep_mdl_tmp(:) 
 real                      :: sumc, sumd, x1, r, fraction, gridis, gridie
 real, parameter           :: undefined_value = -999.

!----------------------------------------------------------------------
! for model grids fully or partially located in the southern
! hemisphere, the user must select sh data (either autosnow or afwa).
! this restriction is relaxed for the ndas domain, which is 
! has a small part below the equator.
!----------------------------------------------------------------------

 if (minval(lats_mdl) < -10.0 .and. .not. use_sh_afwa .and. .not. use_autosnow) then
   print*,"- MUST SELECT EITHER AFWA OR AUTOSNOW DATA FOR MODEL GRID WITH SH POINTS, ABORT."
   call w3tage('SNO2MDL')
   call errexit(54)
 endif

!----------------------------------------------------------------------
! if model grid is totally within the southern hemisphere, set flags
! so that nh afwa and nh nesdis is not processed.  these flags
! are set to false if user inadvertantly selects these data sources.
!----------------------------------------------------------------------

 if (maxval(lats_mdl) < 0.0) then 
   use_nh_afwa=.false.
   use_nesdis=.false.
 endif

!----------------------------------------------------------------------
! if model grid is partially or fully located in the northern 
! hemisphere, the user must select either nesdis or nh afwa data.
!----------------------------------------------------------------------

 if (maxval(lats_mdl) > 0.0 .and. .not. use_nh_afwa .and. .not. use_nesdis) then
   print*,"- MUST SELECT EITHER NESDIS OR AFWA DATA FOR MODEL GRID WITH NH POINTS, ABORT."
   call w3tage('SNO2MDL')
   call errexit(54)
 endif

!----------------------------------------------------------------------
! if model grid is totally within the northern hemisphere, set flag
! so that sh data is not processed.  these flags are set to false
! if user inadvertantly selects this data.
!----------------------------------------------------------------------

 if (minval(lats_mdl) > 0.0) then ! is model grid totally within northern hemisphere?
   use_sh_afwa=.false.
   use_autosnow=.false.
 endif

!----------------------------------------------------------------------
! if selected, interpolate nesdis data to model grid.
!----------------------------------------------------------------------

 NESDIS : if (use_nesdis) then

   ipopt = 0
   if (nesdis_res < (0.5*resol_mdl)) then
     print*,"- INTERPOLATE NH NESDIS DATA TO MODEL GRID USING BUDGET METHOD."
     ipopt(1)=2  ! break model grid cell into 25 points.
     ipopt(2:4)=1  ! 25 points are weighted equally.
     ipopt(5)=10  ! 10% coverage of valid data in box
     ipopt(20) = nint(100.0 / nesdis_res) + 1   ! search box width of 100 km.
     kgds_mdl_tmp = kgds_mdl
     kgds_mdl_tmp(1) = kgds_mdl_tmp(1) - 255 ! subset of grid
     int_opt = 3
     no = ijmdl
   else
     print*,"- INTERPOLATE NH NESDIS DATA TO MODEL GRID USING NEIGHBOR METHOD."
     ipopt(1) = nint(100.0 / nesdis_res) + 1   ! search box width of 100 km.
     kgds_mdl_tmp = kgds_mdl
     kgds_mdl_tmp(1) = -1  ! for subsection of model grid.
     int_opt = 2
     no = ijmdl  ! an input when kgds(1) < 0 (subset of grid)
   end if

   allocate (snow_cvr_mdl_1d(ijmdl))
   snow_cvr_mdl_1d = 0.0

   allocate (bitmap_mdl(ijmdl))
   bitmap_mdl=.false.  ! if interpolation routine can't find data
                       ! at a point, this flag is false.

   call ipolates(int_opt, ipopt, kgds_nesdis, kgds_mdl_tmp,   &
                (inesdis*jnesdis), ijmdl,               &
                 1, 1, bitmap_nesdis, snow_cvr_nesdis,  &
                 no, lats_mdl, lons_mdl, ibo, bitmap_mdl,     &
                 snow_cvr_mdl_1d, iret)

   deallocate (bitmap_nesdis, snow_cvr_nesdis)

   if (iret /= 0) then
     print*,"- ERROR IN INTERPOLATION ROUTINE. IRET IS: ", iret 
     call w3tage('SNO2MDL')
     call errexit(55)
   endif

!-----------------------------------------------------------------------
! if the interpolation routines did not find valid nesdis data
! in the vicinity of the model point, need to set a default value
! of snow cover.  south of user-defined latitude threshold, set
! to zero.  otherwise, see if the nearest neighbor nesdis point is
! sea ice.  if so, assume model point is snow covered.
!-----------------------------------------------------------------------

   do ij = 1, ijmdl
     if (lats_mdl(ij) < 0.0) cycle  ! only consider nh model points
     if (.not. bitmap_mdl(ij)) then 
       if (lats_mdl(ij) <= lat_threshold) then
         snow_cvr_mdl_1d(ij) = 0.0
       else 
         call gdswiz(kgds_nesdis,-1,1,undefined_value,gridi,gridj, &
                     lons_mdl(ij),lats_mdl(ij),nret,0,dum,dum) 
         if (nret /= 1) then
           print*,"- ERROR!! MODEL POINT OUTSIDE NESDIS GRID."
           snow_cvr_mdl_1d(ij) = 0.0
         else
           ii = nint(gridi)
           jj = nint(gridj)
           if (sea_ice_nesdis(ii,jj) == 1) then
             snow_cvr_mdl_1d(ij) = 100.0
           else
             snow_cvr_mdl_1d(ij) = 0.0
           end if
         end if
       end if
     end if
   enddo

   deallocate (sea_ice_nesdis)
   deallocate (bitmap_mdl)

 endif NESDIS

!----------------------------------------------------------------------
! now interpolate nh afwa snow depth data.
!----------------------------------------------------------------------

 NH_AFWA : if (use_nh_afwa) then

!----------------------------------------------------------------------
! determine interpolation method based on the resolution of 
! afwa data and the model grid.  
!----------------------------------------------------------------------

   ipopt = 0
   if (afwa_res < (0.5*resol_mdl)) then
     print*,"- INTERPOLATE NH AFWA DATA TO MODEL GRID USING BUDGET METHOD."
     ipopt(1)=-1  ! break model grid cell into 25 points.
     ipopt(2)=-1  ! 25 points are weighted equally.
     ipopt(20) = nint(100.0 / afwa_res) + 1   ! search box width of 100 km.
     kgds_mdl_tmp = kgds_mdl
     no = ijmdl
     int_opt = 3
   else
     print*,"- INTERPOLATE NH AFWA DATA TO MODEL GRID USING NEIGHBOR METHOD."
     ipopt(1) = nint(100.0 / afwa_res) + 1   ! search box width of 100 km.
     kgds_mdl_tmp = kgds_mdl
     kgds_mdl_tmp(1) = -1  ! for subsection of model grid.
     int_opt = 2
     no = ijmdl  ! an input when kgds(1) < 0 (subset of grid)
   end if

   allocate (snow_dep_mdl_tmp(ijmdl))
   snow_dep_mdl_tmp = 0.0

   allocate (bitmap_mdl(ijmdl))
   bitmap_mdl = .false.

   call ipolates(int_opt, ipopt, kgds_afwa_nh, kgds_mdl_tmp,    &
                (iafwa*jafwa), ijmdl,  &
                 1, 1, bitmap_afwa_nh, snow_dep_afwa_nh, &
                 no, lats_mdl, lons_mdl, ibo, bitmap_mdl,     &
                 snow_dep_mdl_tmp, iret)

   deallocate(bitmap_afwa_nh, snow_dep_afwa_nh)

   if (iret /= 0) then
     print*,"- ERROR IN INTERPOLATION ROUTINE. IRET IS: ", iret 
     call w3tage('SNO2MDL')
     call errexit(55)
   endif

!----------------------------------------------------------------------
! if interpolation did not find afwa data near the model point, then
! use a nominal value based on latitude threshold.  this value
! may be overwritten below depending on nesdis cover data (if
! user selects nesdis data).
!----------------------------------------------------------------------

   do ij = 1, ijmdl
     if (lats_mdl(ij) >= 0.) then  ! only consider model pts in n hemi.
       if (.not. bitmap_mdl(ij)) then
         if (abs(lats_mdl(ij)) >= lat_threshold) then
           snow_dep_mdl_tmp(ij) = min_snow_depth
         else
           snow_dep_mdl_tmp(ij) = 0.0
         endif
       endif
     endif
   enddo

   deallocate(bitmap_mdl)

 endif NH_AFWA

!----------------------------------------------------------------------
! if nh data selected, use it to determine the cover and depth
! on the model grid.
!----------------------------------------------------------------------

 allocate (snow_dep_mdl(imdl,jmdl))
 allocate (snow_cvr_mdl(imdl,jmdl))
 snow_cvr_mdl = 0.0
 snow_dep_mdl = 0.0

 if (use_nh_afwa .and. use_nesdis) then  ! set depth/cover on nesdis/afwa blend
   print*,"- BLEND NESDIS AND AFWA DATA IN NH."
   do ij = 1, ijmdl
     if (lats_mdl(ij) >= 0.0) then
       if (snow_cvr_mdl_1d(ij) >= snow_cvr_threshold) then
         snow_dep_mdl(ipts_mdl(ij),jpts_mdl(ij)) =   &
                      max(snow_dep_mdl_tmp(ij), min_snow_depth)
       endif   
       snow_cvr_mdl(ipts_mdl(ij),jpts_mdl(ij)) = snow_cvr_mdl_1d(ij)
     endif
   enddo
   deallocate (snow_cvr_mdl_1d)
   deallocate (snow_dep_mdl_tmp)
 elseif (use_nh_afwa) then  ! set depth/cover on afwa only
   print*,"- SET DEPTH/COVER FROM AFWA DATA IN NH."
   do ij = 1, ijmdl
     if (lats_mdl(ij) >= 0.0) then
       if (snow_dep_mdl_tmp(ij) > 0.0) then
         snow_cvr_mdl(ipts_mdl(ij),jpts_mdl(ij)) = 100.0
         snow_dep_mdl(ipts_mdl(ij),jpts_mdl(ij)) = snow_dep_mdl_tmp(ij)
       endif
     endif
   enddo
   deallocate (snow_dep_mdl_tmp)
 elseif (use_nesdis) then  ! set depth/cover on nesdis only
   print*,"- SET DEPTH/COVER FROM NESDIS DATA IN NH."
   do ij = 1, ijmdl
     if (lats_mdl(ij) >= 0.0) then
!      if (snow_cvr_mdl_1d(ij) >= snow_cvr_threshold) then
!        snow_dep_mdl(ipts_mdl(ij),jpts_mdl(ij)) = min_snow_depth
!      endif   
       snow_cvr_mdl(ipts_mdl(ij),jpts_mdl(ij)) = snow_cvr_mdl_1d(ij)
     endif
   enddo
   deallocate (snow_cvr_mdl_1d)
 end if

!----------------------------------------------------------------------
! if selected, interpolate autosnow data to model grid.
!----------------------------------------------------------------------

 AUTOSNOW : if (use_autosnow) then

   ipopt = 0
   if (autosnow_res < (0.5*resol_mdl)) then
     print*,"- INTERPOLATE AUTOSNOW DATA TO MODEL GRID USING BUDGET METHOD."
     ipopt(1)=2    ! break model grid cell into 25 points.
     ipopt(2:4)=1  ! 25 points are weighted equally.
     ipopt(5)=10   ! 10% coverage of valid data in box
     ipopt(20) = nint(100.0 / autosnow_res) + 1   ! search box width of 100 km.
     kgds_mdl_tmp = kgds_mdl
     kgds_mdl_tmp(1) = kgds_mdl_tmp(1) - 255 ! subset of grid
     int_opt = 3
     no = ijmdl
   else
     print*,"- INTERPOLATE AUTOSNOW DATA TO MODEL GRID USING NEIGHBOR METHOD."
     ipopt(1) = nint(100.0 / autosnow_res) + 1   ! search box width of 100 km.
     kgds_mdl_tmp = kgds_mdl
     kgds_mdl_tmp(1) = -1  ! for subsection of model grid.
     int_opt = 2
     no = ijmdl  ! an input when kgds(1) < 0 (subset of grid)
   end if

   allocate (snow_cvr_mdl_1d(ijmdl))
   snow_cvr_mdl_1d = 0.0

   allocate (bitmap_mdl(ijmdl))
   bitmap_mdl=.false.  ! if interpolation routine can't find data
                       ! at a point, this flag is false.

   call ipolates(int_opt, ipopt, kgds_autosnow, kgds_mdl_tmp,   &
                (iautosnow*jautosnow), ijmdl,               &
                 1, 1, bitmap_autosnow, snow_cvr_autosnow,  &
                 no, lats_mdl, lons_mdl, ibo, bitmap_mdl,     &
                 snow_cvr_mdl_1d, iret)

   deallocate (snow_cvr_autosnow, bitmap_autosnow)

   if (iret /= 0) then
     print*,"- ERROR IN INTERPOLATION ROUTINE. IRET IS: ", iret 
     call w3tage('SNO2MDL')
     call errexit(55)
   endif

!----------------------------------------------------------------------
! if interpolation fails to find autosnow data, set the cover
! at the model point to a nomimal value.
!----------------------------------------------------------------------

   do ij = 1, ijmdl
     if (lats_mdl(ij) < 0.0) then
       if (.not. bitmap_mdl(ij)) then
         if (abs(lats_mdl(ij)) <= lat_threshold) then
           snow_cvr_mdl_1d(ij) = 0.0
         else 
           snow_cvr_mdl_1d(ij) = 100.0
         end if
       end if
     end if
   enddo

   deallocate (bitmap_mdl)

 endif AUTOSNOW

!----------------------------------------------------------------------
! now interpolate sh afwa snow depth data.
!----------------------------------------------------------------------

 SH_AFWA : if (use_sh_afwa) then

!----------------------------------------------------------------------
! determine interpolation method based on the resolution of 
! afwa data and the model grid.  
!----------------------------------------------------------------------

   ipopt = 0
   if (afwa_res < (0.5*resol_mdl)) then
     print*,"- INTERPOLATE SH AFWA DATA TO MODEL GRID USING BUDGET METHOD."
     ipopt(1)=-1  ! break model grid cell into 25 points.
     ipopt(2)=-1  ! 25 points are weighted equally.
     ipopt(20) = nint(100.0 / afwa_res) + 1   ! search box width of 100 km.
     kgds_mdl_tmp = kgds_mdl
     no = ijmdl
     int_opt = 3
   else
     print*,"- INTERPOLATE SH AFWA DATA TO MODEL GRID USING NEIGHBOR METHOD."
     ipopt(1) = nint(100.0 / afwa_res) + 1   ! search box width of 100 km.
     kgds_mdl_tmp = kgds_mdl
     kgds_mdl_tmp(1) = -1  ! for subsection of model grid.
     int_opt = 2
     no = ijmdl  ! an input when kgds(1) < 0 (subset of grid)
   end if

   allocate (snow_dep_mdl_tmp(ijmdl))
   snow_dep_mdl_tmp = 0.0

   allocate (bitmap_mdl(ijmdl))
   bitmap_mdl = .false.

   call ipolates(int_opt, ipopt, kgds_afwa_sh, kgds_mdl_tmp,    &
                (iafwa*jafwa), ijmdl,  &
                 1, 1, bitmap_afwa_sh, snow_dep_afwa_sh, &
                 no, lats_mdl, lons_mdl, ibo, bitmap_mdl,     &
                 snow_dep_mdl_tmp, iret)

   if (iret /= 0) then
     print*,"- ERROR IN INTERPOLATION ROUTINE. IRET IS: ", iret 
     call w3tage('SNO2MDL')
     call errexit(55)
   endif

   deallocate (bitmap_afwa_sh, snow_dep_afwa_sh)

!----------------------------------------------------------------------
! if interpolation does not find afwa data, set model point to
! a nominal value.
!----------------------------------------------------------------------

   do ij = 1, ijmdl
     if (lats_mdl(ij) < 0.) then
       if (.not. bitmap_mdl(ij)) then
         if (abs(lats_mdl(ij)) >= lat_threshold) then
           snow_dep_mdl_tmp(ij) = min_snow_depth
         else
           snow_dep_mdl_tmp(ij) = 0.0
         endif
       endif
     endif
   enddo

  deallocate(bitmap_mdl)

 endif SH_AFWA

!----------------------------------------------------------------------
! if nh data selected, use it to determine the cover and depth
! on the model grid.
!----------------------------------------------------------------------

 if (use_sh_afwa .and. use_autosnow) then  ! set depth/cover on autosnow/afwa blend
   print*,"- BLEND NESDIS AND AFWA DATA IN SH."
   do ij = 1, ijmdl
     if (lats_mdl(ij) < 0.0) then
       if (snow_cvr_mdl_1d(ij) >= snow_cvr_threshold) then
         snow_dep_mdl(ipts_mdl(ij),jpts_mdl(ij)) =   &
                      max(snow_dep_mdl_tmp(ij), min_snow_depth)
       endif   
       snow_cvr_mdl(ipts_mdl(ij),jpts_mdl(ij)) = snow_cvr_mdl_1d(ij)
     endif
   enddo
   deallocate (snow_cvr_mdl_1d)
   deallocate (snow_dep_mdl_tmp)
 elseif (use_sh_afwa) then  ! set depth/cover on afwa only
   print*,"- SET DEPTH/COVER FROM AFWA DATA IN SH."
   do ij = 1, ijmdl
     if (lats_mdl(ij) < 0.0) then
       if (snow_dep_mdl_tmp(ij) > 0.0) then
         snow_cvr_mdl(ipts_mdl(ij),jpts_mdl(ij)) = 100.0
         snow_dep_mdl(ipts_mdl(ij),jpts_mdl(ij)) = snow_dep_mdl_tmp(ij)
       endif
     endif
   enddo
   deallocate (snow_dep_mdl_tmp)
 elseif (use_autosnow) then  ! set depth/cover on autosnow only
   print*,"- SET DEPTH/COVER FROM AUTOSNOW IN SH."
   do ij = 1, ijmdl
     if (lats_mdl(ij) < 0.0) then
!      if (snow_cvr_mdl_1d(ij) >= snow_cvr_threshold) then
!        snow_dep_mdl(ipts_mdl(ij),jpts_mdl(ij)) = min_snow_depth
!      endif   
       snow_cvr_mdl(ipts_mdl(ij),jpts_mdl(ij)) = snow_cvr_mdl_1d(ij)
     endif
   enddo
   deallocate (snow_cvr_mdl_1d)
 end if

!----------------------------------------------------------------------
! if a global model grid, and if running on thinned grid, then
! take a linear weighting of full points located within the thin points.
! "4" is grid indicator for a gaussian grid.
!----------------------------------------------------------------------

 if (kgds_mdl(1) == 4 .and. thinned) then

   ijmdl2 = sum(lonsperlat_mdl) * 2
   allocate (snow_cvr_mdl_1d(ijmdl2))
   allocate (lsmask_1d(ijmdl2))
   allocate (snow_dep_mdl_tmp(ijmdl2))

   lsmask_1d = 0.0
   snow_cvr_mdl_1d = 0.0
   snow_dep_mdl_tmp = 0.0

   ij = 0
   do j = 1, jmdl
     jj = j
     if (jj > jmdl/2) jj = jmdl - j + 1
     r = float(imdl) / float(lonsperlat_mdl(jj))
     do i = 1, lonsperlat_mdl(jj)
       ij = ij + 1
       x1 = (i-1)*r
       imid = nint(x1+1.0)
       lsmask_1d(ij) = lsmask_mdl_sav(imid,j)
       if (lsmask_mdl_sav(imid,j) == 0.0) cycle
       gridis=x1+1.0-r/2.
       istart=nint(gridis)
       gridie=x1+1.0+r/2.
       iend=nint(gridie)
       sumc = 0.0   ! cover
       sumd = 0.0   ! depth
       do ii = istart, iend
         if (ii == istart) then
           fraction = 0.5 - (gridis - float(istart))
         elseif (ii == iend) then
           fraction = 0.5 + (gridie - float(iend))
         else
           fraction = 1.0
         endif
         if (fraction < 0.0001) cycle
         iii = ii
         if (iii < 1) iii = imdl + iii
         sumc = sumc + fraction * snow_cvr_mdl(iii,j)
         sumd = sumd + fraction * snow_dep_mdl(iii,j)
       enddo
       snow_cvr_mdl_1d(ij) = sumc / r
       snow_dep_mdl_tmp(ij) = 0.0
       if (snow_cvr_mdl_1d(ij) > snow_cvr_threshold) then
         snow_dep_mdl_tmp(ij) = max(sumd / r,min_snow_depth)
       end if
    enddo
   enddo

   deallocate (lsmask_mdl_sav)

!----------------------------------------------------------------------
! now place thinned points into 2-d array for output.
!----------------------------------------------------------------------

   allocate (idum(imdl,jmdl))
   idum = 0
   call uninterpred(1, idum, lsmask_1d, lsmask_mdl, imdl, jmdl, ijmdl2, lonsperlat_mdl)
   call uninterpred(1, idum, snow_cvr_mdl_1d, snow_cvr_mdl, imdl, jmdl, ijmdl2, lonsperlat_mdl)
   deallocate(snow_cvr_mdl_1d)
   call uninterpred(1, idum, snow_dep_mdl_tmp, snow_dep_mdl, imdl, jmdl, ijmdl2, lonsperlat_mdl)
   deallocate(snow_dep_mdl_tmp)
   deallocate(idum)

 end if

!cggg

 if (kgds_mdl(1) == 4) then
   if (bad_afwa_nh .or. bad_afwa_sh) then
     snow_dep_mdl=0.0
   endif
 endif 

!----------------------------------------------------------------------
! grib the interpolated data.
!----------------------------------------------------------------------

 call gribit

 deallocate (snow_cvr_mdl)
 deallocate (snow_dep_mdl)

 return

 end subroutine interp

 subroutine gribit
!$$$  subprogram documentation block
!
! subprogram:   gribit
!   prgmmr: gayno          org: w/np2     date: 2005-dec-16
!
! abstract:  grib snow cover and depth on the model grid
!
! program history log:
! 2005-dec-16  gayno    - initial version
!
! usage: call gribit
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! remarks: none.
!      
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 implicit none

 integer                    :: iret 
 integer, parameter         :: lugb = 24    ! unit number of output grib file
 integer                    :: kpds(200)

 logical*1                  :: lbms(imdl,jmdl)

!----------------------------------------------------------------------
! set up pds section.  don't need to set the gds section.
! since the model grid is not changing, use the kgds array 
! already determined in module model_grid.   
!----------------------------------------------------------------------

 kpds = 0

 kpds(1)  = 7           ! center id
 kpds(2)  = 25          ! process id number. this determined from the 
                        ! input data as we are simply interpolating
                        ! that data to a different grid.  should
                        ! i request a process id for my codes? 
 kpds(3)  = grid_id_mdl ! grid specified in gds
 kpds(4)  = 192         ! include gds and a bit map section  
 kpds(5)  = 238         ! parameter number for snow cover
 kpds(6)  = 1           ! level - ground or water surface
 kpds(7)  = 0           ! height pressure of level
 kpds(8)  = grib_year   ! year of century     the time info is determined
 kpds(9)  = grib_month  ! month               by operational requirements
 kpds(10) = grib_day    ! day
 kpds(11) = grib_hour   ! hour
 kpds(12) = 0           ! minute
 kpds(13) = 1           ! fcst time unit - hour
 kpds(14) = 0           ! period of time, p1.  set to '0' for analysis
 kpds(15) = 0           ! number of time units, p2. 
 kpds(16) = 1           ! initialized analysis product
 kpds(17) = 0           ! number in average
 kpds(18) = 1           ! grib edition 1
 kpds(19) = 3           ! parameter table version number
 kpds(20) = 0           ! number missing from avg/accum
 kpds(21) = grib_century ! century - set as in the input file
 kpds(22) = 0           ! decimal scale factor
 kpds(23) = 4           ! subcenter - emc  
 kpds(24) = 0           ! reserved
 kpds(25) = 0           ! reserved

 lbms = .false.         ! set bitmap section
  
 where(lsmask_mdl > 0.0)  lbms = .true. 
 
 print*,"- OPEN OUTPUT GRIB FILE ", trim(model_snow_file)
 call baopenw(lugb, model_snow_file, iret)

 if (iret /= 0) then
   print*,'- ERROR OPENING OUTPUT GRIB FILE. IRET IS ', iret
   call w3tage('SNO2MDL')
   call errexit(59)
 end if

 print*,"- WRITE OUTPUT GRIB FILE ", trim(model_snow_file)
 call putgb (lugb, (imdl*jmdl), kpds, kgds_mdl, lbms,  &
             snow_cvr_mdl, iret)

 if (iret /= 0) then
   print*,'- ERROR WRITING OUTPUT GRIB FILE. IRET IS ', iret
   call w3tage('SNO2MDL')
   call errexit(58)
 end if

! for regional model, if afwa data not used, don't output a depth
! record.

 if (kgds_mdl(1) /= 4) then
   if (.not. use_nh_afwa .and. .not. use_sh_afwa) goto 88 
 endif

 kpds(5)  = 66  ! parameter number for snow depth
 kpds(22) = 4   ! scaling factor.  to nearest mm

 call putgb (lugb, (imdl*jmdl), kpds, kgds_mdl, lbms,  &
             snow_dep_mdl, iret)

 if (iret /= 0) then
   print*,'- ERROR WRITING OUTPUT GRIB FILE. IRET IS ', iret
   call w3tage('SNO2MDL')
   call errexit(57)
 end if
 
 88 call baclose(lugb, iret)

 return

 end subroutine gribit

 end module snow2mdl
