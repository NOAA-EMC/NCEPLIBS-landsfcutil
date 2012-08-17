 module sno2mdl
!$$$  module documentation block
!
! module:    sno2mdl
!   prgmmr: gayno         org: w/np2     date: 2005-dec-16
!
! abstract: interpolate snow data to model grid and grib the result
!
! program history log:
!   2005-DEC-16  gayno   - initial version
!
! usage: use sno2mdl
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

 use model_grid,           only   : resol_mdl,   &
                                    imdl,        &
                                    jmdl,        &
                                    ijmdl,       &
                                    ipts_mdl,    &
                                    jpts_mdl,    &
                                    lsmask_mdl,  &
                                    lats_mdl,    &
                                    lons_mdl,    &
                                    kgds_mdl,    &
                                    grid_id_mdl, &
                                    thinned,     &
                                    lonsperlat_mdl

 use snowdat,              only   : nesdis_res,        &                                 
                                    afwa_res,          &
                                    inesdis,           &
                                    jnesdis,           &
                                    mesh_nesdis,       &
                                    snow_cvr_nesdis,   &
                                    sea_ice_nesdis,    &
                                    bitmap_nesdis,     &
                                    iafwa,             &
                                    jafwa,             &
                                    snow_dep_afwa,     &
                                    bitmap_afwa ,      &
                                    no_afwa,           &
                                    kgds_nesdis,       &
                                    kgds_afwa_nh,      &
                                    kgds_afwa_sh 

 use read_write_utils,     only   : full_to_thin

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
!
! usage: call interp
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! remarks:  in the nh, snow depth on the model grid is a merger of the 
!   nesdis snow cover product and the afwa snow depth product.  
!
!   the nesdis snow cover is given priority.  if nesdis indicates snow at 
!   a point, the afwa depth is used, but if the afwa depth is zero, a
!   nominal value of snow is used.
!
!   if the afwa product indicates snow, but the nesdis product does
!   not, then the snow depth is set to zero.
!
!   in the sh, where there is no nesdis product, the afwa product is
!   used as is.
!      
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 implicit none

 integer                   :: hemi, i, j, ii, jj, ij
 integer                   :: int_opt, ipopt(20)
 integer                   :: kgds_afwa(200), kgds_mdl_tmp(200)
 integer                   :: no, ibo, iret, nret

 logical*1, allocatable    :: bitmap_mdl(:,:)

 real                      :: dum
 real                      :: gridi
 real                      :: gridj
 real, allocatable         :: snow_cvr_mdl_1d(:)
 real, allocatable         :: snow_dep_mdl_tmp(:,:) 
 real, parameter           :: undefined_value = -999.

!----------------------------------------------------------------------
! call interpolation routine for nesdis snow cover data.
!----------------------------------------------------------------------

 ipopt = 0

 if (nesdis_res < (0.5*resol_mdl)) then
   print*,"- INTERPOLATE NESDIS DATA TO MODEL GRID USING BUDGET METHOD."
   ipopt(1)=2  ! break model grid cell into 25 points.
   ipopt(2:4)=1  ! 25 points are weighted equally.
   ipopt(5)=10  ! 10% coverage of valid data in box
   ipopt(20) = nint(100.0 / nesdis_res) + 1   ! search box width of 100 km.
   kgds_mdl_tmp = kgds_mdl
   kgds_mdl_tmp(1) = kgds_mdl_tmp(1) - 255 ! subset of grid
   int_opt = 3
   no = ijmdl
 else
   print*,"- INTERPOLATE NESDIS DATA TO MODEL GRID USING NEIGHBOR METHOD."
   ipopt(1) = nint(100.0 / nesdis_res) + 1   ! search box width of 100 km.
   kgds_mdl_tmp = kgds_mdl
   kgds_mdl_tmp(1) = -1  ! for subsection of model grid.
   int_opt = 2
   no = ijmdl  ! an input when kgds(1) < 0 (subset of grid)
 end if

 allocate (bitmap_mdl(ijmdl,1))
 bitmap_mdl=.false.  ! if interpolation routine can't find data
                     ! at a point, this flag is false.

 allocate (snow_cvr_mdl_1d(ijmdl))

 call ipolates(int_opt, ipopt, kgds_nesdis, kgds_mdl_tmp,   &
              (inesdis*jnesdis), ijmdl,               &
               1, 1, bitmap_nesdis, snow_cvr_nesdis,  &
               no, lats_mdl, lons_mdl, ibo, bitmap_mdl,     &
               snow_cvr_mdl_1d, iret)

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
   if (.not. bitmap_mdl(ij,1)) then
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

 deallocate (bitmap_mdl)

!----------------------------------------------------------------------
! now interpolate afwa snow depth data.
!----------------------------------------------------------------------

 allocate (snow_dep_mdl_tmp(ijmdl,3))
 snow_dep_mdl_tmp = 0.0

 AFWA_AVAIL : if (.not. no_afwa) then

   ipopt = 0
   if (afwa_res < (0.5*resol_mdl)) then
     print*,"- INTERPOLATE AFWA DATA TO MODEL GRID USING BUDGET METHOD."
     ipopt(1)=-1  ! break model grid cell into 25 points.
     ipopt(2)=-1  ! 25 points are weighted equally.
     ipopt(20) = nint(100.0 / afwa_res) + 1   ! search box width of 100 km.
     kgds_mdl_tmp = kgds_mdl
     no = ijmdl
     int_opt = 3
   else
     print*,"- INTERPOLATE AFWA DATA TO MODEL GRID USING NEIGHBOR METHOD."
     ipopt(1) = nint(100.0 / afwa_res) + 1   ! search box width of 100 km.
     kgds_mdl_tmp = kgds_mdl
     kgds_mdl_tmp(1) = -1  ! for subsection of model grid.
     int_opt = 2
     no = ijmdl  ! an input when kgds(1) < 0 (subset of grid)
   end if

   allocate (bitmap_mdl(ijmdl,2))
   bitmap_mdl = .false.

   kgds_afwa = 0

   do hemi = 1, 2

     if (hemi == 1) kgds_afwa(1:25) = kgds_afwa_nh
     if (hemi == 2) kgds_afwa(1:25) = kgds_afwa_sh

     print*,"- INTERPOLATE AFWA DATA TO MODEL GRID FOR HEMI: ", hemi

     call ipolates(int_opt, ipopt, kgds_afwa, kgds_mdl_tmp,    &
                  (iafwa*jafwa), ijmdl,  &
                   1, 1, bitmap_afwa(:,:,hemi), snow_dep_afwa(:,:,hemi), &
                   no, lats_mdl, lons_mdl, ibo, bitmap_mdl(:,hemi),     &
                   snow_dep_mdl_tmp(:,hemi), iret)

     if (iret /= 0) then
       print*,"- ERROR IN INTERPOLATION ROUTINE. IRET IS: ", iret 
       call w3tage('SNO2MDL')
       call errexit(55)
     endif

   enddo

!-----------------------------------------------------------------------
! merge the afwa data from each hemi.
!-----------------------------------------------------------------------

   snow_dep_mdl_tmp(:,3) = 0.0

   do ij = 1, ijmdl
    if (bitmap_mdl(ij,1) .and. bitmap_mdl(ij,2)) then
      if (lats_mdl(ij) >= 0.) then
        snow_dep_mdl_tmp(ij,3) = snow_dep_mdl_tmp(ij,1)
      else
        snow_dep_mdl_tmp(ij,3) = snow_dep_mdl_tmp(ij,2)
      endif
    elseif (bitmap_mdl(ij,1) .and. .not. bitmap_mdl(ij,2)) then
      snow_dep_mdl_tmp(ij,3) = snow_dep_mdl_tmp(ij,1)
    elseif (.not. bitmap_mdl(ij,1) .and. bitmap_mdl(ij,2)) then
      snow_dep_mdl_tmp(ij,3) = snow_dep_mdl_tmp(ij,2)
    else
      if (abs(lats_mdl(ij)) >= lat_threshold) then
         snow_dep_mdl_tmp(ij,3) = min_snow_depth
      endif
    endif
  enddo

  deallocate(bitmap_mdl)

 end if AFWA_AVAIL

!-----------------------------------------------------------------------
! in the nh, merge the nesdis and afwa data into a single snow depth
! product.  in the sh, set the snow cover based on the presence of 
! snow in the afwa data.  also, put final interpolated data into
! 2-d arrays for output.
!-----------------------------------------------------------------------

 if (no_afwa) then
   print*,"- SET SNOW DEPTH TO DEFAULT VALUE BASED ON NESDIS SNOW COVER."
 else
   print*,"- MERGE NESDIS AND AFWA DATA INTO FINAL PRODUCT."
 end if
 
 allocate (snow_dep_mdl(imdl,jmdl))
 allocate (snow_cvr_mdl(imdl,jmdl))
 snow_cvr_mdl = 0.0
 snow_dep_mdl = 0.0

 do ij = 1, ijmdl
   NH_OR_SH : if (lats_mdl(ij) >= 0.0) then
     snow_cvr_mdl(ipts_mdl(ij),jpts_mdl(ij)) = snow_cvr_mdl_1d(ij)
     if (snow_cvr_mdl_1d(ij) >= snow_cvr_threshold) then
       snow_dep_mdl(ipts_mdl(ij),jpts_mdl(ij)) =   &
                max(snow_dep_mdl_tmp(ij,3), min_snow_depth)   
     end if
   else   ! southern hemisphere
     if (snow_dep_mdl_tmp(ij,3) > 0.0) then
         snow_cvr_mdl(ipts_mdl(ij),jpts_mdl(ij)) = 100.0
         snow_dep_mdl(ipts_mdl(ij),jpts_mdl(ij)) = snow_dep_mdl_tmp(ij,3)
     end if
   end if NH_OR_SH
 enddo

 deallocate (snow_dep_mdl_tmp, snow_cvr_mdl_1d)

!----------------------------------------------------------------------
! if a global model grid, and if running on thinned grid, then
! fill in non-processed mask points as is done in the model.
! "4" is grid indicator for a gaussian grid.
!----------------------------------------------------------------------

 if (kgds_mdl(1) == 4 .and. thinned) then
   call full_to_thin(lsmask_mdl, imdl, jmdl, lonsperlat_mdl)
   call full_to_thin(snow_dep_mdl, imdl, jmdl, lonsperlat_mdl)
   call full_to_thin(snow_cvr_mdl, imdl, jmdl, lonsperlat_mdl)
 end if

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

 kpds(5)  = 66  ! parameter number for snow depth
 kpds(22) = 3   ! scaling factor.  to nearest mm

 call putgb (lugb, (imdl*jmdl), kpds, kgds_mdl, lbms,  &
             snow_dep_mdl, iret)

 if (iret /= 0) then
   print*,'- ERROR WRITING OUTPUT GRIB FILE. IRET IS ', iret
   call w3tage('SNO2MDL')
   call errexit(57)
 end if
 
 call baclose(lugb, iret)

 return

 end subroutine gribit

 end module sno2mdl
 