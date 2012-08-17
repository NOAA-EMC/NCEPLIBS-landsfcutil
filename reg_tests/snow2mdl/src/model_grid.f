 module model_grid
!$$$  module documentation block
!            
! module:    model_grid
!   prgmmr: gayno         org: w/np2     date: 2005-dec-16
!
! abstract: read in data defining the model grid.
!     
! program history log:
!   2005-dec-16  gayno   - initial version
!   2007-nov-30  gayno   - improved method for thinning gfs grids.
!                          added nam b-grids.
!
! usage: use model_grid
!
! remarks: some variable definitions
!   grid_id_mdl    - grib id of model grid, 4-gaussian, 203-egrid
!   i/jpts_mdl     - i/j index of point on full grid
!   imdl           - i-dimension of model grid
!   jmdl           - j-dimension of model grid
!   ijmdl          - total number of model land points
!   kgds_mdl       - holds grib gds info of model grid
!   lats_mdl       - latitudes of model grid points
!   lons_mdl       - longitudes of model grid points
!   lonsperlat     - for global grids, the number of i points
!                    in each row (decrease toward pole)
!   lsmask_mdl     - land mask of model grid (0 - non land, 1-land)
!                    for global grids run thinned, will contain
!                    a modified version of the original mask
!                    that has land at all points encompassed by a 
!                    thinned point
!   lsmask_mdl_sav - saved copy of land mask of model grid (0 - non land, 1-land)
!                    only used for global thinned grids.
!   resol_mdl      - approximate model resolution in km.
!   thinned        - when true, global grids will run thinned
!                    (# i points decrease toward pole)
!
! attributes:
!   language: fortran 90
!   machine:  ibm sp
!
!$$$

 use program_setup, only         : model_lsmask_file, &
                                   model_lon_file, &
                                   model_lat_file

 use consts, only                : lonsperlat_t62,    &
                                   lonsperlat_t126,   &
                                   lonsperlat_t170,   &
                                   lonsperlat_t190,   &
                                   lonsperlat_t254,   &
                                   lonsperlat_t382,   &
                                   lonsperlat_t574,   &
                                   lonsperlat_t878

 integer                        :: grid_id_mdl
 integer                        :: imdl
 integer                        :: jmdl
 integer                        :: ijmdl ! only land points
 integer, allocatable           :: ipts_mdl(:), jpts_mdl(:) 

 integer                        :: kgds_mdl(200)
 integer, allocatable           :: lonsperlat_mdl (:)
 
! logical, parameter             :: thinned = .false.
 logical, parameter             :: thinned = .true.

 real, allocatable              :: lats_mdl    (:)
 real, allocatable              :: lons_mdl    (:)
 real, allocatable              :: lsmask_mdl  (:,:)
 real, allocatable              :: lsmask_mdl_sav (:,:)
 real                           :: resol_mdl  ! in km

 contains

 subroutine read_mdl_grid_info
!$$$  subprogram documentation block
!              
! subprogram:    read_mdl_grid_info
!   prgmmr: gayno          org: w/np2     date: 2005-dec-16
!
! abstract: read latitude, longitude, land/sea mask on the
!   model grid.
!
! program history log:
! 2005-dec-16  gayno    - initial version
! 2007-nov-30  gayno    - improved method for thinning gfs grids
!                         added nam b-grids
!
! usage: call read_mdl_grid_info
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

 character*150           :: fngrib

!cggg need to distinquish between t574 and t878 which have the same dimensions
 integer :: grid

 integer                 :: i, j, ij, jj
 integer                 :: ii, iii, istart, iend, imid
 integer                 :: iret
 integer, parameter      :: iunit = 14  ! unit of grib file
 integer                 :: jgds(200)
 integer                 :: jpds(200)
 integer                 :: lskip
 integer, parameter      :: lugi = 0    ! unit of grib index file - not used
 integer                 :: kgds(200)
 integer                 :: kpds(200)
 integer                 :: message_num
 integer                 :: numbytes
 integer                 :: numpts

 logical*1, allocatable  :: lbms(:)

 real                    :: gridis, gridie, fraction, x1, r
 real, allocatable       :: lats_mdl_temp  (:,:)
 real, allocatable       :: lons_mdl_temp  (:,:)
 
 print*,"- READ MODEL GRID INFORMATION"

!-----------------------------------------------------------------------
! read latitudes on the model grid
!-----------------------------------------------------------------------

 fngrib = model_lat_file

 print*,"- OPEN MODEL LAT FILE ", trim(fngrib)
 call baopenr (iunit, fngrib, iret)

 if (iret /= 0) then
   print*,'- BAD OPEN, IRET IS ', iret
   call w3tage('SNO2MDL')
   call errexit(80)
 end if

!-----------------------------------------------------------------------
! tell degribber to search for latitudes
!-----------------------------------------------------------------------

 lskip   = -1  ! read beginning of file
 jgds    = -1
 jpds    = -1
 jpds(5) = 176 ! latitude
 kgds    = -1   
 kpds    = -1  

 print*,"- GET GRIB HEADER"
 call getgbh(iunit, lugi, lskip, jpds, jgds, numbytes,  &
             numpts, message_num, kpds, kgds, iret)

 if (iret /= 0) then
   print*,'- BAD READ OF GRIB HEADER. IRET IS ', iret
   call w3tage('SNO2MDL')
   call errexit(81)
 end if

!-----------------------------------------------------------------------
! save gds for gribbing the interpolated data later.
!-----------------------------------------------------------------------

 kgds_mdl = kgds

!-----------------------------------------------------------------------
! get model specs from header.
!-----------------------------------------------------------------------

 imdl = kgds(2)  ! i-dimension of model grid
 jmdl = kgds(3)  ! j-dimension of model grid

 grid_id_mdl = kpds(3) ! grib grid id number. sect 1, oct 7

!cggg
 grid=kpds(3)

!-----------------------------------------------------------------------
! model resolution (km) is used to determine the type of interpolation.
!-----------------------------------------------------------------------

 if (kgds(1) == 4) then  ! gaussian grid
   resol_mdl = float(kgds(9)) / 1000.0 * 111.0
 else if (kgds(1) == 203) then  ! e-grid 
   resol_mdl = sqrt( (float(kgds(9)) / 1000.0)**2   +    &
                   (float(kgds(10)) / 1000.0)**2  )
   resol_mdl = resol_mdl * 111.0
 else if (kgds(1) == 205) then  ! b-grid 
   resol_mdl = ((float(kgds(9)) / 1000.0) + (float(kgds(10)) / 1000.0)) &
                * 0.5 * 111.0
 else
   print*,'- UNRECOGNIZED MODEL GRID'
   call w3tage('SNO2MDL')
   call errexit(79)
 end if

 allocate(lats_mdl_temp(imdl,jmdl))
 allocate(lbms(imdl*jmdl))

 print*,"- DEGRIB DATA"
 call getgb(iunit, lugi, (imdl*jmdl), lskip, jpds, jgds, &
            numpts, message_num, kpds, kgds, lbms, lats_mdl_temp, iret)

 if (iret /= 0) then
   print*,'- BAD DEGRIB OF FILE. IRET IS ',iret
   call w3tage('SNO2MDL')
   call errexit(82)
 end if

 call baclose(iunit,iret)

!-----------------------------------------------------------------------
! read longitudes on the model grid.
!-----------------------------------------------------------------------

 fngrib = model_lon_file

 print*,"- OPEN MODEL LON FILE ", trim(fngrib)
 call baopenr (iunit, fngrib, iret)

 if (iret /= 0) then
   print*,"- BAD OPEN. IRET IS ", iret
   call w3tage('SNO2MDL')
   call errexit(83)
 end if

 lskip   = -1  
 kgds    = -1   
 kpds    = -1  
 jgds    = -1
 jpds    = -1
 jpds(5) = 177  ! longitude 

 allocate(lons_mdl_temp(imdl,jmdl))

 print*,"- DEGRIB DATA"
 call getgb(iunit, lugi, (imdl*jmdl), lskip, jpds, jgds, &
            numpts, message_num, kpds, kgds, lbms, lons_mdl_temp, iret)

 if (iret /= 0) then
   print*,'- BAD DEGRIB OF DATA. IRET IS ',iret
   call w3tage('SNO2MDL')
   call errexit(84)
 end if

 call baclose(iunit, iret)

!-----------------------------------------------------------------------
! read model land/sea mask. 
!-----------------------------------------------------------------------

 fngrib = model_lsmask_file

 print*,"- OPEN MODEL LANDMASK FILE ", trim(fngrib)
 call baopenr (iunit, fngrib, iret)

 if (iret /= 0) then
   print*,'- BAD OPEN OF FILE. IRET IS ', iret
   call w3tage('SNO2MDL')
   call errexit(85)
 end if

 lskip   = -1 
 kgds    = -1  
 kpds    = -1 
 jpds    = -1
 jgds    = -1
 jpds(5) = 81   ! land-sea mask

 allocate(lsmask_mdl(imdl,jmdl))

 print*,"- DEGRIB DATA"
 call getgb(iunit, lugi, (imdl*jmdl), lskip, jpds, jgds, &
            numpts, message_num, kpds, kgds, lbms, lsmask_mdl, iret)

 if (iret /= 0) then
   print*,'- BAD DEGRIB OF DATA. IRET IS ',iret
   call w3tage('SNO2MDL')
   call errexit(86)
 end if

 call baclose(iunit,iret)

 deallocate (lbms)

!-----------------------------------------------------------------------
! global model runs on a thinned grid (# grid points decreases
! towards the poles).  if thinned logical is set, and this is a
! gaussian grid, modify the land/sea mask to account for the
! fact that delta x increases toward the poles.
!-----------------------------------------------------------------------

 if (kgds(1) == 4 .and. thinned) then

   print*,"- RUNNING A THINNED GRID"

   allocate (lonsperlat_mdl(jmdl/2))

   if (imdl == 192 .and. jmdl == 94) then      ! t62 grid
     lonsperlat_mdl = lonsperlat_t62
   elseif (imdl == 384 .and. jmdl == 190) then ! t126 grid
     lonsperlat_mdl = lonsperlat_t126
   elseif (imdl == 512 .and. jmdl == 256) then ! t170 grid
     lonsperlat_mdl = lonsperlat_t170
   elseif (imdl == 576 .and. jmdl == 288) then ! t190 grid
     lonsperlat_mdl = lonsperlat_t190
   elseif (imdl == 768 .and. jmdl == 384) then ! t254 grid
     lonsperlat_mdl = lonsperlat_t254
   elseif (imdl == 1152 .and. jmdl == 576) then ! t382 grid
     lonsperlat_mdl = lonsperlat_t382
   elseif (imdl == 1760 .and. jmdl == 880 .and. grid==9) then ! t574 grid
     lonsperlat_mdl = lonsperlat_t574
   print*,'574 lonsperlat'
   elseif (imdl == 1760 .and. jmdl == 880) then ! t878 grid
     lonsperlat_mdl = lonsperlat_t878
   print*,'878 lonsperlat'
   else
     print*,'- UNKNOWN GLOBAL GRID. ABORT.'
     call w3tage('SNO2MDL')
     call errexit(76)
   end if

   allocate (lsmask_mdl_sav(imdl,jmdl))
   lsmask_mdl_sav = lsmask_mdl
   lsmask_mdl = 0.0   ! this will identify land points to be processed by
                      ! the ipolates routines.

!-----------------------------------------------------------------------
! loop over every point on the thinned grid.  calculate the start/end
! bounds with respect to the full grid in the 'i' direction.  if
! the thinned point contains land, set all full grid points within
! the bounds to be land.  this modified mask will identify the
! points to be processed by ipolates.  after the call to ipolates,
! the thinned points will be set to a linear weighting of the full points
! located within the thinned point.
!-----------------------------------------------------------------------

   do j = 1, jmdl
     jj = j
     if (j > jmdl/2) jj = jmdl - j + 1
     r = float(imdl)/ float(lonsperlat_mdl(jj))
     do i = 1, lonsperlat_mdl(jj)
       x1=float(i-1)*r
       imid = nint(x1+1.0)  ! for this thinned grid point, this is
                            ! the nearest 'i' index on the full grid.
       if (lsmask_mdl_sav(imid,j) > 0.0) then
         gridis = x1+1.0-r/2.
         istart = nint(gridis)
         gridie = x1+1.0+r/2.
         iend   = nint(gridie)
         do ii = istart, iend
           if (ii == istart) then
             fraction = 0.5 - (gridis - float(istart))
             if (fraction < 0.0001) cycle
           endif
           if (ii == iend) then
             fraction = 0.5 + (gridie - float(iend))
             if (fraction < 0.0001) cycle
           endif
           iii = ii
           if (iii < 1) iii = imdl + iii
           lsmask_mdl(iii,j) = lsmask_mdl_sav(imid,j)
         enddo
       endif
     enddo
   enddo
  
 end if

!-----------------------------------------------------------------------
! program only worries about land points.   save i/j coordinate
! with respect to 2-d grid.
!-----------------------------------------------------------------------

 ij = 0

 do j = 1, jmdl
 do i = 1, imdl
   if (lsmask_mdl(i,j) > 0.0) then
     ij = ij+1
   end if
 enddo
 enddo

 ijmdl = ij

 allocate (lats_mdl(ijmdl))
 allocate (lons_mdl(ijmdl))
 allocate (ipts_mdl(ijmdl))
 allocate (jpts_mdl(ijmdl))

 ij = 0
 do j = 1, jmdl
 do i = 1, imdl
   if (lsmask_mdl(i,j) > 0.0) then
     ij = ij+1
     lats_mdl(ij) = lats_mdl_temp(i,j)
     lons_mdl(ij) = lons_mdl_temp(i,j)
     ipts_mdl(ij) = i
     jpts_mdl(ij) = j
   end if
 enddo
 enddo

 deallocate (lats_mdl_temp, lons_mdl_temp)

 return

 end subroutine read_mdl_grid_info

 subroutine model_grid_cleanup
!$$$  subprogram documentation block
!              
! subprogram:    model_grid_cleanup
!   prgmmr: gayno          org: w/np2     date: 2005-dec-16
!
! abstract: this deallocate this module's allocatable array.
!
! program history log:
! 2005-dec-16  gayno    - initial version
!
! usage: call model_grid_cleanup
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

 if (allocated(lsmask_mdl))     deallocate(lsmask_mdl)
 if (allocated(lats_mdl))       deallocate(lats_mdl)
 if (allocated(lons_mdl))       deallocate(lons_mdl)
 if (allocated(lonsperlat_mdl)) deallocate(lonsperlat_mdl)
 if (allocated(ipts_mdl))       deallocate(ipts_mdl)
 if (allocated(jpts_mdl))       deallocate(jpts_mdl)
  
 return

 end subroutine model_grid_cleanup

 end module model_grid
