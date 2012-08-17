 module snowdat
!$$$  module documentation block
!
! module:    snowdat
!   prgmmr: gayno         org: w/np2     date: 2005-dec-16
!
! abstract: read in afwa and nesdis snow data.
!
! program history log:
!   2005-dec-16  gayno   - initial version
!   2007-aug-10  gayno   - allow program to run with no nesdis data
!                          add 16th mesh afwa grib data
!   2008-feb-04  gayno   - add autosnow cover data for sh.
!   2009-jun-03  gayno   - add qc check for nesdis/afwa data.
!
! usage: use snowdat
!
! remarks: some variable definitions
!   afwa_res           - resolution of afwa data in km
!   autosnow_res       - resolution of autosnow in km
!   bad_afwa_Xh        - is afwa data corrupt?
!   bad_nesdis         - is nesdis ims data corrupt?
!   bitmap_afwa_Xh     - bitmap of afwa grid (false-non land, true-land)
!   bitmap_nesdis      - bitmap of nesdis grid (false-non land, true-land)
!   iafwa              - i-dimension of afwa grid
!   jafwa              - j-dimension of afwa grid
!   iautosnow          - i-dimension of autosnow grid
!   jautosnow          - j-dimension of autosnow grid
!   inesdis            - i-dimension of nesdis grid
!   jnesdis            - j-dimension of nesdis grid
!   kgds_afwa_Xh       - afwa grid description section (grib section 2)
!   kgds_autosnow      - autosnow grid description section (grib section 2)
!   kgds_nesdis        - nesdis grid description section (grib section 2)
!   mesh_nesdis        - nesdis data is 96th mesh (or bediant)
!   nesdis_res         - resolution of nesdis data in km
!   sea_ice_nesdis     - nesdis sea ice flag (0-open water, 1-ice)
!   snow_cvr_autosnow  - autosnow snow cover flag (0-no, 100-yes)
!   snow_cvr_nesdis    - nesdis snow cover flag (0-no, 100-yes)
!   snow_dep_afwa_Xh   - afwa snow depth data (inches*10 on input, 
!                                              meters on output)
!   use_xh_afwa        - true if afwa data to be used
!   use_autosnow       - true if autosnow data to be used
!   use_nesdis         - true if nesdis data to be used
!
! attributes:
!   language: fortran 90
!   machine:  ibm sp
!
!$$$

 use program_setup, only  : autosnow_file,       &
                            nesdis_snow_file,    &
                            nesdis_lsmask_file,  &
                            afwa_snow_nh_file,   &
                            afwa_snow_sh_file,   &
                            afwa_lsmask_nh_file, &
                            afwa_lsmask_sh_file

 use model_grid, only     : imdl,                &
                            jmdl

 integer                 :: iafwa
 integer                 :: iautosnow
 integer                 :: inesdis 
 integer                 :: jafwa
 integer                 :: jautosnow
 integer                 :: jnesdis
 integer                 :: kgds_afwa_nh(200)
 integer                 :: kgds_afwa_nh_8th(200)
 integer                 :: kgds_afwa_sh(200)
 integer                 :: kgds_afwa_sh_8th(200)
 integer                 :: kgds_autosnow(200)
 integer                 :: kgds_nesdis(200)
 integer                 :: mesh_nesdis
 integer*1, allocatable  :: sea_ice_nesdis(:,:)  

 logical                 :: bad_afwa_nh, bad_afwa_sh, bad_nesdis
 logical*1, allocatable  :: bitmap_afwa_nh(:,:)
 logical*1, allocatable  :: bitmap_afwa_sh(:,:)
 logical*1, allocatable  :: bitmap_nesdis(:,:)
 logical*1, allocatable  :: bitmap_autosnow(:,:)
 logical                 :: use_nh_afwa, use_sh_afwa
 logical                 :: use_autosnow, use_nesdis

 real                    :: autosnow_res  ! in km
 real                    :: afwa_res  ! in km
 real                    :: nesdis_res
 real, allocatable       :: snow_cvr_nesdis(:,:)  
 real, allocatable       :: snow_cvr_autosnow(:,:)  
 real, allocatable       :: snow_dep_afwa_nh(:,:) 
 real, allocatable       :: snow_dep_afwa_sh(:,:) 

! the afwa 8th mesh binary data has no grib header, so set it from these
! data statements. needed for ipolates routines.

 data kgds_afwa_nh_8th/5,2*512,-20826,145000,8,-80000,2*47625,0,  &
                       9*0,255,180*0/
 data kgds_afwa_sh_8th/5,2*512,20826,-125000,8,-80000,2*47625,128, &
                       9*0,255,180*0/

 contains

 subroutine readautosnow
!$$$  subprogram documentation block
!
! subprogram:    readautosnow
!   prgmmr: gayno          org: w/np2     date: 2008-feb-04
!
! abstract:  read autosnow snow cover. 
!
! program history log:
! 2008-feb-04  gayno    - initial version
!
! usage: call readautosnow
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! remarks: autosnow data available only for southern hemis.
!          autosnow data is in grib 2.          
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 use grib_mod  ! grib 2 libraries

 implicit none

 type(gribfield)            :: gfld

 integer                    :: iret, j, k, lugb, lugi
 integer                    :: jdisc, jgdtn, jpdtn
 integer                    :: jids(200), jgdt(200), jpdt(200)

 logical                    :: unpack

 real                       :: ratio

 use_autosnow = .true.

 if ( len_trim(autosnow_file) == 0 ) then
   print*,"- WILL NOT USE AUTOSNOW DATA."
   use_autosnow = .false.
   return
 end if

 print*,"- OPEN AND READ AUTOSNOW FILE ", trim(autosnow_file)

 lugb=10
 call baopenr(lugb,autosnow_file,iret)

 if (iret /= 0) then
   print*,'- BAD OPEN OF FILE, IRET IS ', iret
   call w3tage('SNO2MDL')
   call errexit(74)
 endif

 j       = 0      ! search at beginning of file
 lugi    = 0      ! no grib index file
 jdisc   = 0      ! search for discipline; 0 - meteorological products
 jpdtn   = 30     ! search for product definition template number; 30 - satellite product
 jgdtn   = 0      ! search for grid definition template number; 0 - lat/lon grid
 jids    = -9999  ! array of values in identification section, set to wildcard
 jgdt    = -9999  ! array of values in grid definiation template 3.m
 jpdt    = -9999  ! array of values in product definition template 4.n
 jpdt(1) = 1      ! search for parameter category - moisture
 jpdt(2) = 42     ! search for parameter number - snow cover in percent.
 unpack  = .true. ! unpack data

 call getgb2(lugb, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
             unpack, k, gfld, iret)

 if (iret /=0) then
  print*,'- BAD READ OF FILE, IRET IS ', iret
  call w3tage('SNO2MDL')
  call errexit(75)
 endif

 print*,"- TIME OF DATA (YYYYMMDDHH): ", gfld%idsect(6),gfld%idsect(7), &
                                         gfld%idsect(8),gfld%idsect(9)

 call baclose (lugb, iret)

 iautosnow =  gfld%igdtmpl(8)   ! i/j dimensions of grid
 jautosnow =  gfld%igdtmpl(9)

!-------------------------------------------------------------------
! ipolates requires the grib 1 kgds array.  so populate it from
! the corresponding grib 2 array.  assumes autosnow is on
! a regular lat/lon grid.
!-------------------------------------------------------------------

 kgds_autosnow     = 0
 kgds_autosnow(1)  = 0
 kgds_autosnow(2)  = iautosnow     ! num pts i direction
 kgds_autosnow(3)  = jautosnow     ! num pts j direction

!-------------------------------------------------------------------
! grib 1 stores gds info to 3 decimal places.  grib 2 allows 
! the user to define the number of decimal places.  
!-------------------------------------------------------------------
  
 ratio = float(gfld%igdtmpl(10))/float(gfld%igdtmpl(11))*1.E3

 kgds_autosnow(4)  =  nint(gfld%igdtmpl(12)*ratio)  ! lat of first point
 kgds_autosnow(5)  =  nint(gfld%igdtmpl(13)*ratio)  ! lon of first point
 kgds_autosnow(6)  =  gfld%igdtmpl(14)              ! resol and component flag
 kgds_autosnow(7)  =  nint(gfld%igdtmpl(15)*ratio)  ! lat of last point
 kgds_autosnow(8)  =  nint(gfld%igdtmpl(16)*ratio)  ! lon of last point
 kgds_autosnow(9)  =  nint(gfld%igdtmpl(17)*ratio)  ! dx
 kgds_autosnow(10) =  nint(gfld%igdtmpl(18)*ratio)  ! dy
 kgds_autosnow(11) =  gfld%igdtmpl(19)              ! scanning mode flag 
 kgds_autosnow(19) =  0        ! num vertical coordinate parameters
 kgds_autosnow(20) =  255      ! set to 255

 autosnow_res = gfld%igdtmpl(17) * float(gfld%igdtmpl(10)) / &
                float(gfld%igdtmpl(11)) * 111.0 ! approx. resolution in km
 
 allocate (bitmap_autosnow(iautosnow,jautosnow))
 bitmap_autosnow = reshape (gfld%bmap , (/iautosnow,jautosnow/) )
 
 allocate (snow_cvr_autosnow(iautosnow,jautosnow))
 snow_cvr_autosnow = reshape (gfld%fld , (/iautosnow,jautosnow/) )

 call gf_free(gfld)  ! free up memory

 end subroutine readautosnow

 subroutine readnesdis
!$$$  subprogram documentation block
!
! subprogram:    readnesdis
!   prgmmr: gayno          org: w/np2     date: 2005-dec-16
!
! abstract:  read nesdis snow cover/ice data. 
!
! program history log:
! 2005-dec-16  gayno    - initial version
!
! usage: call readnesdis
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! remarks: nesdis data available only for n hemis.
!          
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 implicit none

 integer, parameter         :: iunit = 13  ! grib file unit number

 integer*4, allocatable     :: dummy4(:,:)
 integer                    :: i, j
 integer                    :: iret
 integer                    :: jgds(200)
 integer                    :: jpds(200)
 integer                    :: lskip
 integer, parameter         :: lugi = 0    ! grib index file unit number - not used
 integer                    :: kgds(200)
 integer                    :: kpds(200)
 integer                    :: message_num
 integer                    :: numbytes
 integer                    :: numpts

 real, allocatable          :: dummy(:,:)
 
 use_nesdis = .true.

 if ( len_trim(nesdis_snow_file) == 0 ) then
   print*,"- WILL NOT USE NESDIS DATA."
   use_nesdis = .false.
   return
 end if

 print*,"- OPEN AND READ NESDIS IMS SNOW FILE ", trim(nesdis_snow_file)

 call baopenr (iunit, nesdis_snow_file, iret)

 if (iret /= 0) then
   print*,'- BAD OPEN OF FILE, IRET IS ', iret
   call w3tage('SNO2MDL')
   call errexit(73)
 end if

!-----------------------------------------------------------------------
! tell degribber to look for requested data.
!-----------------------------------------------------------------------

 lskip    = -1
 jpds     = -1
 jgds     = -1
 jpds(5)  = 91     ! ice cover
 kpds     = jpds
 kgds     = jgds

 print*,"- GET GRIB HEADER"

 call getgbh(iunit, lugi, lskip, jpds, jgds, numbytes,  &
             numpts, message_num, kpds, kgds, iret)

 if (iret /= 0) then
   print*,"- BAD DEGRIB OF HEADER. IRET IS ", iret
   call w3tage('SNO2MDL')
   call errexit(72)
 end if

 kgds_nesdis = kgds
 inesdis     = kgds(2)
 jnesdis     = kgds(3)

 mesh_nesdis = inesdis / 64
 nesdis_res  = 381. / float(mesh_nesdis)   ! in km

 print*,"- DATA VALID AT (YYMMDDHH): ", kpds(8:11)
 
 allocate (dummy(inesdis,jnesdis))
 allocate (sea_ice_nesdis(inesdis,jnesdis))
 allocate (bitmap_nesdis(inesdis,jnesdis))

 print*,"- DEGRIB SEA ICE."

 call getgb(iunit, lugi, (inesdis*jnesdis), lskip, jpds, jgds, &
            numpts, lskip, kpds, kgds, bitmap_nesdis, dummy, iret)

 if (iret /= 0) then
   print*,"- BAD DEGRIB OF DATA. IRET IS ", iret
   call w3tage('SNO2MDL')
   call errexit(71)
 end if

 sea_ice_nesdis = nint(dummy)  ! only needed as yes/no flag
 deallocate (dummy)

 lskip    = -1
 jpds     = -1
 jgds     = -1
 jpds(5)  = 238     ! snow cover
 kpds     = jpds
 kgds     = jgds

 allocate (snow_cvr_nesdis(inesdis,jnesdis))

 print*,"- DEGRIB SNOW COVER."

 call getgb(iunit, lugi, (inesdis*jnesdis), lskip, jpds, jgds, &
            numpts, lskip, kpds, kgds, bitmap_nesdis, snow_cvr_nesdis, iret)

 if (iret /= 0) then
   print*,"- BAD DEGRIB OF DATA. IRET IS ", iret
   call w3tage('SNO2MDL')
   call errexit(70)
 end if

 call baclose(iunit,iret)

!-----------------------------------------------------------------------
! the 16th mesh nesdis grib data does not have a proper
! bitmap section.  therefore, need to read in the mask
! from file.  but the 96th mesh data has a proper bitmap, so use it.
!-----------------------------------------------------------------------

 if (mesh_nesdis == 16) then

   print*,"- OPEN NESDIS 16TH MESH LAND MASK: ", trim(nesdis_lsmask_file)

   open(43, file=trim(nesdis_lsmask_file), form="formatted", &
        iostat = iret)

   if (iret /= 0) then
     print*,"- ERROR OPENING NESDIS LAND MASK FILE. ISTAT IS: ", iret
     call errexit(87)
   end if

   print*,"- READ NESDIS 16TH MESH LAND MASK."

   allocate (dummy4(inesdis,jnesdis))
   
   do j = 1, 1024
     read(43, 123, iostat=iret) (dummy4(i,j),i=1,1024)
     if (iret /= 0) then
       print*,"- ERROR READING NESDIS LAND MASK FILE. ISTAT IS: ", iret
       call errexit(88)
     end if
   enddo

   close (43)

!-----------------------------------------------------------------------
! the file has 0-sea, 1-land, 9-off hemi.  this code expects
! 0-non-land (or don't use data), 1-land (use data).
!-----------------------------------------------------------------------

   bitmap_nesdis=.false.
   do j = 1, 1024
     do i = 1, 1024
       if (dummy4(i,j) == 1) bitmap_nesdis(i,j) = .true.
     enddo
   enddo
 
   deallocate(dummy4)

123 FORMAT(80I1)

 endif

 bad_nesdis=.false.
 call nh_climo_check(kgds_nesdis,snow_cvr_nesdis,bitmap_nesdis,inesdis,jnesdis,2)

!-----------------------------------------------------------------------
! for the 2009 nmm-b implementation, it was decided to not run with
! afwa only.  so even if afwa data is current and not corrupt, 
! but the ims is bad, then abort program. exception, if ims is very old
! (there is a catastropic outage) then program will run with afwa
! only.  this is done by setting the nesdis_snow_file variable to
! a zero length string (i.e., ims data not selected).  this variable
! setting is accomplished in the run script. 
!-----------------------------------------------------------------------

 if (bad_nesdis) then
   print*,'- *** IMS DATA BAD, DO NOT USE.'
   print*,'- *** DONT RUN PROGRAM.'
   use_nesdis=.false.
   call w3tage('SNO2MDL')
   call errexit(54)
   stop
 endif

 return

 end subroutine readnesdis

 subroutine readafwa
!$$$  subprogram documentation block
!
! subprogram:    readafwa
!   prgmmr: gayno          org: w/np2     date: 2005-dec-16
!
! abstract:  read nh and sh afwa snow depth data and
!   land sea mask. 
!
! program history log:
! 2005-dec-16  gayno    - initial version
! 2007-nov-28  gayno    - read 16th mesh afwa data in grib format
!
! usage: call readafwa
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! remarks: read logic for binary data taken from hua-lu's code:
!          /nwprod/sorc/grib_snowgrib.fd
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 implicit none

 integer                       :: jgds(200), jpds(200), kgds(200), kpds(200)
 integer                       :: istat
 integer                       :: lugi, lskip, numbytes, numpts, message_num

 logical                       :: isgrib

 bad_afwa_nh=.false.
 bad_afwa_sh=.false.

 use_nh_afwa = .true.
 use_sh_afwa = .true.

 if (len_trim(afwa_snow_nh_file) == 0 .and.   &
     len_trim(afwa_snow_sh_file) == 0 ) then
   print*,"- WILL NOT USE AFWA DATA."
   use_nh_afwa = .false.
   use_sh_afwa = .false.
   return
 end if

 if ( len_trim(afwa_snow_nh_file) > 0 ) then  ! afwa nh data selected

   call grib_check(afwa_snow_nh_file, isgrib)

   if (.not. isgrib) then ! old ncep binary format

     iafwa = 512
     jafwa = 512
     afwa_res = 47.625   ! in kilometers
     kgds_afwa_nh = kgds_afwa_nh_8th

     allocate (snow_dep_afwa_nh(iafwa,jafwa))
     call read_afwa_binary(afwa_snow_nh_file, snow_dep_afwa_nh)

     allocate (bitmap_afwa_nh(iafwa,jafwa))
     call read_afwa_mask(afwa_lsmask_nh_file, bitmap_afwa_nh) 

   else ! afwa data is grib

     print*,"- OPEN AND READ AFWA SNOW FILE ", trim(afwa_snow_nh_file)

     call baopenr (11, afwa_snow_nh_file, istat)

     if (istat /= 0) then
       print*,'- BAD OPEN OF FILE, ISTAT IS ', istat
       call w3tage('SNO2MDL')
       call errexit(60)
     end if

!-----------------------------------------------------------------------
! tell degribber to look for requested data.
!-----------------------------------------------------------------------

     lugi     = 0
     lskip    = -1
     jpds     = -1
     jgds     = -1
     jpds(5)  = 66     ! snow depth
     kpds     = jpds
     kgds     = jgds

     print*,"- GET GRIB HEADER"
     call getgbh(11, lugi, lskip, jpds, jgds, numbytes,  &
                 numpts, message_num, kpds, kgds, istat)

     if (istat /= 0) then
       print*,"- BAD DEGRIB OF HEADER. ISTAT IS ", istat
       call w3tage('SNO2MDL')
       call errexit(61)
     end if

     iafwa = kgds(2)
     jafwa = kgds(3)
     afwa_res = float(kgds(8))*0.001  ! in km.  

     print*,"- DATA VALID AT (YYMMDDHH): ", kpds(8:11)

     print*,"- DEGRIB SNOW DEPTH."

     allocate(bitmap_afwa_nh(iafwa,jafwa))
     allocate(snow_dep_afwa_nh(iafwa,jafwa))

     call getgb(11, lugi, (iafwa*jafwa), lskip, jpds, jgds, &
                numpts, lskip, kpds, kgds, bitmap_afwa_nh, snow_dep_afwa_nh, istat)

     if (istat /= 0) then
       print*,"- BAD DEGRIB OF DATA. ISTAT IS ", istat
       call w3tage('SNO2MDL')
       call errexit(61)
     end if

     kgds_afwa_nh = kgds

     kgds_afwa_nh(7) = -80000  ! ipolates definition of orientation angle is
                               ! 180 degrees off from grib standard.

     call baclose(11, istat) 

   endif ! is nh afwa data grib?

   call nh_climo_check(kgds_afwa_nh,snow_dep_afwa_nh,bitmap_afwa_nh,iafwa,jafwa,1)

 else

   use_nh_afwa=.false.

 endif

!-----------------------------------------------------------------------
! now, read southern hemisphere data.
!-----------------------------------------------------------------------

 if ( len_trim(afwa_snow_sh_file) > 0 ) then

   call grib_check(afwa_snow_sh_file, isgrib)

   if (.not. isgrib) then ! old ncep binary format

     iafwa = 512
     jafwa = 512
     afwa_res = 47.625
     kgds_afwa_sh = kgds_afwa_sh_8th

     allocate (snow_dep_afwa_sh(iafwa,jafwa))
     call read_afwa_binary(afwa_snow_sh_file, snow_dep_afwa_sh)

     allocate (bitmap_afwa_sh(iafwa,jafwa))
     call read_afwa_mask(afwa_lsmask_sh_file, bitmap_afwa_sh) 

   else   ! sh afwa data is grib

     print*,"- OPEN AND READ AFWA SNOW FILE ", trim(afwa_snow_sh_file)

     call baopenr (11, afwa_snow_sh_file, istat)

     if (istat /= 0) then
       print*,'- BAD OPEN OF FILE, ISTAT IS ', istat
       call w3tage('SNO2MDL')
       call errexit(60)
     end if

!-----------------------------------------------------------------------
! tell degribber to look for requested data.
!-----------------------------------------------------------------------

     lugi     = 0
     lskip    = -1
     jpds     = -1
     jgds     = -1
     jpds(5)  = 66     ! snow cover
     kpds     = jpds
     kgds     = jgds

     print*,"- GET GRIB HEADER"
     call getgbh(11, lugi, lskip, jpds, jgds, numbytes,  &
                 numpts, message_num, kpds, kgds, istat)

     if (istat /= 0) then
       print*,"- BAD DEGRIB OF HEADER. ISTAT IS ", istat
       call w3tage('SNO2MDL')
       call errexit(61)
     end if

     iafwa = kgds(2)
     jafwa = kgds(3)
     afwa_res = float(kgds(8))*0.001  ! in km.  

     print*,"- DATA VALID AT (YYMMDDHH): ", kpds(8:11)

     print*,"- DEGRIB SNOW DEPTH."

     allocate(bitmap_afwa_sh(iafwa,jafwa))
     allocate(snow_dep_afwa_sh(iafwa,jafwa))

     call getgb(11, lugi, (iafwa*jafwa), lskip, jpds, jgds, &
                numpts, lskip, kpds, kgds, bitmap_afwa_sh, snow_dep_afwa_sh, istat)

     if (istat /= 0) then
       print*,"- BAD DEGRIB OF DATA. ISTAT IS ", istat
       call w3tage('SNO2MDL')
       call errexit(61)
     end if

     kgds_afwa_sh = kgds

     kgds_afwa_sh(7) = -80000  ! ipolates definition of orientation angle is
                               ! 180 degrees off from grib standard.

     call baclose(11, istat)

   endif  ! is sh afwa data grib or not?

   call afwa_check_sh

 else

   use_sh_afwa = .false.

 endif

!-------------------------------------------------------------------
!if either hemisphere is bad, don't trust all hemispheres
!-------------------------------------------------------------------

 if (bad_afwa_nh .or. bad_afwa_sh) then
   print*,'- *** AFWA DATA BAD, DO NOT USE.'
   use_nh_afwa = .false.
   use_sh_afwa = .false.
 endif

 return

 end subroutine readafwa 

 subroutine nh_climo_check(kgds_data,snow_data,bitmap_data,idata,jdata,isrc)
!$$$  subprogram documentation block
!
! subprogram:    nh_climo_check
!   prgmmr: gayno          org: w/np2     date: 2009-jun-3
!
! abstract:  check for corrupt nh data by comparing it
!            to climatology
!  
! program history log:
! 2009-jun-3   gayno    - initial version
!
! usage: call nh_climo_check
!
!   input argument list:  kgds_data, snow_data, bitmap_data,
!                         idata, jdata, isrc
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

 use program_setup, only    : climo_qc_file,  &
                              grib_year, grib_month, grib_day, &
                              grib_century


 implicit none

! describes the climo data grid.
 integer, parameter        :: iclim = 1080
 integer, parameter        :: jclim = 270
 real,  parameter          :: lat11_clim = 90.0
 real,  parameter          :: lon11_clim = -180.0
 real,  parameter          :: dx_clim = 1./3.
 real,  parameter          :: dy_clim = 1./3.

 integer, intent(in)       :: idata, jdata, kgds_data(200), isrc
 logical*1, intent(in)     :: bitmap_data(idata,jdata)
 real,      intent(in)     :: snow_data(idata,jdata)

! local variables
 integer                   :: idat(8), jdow, jdoy, jday
 integer                   :: century, year, week, iret, lugb, i, j, ii, jj
 integer                   :: lskip, jpds(200), jgds(200), kpds(200), kgds(200)
 integer                   :: numbytes, numpts, message_num, nret
 integer                   :: count_nosnow_climo, count_nosnow_data
 integer                   :: count_snow_climo, count_snow_data, count_grosschk_data

 logical*1, allocatable    :: bitmap_clim(:,:)

 real, allocatable         :: climo(:,:)
 real                      :: fill, percent, x, y
 real, allocatable         :: xpts(:,:),ypts(:,:),rlon_data(:,:),rlat_data(:,:), &
                              crot(:,:),srot(:,:)
 real                      :: thresh_gross, thresh

 if (len_trim(climo_qc_file)==0) return
 print*,"- QC DATA SOURCE AGAINST CLIMO."
 print*,"- OPEN CLIMO SNOW COVER FILE ",trim(climo_qc_file)
 lugb=11
 call baopenr(lugb,climo_qc_file,iret)

 if (iret /= 0) then
   print*,"- BAD OPEN, WILL NOT PERFORM QC ", iret
   return
 endif

!---------------------------------------------------------------
! climo file is weekly. so calculate the current week
! then read that record from the climo file.
!---------------------------------------------------------------

 if (grib_year == 100) then
   century = grib_century
 else
   century = grib_century-1
 endif

 year = century*100 + grib_year

 idat=0
 idat(1)=year
 idat(2)=grib_month
 idat(3)=grib_day

 call w3doxdat(idat,jdow,jdoy,jday)

! the climo file date is the beginning of the 7 day period

 week = nint((jdoy+3.)/7.)
 if (week==0) week=52
 if (week==53) week=1

 print*,"- READ CLIMO FOR WEEK ",week
 lskip    = -(week)
 jpds     = -1
 jgds     = -1
 kpds     = jpds
 kgds     = jgds

 allocate(climo(iclim,jclim))
 allocate(bitmap_clim(iclim,jclim))

 call getgb(lugb, 0, (iclim*jclim), lskip, jpds, jgds, &
            numpts, lskip, kpds, kgds, bitmap_clim, climo, iret)

 if (iret /= 0) then 
   print*,"- ERROR READING GRIB FILE ", iret
   print*,"- WILL NOT PERFORM QC."
   deallocate(climo, bitmap_clim)
   call baclose(lugb,iret)
   return
 endif

 call baclose(lugb,iret)

 fill=999.
 allocate(xpts(idata,jdata))
 allocate(ypts(idata,jdata))
 allocate(rlon_data(idata,jdata))
 allocate(rlat_data(idata,jdata))
 allocate(crot(idata,jdata))
 allocate(srot(idata,jdata))
 do j=1,jdata
 do i=1,idata
   xpts(i,j)=i
   ypts(i,j)=j
 enddo
 enddo

 print*,"- CALC LAT/LONS OF SOURCE POINTS."
 call gdswiz05(kgds_data,1,(idata*jdata),fill,xpts,ypts,rlon_data,rlat_data,nret,0,  &
               crot,srot)

 deallocate(xpts,ypts,crot,srot)

 if (nret /= (idata*jdata)) then
   print*,"- CALC FAILED. WILL NOT PERFORM QC."
   deallocate (rlon_data,rlat_data)
   return
 endif

!---------------------------------------------------------------
! loop over all data points in nh.  gross check data.
! afwa is a depth in meters, ims is % coverage.  there should be 
! no neg values or very large values. if point passes gross check,
! then check against climatology.  find the
! nearest point on the climo snow cover grid.  if
! climo indicates snow is likely (100% coverage), then
! check if afwa/ims has snow.  if climo indicates snow is
! impossible (0% coverage), then check if afwa/ims has no snow.  if
! afwa/ims differs from climo too much, then afwa/ims is
! considered suspect and will not be used.
!---------------------------------------------------------------

 count_nosnow_climo=0
 count_nosnow_data=0
 count_snow_data=0
 count_snow_climo=0
 count_grosschk_data=0

 if (isrc==1) then
   thresh_gross=50.0   ! afwa data is depth in meters
   thresh=.005
 elseif (isrc==2) then
   thresh_gross=100.0  ! nesdis ims data is coverage
   thresh=50.0
 endif

 do j=1,jdata
 do i=1,idata
   if (rlat_data(i,j)>0.0 .and. bitmap_data(i,j)) then
     if (snow_data(i,j) < 0.0 .or. snow_data(i,j) > thresh_gross) then
       count_grosschk_data=count_grosschk_data+1
       cycle
     endif
     y = (lat11_clim-rlat_data(i,j))/dy_clim + 1.0
     if (rlon_data(i,j)>180.0) rlon_data(i,j)=rlon_data(i,j)-360.0
     x = (rlon_data(i,j)-lon11_clim)/dx_clim + 1.0
     jj=nint(y)
     if (jj<1) jj=1
     if (jj>jclim) jj=jclim
     ii=nint(x)
     if (ii<1) ii=ii+iclim
     if (ii>iclim) ii=ii-iclim
     if (bitmap_clim(ii,jj)) then  ! climo point is land
       if (climo(ii,jj) <1.0) then ! climo point is snow impossible
         count_nosnow_climo=count_nosnow_climo+1
         if (snow_data(i,j) == 0.0) then
           count_nosnow_data=count_nosnow_data+1
         endif
       endif
       if (climo(ii,jj) > 99.) then  ! climo point is snow likely
         count_snow_climo=count_snow_climo+1
         if (snow_data(i,j) >thresh) then
           count_snow_data=count_snow_data+1
         endif
       endif
     endif
   endif
 enddo
 enddo

 if (count_grosschk_data > 1) then
   print*,'- NUMBER OF DATA POINTS THAT FAIL GROSS CHECK ',count_grosschk_data
   if (isrc==1) then
     bad_afwa_nh=.true.
   elseif(isrc==2) then
     bad_nesdis=.true.
   endif
   goto 99
 endif

 percent = float(count_snow_climo-count_snow_data) / float(count_snow_climo)
 percent = percent*100.
 print*,'- NUMBER OF DATA POINTS THAT SHOULD HAVE SNOW ',count_snow_climo
 print*,'- NUMBER OF THESE POINTS THAT ARE BARE GROUND ',(count_snow_climo-count_snow_data), &
        ' OR ', percent, '%'

 if (percent>50.0) then
   print*,"- PERCENTAGE OF BARE GROUND POINTS EXCEEDS ACCEPTABLE LEVEL."
   print*,"- WILL NOT USE SOURCE DATA." 
   if (isrc==1) then
     bad_afwa_nh=.true.
   elseif(isrc==2) then
     bad_nesdis=.true.
   end if
 endif
   
 percent = float(count_nosnow_climo-count_nosnow_data) / float(count_nosnow_climo)
 percent = percent*100.
 print*,'- NUMBER OF DATA POINTS THAT SHOULD *NOT* HAVE SNOW ',count_nosnow_climo
 print*,'- NUMBER OF THESE POINTS WITH SNOW ',(count_nosnow_climo-count_nosnow_data), &
        ' OR ', percent, '%'

 if (percent>20.0) then
   print*,"- PERCENTAGE OF POINTS WITH SNOW EXCEEDS ACCEPTABLE LEVEL."
   print*,"- WILL NOT USE SOURCE DATA." 
   if (isrc==1) then
     bad_afwa_nh=.true.
   elseif(isrc==2) then
     bad_nesdis=.true.
   endif
 endif

 99 continue

 if (allocated(rlat_data)) deallocate (rlat_data)
 if (allocated(rlon_data)) deallocate (rlon_data)
 if (allocated(climo)) deallocate (climo)
 if (allocated(bitmap_clim)) deallocate (bitmap_clim)

 return

 end subroutine nh_climo_check

 subroutine afwa_check_sh
!$$$  subprogram documentation block
!
! subprogram:    afwa_check_sh
!   prgmmr: gayno          org: w/np2     date: 2009-jun-3
!
! abstract:  check for corrupt afwa data
!  
! program history log:
! 2009-jun-3   gayno    - initial version
!
! usage: call afwa_check(hemi)
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

 integer             :: kgds(200), nret, n, count_snow, count_nosnow
 integer, parameter  :: npts=1

 real                :: fill, xpts, ypts
 real                :: rlon, rlat, crot, srot

 print*,'- QC DATA IN SH.'

 kgds=0
 fill=9999.

 count_snow=0
 count_nosnow=0
 kgds=kgds_afwa_sh
 rlat=-88.0
 rlon=0.
 call gdswiz05(kgds,(-1),npts,fill,xpts,ypts,rlon,rlat,nret,0,crot,srot)
 if (snow_dep_afwa_sh(nint(xpts),nint(ypts)) < 0.001) then
   print*,'- ** NO SNOW IN ANTARCTICA AT 88S 0W: ',snow_dep_afwa_sh(nint(xpts),nint(ypts))
   count_nosnow=count_nosnow+1
 endif
 rlat=-80.0
 rlon=135.
 call gdswiz05(kgds,(-1),npts,fill,xpts,ypts,rlon,rlat,nret,0,crot,srot)
 if (snow_dep_afwa_sh(nint(xpts),nint(ypts)) < 0.001) then
   print*,'- ** NO SNOW IN ANTARCTICA AT 80S 135E: ',snow_dep_afwa_sh(nint(xpts),nint(ypts))
   count_nosnow=count_nosnow+1
 endif
 rlat=-75.0
 rlon=45.
 call gdswiz05(kgds,(-1),npts,fill,xpts,ypts,rlon,rlat,nret,0,crot,srot)
 if (snow_dep_afwa_sh(nint(xpts),nint(ypts)) < 0.001) then
   print*,'- ** NO SNOW IN ANTARCTICA AT 75S 45E: ',snow_dep_afwa_sh(nint(xpts),nint(ypts))
   count_nosnow=count_nosnow+1
 endif
 rlat=-10.
 rlon=-45.
 call gdswiz05(kgds,(-1),npts,fill,xpts,ypts,rlon,rlat,nret,0,crot,srot)
 if (snow_dep_afwa_sh(nint(xpts),nint(ypts)) > 0.0) then
   print*,'- ** SNOW IN SOUTH AMERICA: ',snow_dep_afwa_sh(nint(xpts),nint(ypts))
   count_snow=count_snow+1
 endif
 rlat=-20.0
 rlon=130.
 call gdswiz05(kgds,(-1),npts,fill,xpts,ypts,rlon,rlat,nret,0,crot,srot)
 if (snow_dep_afwa_sh(nint(xpts),nint(ypts)) > 0.0) then
   print*,'- ** SNOW IN AUSTRALIA: ',snow_dep_afwa_sh(nint(xpts),nint(ypts))
   count_snow=count_snow+1
 endif
 rlat=-9.0
 rlon=25.
 call gdswiz05(kgds,(-1),npts,fill,xpts,ypts,rlon,rlat,nret,0,crot,srot)
 if (snow_dep_afwa_sh(nint(xpts),nint(ypts)) > 0.0) then
   print*,'- ** SNOW IN AFRICA: ',snow_dep_afwa_sh(nint(xpts),nint(ypts))
   count_snow=count_snow+1
 endif
 if (count_snow > 2) then
   print*,'- ** SNOW ANALYZED AT MULTIPLE POINTS THAT SHOULD BE SNOW FREE.'
   print*,'- ** DONT USE AFWA DATA'
   bad_afwa_sh=.true.
 endif
 if (count_nosnow > 2) then
   print*,'- ** NO SNOW ANALYZED AT MULTIPLE POINTS THAT SHOULD BE SNOW COVERED.'
   print*,'- ** DONT USE AFWA DATA'
   bad_afwa_sh=.true.
 endif

 end subroutine afwa_check_sh

 subroutine snowdat_cleanup
!$$$  subprogram documentation block
!
! subprogram:    snowdat_cleanup
!   prgmmr: gayno          org: w/np2     date: 2005-dec-16
!
! abstract:  free up memory.
!  
! program history log:
! 2005-dec-16  gayno    - initial version
!
! usage: call snow_dat_cleanup
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

 if (allocated(bitmap_autosnow))    deallocate (bitmap_autosnow)
 if (allocated(bitmap_afwa_nh))     deallocate (bitmap_afwa_nh)
 if (allocated(bitmap_afwa_sh))     deallocate (bitmap_afwa_sh)
 if (allocated(bitmap_nesdis))      deallocate (bitmap_nesdis)
 if (allocated(snow_cvr_nesdis))    deallocate (snow_cvr_nesdis)
 if (allocated(snow_cvr_autosnow))  deallocate (snow_cvr_autosnow)
 if (allocated(sea_ice_nesdis))     deallocate (sea_ice_nesdis)
 if (allocated(snow_dep_afwa_nh))   deallocate (snow_dep_afwa_nh)
 if (allocated(snow_dep_afwa_sh))   deallocate (snow_dep_afwa_sh)

 return

 end subroutine snowdat_cleanup

 subroutine grib_check(file_name, isgrib)
!$$$  subprogram documentation block
!
! subprogram:    grib_check
!   prgmmr: gayno          org: w/np2     date: 2007-nov-28
!
! abstract:  determine whether file is grib or not.
!
! program history log:
! 2007-nov-28  gayno    - initial version
! 2011-apr-26  gayno    - replace my simple-minded logic
!                         with call to w3lib routin skgb.
!
! usage: call grib_check(file_name, isgrib)
!
!   input argument list:  file_name - file name
!
!   output argument list: isgrib - .true. is grib file
!
! remarks: none.
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$
 

 implicit none

 character*(*), intent(in)         :: file_name
 integer                           :: istat, iseek, mseek, lskip, lgrib
 logical, intent(out)              :: isgrib

 print*,"- CHECK FILE TYPE OF: ", trim(file_name)
 call baopenr (11, file_name, istat)

 if (istat /= 0) then
   print*,'- ** BAD OPEN.  ISTAT IS ',istat
   call w3tage('SNO2MDL')
   call errexit(60)
 end if

 iseek = 0
 mseek = 64
 call skgb(11, iseek, mseek, lskip, lgrib)

 call baclose(11, istat)

 if (lgrib > 0) then
   isgrib = .true.
   print*,"- FILE IS GRIB"
 else
   isgrib = .false.
   print*,"- FILE IS BINARY"
 endif

 return

 end subroutine grib_check

 subroutine read_afwa_binary(file_name, snow_dep_afwa) 
!$$$  subprogram documentation block
!
! subprogram:    read_afwa_binary
!   prgmmr: gayno          org: w/np2     date: 2007-nov-28
!
! abstract:  read afwa binary snow depth file
!  
! program history log:
! 2007-nov-28  gayno    - initial version
!
! usage: call grib_check(file_name, snow_dep_afwa)
!
!   input argument list:  file_name - file name
!
!   output argument list: snow_dep_afwa - snow depth in meters
!
! remarks: read logic for binary data taken from hua-lu's code:
!          /nwprod/sorc/grib_snowgrib.fd
!          
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 implicit none

 character*8                            :: afwa_file_info(2)
 character*(*), intent(in)              :: file_name

 integer*2, allocatable                 :: dummy(:,:)
 integer                                :: i,j, istat
 integer, parameter                     :: iafwa = 512
 integer, parameter                     :: jafwa = 512

 real, intent(out)                      :: snow_dep_afwa(iafwa,jafwa)

 print*,"- OPEN AFWA BINARY FILE ", trim(file_name)
 open (11, file=trim(file_name), form="unformatted", iostat=istat)

 if (istat /= 0) then
   print*,'- ** BAD OPEN.  ISTAT IS ',istat
   call w3tage('SNO2MDL')
   call errexit(60)
 end if

 print*,"- READ AFWA BINARY FILE ", trim(file_name)
 read(11, iostat = istat) afwa_file_info

 if (istat /= 0) then
   print*,'- ** BAD READ.  ISTAT IS ',istat
   call w3tage('SNO2MDL')
   call errexit(61)
 end if

 print*,"- AFWA DATA IS ", afwa_file_info(1), " AT TIME ", afwa_file_info(2)(2:7)

 allocate(dummy(iafwa,jafwa))

 read(11, iostat=istat) (dummy(i,1),i=1,iafwa)

 if (istat /= 0) then
   print*,'- ** BAD READ.  ISTAT IS ',istat
   call w3tage('SNO2MDL')
   call errexit(61)
 end if
 
 do j = 1, jafwa
   read(11, iostat=istat) (dummy(i,j),i=1,iafwa)
   if (istat /= 0) then
     print*,'- ** BAD READ.  ISTAT IS ',istat
     call w3tage('SNO2MDL')
     call errexit(61)
   end if
 enddo

 close(11)

!-----------------------------------------------------------------------
! "4090" is the sea ice flag.  we don't use the afwa sea ice.
!-----------------------------------------------------------------------

 where (dummy == 4090) dummy = 0

 snow_dep_afwa = float(dummy)  

!---------------------------------------------------------------------
! afwa data is a snow depth in units of tenths of inches.
! convert this to meters.
!---------------------------------------------------------------------

 snow_dep_afwa = snow_dep_afwa * 2.54 / 1000.0

 deallocate (dummy)

 return

 end subroutine read_afwa_binary

 subroutine read_afwa_mask(file_name, bitmap_afwa)
!$$$  subprogram documentation block
!
! subprogram:    read_afwa_mask
!   prgmmr: gayno          org: w/np2     date: 2007-nov-28
!
! abstract:  read afwa land mask file to get a bitmap
!  
! program history log:
! 2007-nov-28  gayno    - initial version
!
! usage: call read_afwa_mask(file_name, bitmap_afwa)
!
!   input argument list:  file_name - land mask file name
!
!   output argument list: bitmap_afwa - .true. if land
!
! remarks: none.
!          
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 implicit none

 character*(*), intent(in)         :: file_name

 integer                           :: i, j, istat
 integer, parameter                :: iafwa = 512
 integer, parameter                :: jafwa = 512
 integer*4, allocatable            :: dummy4(:,:)

 logical*1, intent(out)            :: bitmap_afwa(iafwa,jafwa)

 allocate (dummy4(iafwa,jafwa))
 bitmap_afwa = .false.

 print*,'- OPEN AFWA MASK FILE ', trim(file_name)
 open(11, file=trim(file_name), access='direct', &
      recl=iafwa*jafwa*4, iostat=istat)

 if (istat /= 0) then
   print*,'- ** BAD OPEN. ISTAT IS ', istat
   call w3tage('SNO2MDL')
   call errexit(62)
 end if

 print*,'- READ AFWA MASK FILE ', trim(file_name)
 read(11, rec=1, iostat=istat) dummy4

 if (istat /= 0) then
   print*,'- ** BAD READ. ISTAT IS ', istat
   call w3tage('SNO2MDL')
   call errexit(63)
 end if
 
 close(11)

!-----------------------------------------------------------------------
! here -1-offhemi, 1-ocean, 2-land, 4-coast.  
!-----------------------------------------------------------------------

 do j = 1, jafwa
   do i = 1, iafwa
     if (dummy4(i,j) > 1) then
       bitmap_afwa(i,j) = .true.
     endif
   enddo
 enddo

 deallocate (dummy4)

 end subroutine read_afwa_mask

 end module snowdat
