 program driver
!$$$  main program documentation block
!                .      .    .                                       .
! main program: snow2mdl
!   prgmmr: gayno            ORG: NP2                DATE: 2005-dec-16
!
! abstract: interpolates nesdis snow cover, autosnow snow cover, and 
!   afwa snow depth data to a regional nam grid or global gaussian grid. 
!   program outputs a snow cover and snow depth record in grib format.
!
! program history log:
!   2005-dec-16  gayno     initial version
!   2007-nov-30  gayno     added processing of nam b-grids.
!                          improved thinning for gfs grids.
!   2008-feb-01  gayno     added option to use autosnow data
!                          in southern hemisphere.
!
! usage: model specific files use the following naming convention:
!        PREFIX = global or hiresw (nmm regional)
!        GRID   = grid name (t254, centnmm, etc)
!
!   input files:
!     fort.81                       - configuration namelist
!     imssnow96.grb                 - nesdis 4km (96 mesh) ims snow cover data
!     imssnow.grb                   - nesdis 23km (16 mesh) ims snow cover data
!     nam_imsmask                   - nesdis 23km ims land mask
!     PRD.SPPROD.SNODEPH.NHMAMAP    - afwa nh snow depth data (8th mesh binary)
!     PRD.SPPROD.SNODEPH.SHMAMAP    - afwa sh snow depth data (8th mesh binary)
!     afwa_slmask_nh_8              - afwa nh land/sea mask (8th mesh)
!     afwa_slmask_sh_8              - afwa sh land/sea mask (8th mesh)
!     NPR.SNWN.SP.S1200.MESH16      - afwa nh snow depth data (16th mesh grib)
!     NPR.SNWS.SP.S1200.MESH16      - afwa sh snow depth data (16th mesh grib) 
!     SH4km_autosnow_MMDDYYYY.grb   - autosnow sh cover data
!     $PREFIX_latitudes.$GRID.grb   - latitudes on model grid 
!                                     (h point lats for nmm) 
!     $PREFIX_longitudes.$GRID.grb  - longitudes on model grid 
!                                     (h point lons for nmm)
!     $PREFIX_slmask.$GRID.grb      - model land/sea mask
!
!   output files:  (including scratch files)
!     $PREFIX_snow.YYYYMMDDHH.$GRID.grb
!
!   exit states:
!     cond =   0 - successful run
!          =  54 - bad input data selection
!          =  55 - error in interpolation routine
!          =  57 - bad write of model snow depth data
!          =  58 - bad write of model snow cover data
!          =  59 - bad open of model output grib file
!          =  60 - bad open afwa snow data
!          =  61 - bad read afwa snow data
!          =  62 - bad open afwa land mask
!          =  63 - bad read afwa land mask
!          =  70 - bad degrib of nesdis ims snow cover data
!          =  71 - bad degrib of nesdis ims sea ice data
!          =  72 - bad degrib of nesdis ims data header
!          =  73 - bad open of nesdis ims data
!          =  74 - bad open of autosnow data
!          =  75 - bad degrib of autosnow data 
!          =  76 - unknown global model grid
!          =  77 - bad open on configuration namelist
!          =  78 - bad read on configuration namelist
!          =  79 - unrecognized model grid type
!          =  80 - bad open on model latitude grib file
!          =  81 - bad read on model latitude grib header
!          =  82 - bad degrib of model latitude data
!          =  83 - bad open on model longitude file
!          =  84 - bad degrib of model longitude data
!          =  85 - bad open of model land mask file
!          =  86 - bad degrib of model land mask data
!          =  87 - bad open of nesdis ims land mask file
!          =  88 - bad read of nesdis ims land mask data
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
!   see module snow2mdl for more details.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

 use snowdat,       only           : readnesdis, &
                                     readafwa,   &
                                     readautosnow

 use model_grid,    only           : read_mdl_grid_info, &
                                     model_grid_cleanup

 use snow2mdl,      only           : interp

 use program_setup, only           : read_config_nml

 implicit none

 call w3tagb('SNO2MDL',2005,350,0000,'NP2')

 print*,''
 print*,"***********************"
 print*,"*** BEGIN EXECUTION ***"
 print*,"***********************"

!-----------------------------------------------------------------------
! get configuration stuff.
!-----------------------------------------------------------------------

 call read_config_nml

!-----------------------------------------------------------------------
! read input snow data.
!-----------------------------------------------------------------------

 call readautosnow

 call readnesdis

 call readafwa

!-----------------------------------------------------------------------
! read information about the model grid to which the
! snow data will be interpolated.
!-----------------------------------------------------------------------

 call read_mdl_grid_info

!-----------------------------------------------------------------------
! interpolate the data to the model grid, then write
! it to a grib file.
!-----------------------------------------------------------------------
 
 call interp

!-----------------------------------------------------------------------
! free up memory
!-----------------------------------------------------------------------

 call model_grid_cleanup

 print*,''
 print*,'****************************'
 print*,'**** NORMAL TERMINATION ****'
 print*,'****************************'

 call w3tage('SNO2MDL')

 stop

 end program driver
