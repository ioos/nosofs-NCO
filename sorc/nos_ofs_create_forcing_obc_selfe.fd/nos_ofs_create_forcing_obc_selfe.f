C----------------------------------------------------------------------------------
C
C Program Name:  nos_ofs_create_forcing_obc_selfe.f
C
C Directory: ./nosofs_shared.v1.0.0/sorc/nos_ofs_create_forcing_obc_selfe.fd
C
C Purpose:    This Program is used to generate lateral open boundary condition files for SELFE 
C             from either climatological dataset WOA05 or Navy Coastal Ocean Model (NCOM) products 
C             in CCS data tank, or ETSS nontidal water levels.
C             The data on NCOM grid is horizontally interpolated onto SELFE open boundary grid points
C             using either remesh or nature neighbors (both bicubic and bilinear routine do not work),
C             and then vertically interpolated onto SELFE vertical levels from NCOM 
C             z-coordinate vertical levels linearly.

C             Refer to HPC_COMF Technical Report for more detail information.               

C Current contact:   Aijun Zhang
C         Org:  NOS/CO-OPS/OD   Phone:  301-713-2890 ext. 127 
C                    aijun.zhang@Noaa.gov 
C Attributes:
C  Language:  Fortran
C  Computer:  DEW/MIST at NCEP  
C
C  Compile command:  gmake -f makefile
C
C Subprograms called: sal78.f nos_ofs_tideprediction.f nos_ofs_obc_write_netcdf_selfe.f utility.f
C
C Input Data files:
C   "/dcom/us007003/20081120/wgrdbul/ncom_glb_reg1_2008112000.nc.gz"
C    
C Usage:   nos_ofs_create_forcing_obc_selfe < Fortran_OBC.ctl > Fortran_OBC.log 
C
C
C Input Parameters:
C           OFS         : name of Operational Forecast System, e.g. CBOFS, TBOFS
C        Ocean_MODEL    : Name of numerical ocean model used in OFS, e.g. ROMS, FVCOM
C        DBASE_WWL      : data source for nontidal water level OBC
C        DBASE_TS       : data source for temperature and salinity OBC
C        TIME_START     : Start time 
C        FORHRS         : Length of forecast time in hours
C        IGRD           : indicator of horizontal interpolation method
C                        =1:  remesh using triangulation techniques
C 		         =2: bicubic routine from ROMS
C			 =3: bilinear routine from ROMS
C                        =4: nature neighbours
C        FIXofs         : path to store NOS static data files 
C        GRIDFILE       : Grid file name of the OFS			   
C        OUTPUTFILE     : Output file name   
C        MINLON         :longitude of lower left/southwest corner to cover the OFS domain
C        MINLAT         :latitude of lower left /southwest corner to cover the OFS domain
C        MINLON         :longitude of upper right/northeast corner to cover the OFS domain
C        MINLAT         :latitude of  upper right/northeast corner to cover the OFS domain
C        KBm            : Number of vertical levels for temperature and salinity of the OFS 
C       THETA_S         :S-coordinate surface control parameter, [0 < theta_s < 20].
C       THETA_B         :S-coordinate bottom  control parameter, [0 < theta_b < 1].
C       TCLINE          :Width (m) of surface or bottom boundary layer in which
C                        higher vertical resolution is required during stretching.
C
C Output files: 
C    A netCDF open boundary forcing file which contains the required variables. 
C----------------------------------------------------------------------------------
      parameter (NMAX=9000)
      include 'netcdf.inc'
      character*200 OFS,OCEAN_MODEL*10,DBASE_WL*20,DBASE_TS,FILE_TS
      character*200 FIN,FOUT,GRIDFILE,netcdf_file,FIXofs,ETSSFILE
      character*200 BUFFER,CMD*132,VNAME,ANAME,OBC_CTL_FILE,HC_FILE
      character*200 BUFFER1,HC_FILE_OFS,FILE_NCOM(100),NCOMDIR,NOSWLDIR
      character*200 FILE_NCOM_TIDE(100),CDATE*8,FNAME,FIN_TIDE,CHH*2
      character*200 FDIR,STYPE,VGRIDFILE,NCOM_FILE,T_NUDGE_FILE
      character*200 NOSBUFR,USGSBUFR
      character*26,allocatable :: Times(:)
      integer, allocatable :: Itime(:)
      integer, allocatable :: Itime2(:)
      character*200 CORMSLOG,DBASE_TS_ORI,INIT_FILE_NOWCAST
      character*200 START_TIME, END_TIME,BUFRFILE,OBC_CLIM_FILE
      character*200 START_TIMEm1
      real*8 jday_start,jbase_date,JULIAN,yearb,monthb,dayb,hourb
      real*8 jday,jdays,jdaye,jday0,js_etss,je_etss
      real minlon,minlat,maxlat,maxlon,missvalue
      LOGICAL FEXIST,USGS_L
      CHARACTER (LEN=10) BIG_BEN(3),CURRENT_TIME*20
      CHARACTER globalstr(9)*120
      INTEGER DATE_TIME(8)
      INTEGER BASE_DATE(4)
      INTEGER DAYS_PER_MONTH(12)
      DATA (DAYS_PER_MONTH(i),I=1,12) /
     &31,28,31,30,31,30,31,31,30,31,30,31/ 
      integer grbunit,NCID,NCIDOUT
      real latsw,lonsw,LaD,LoV,dx_grb
cc allocatable arrays for ROMS model
      integer, allocatable :: nv(:,:)  
      integer, allocatable :: nvOBC(:,:)  
      real*4, allocatable :: lonm  (:)
      real*4, allocatable :: latm  (:)
      real*4, allocatable :: lonc  (:)
      real*4, allocatable :: latc  (:)
      real*4, allocatable :: sigma(:)
      real*4, allocatable :: siglay(:,:)
      real*4, allocatable :: siglev(:,:)
      real*4, allocatable :: ZSIGMA(:)
      real*4, allocatable :: maskm  (:,:)
      real*4, allocatable :: hm  (:)
      real*4, allocatable :: ubarOBC  (:,:)
      real*4, allocatable :: vbarOBC  (:,:)
      real*4, allocatable :: Iout(:,:)
      real*4, allocatable :: Jout(:,:)
      real*4, allocatable :: TIME_M  (:)
      
      real*4, allocatable :: TIMEOBC  (:)
      real*4, allocatable :: WLOBC  (:,:)
      real*4, allocatable :: tempOBC  (:,:,:)
      real*4, allocatable :: saltOBC  (:,:,:)
      real*4, allocatable :: uOBC  (:,:,:)
      real*4, allocatable :: vOBC  (:,:,:)
      real*4, allocatable :: tempOBC_M  (:,:,:)
      real*4, allocatable :: saltOBC_M  (:,:,:)
      real*4, allocatable :: uOBC_M  (:,:,:)
      real*4, allocatable :: vOBC_M  (:,:,:)
      real*4, allocatable :: ueOBC_M  (:,:,:)
      real*4, allocatable :: veOBC_M  (:,:,:)
!  allocatable arrays for variables in NCOM file
      real*4, allocatable :: lon  (:,:)
      real*4, allocatable :: lat  (:,:)
      real*4, allocatable :: zeta_time  (:)
      real*4, allocatable :: ts_time  (:)
      real*4, allocatable :: depth  (:)
      
      real*4, allocatable :: lonsub  (:,:)
      real*4, allocatable :: latsub  (:,:)
      real*4, allocatable :: masksub  (:,:)
      real*4, allocatable :: WL  (:,:,:)
      real*4, allocatable :: temp (:,:,:,:)
      real*4, allocatable :: salt (:,:,:,:)
      real*4, allocatable :: u (:,:,:,:)
      real*4, allocatable :: v (:,:,:,:)
      real*4, allocatable :: tide_NCOM  (:,:,:)

      real*4, allocatable :: lon_ETSS  (:,:)
      real*4, allocatable :: lat_ETSS  (:,:)
      real*4, allocatable :: WL_ETSS  (:,:)

      real*4, allocatable :: tide_amp  (:,:)
      real*4, allocatable :: tide_epoc (:,:)
      real*4, allocatable :: tide_speed(:,:)
      character,allocatable :: stationID(:,:)
      character,allocatable :: constituents(:,:)
      real amp(37),epoc(37)
! temporary arrays
      real*4, allocatable :: tmp1d  (:)
      real*4, allocatable :: tmp2d  (:,:)
      real*4, allocatable :: tmp3d  (:,:,:)
      real*4, allocatable :: tmp4d  (:,:,:,:)
      real*8, allocatable :: dtmp4d  (:,:,:,:)
      integer, allocatable :: itmp4d  (:,:,:,:)
      real*4, allocatable :: oned1(:)
      real*4, allocatable :: oned2(:)
      real*4, allocatable :: oned3(:)
      real*4, allocatable :: oned4(:)
      real*4, allocatable :: outm(:,:)
      real*4, allocatable :: XINP(:)
      real*4, allocatable :: YINP(:)
      real*4, allocatable :: ZINP(:)
      real*4, allocatable :: XOUT(:)
      real*4, allocatable :: YOUT(:)
      real*4, allocatable :: ZOUT(:)
      INTEGER :: status      ! Return status
      integer, allocatable :: weightnodes(:,:)  
      integer, allocatable :: ITMP3D(:,:,:)
      real*4, allocatable :: weights(:,:)       
      integer n3(3),ITMP1D(2000)
      real w3(3)
      integer dimids(5),COUNT(4),DIMS(4)
! variables for real time observations
      CHARACTER*8  SUBSET,NAME                                  
      character*20 stnbufrid
      Real*8 DATES(5),RTIM(6),RPID
      Real*8 data1(2,500),data2(4,500),data3(5)
      real*8 xlocat(5)
      DATA BMISS /10E10/                                                
      equivalence(NAME,RPID)
CC variables for OBC_CTL_FILE     
      integer, allocatable :: SID(:)
      character*20,allocatable :: NOS_ID(:)
      character*5,allocatable :: NWS_ID(:)
      character*4,allocatable :: AGENCY_ID(:)
      character*1,allocatable :: OBC_ID(:)
      real*4, allocatable :: DATUM(:)
      integer, allocatable :: WL_FLAG(:)
      integer, allocatable :: TS_FLAG(:)
      integer, allocatable ::BACKUP_SID(:)
      integer, allocatable :: GRIDID_STA(:)
      real*4, allocatable :: As(:)

      integer, allocatable :: GRIDID(:)
      integer, allocatable :: IOBC(:)
      integer, allocatable :: JOBC(:)
      integer, allocatable :: WL_STA(:)
      integer, allocatable :: TS_STA(:)
      integer, allocatable :: WL_SID_1(:)
      integer, allocatable :: WL_SID_2(:)
      integer, allocatable :: TS_SID_1(:)
      integer, allocatable :: TS_SID_2(:)
      real*4, allocatable :: WL_S_1(:)
      real*4, allocatable :: WL_S_2(:)
      real*4, allocatable :: TS_S_1(:)
      real*4, allocatable :: TS_S_2(:)
      integer, allocatable :: CU_STA(:)
      integer, allocatable :: CU_SID_1(:)
      integer, allocatable :: CU_SID_2(:)
      real*4, allocatable :: CU_S_1(:)
      real*4, allocatable :: CU_S_2(:)
      
      integer, allocatable :: NTR(:)
      integer, allocatable :: NTR_T(:)
      integer, allocatable :: NTR_S(:)
      real*4, allocatable :: RTIME(:,:)
      real*4, allocatable :: RTIME_T(:,:)
      real*4, allocatable :: RTIME_S(:,:)
      real*4, allocatable :: TIME_PRD(:)
      real*4, allocatable :: WL_PRD(:,:)
      real*4, allocatable :: WL_OBS(:,:)
      real*4, allocatable :: SWL_OBS(:,:)
      real*4, allocatable :: T_OBS(:,:) 
      real*4, allocatable :: S_OBS(:,:) 
      real*4, allocatable :: lonOBC(:) 
      real*4, allocatable :: latOBC(:) 
      real*4, allocatable :: loneOBC(:) 
      real*4, allocatable :: lateOBC(:) 
      real*4, allocatable :: hOBC(:) 
      real*4, allocatable :: AVGERR(:) 
      real*4, allocatable :: AVGERR_T(:) 
      real*4, allocatable :: AVGERR_S(:) 

      real*4, allocatable :: TIME_clim(:)
      real*4, allocatable :: T_clim(:,:,:) 
      real*4, allocatable :: S_clim(:,:,:) 

      integer, allocatable :: K_clim(:)
      real*4, allocatable :: depth_clim(:,:)

      real*4, allocatable :: ZKU(:)
      real*4, allocatable :: ZKL(:)

      integer, allocatable :: partition(:)
! for SELFE OBC Nudging
      integer, allocatable :: node_t_nudge(:)
      real*4, allocatable :: weight_t_nudge (:)
      real*4, allocatable :: lon_nudge(:) 
      real*4, allocatable :: lat_nudge(:) 
      real*4, allocatable :: ztot(:) 
      real*4, allocatable :: zcor(:) 
      real*4 time_seconds
!-----------------------------------------------------------------------
!  read parameters from the Fortran control "Fortran_OBC.ctl"
!-----------------------------------------------------------------------
      
      read(5,'(a200)')OFS
      read(5,'(a10)')OCEAN_MODEL
      read(5,'(a20)')DBASE_WL
!      if (trim(DBASE_WL) .eq. "ETSS" )then
         read(5,'(a200)')ETSSFILE
!      endif  
      read(5,'(a200)')DBASE_TS
      DBASE_TS_ORI=DBASE_TS
      read(5,'(a200)')NCOMDIR
      read(5,'(a200)')NOSWLDIR
      read(5,'(a200)')BUFFER
      NOSBUFR=trim(adjustL(BUFFER))
      read(5,'(a200)')BUFFER
      USGSBUFR=trim(adjustL(BUFFER))

      read(5,'(a200)')BUFFER
      START_TIME=trim(adjustL(BUFFER))
      read(START_TIME,'(I4,4I2)')IYRS,IMMS,IDDS,IHHS,IMNS

      read(5,'(a200)')BUFFER
      END_TIME=trim(adjustL(BUFFER))
      read(END_TIME,'(I4,4I2)')IYRE,IMME,IDDE,IHHE,IMNE
!      read(5,*)IYRS,IMMS,IDDS,IHHS
!      read(5,*)FORHRS
      read(5,*)IGRD
      read(5,'(a200)')BUFFER
      do i=1,len_trim(BUFFER)
          if(BUFFER(i:I) .eq. "'" .or. BUFFER(i:I) .eq. '"')then
	    BUFFER(i:I)=' '
	  endif    
      enddo
      FIXofs=trim(adjustL(BUFFER))
      
      read(5,'(a200)')BUFFER
      do i=1,len_trim(BUFFER)
          if(BUFFER(i:I) .eq. "'" .or. BUFFER(i:I) .eq. '"')then
	    BUFFER(i:I)=' '
	  endif    
      enddo
!      GRIDFILE=TRIM(FIXofs)//'/'//TRIM(OFS)//'/'//trim(adjustL(BUFFER))
      GRIDFILE=trim(adjustL(BUFFER))
      print *,'gridfile=',trim(gridfile)
      read(5,'(a200)')BUFFER
      do i=1,len_trim(BUFFER)
          if(BUFFER(i:I) .eq. "'" .or. BUFFER(i:I) .eq. '"')then
	    BUFFER(i:I)=' '
	  endif    
      enddo
      HC_FILE_OFS=trim(adjustL(BUFFER))
      print *,'HC_FILE_OFS=',trim(HC_FILE_OFS)
      read(5,'(a200)')BUFFER
      do i=1,len_trim(BUFFER)
          if(BUFFER(i:I) .eq. "'" .or. BUFFER(i:I) .eq. '"')then
	    BUFFER(i:I)=' '
	  endif    
      enddo
      OBC_CTL_FILE=trim(adjustL(BUFFER))
      print *,'OBC_CTL_FILE=',trim(OBC_CTL_FILE)
      read(5,'(a200)')BUFFER
      do i=1,len_trim(BUFFER)
          if(BUFFER(i:I) .eq. "'" .or. BUFFER(i:I) .eq. '"')then
	    BUFFER(i:I)=' '
	  endif    
      enddo
      OBC_CLIM_FILE=trim(adjustL(BUFFER))
      print *,'OBC_CLIM_FILE=',trim(OBC_CLIM_FILE)


      read(5,'(a200)')BUFFER
      do i=1,len_trim(BUFFER)
         if(BUFFER(i:I) .eq. "'" .or. BUFFER(i:I) .eq. '"')then
	    BUFFER(i:I)=' '
	 endif    
      enddo
      netcdf_file=trim(adjustL(BUFFER))
      read(5,'(a200)')BUFFER
      do i=1,len_trim(BUFFER)
          if(BUFFER(i:I) .eq. "'" .or. BUFFER(i:I) .eq. '"')then
	    BUFFER(i:I)=' '
	  endif    
      enddo
      CORMSLOG=trim(adjustL(BUFFER))
      print *,'CORMSLOG=',trim(CORMSLOG)
      read(5,'(a200)')BUFFER
      do i=1,len_trim(BUFFER)
         if(BUFFER(i:I) .eq. "'" .or. BUFFER(i:I) .eq. '"')then
            BUFFER(i:I)=' '
	 endif    
      enddo
      BUFFER=trim(adjustL(BUFFER))
      read(BUFFER,'(I4,3i2)')base_date
      read(5,*)minlon
      read(5,*)minlat
      read(5,*)maxlon
      read(5,*)maxlat
      read(5,*)KBm
      if (trim(OCEAN_MODEL) .eq. "ROMS" )then
         read(5,*)THETA_S
         read(5,*)THETA_B
         read(5,*)TCLINE
      else  
         read(5,'(a200)')BUFFER
         do i=1,len_trim(BUFFER)
           if(BUFFER(i:I) .eq. "'" .or. BUFFER(i:I) .eq. '"')then
	     BUFFER(i:I)=' '
	   endif    
         enddo
         VGRIDFILE=trim(adjustL(BUFFER))
      endif  
!      read(5,'(a200)')BUFFER
!      do i=1,len_trim(BUFFER)
!          if(BUFFER(i:I) .eq. "'" .or. BUFFER(i:I) .eq. '"')then
!	    BUFFER(i:I)=' '
!	  endif    
!      enddo
!      NCOM_FILE=trim(adjustL(BUFFER))
      read(5,'(a200)')BUFFER
      do i=1,len_trim(BUFFER)
          if(BUFFER(i:I) .eq. "'" .or. BUFFER(i:I) .eq. '"')then
	    BUFFER(i:I)=' '
	  endif    
      enddo
      T_NUDGE_FILE=trim(adjustL(BUFFER))
      read(5,*)DELT_TS
!-----------------------------------------------------------------------
!  calculate base time, start time, and end time in days
!-----------------------------------------------------------------------
      ICORMS=43
      CLOSE(ICORMS)
      OPEN(ICORMS,FILE=trim(CORMSLOG),STATUS='OLD',POSITION='APPEND')
      WRITE(ICORMS,'(a)')'BEGIN SECTION OF GENERATING OBC' 
      yearb=base_date(1)
      monthb=base_date(2)
      dayb=base_date(3)
      hourb=base_date(4)
      jbase_date=JULIAN(yearb,monthb,dayb,hourb)
      print *,'jbase_date=',jbase_date
      yearb=IYRS
      monthb=IMMS
      dayb=IDDS
      hourb=IHHS   !! do not need minutes to construct ETSS file name
      jdays=JULIAN(yearb,monthb,dayb,hourb)
      day_start=jdays-jbase_date
      yearb=IYRE
      monthb=IMME
      dayb=IDDE
      hourb=IHHE   !! do not need minutes to construct ETSS file name
      jdaye=JULIAN(yearb,monthb,dayb,hourb)
      day_end=jdaye-jbase_date

!      day_start=day_start-1.0/24.0
!      day_end=day_end+1.0/24.0
!      day_end=day_start+FORHRS/24.0
      print *,'base_date= ',base_date
      write(*,*)'domin=',minlon,minlat,maxlon,maxlat
      write(*,*)'model start & end time= ',day_start,day_end
!-----------------------------------------------------------------------
!   reading in NOS tide constituents at all NWLON stations
!-----------------------------------------------------------------------
!      HC_FILE=TRIM(FIXofs)//'/nos.ofs.HC_NWLON.nc'
      HC_FILE='nos.ofs.HC_NWLON.nc'
      INQUIRE(FILE=trim(HC_FILE),EXIST=FEXIST)
      IF(.NOT. FEXIST)THEN
          WRITE(*,*)'Harmonic Constant NetCDF file is not found'
	  WRITE(*,*)'Please check nos.ofs.HC_NWLON.nc in FIX directory'
	  WRITE(*,*)'Provide correct Harmonic Constant File Name'
	  WRITE(*,*)TRIM(OFS)//' stop here'
          WRITE(ICORMS,'(a)')'CRITICAL FAILURE IN CREATING OBC' 
          STOP
      ELSE  

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
        VNAME='stationID'
        STATUS = NF_OPEN(HC_FILE, NF_NOWRITE, NCID)
        STATUS = NF_INQ(NCID,NDIMS,NVARS,NGATTS,UNLIMDIMID)
C   NDIMS=number of dimension parameters
C   NVARS=number of total variables in the netcdf file
C   NGATTS= number of global attributes
C   UNLIMDIMID= dimension ID which is unlimited.
!      PRINT *,'NDIMS=',NDIMS,NVARS,NGATTS,UNLIMDIMID
        DO I=1,NDIMS
           STATUS = NF_INQ_DIM(NCID,i,BUFFER,ILATID)  !! extract dimension name
           STATUS = NF_INQ_DIMLEN(NCID,i,ILATID)
           IF (trim(BUFFER) .eq. 'Station')then
              NWLON_STA=ILATID
           ELSEIF((trim(BUFFER) .eq. 'Constituents'))then
              NCON=ILATID
           ELSEIF (trim(BUFFER) .eq. 'staID')then
              NCHAR=ILATID
           endif
        ENDDO

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        STATUS = NF_INQ_VARID(NCID,'stationID',IDVAR)
        STATUS = NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
        status = NF_INQ_VARDIMID(NCID,IDVAR,dimids)
        do i=1,NDIMS
             STATUS = NF_INQ_DIMLEN(NCID,dimids(i),DIMS(i))
             write(*,*) TRIM(VNAME),' dim ',i,' = ',DIMS(i)
        enddo
        allocate(stationID(DIMS(1),DIMS(2)) )
        STATUS = NF_GET_VAR_TEXT(NCID,IDVAR,stationID)

        DO I=1,4
          DIMS(I)=1
        ENDDO	
        STATUS = NF_INQ_VARID(NCID,'constituentName',IDVAR)
        STATUS = NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
        status = NF_INQ_VARDIMID(NCID,IDVAR,dimids)
        do i=1,NDIMS
             STATUS = NF_INQ_DIMLEN(NCID,dimids(i),DIMS(i))
        enddo
        allocate(constituents(DIMS(1),DIMS(2)) )
        STATUS = NF_GET_VAR_TEXT(NCID,IDVAR,constituents)
        DO I=1,4
          DIMS(I)=1
        ENDDO	

        STATUS = NF_INQ_VARID(NCID,'amplitude',IDVAR)
        STATUS = NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
        status = NF_INQ_VARDIMID(NCID,IDVAR,dimids)
        do i=1,NDIMS
             STATUS = NF_INQ_DIMLEN(NCID,dimids(i),DIMS(i))
             write(*,*) TRIM(VNAME),' dim ',i,' = ',DIMS(i)
        enddo
        allocate(tide_amp(DIMS(1),DIMS(2)) )
        allocate(tide_epoc(DIMS(1),DIMS(2)))
        STATUS = NF_INQ_VARID(NCID,'amplitude',IDVAR)
        STATUS = NF_GET_VAR_REAL(NCID,IDVAR,tide_amp)
        STATUS = NF_INQ_VARID(NCID,'phase',IDVAR)
        STATUS = NF_GET_VAR_REAL(NCID,IDVAR,tide_epoc)
        STATUS = NF_CLOSE(NCID)
      ENDIF


!-----------------------------------------------------------------------
!   reading in FVCOM or SELFE model grid information
!-----------------------------------------------------------------------
      IF (trim(OCEAN_MODEL) .EQ. "FVCOM" .OR. 
     1    trim(OCEAN_MODEL) .EQ. "SELFE")THEN
        WRITE(*,*)'Reading Unstructured grid file ...',trim(GRIDFILE)
        OPEN(40,file=trim(GRIDFILE))
        read(40,*)
        READ(40,*)NELEm,NODEm
        ALLOCATE(lonm(NODEm) )
        ALLOCATE(latm(NODEm) )
!        ALLOCATE(angm(NODEm) )
        ALLOCATE(lonc(NELEm) )
	ALLOCATE(latc(NELEm) )
!        IF (TRIM(OCEAN_MODEL) .EQ. "FVCOM" )THEN
!          allocate(uwindm(NELEm))
!          allocate(vwindm(NELEm))

!        ELSEIF (TRIM(OCEAN_MODEL) .EQ. "SELFE" )THEN
!          allocate(uwindm(NODEm))
!          allocate(vwindm(NODEm))
!	ENDIF  
        ALLOCATE(hm(NODEm) )
        ALLOCATE(nv(NELEm,3))
        do n=1,NODEm
          read(40,*) im,lonm(n),latm(n),hm(n)
	  if(lonm(n) .GT. 180.0)lonm(n)=lonm(n)-360.
        enddo
        do n=1,nelem
          read(40,*) idum,idum,(nv(n,i),i=1,3)
          lonc(n)=(lonm(nv(n,1))+lonm(nv(n,2))+lonm(nv(n,3)) )/3.0
          latc(n)=(latm(nv(n,1))+latm(nv(n,2))+latm(nv(n,3)) )/3.0
	  if(lonc(n) .GT. 180.0)lonc(n)=lonc(n)-360.
        enddo
        CLOSE(40)
      ENDIF
      IF (trim(OCEAN_MODEL) .EQ. "SELFE" .OR. 
     1    trim(OCEAN_MODEL) .EQ. "selfe")THEN
        ALLOCATE(weight_t_nudge(NODEm) )
	OPEN(40,file=trim(T_NUDGE_FILE))
	READ(40,*)
	READ(40,*)
	NUM_T_NUDGE=0
        do n=1,NODEm
          read(40,*) im,dummylon,dummylat,weight_t_nudge(n)
	  if(weight_t_nudge(n) .GT. 0.0)then
	    NUM_T_NUDGE=NUM_T_NUDGE+1
	  endif  
        enddo
        allocate (lon_nudge(NUM_T_NUDGE) )
        allocate (lat_nudge(NUM_T_NUDGE) )
        ALLOCATE(node_t_nudge(NUM_T_NUDGE) )
        CLOSE(40)
      ENDIF
      hmin=minval(hm)
      hmax=maxval(hm)
C -------------------------------------------------------------------
C   open and read OBC control file  
C -------------------------------------------------------------------
      WRITE(*,*)'Reading OBC control file'
      
      OPEN(20,FILE=TRIM(OBC_CTL_FILE) )
      READ(20,*)NSTA,NOBC,NEOBC,DELT 
      WRITE(*,*)'NSTA= ',NSTA,'NOBC=',NOBC,
     1 'NEOBC= ',NEOBC,'DELT= ',DELT
      NOBC_ORI=NOBC
! Arrays for real time observations
      ALLOCATE(SID(NSTA) )
      allocate(NOS_ID(NSTA) )
      allocate (NWS_ID(NSTA))
      allocate (AGENCY_ID(NSTA))
      allocate ( DATUM(NSTA))
      allocate ( WL_FLAG(NSTA))
      allocate ( TS_FLAG(NSTA))
      allocate (BACKUP_SID(NSTA))
      allocate ( GRIDID_STA(NSTA) )
      allocate ( As(NSTA) )

      allocate ( GRIDID(NOBC) )
      allocate ( IOBC(NOBC) )
      allocate ( JOBC(NEOBC) )
      allocate ( WL_STA(NOBC) )
      allocate ( TS_STA(NOBC) )
      allocate ( WL_SID_1(NOBC) )
      allocate ( WL_SID_2(NOBC) )
      allocate ( TS_SID_1(NOBC) )
      allocate ( TS_SID_2(NOBC) )
      allocate ( WL_S_1(NOBC) )
      allocate ( WL_S_2(NOBC) )
      allocate ( TS_S_1(NOBC) )
      allocate ( TS_S_2(NOBC) )
      allocate (OBC_ID(NOBC))
      allocate (lonOBC(NOBC))
      allocate (latOBC(NOBC))
      allocate (loneOBC(NEOBC))
      allocate (lateOBC(NEOBC))
      allocate (hOBC(NOBC))
      allocate (nvOBC(NEOBC,3))
      allocate (partition(NEOBC) )
      allocate (siglay(NOBC,KBm) )
      allocate (siglev(NOBC,KBm+1) )
      allocate(sigma(KBm+1) )
      allocate(Zsigma(KBm+1) )

      ALLOCATE( NTR(NSTA) )
      ALLOCATE( NTR_T(NSTA) )
      ALLOCATE( NTR_S(NSTA) )
      allocate ( RTIME(NSTA,NMAX) )
      allocate ( RTIME_T(NSTA,NMAX) )
      allocate ( RTIME_S(NSTA,NMAX) )
      allocate ( TIME_PRD(NMAX) )
      allocate ( WL_PRD(NSTA,NMAX) )
      allocate ( WL_OBS(NSTA,NMAX) )
      allocate ( SWL_OBS(NSTA,NMAX) )
      allocate ( T_OBS(NSTA,NMAX) )
      allocate ( S_OBS(NSTA,NMAX) )


      ALLOCATE( K_clim(NSTA) )
      allocate (TIME_clim(NMAX) )
      allocate (depth_clim(NSTA,50) ) 
      allocate (T_clim(NSTA,NMAX,50) ) 
      allocate (S_clim(NSTA,NMAX,50) ) 


      IF (ALLOCATED(oned1)) DEALLOCATE(oned1)
      IF (ALLOCATED(oned2)) DEALLOCATE(oned2)
      IF (ALLOCATED(oned3)) DEALLOCATE(oned3)
      IF (ALLOCATED(oned4)) DEALLOCATE(oned4)
      allocate(oned1(NMAX) )
      allocate(oned2(NMAX) )
      allocate(oned3(NMAX) )
      allocate(oned4(NMAX) )

14    READ(20,*)BUFFER 
      BUFFER=trim(adjustL(BUFFER))
      LL=len_trim(BUFFER)
      IF (LL .LE. 0)GOTO 14
      READ(20,*)BUFFER 
      DO N=1,NSTA
        READ(20,*)SID(N),NOS_ID(N),NWS_ID(N),AGENCY_ID(N),DATUM(N)
     &	,WL_FLAG(N),TS_FLAG(N),BACKUP_SID(N),GRIDID_STA(N),As(N)
      ENDDO
16    READ(20,*)BUFFER 
      BUFFER=trim(adjustL(BUFFER))
      LL=len_trim(BUFFER)
      IF (LL .LE. 0)GOTO 16
      READ(20,*)BUFFER
      DO N=1,NOBC
        READ(20,*)GRIDID(N),IOBC(N),WL_STA(N),WL_SID_1(N),
     &	WL_S_1(N),WL_SID_2(N),WL_S_2(N),TS_STA(N),TS_SID_1(N),
     &  TS_S_1(N),TS_SID_2(N),TS_S_2(N)
        lonOBC(N)=lonm(IOBC(N) )
        latOBC(N)=latm(IOBC(N) )
        hOBC(N)=hm(IOBC(N) )
	if(lonOBC(n) .GT. 180.0)lonOBC(n)=lonOBC(n)-360.
      ENDDO
17    READ(20,*)BUFFER 
      BUFFER=trim(adjustL(BUFFER))
      LL=len_trim(BUFFER)
      IF (LL .LE. 0)GOTO 17
      READ(20,*)BUFFER 
      DO N=1,NEOBC
        READ(20,*)Idummy,JOBC(N)
        loneOBC(N)=lonc(JOBC(N) )
        lateOBC(N)=latc(JOBC(N) )
	DO L=1,3
	  nvOBC(N,L)=nv(JOBC(N),L)
	ENDDO  
	if(loneOBC(n) .GT. 180.0)loneOBC(n)=loneOBC(n)-360.
      ENDDO
      USGS_L=.FALSE.
      DO N=1,NSTA
        IF(TRIM(AGENCY_ID(N)) .EQ. "USGS" )USGS_L=.TRUE.
      ENDDO
      NREC=NINT( (day_end-day_start)*86400/DELT)+1
      IF (ALLOCATED(TIME_M)) DEALLOCATE(TIME_M)
      allocate (TIME_M(NREC) )
      DO N=1,NREC
	 TIME_M(N)=day_start+(N-1)*DELT/86400.0
      ENDDO
!-----------------------------------------------------------------------
!  Define S-Curves in domain [-1 < sc < 0] at vertical W- and
!  RHO-points.
!-----------------------------------------------------------------------
      IF (trim(OCEAN_MODEL) .EQ. "SELFE" .OR. 
     1    trim(OCEAN_MODEL) .EQ. "selfe")THEN
        write(*,*)'vgrid=',trim(VGRIDFILE)
        OPEN(40,file=trim(VGRIDFILE))
13      read(40,'(a200)',end=15)BUFFER
        write(*,*)'buffer=',trim(buffer)
        if(BUFFER(1:1) .eq. '!' .OR. BUFFER(1:1) .eq. 'C')THEN
	  WRITE(*,*)'reading a comment line'
	  goto 13
	else  
          BACKSPACE(40)
          READ(40,*)nvrt,KZ,h_s
	  if(nvrt .NE. KBm)THEN
	    WRITE(*,*)
     1	    'vertical parameters KBm and KB are not consistent' 
            WRITE(*,*)'check main ctl and vgrid files'
	    STOP
	  ENDIF 
          IF (ALLOCATED(ztot)) DEALLOCATE(ztot)
          IF (ALLOCATED(zcor)) DEALLOCATE(zcor)
          IF (ALLOCATED(sigma)) DEALLOCATE(sigma)
          ALLOCATE(ztot(nvrt) )
          ALLOCATE(zcor(nvrt) )
          ALLOCATE(sigma(nvrt) )
          read(40,*)  ! for adding comment "Z levels"
          DO I=1,KZ-1
            read(40,*)idummy,ztot(I)
          ENDDO	
          read(40,*) ! level kz
	  ztot(kz)=-h_s
          nsig=nvrt-kz+1  ! # of S levels including "bottom" & free surface
          read(40,*)  ! for adding comment "S levels"
          read(40,*)h_c,theta_b,theta_f
	  if (h_c .lt. 5.0)THEN 
	     write(*,*)'h_c needs to be larger: ',h_c
	     stop
	  endif
	  if ( theta_b .lt. 0.0 .or. theta_b .gt. 1.0)then
	    write(*,*)'Wrong theta_b: ', theta_b
	    stop
	  endif  
	  if ( theta_f .le. 0.0)then
	    write(*,*)'Wrong theta_f: ', theta_f
	    stop
	  endif  
          do k=kz,nvrt
            kin=k-kz+1
	    read(40,*)idummy,sigma(kin)
            print*, 'K, sigma=',kin,sigma(kin)
          enddo !k
!          do n=1,NODEm
!	    h0=0.01   !min. depth for wet/dry;
!            eta=0.0   !elevation zeta
!	    dp=hm(n)  ! water depth
!            call zcor_SZ(dp,eta,h0,h_s,h_c,theta_b,theta_f,kz,nvrt
!     1      ,ztot,sigma,zcor,idry,kbp)
!            DO K=kbp,nvrt
!	      siglev(I,K)=zcor(K)
!	    ENDDO  
!            DO K=1,kbp-1
!	      siglev(I,K)=zcor(K)
!	    ENDDO  
!          enddo
        endif
	  
      ENDIF
15    CLOSE(40)

!      DO K=1,KBm
!        WRITE(*,*)'K= ',K,(siglev(I,K),I=1,3)      
!      ENDDO
!-----------------------------------------------------------------------
!  End of Define sigma coordinates in domain [-1 < sc < 0] at vertical points.
!-----------------------------------------------------------------------
      IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
      IF (ALLOCATED(itmp4d)) DEALLOCATE(itmp4d)

C------------------------------------------------------------------
C  Begin to read in climatological temperature and salinity 
C  and temporally interpolated into equally interval of DELT 
C------------------------------------------------------------------
!      NREC=NINT( (jdaye-jdays)*24/DELT)+1
      NREC=NINT( (day_end-day_start)*86400/DELT)+1
      DO N=1,NREC
	 TIME_clim(N)=day_end-day_start+(N-1)*DELT/86400.0
      ENDDO
      NT_clim=NREC
      DO N=1,NSTA
         IYR=IYRS
         yearb=IYRS
         IF(TS_FLAG(N) .GT. 0)THEN
            BUFFER1='nos.'//trim(OFS)//'.obc.clim.ts.'
	    BUFFER=TRIM(BUFFER1)//TRIM(NOS_ID(N))//'.dat'
!	    FIN=TRIM(FIXofs)//'/'//TRIM(BUFFER)
	    FIN=TRIM(BUFFER)
	    write(*,*)'FIN= ',trim(FIN)
            INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
            IF(.NOT. FEXIST)THEN
	      WRITE(*,*)'climatologic T&S file is not found'
	      WRITE(*,*)'please provide climatologic T&S file at '
	      WRITE(*,*)'station ', TRIM(NOS_ID(N))
              WRITE(ICORMS,'(a)')'CRITICAL FAILURE IN CREATING OBC' 
	      STOP
	    ELSE  
              yearb=IYRS
              monthb=1.0
              dayb=1.0
              hourb=0
              jday0=JULIAN(yearb,monthb,dayb,hourb)

	      CLOSE(10)
	      OPEN(10,FILE=TRIM(FIN) )
	      READ(10,*)K_clim(N)
	      READ(10,*)(DEPTH_clim(N,K),K=1,K_clim(N))
              READ(10,*)
	      ICOUNT=0
C18	      READ(10,*,end=20)mon,IDD,(ONED1(K),ONED2(K),K=1,K_clim(N))
18            READ(10,*,end=20)dummy,mon,IDD,
     &        (ONED1(K),ONED2(K),K=1,K_clim(N))

              ICOUNT=ICOUNT+1
C              yearb=IYRS
C              monthb=mon
C              dayb=IDD
C              hourb=0
C              day0=JULIAN(yearb,monthb,dayb,hourb)-jbase_date
               day0=jday0+dummy-jbase_date
	      IF((ICOUNT .EQ. 1) .and. (day0 .GT. day_start) )then
                 ONED3(ICOUNT)=day_start
	         DO K=1,K_clim(N)
	           T_clim(N,ICOUNT,K)=ONED1(K)
	           S_clim(N,ICOUNT,K)=ONED2(K)
	         ENDDO
                 ICOUNT=ICOUNT+1
	      ENDIF
              ONED3(ICOUNT)=day0
	      DO K=1,K_clim(N)
	        T_clim(N,ICOUNT,K)=ONED1(K)
	        S_clim(N,ICOUNT,K)=ONED2(K)
	      ENDDO
	      GOTO 18
20            CONTINUE
	      CLOSE(10)
	      IF(ONED3(ICOUNT) .LT. day_end )then
                 ICOUNT=ICOUNT+1
                 ONED3(ICOUNT)=day_end
	         DO K=1,K_clim(N)
	           T_clim(N,ICOUNT,K)=ONED1(K)
	           S_clim(N,ICOUNT,K)=ONED2(K)
	         ENDDO
	      ENDIF
              WRITE(*,*)'time range= ',day_start,day_end,
     &	      ONED3(1),ONED3(ICOUNT),ICOUNT
	      DO K=1,K_clim(N)
              DO N1=1,ICOUNT
	        oned1(N1)=T_clim(N,N1,K)
	        oned2(N1)=S_clim(N,N1,K)
	      ENDDO  
	      CALL lineararray(NREC,TIME_clim,oned4,ICOUNT,oned3,oned1)
              DO N1=1,NREC
	       T_clim(N,N1,K) =oned4(N1)
              ENDDO
	      CALL lineararray(NREC,TIME_clim,oned4,ICOUNT,oned3,oned2)
              DO N1=1,NREC
	       S_clim(N,N1,K) =oned4(N1)
              ENDDO
	      
              ENDDO
	      
            ENDIF
         ENDIF
      ENDDO


C------------------------------------------------------------------
C-------- Begin to read in WOA05 NetCDF file ----------------------
C------------------------------------------------------------------
30    CONTINUE
      IF (TRIM(DBASE_TS) .EQ. 'WOA05' )THEN
!	 FIN=TRIM(FIXofs)//'/'//TRIM(OBC_CLIM_FILE)
	 FIN=TRIM(OBC_CLIM_FILE)
         INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
         IF(.NOT. FEXIST)THEN
	    WRITE(*,*)'Climatologic T&S file (WOA) is not found'
	    WRITE(*,*)'Please provide climatologic T&S file of:'
	    WRITE(*,*)TRIM(OBC_CLIM_FILE)//' in '//TRIM(FIXofs)
         WRITE(ICORMS,'(a)')'Climatologic T&S file (WOA) is not found'
         WRITE(ICORMS,'(a)')TRIM(OBC_CLIM_FILE)//' in '//TRIM(FIXofs)
         WRITE(ICORMS,'(a)')'CRITICAL FAILURE IN CREATING OBC' 
 	    STOP
	 ENDIF  
         FILE_TS=TRIM(FIN)
	 VNAME='t0112an1'
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IM=DIMS(1)
	 JM=DIMS(2)
	 KB=DIMS(3)
	 NT=DIMS(4)
         IF (ALLOCATED(lon)) DEALLOCATE(lon)
         IF (ALLOCATED(lat)) DEALLOCATE(lat)
         IF (ALLOCATED(depth)) DEALLOCATE(depth)
         IF (ALLOCATED(ts_time)) DEALLOCATE(ts_time)
         ALLOCATE(lon(IM,JM) )
         ALLOCATE(lat(IM,JM) )
         ALLOCATE(depth(KB) )
         ALLOCATE(ts_time(NT+2) )

        DO I=1,4
          DIMS(I)=1
        ENDDO	
	 VNAME='lon'
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
	 write(*,*)'dims=',dims,NDIM
         IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
         ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
         DO I=1,IM
         DO J=1,JM
            lon(i,j)=TMP4D(i,1,1,1)
 	    if(lon(i,j) .gt. 180)lon(i,j)=lon(i,j)-360.
         ENDDO
         ENDDO
        DO I=1,4
          DIMS(I)=1
        ENDDO	
	 VNAME='lat'
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
         ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
         DO I=1,IM
         DO J=1,JM
            lat(i,j)=TMP4D(J,1,1,1)
         ENDDO
         ENDDO
	 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
CC select subdomain I and J index        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         IMIN=99999
         IMAX=-9999
         JMIN=99999
         JMAX=-9999
         DO I=1,IM
         DO J=1,JM
           IF(lon(i,j) .GE. minlon .and. lon(i,j) .LE. maxlon
     &     .and. lat(i,j) .GE. minlat .and. lat(i,j) .LE. maxlat)then
             IF(I .LT. IMIN)IMIN=I 	  
             IF(I .GT. IMAX)IMAX=I 	  
             IF(J .LT. JMIN)JMIN=J 	  
             IF(J .GT. JMAX)JMAX=J
	   ENDIF
         ENDDO
         ENDDO
	 IMIN=IMIN-1     !!extending WOA05 coverage
	 IMAX=IMAX+1	  	  
	 JMIN=JMIN-1
	 JMAX=JMAX+1	  	  
         ISUB=IMAX-IMIN+1	 
         JSUB=JMAX-JMIN+1	 
         write(*,*)'subdomain I J=',ISUB,JSUB,IMIN,IMAX,JMIN,JMAX	        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc allocate sizes of arrays 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         NNSUB=ISUB*JSUB
         IF (ALLOCATED(lonsub)) DEALLOCATE(lonsub)
         IF (ALLOCATED(latsub)) DEALLOCATE(latsub)
         IF (ALLOCATED(masksub)) DEALLOCATE(masksub)
         IF (ALLOCATED(temp)) DEALLOCATE(temp)
         IF (ALLOCATED(salt)) DEALLOCATE(salt)
         allocate(lonsub(ISUB,JSUB))
         allocate(latsub(ISUB,JSUB))
         allocate(masksub(ISUB,JSUB))
         allocate(temp(ISUB,JSUB,KB,NT+2))
         allocate(salt(ISUB,JSUB,KB,NT+2))
!         allocate(temp(ISUB,JSUB,KB,240))
!         allocate(salt(ISUB,JSUB,KB,240))
         DO J=1,JSUB
         DO I=1,ISUB
          lonsub(i,j)=lon(IMIN+I-1,JMIN+J-1)
          latsub(i,j)=lat(IMIN+I-1,JMIN+J-1)
	  write(55,'(2f12.4,2i5)')lonsub(i,j),latsub(i,j),i,j
         ENDDO
         ENDDO   
         close(55) 
        DO I=1,4
          DIMS(I)=1
        ENDDO	
         VNAME='depth'         
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
         ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
         DO K=1,KB
            depth(K)=TMP4D(K,1,1,1)
         ENDDO
        if(depth(KB) .LT. hmax)depth(KB)=hmax
        DO I=1,4
          DIMS(I)=1
        ENDDO	

         VNAME='time'         
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
         ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
         yearb=IYRS
         monthb=1
         dayb=1
         hourb=0
         jday=JULIAN(yearb,monthb,dayb,hourb)
	 ts_time(1)=jday-jbase_date
	 ts_time(NT+2)=370.0+jday-jbase_date  !! for crossing year problem zaj
         DO N=2,NT+1
            ts_time(N)=TMP4D(N-1,1,1,1)+jday-jbase_date
!	    write(*,*)N,ts_time(n)
         ENDDO

        DO I=1,4
          DIMS(I)=1
        ENDDO	
	 VNAME='t0112an1'
         missvalue=1.e+20
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
         ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
         DO I=1,ISUB
         DO J=1,JSUB
         DO K=1,KB
         DO N=2,NT+1
           TEMP(I,J,K,N)=TMP4D(IMIN+I-1,JMIN+J-1,K,N-1)
         if(abs(TEMP(I,J,K,N)-missvalue) .LE. 0.001)TEMP(I,J,K,N)=-99999.9
         ENDDO	
         ENDDO	
         ENDDO	
         ENDDO
         DO I=1,ISUB
         DO J=1,JSUB
         DO K=1,KB
           TEMP(I,J,K,1)=(TEMP(I,J,K,2)+TEMP(I,J,K,NT+1) )/2.0
           TEMP(I,J,K,NT+2)=TEMP(I,J,K,1)
         ENDDO	
         ENDDO	
         ENDDO	
        DO I=1,4
          DIMS(I)=1
        ENDDO	
	 VNAME='s0112an1'
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
         ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
         DO I=1,ISUB
         DO J=1,JSUB
         DO K=1,KB
         DO N=2,NT+1
           SALT(I,J,K,N)=TMP4D(IMIN+I-1,JMIN+J-1,K,N-1)
         if(abs(SALT(I,J,K,N)-missvalue) .LE. 0.001)SALT(I,J,K,N)=-99999.9
         ENDDO	
         ENDDO	
         ENDDO	
         ENDDO
         DO I=1,ISUB
         DO J=1,JSUB
         DO K=1,KB
           SALT(I,J,K,1)=(SALT(I,J,K,2)+SALT(I,J,K,NT+1) )/2.0
           SALT(I,J,K,NT+2)=SALT(I,J,K,1)
         ENDDO	
         ENDDO	
         ENDDO	
        DO I=1,4
          DIMS(I)=1
        ENDDO	
	 VNAME='landsea'
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
         ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
         DO I=1,ISUB
         DO J=1,JSUB
           masksub(I,J)=TMP4D(IMIN+I-1,JMIN+J-1,1,1)
!	   write(*,*)'lan mask=',I,J,masksub(I,J)
         ENDDO	
         ENDDO	
         DO I=1,ISUB
         DO J=1,JSUB
         DO K=1,KB
	   write(*,*)'K=',K,lonsub(I,J),latsub(I,J),masksub(I,J)
     &	   ,TEMP(I,J,K,2),SALT(I,J,K,2)
         ENDDO	
         ENDDO	
         ENDDO

	 NT=NT+2
	 WRITE(*,*)'WOA05 NT+2=',NT 	
      ENDIF
C------------------------------------------------------------------
C     Read and process available NCOM forecast files by the provided start time       
C------------------------------------------------------------------
      IF ( (TRIM(DBASE_TS) .EQ. 'NCOM') 
     & .OR.(TRIM(DBASE_TS) .EQ. 'NCOMeast')
     & .OR.(TRIM(DBASE_TS) .EQ. 'AMSEAS') )THEN
         NCOM_FILE=TRIM(DBASE_TS)//"_FILE"
!         NCOM_FILE="NCOM_FILE"
         IFILE_NCOM=0
         INQUIRE(FILE=trim(NCOM_FILE),EXIST=FEXIST)
         if(FEXIST)THEN
           CLOSE(60)
           OPEN(60,file=trim(NCOM_FILE))
           DO I=1,999
             READ(60,'(a100)',END=333)BUFFER
             FILE_NCOM(I)=trim(adjustL(BUFFER))
             READ(60,'(a100)',END=333)BUFFER
             FILE_NCOM_TIDE(I)=trim(adjustL(BUFFER))
           ENDDO
333        CLOSE(60)
	   IFILE_NCOM=I-1
         ENDIF
         WRITE(*,*)'Total number of NCOM FILES= ',IFILE_NCOM
	 IF(IFILE_NCOM .LT. 1)THEN
	     WRITE(*,*)'No NCOM file is available in this period'
             WRITE(*,*)'use WOA05 climatological dataset to replace'
             DBASE_TS='WOA05'
             WRITE(ICORMS,'(a)')'NCOM FILE IS NOT FOUND' 
             WRITE(ICORMS,'(a)')'USE CLIMATOLOGIC BACKUP WOA05' 
	     IF ( TRIM(DBASE_WL) .EQ. 'NCOM')DBASE_WL='ETSS'
             GOTO 30
	 ENDIF  
	 DO I=1,IFILE_NCOM
	    WRITE(*,*)'I=',I,TRIM(FILE_NCOM(I))
	    WRITE(*,*)'I=',I,TRIM(FILE_NCOM_TIDE(I))
	 ENDDO
C-----------------------------------------------------------------------
C     Open and read all NCOM netCDF files available during start time to end time       
C-----------------------------------------------------------------------
         FIN=TRIM(FILE_NCOM(1))
         DO I=1,4
          DIMS(I)=1
         ENDDO	
         VNAME='salinity'
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IM=DIMS(1)
	 JM=DIMS(2)
	 KB=DIMS(3)
	 NT=DIMS(4)
	 WRITE(*,*)'NCOM IM=',IM,'JM= ',JM,'KB= ',KB,'NT= ',NT
         IF (ALLOCATED(lon)) DEALLOCATE(lon)
         IF (ALLOCATED(lat)) DEALLOCATE(lat)
         IF (ALLOCATED(depth)) DEALLOCATE(depth)
         IF (ALLOCATED(zeta_time)) DEALLOCATE(zeta_time)
         IF (ALLOCATED(ts_time)) DEALLOCATE(ts_time)
         ALLOCATE(lon(IM,JM) )
         ALLOCATE(lat(IM,JM) )
         ALLOCATE(depth(KB) )
         ALLOCATE(zeta_time(NMAX) )
         ALLOCATE(ts_time(NMAX) )
        DO I=1,4
          DIMS(I)=1
        ENDDO	
	 VNAME='lon'
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
         ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,7)
         DO I=1,IM
         DO J=1,JM
            lon(i,j)=TMP4D(i,1,1,1)
 	    if(lon(i,j) .gt. 180)lon(i,j)=lon(i,j)-360.
         ENDDO
         ENDDO
        DO I=1,4
          DIMS(I)=1
        ENDDO	
	 VNAME='lat'
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
         ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,7)
         DO I=1,IM
         DO J=1,JM
            lat(i,j)=TMP4D(J,1,1,1)
         ENDDO
         ENDDO
        DO I=1,4
          DIMS(I)=1
        ENDDO	
	 VNAME='depth'
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
         ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,7)
         DO K=1,KB
            depth(K)=TMP4D(K,1,1,1)
         ENDDO
C-----------------------------------------------------------------------
C        select subdomain I and J index        
C-----------------------------------------------------------------------
         IMIN=99999
         IMAX=-9999
         JMIN=99999
         JMAX=-9999
         DO I=1,IM
         DO J=1,JM
           IF(lon(i,j) .GE. minlon .and. lon(i,j) .LE. maxlon
     &    .and. lat(i,j) .GE. minlat .and. lat(i,j) .LE. maxlat)then
             IF(I .LT. IMIN)IMIN=I 	  
             IF(I .GT. IMAX)IMAX=I 	  
             IF(J .LT. JMIN)JMIN=J 	  
             IF(J .GT. JMAX)JMAX=J
	   ENDIF
         ENDDO
         ENDDO	  	  
         ISUB=IMAX-IMIN+1	 
         JSUB=JMAX-JMIN+1	 
         write(*,*)'subdomain I J=',ISUB,JSUB,IMIN,IMAX,JMIN,JMAX
C-----------------------------------------------------------------------
C allocate sizes of arrays for NCOM products 
C-----------------------------------------------------------------------
         NNSUB=ISUB*JSUB
         IF (ALLOCATED(lonsub)) DEALLOCATE(lonsub)
         IF (ALLOCATED(latsub)) DEALLOCATE(latsub)
         IF (ALLOCATED(temp)) DEALLOCATE(temp)
         IF (ALLOCATED(salt)) DEALLOCATE(salt)
!         IF (ALLOCATED(u)) DEALLOCATE(u)
!         IF (ALLOCATED(v)) DEALLOCATE(v)
         IF (ALLOCATED(WL)) DEALLOCATE(WL)
         IF (ALLOCATED(tide_NCOM)) DEALLOCATE(tide_NCOM)
         allocate(lonsub(ISUB,JSUB))
         allocate(latsub(ISUB,JSUB))
         allocate(temp(ISUB,JSUB,KB,NT*10))
         allocate(salt(ISUB,JSUB,KB,NT*10))
!         allocate(u(ISUB,JSUB,KB,NT*10))
!         allocate(v(ISUB,JSUB,KB,NT*10))
         allocate(WL(ISUB,JSUB,NT*10))
         allocate(tide_NCOM(ISUB,JSUB,NT*10))
         DO N=1,NT*10
         DO J=1,JSUB
         DO I=1,ISUB
	    WL(I,J,N)=0.0
	    tide_NCOM(I,J,N)=0.0
         ENDDO
         ENDDO
         ENDDO   
         DO J=1,JSUB
         DO I=1,ISUB
            lonsub(i,j)=lon(IMIN+I-1,JMIN+J-1)
            latsub(i,j)=lat(IMIN+I-1,JMIN+J-1)
!	    write(77,'(2F12.4,2I6)')lonsub(i,j),latsub(i,j),I,J
         ENDDO
         ENDDO   
         ICOUNT=1
	 NREC=1
         N0=0
         TS_TIME(1)=-9999.0	 
	 DO IFILE=1,IFILE_NCOM
           FIN=TRIM(FILE_NCOM(IFILE))
	   write(*,*)'reading NCOM NetCDF file...',IFILE,TRIM(FIN)
	   VNAME='time'
	   ANAME='units'
           DO I=1,4
             DIMS(I)=1
           ENDDO	
           CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
           IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
           ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
	   NT=DIMS(1)
           CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,6)
	   ANAME=trim(adjustL(ANAME))
	   LEN=LEN_TRIM(ANAME)
           LL=INDEX(ANAME,'minute')         
	   IF(LL .GT. 0)scale_time=1.0/1440.0
           LL=INDEX(ANAME,'hour')         
	   IF(LL .GT. 0)scale_time=1.0/24.0

	   ANAME='time_origin'
           CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,6)
	   ANAME=trim(adjustL(ANAME))
           write(*,*)'time_origin=',trim(ANAME)
           read(ANAME,80)IYR,IMM,IDD,IHH
80         format(I4,1x,i2,1x,i2,1x,i2)
           write(*,*)'basetime=',IYR,IMM,IDD,IHH
           yearb=IYR
           monthb=IMM
           dayb=IDD
           hourb=IHH
           jday=JULIAN(yearb,monthb,dayb,hourb)
           CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,7)
           dummy=TMP4D(1,1,1,1)/24.+jday-jbase_date
	   print *,dummy,TMP4D(1,1,1,1),scale_time,jday,jbase_date
	   
	   N0=0
           DO N=1,NREC
             IF(TS_TIME(N) .GE. DUMMY)THEN
	       N0=N-1 
	       GOTO 82
	     ENDIF  
           ENDDO	
82         CONTINUE
           IF( N .GT. NREC .AND. NREC .GT. 1)N0=NREC
           ICOUNT=N0
           
           DO N=1,NT
             TIME_NCOM=TMP4D(N,1,1,1)/24.+jday-jbase_date
 !            if(TIME_NCOM .GT. TS_TIME(ICOUNT) )THEN
               ICOUNT=ICOUNT+1
	       TS_TIME(ICOUNT)=TIME_NCOM
	       print *,'NCOM time=',ICOUNT,TS_TIME(ICOUNT)
 !            endif 
	   ENDDO   
           write(*,*)'first time= ',dummy,TS_TIME(NREC),NREC,N0
           STATUS = NF_OPEN(trim(FIN),NF_NOWRITE, NCID)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     stop 'open NCOM netCDF file of failed'
           ENDIF  
           IF (ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
           ALLOCATE(tmp1d(IM) )
           IF (ALLOCATED(tmp3d)) DEALLOCATE(tmp3d)
           ALLOCATE(tmp3d(IM,JM,NT) )
           IF (ALLOCATED(itmp3d)) DEALLOCATE(itmp3d)
           ALLOCATE(itmp3d(IM,JM,NT) )
           IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
           ALLOCATE(tmp4d(IM,JM,KB,NT) )

           STATUS = NF_INQ_VARID(NCID,'surf_el',IDVAR)
           STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
           STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
           STATUS = NF_GET_ATT_INT(NCID,IDVAR,'missing_value',IATT)
           STATUS = NF_GET_VAR_REAL(NCID,IDVAR,tmp3d)
           print *,'first data=',tmp3d(1,1,1),tmp3d(2,1,1)

	   missvalue=IATT*scale+offset
           print *,'factor=',scale,offset,missvalue
!    !      write(*,*)tmp3d*scale
           ICOUNT=N0
           DO N=1,NT
	     ICOUNT=ICOUNT+1
           DO I=1,ISUB
           DO J=1,JSUB
             WL(I,J,ICOUNT)=TMP3D(IMIN+I-1,JMIN+J-1,N)*scale+offset
             if(abs(WL(I,J,ICOUNT)-missvalue) .LE. 0.001)
     &	     WL(I,J,ICOUNT)=-99999.9
           ENDDO	
           ENDDO	
           ENDDO	
           STATUS = NF_INQ_VARID(NCID,'water_temp',IDVAR)
           STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
           STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
           STATUS = NF_GET_ATT_INT(NCID,IDVAR,'missing_value',IATT)
	   missvalue=IATT*scale+offset
!	   print *,'scale of temp=',scale,offset,missvalue
           STATUS = NF_GET_VAR_REAL(NCID,IDVAR,tmp4d)
           ICOUNT=N0
           DO N=1,NT
	     ICOUNT=ICOUNT+1
           DO I=1,ISUB
           DO J=1,JSUB
           DO K=1,KB
            TEMP(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)*scale+offset
            if(abs(TEMP(I,J,K,ICOUNT)-missvalue) .LE. 0.001)
     &	    TEMP(I,J,K,ICOUNT)=-99999.9
           ENDDO	
           ENDDO	
           ENDDO	
           ENDDO	
           STATUS = NF_INQ_VARID(NCID,'salinity',IDVAR)
           STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
           STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
           STATUS = NF_GET_VAR_REAL(NCID,IDVAR,TMP4D)
           ICOUNT=N0
           DO N=1,NT
	     ICOUNT=ICOUNT+1
           DO I=1,ISUB
           DO J=1,JSUB
           DO K=1,KB
            SALT(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)*scale+offset
            if(abs(SALT(I,J,K,ICOUNT)-missvalue) .LE. 0.001)
     &	    SALT(I,J,K,ICOUNT)=-99999.9
           ENDDO	
           ENDDO	
           ENDDO
           ENDDO
!           STATUS = NF_INQ_VARID(NCID,'water_u',IDVAR)
!           STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
!           STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
!           STATUS = NF_GET_VAR_REAL(NCID,IDVAR,TMP4D)
!           ICOUNT=N0
!           DO N=1,NT
!	     ICOUNT=ICOUNT+1
!           DO I=1,ISUB
!           DO J=1,JSUB
!           DO K=1,KB
!            u(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)*scale+offset
!            if(abs(u(I,J,K,ICOUNT)-missvalue) .LE. 0.001)
!     &	    u(I,J,K,ICOUNT)=-99999.9
!           ENDDO	
!           ENDDO	
!           ENDDO
!           ENDDO
!           STATUS = NF_INQ_VARID(NCID,'water_v',IDVAR)
!           STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
!           STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
!           STATUS = NF_GET_VAR_REAL(NCID,IDVAR,TMP4D)
!           ICOUNT=N0
!           DO N=1,NT
!	     ICOUNT=ICOUNT+1
!           DO I=1,ISUB
!           DO J=1,JSUB
!           DO K=1,KB
!            v(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)*scale+offset
!            if(abs(v(I,J,K,ICOUNT)-missvalue) .LE. 0.001)
!     &	    v(I,J,K,ICOUNT)=-99999.9
!           ENDDO	
!           ENDDO	
!           ENDDO
!           ENDDO
	   NREC=ICOUNT
           STATUS=NF_CLOSE(NCID)
!! Open NCOM tide file if it exists
           FIN=TRIM(FILE_NCOM_TIDE(IFILE))
           INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
           IF(FEXIST)THEN
             STATUS = NF_OPEN(trim(FIN),NF_NOWRITE, NCID)
             IF(STATUS .NE. NF_NOERR)then
	       print *,'error message= ',status
	       stop 'open NCOM tide netCDF file of failed'
             ENDIF  
             STATUS = NF_INQ_VARID(NCID,'SEA_LEVEL',IDVAR)
             STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'FillValue',fillvalue)
             STATUS = NF_GET_VAR_REAL(NCID,IDVAR,tmp3d)
	     print *,'sea_level fillvalue=',fillvalue

             ICOUNT=N0
             DO N=1,NT
	      ICOUNT=ICOUNT+1
              DO I=1,ISUB
              DO J=1,JSUB
	        tide_NCOM(I,J,ICOUNT)=TMP3D(IMIN+I-1,JMIN+J-1,N)
                diff=WL(I,J,ICOUNT)-tide_NCOM(I,J,ICOUNT)
               if(abs(TMP3D(IMIN+I-1,JMIN+J-1,N)) .GT. 29.0 .or.
     1             ABS(WL(I,J,ICOUNT)) .GE. 29.0  )then
       	           diff=-99999.9
	       else  
 !subtract tide from NCOM total water level
                  diff=WL(I,J,ICOUNT)-tide_NCOM(I,J,ICOUNT)
               endif
	       write(26,100)N,I,J,lonsub(i,j),latsub(i,j), 
     1 	       WL(I,J,ICOUNT),tide_NCOM(I,J,ICOUNT),diff
               WL(I,J,ICOUNT)=diff
              ENDDO	
              ENDDO
	      	
             ENDDO
100   FORMAT(3I8,10F12.4)
             STATUS=NF_CLOSE(NCID)
	   ELSE
	     WRITE(*,*)'NCOM tide file does not match NCOM file'
	     WRITE(*,*)'Switch DBASE_WL=ETSS'
             WRITE(ICORMS,'(a)')'Switch DBASE_WL from NCOM to ETSS' 
	     DBASE_WL='ETSS'
          !   ICOUNT=N0
          !   DO N=1,NT
	  !     ICOUNT=ICOUNT+1
          !     DO I=1,ISUB
          !     DO J=1,JSUB
       	  !       WL(I,J,ICOUNT)=-99999.9
          !     ENDDO	
          !     ENDDO	
          !   ENDDO
	       	
	   ENDIF
	   
	 ENDDO
	 NT=NREC 
	 write(*,*)'number of NCOM data NREC= ',NREC

!	 DO N=1,NT
!	  write(*,*)'N=',N,TS_TIME(N)
!	 ENDDO  
! delete all NCOM NetCDF files after reading them	
!	 DO IFILE=1,IFILE_NCOM
!           INQUIRE(FILE=TRIM(FILE_NCOM(IFILE)),EXIST=FEXIST)
!           if(FEXIST)THEN
!             CMD='rm -f '//TRIM(FILE_NCOM(IFILE))
!             call system(trim(CMD))
!	   endif  
!	 ENDDO  
      ENDIF
C-----------------------------------------------------------------------
C     Open and read all RTOFS netCDF files available during start time to end time       
C-----------------------------------------------------------------------
      IF ( TRIM(DBASE_TS) .EQ. 'RTOFS')THEN
         NCOM_FILE=TRIM(DBASE_TS)//"_FILE"
!         NCOM_FILE='RTOFS_FILE'
         IFILE_NCOM=0
         INQUIRE(FILE=trim(NCOM_FILE),EXIST=FEXIST)
         IF(FEXIST)THEN
           CLOSE(60)
           OPEN(60,file=trim(NCOM_FILE))
           DO I=1,999
              READ(60,'(a100)',END=335)BUFFER
              FILE_NCOM(I)=trim(adjustL(BUFFER))
           ENDDO
335        CLOSE(60)
	   IFILE_NCOM=I-1
         ENDIF
         WRITE(*,*)'Total number of RTOFS FILES= ',IFILE_NCOM
	 IF(IFILE_NCOM .LT. 1)THEN
	    WRITE(*,*)'No RTOFS file is available in this period'
            WRITE(*,*)'use WOA05 climatological dataset to replace'
              WRITE(*,*)'use HYCOM Backup to replace'
              DBASE_TS='HYCOM'
              WRITE(ICORMS,'(a)')'RTOFS FILE IS NOT FOUND' 
              WRITE(ICORMS,'(a)')'USE HYCOM BACKUP' 
            GOTO 30
	 ENDIF  
	 DO I=1,IFILE_NCOM
	    WRITE(*,*)'RTOFS I=',I,TRIM(FILE_NCOM(I))
	 ENDDO
C-----------------------------------------------------------------------
C     Open and read all RTOFS netCDF files available during start time to end time       
C-----------------------------------------------------------------------
         FIN=TRIM(FILE_NCOM(1))
         DO I=1,4
          DIMS(I)=1
         ENDDO	
         VNAME='salinity'
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IM=DIMS(1)
	 JM=DIMS(2)
	 KB=DIMS(3)
	 NT=DIMS(4)
	 WRITE(*,*)'RTOFS IM=',IM,'JM= ',JM,'KB= ',KB,'NT= ',NT
         IF (ALLOCATED(lon)) DEALLOCATE(lon)
         IF (ALLOCATED(lat)) DEALLOCATE(lat)
         IF (ALLOCATED(depth)) DEALLOCATE(depth)
         IF (ALLOCATED(zeta_time)) DEALLOCATE(zeta_time)
         IF (ALLOCATED(ts_time)) DEALLOCATE(ts_time)
         ALLOCATE(lon(IM,JM) )
         ALLOCATE(lat(IM,JM) )
         ALLOCATE(depth(KB) )
         ALLOCATE(zeta_time(NMAX) )
         ALLOCATE(ts_time(NMAX) )
         DO I=1,4
          DIMS(I)=1
         ENDDO	
	 VNAME='Longitude'
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
         ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
         WRITE(*,*)'DIM Number of Longitude ',NDIM
         IF(NDIM .EQ. 2)THEN
           DO I=1,IM
           DO J=1,JM
             lon(i,j)=TMP4D(i,j,1,1)
 	     if(lon(i,j) .gt. 180)lon(i,j)=lon(i,j)-360.
           ENDDO
           ENDDO
         ELSEIF(NDIM .EQ. 1)THEN
           DO I=1,IM
           DO J=1,JM
             lon(i,j)=TMP4D(i,1,1,1)
 	     if(lon(i,j) .gt. 180)lon(i,j)=lon(i,j)-360.
           ENDDO
           ENDDO
	 ENDIF  
         DO I=1,4
          DIMS(I)=1
         ENDDO	
	 VNAME='Latitude'
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
         ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
         WRITE(*,*)'DIM Number of Latitude ',NDIM
         IF(NDIM .EQ. 2)THEN
           DO I=1,IM
           DO J=1,JM
             lat(i,j)=TMP4D(i,j,1,1)
           ENDDO
           ENDDO
         ELSEIF(NDIM .EQ. 1)THEN
           DO I=1,IM
           DO J=1,JM
             lat(i,j)=TMP4D(J,1,1,1)
           ENDDO
           ENDDO
         ENDIF
         DO I=1,4
          DIMS(I)=1
         ENDDO	
	 VNAME='Depth'
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
         ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
         DO K=1,KB
            depth(K)=TMP4D(K,1,1,1)
         ENDDO
C-----------------------------------------------------------------------
C        select subdomain I and J index        
C-----------------------------------------------------------------------
         IMIN=99999
         IMAX=-9999
         JMIN=99999
         JMAX=-9999
         DO I=1,IM
         DO J=1,JM
           IF(lon(i,j) .GE. minlon .and. lon(i,j) .LE. maxlon
     &    .and. lat(i,j) .GE. minlat .and. lat(i,j) .LE. maxlat)then
             IF(I .LT. IMIN)IMIN=I 	  
             IF(I .GT. IMAX)IMAX=I 	  
             IF(J .LT. JMIN)JMIN=J 	  
             IF(J .GT. JMAX)JMAX=J
	   ENDIF
         ENDDO
         ENDDO	  	  
         ISUB=IMAX-IMIN+1	 
         JSUB=JMAX-JMIN+1	 
         write(*,*)'subdomain I J=',ISUB,JSUB,IMIN,IMAX,JMIN,JMAX
C-----------------------------------------------------------------------
C allocate sizes of arrays for NCOM products 
C-----------------------------------------------------------------------
         NNSUB=ISUB*JSUB
         IF (ALLOCATED(lonsub)) DEALLOCATE(lonsub)
         IF (ALLOCATED(latsub)) DEALLOCATE(latsub)
         IF (ALLOCATED(temp)) DEALLOCATE(temp)
         IF (ALLOCATED(salt)) DEALLOCATE(salt)
         IF (ALLOCATED(u)) DEALLOCATE(u)
         IF (ALLOCATED(v)) DEALLOCATE(v)
         IF (ALLOCATED(WL)) DEALLOCATE(WL)
         allocate(lonsub(ISUB,JSUB))
         allocate(latsub(ISUB,JSUB))
         allocate(temp(ISUB,JSUB,KB,NT*100))
         allocate(salt(ISUB,JSUB,KB,NT*100))
         allocate(u(ISUB,JSUB,KB,NT*100))
         allocate(v(ISUB,JSUB,KB,NT*100))
         allocate(WL(ISUB,JSUB,NT*100))
         DO N=1,NT*100
         DO J=1,JSUB
         DO I=1,ISUB
	    WL(I,J,N)=0.0
         ENDDO
         ENDDO
         ENDDO   
         DO J=1,JSUB
         DO I=1,ISUB
            lonsub(i,j)=lon(IMIN+I-1,JMIN+J-1)
            latsub(i,j)=lat(IMIN+I-1,JMIN+J-1)
	    write(79,'(2F12.4,2I6)')lonsub(i,j),latsub(i,j),I,J
         ENDDO
         ENDDO  
	 close(79) 
         ICOUNT=0
	 NREC=0
         TIMELAST=-9999.0	 
	 DO IFILE=1,IFILE_NCOM
	   write(*,*)'reading RTOFS NetCDF file...',IFILE
           FIN=TRIM(FILE_NCOM(IFILE))
	   VNAME='MT'
	   ANAME='units'
           DO I=1,4
             DIMS(I)=1
           ENDDO	
           CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
           IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
           ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
	   NT=DIMS(1)
           CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,6)
	   ANAME=trim(adjustL(ANAME))
	   LEN=LEN_TRIM(ANAME)
           LL=INDEX(ANAME,'minute')         
	   IF(LL .GT. 0)scale_time=1.0/1440.0
           LL=INDEX(ANAME,'hour')         
	   IF(LL .GT. 0)scale_time=1.0/24.0
           LL=INDEX(ANAME,'day')         
	   IF(LL .GT. 0)scale_time=1.0
           LL=INDEX(ANAME,'since')         
	   IF(LL .GT. 0)then
             read(ANAME(LL+6:LEN),80)IYR,IMM,IDD,IHH
             write(*,*)'basetime=',IYR,IMM,IDD,IHH
	   else
	     write(*,*)'there is error while reading base date'
	     stop
	   endif    
           yearb=IYR
           monthb=IMM
           dayb=IDD
           hourb=IHH
           jday=JULIAN(yearb,monthb,dayb,hourb)
           CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,7)
           TIME_NCOM=TMP4D(1,1,1,1)*scale_time+jday-jbase_date
           IF(TIMELAST .LT. TIME_NCOM )THEN
               ICOUNT=ICOUNT+1
	       TS_TIME(ICOUNT)=TIME_NCOM
	       TIMELAST=TIME_NCOM 
           ENDIF
           STATUS = NF_OPEN(trim(FIN),NF_NOWRITE, NCID)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     stop 'open RTOFS netCDF file of failed'
           ENDIF  
           IF (ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
           ALLOCATE(tmp1d(IM) )
           IF (ALLOCATED(tmp3d)) DEALLOCATE(tmp3d)
           ALLOCATE(tmp3d(IM,JM,NT) )
           IF (ALLOCATED(itmp3d)) DEALLOCATE(itmp3d)
           ALLOCATE(itmp3d(IM,JM,NT) )
           IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
           ALLOCATE(tmp4d(IM,JM,KB,NT) )
           STATUS = NF_INQ_VARID(NCID,'temperature',IDVAR)
           STATUS = NF_GET_VAR_REAL(NCID,IDVAR,tmp4d)
           DO N=1,NT
           DO I=1,ISUB
           DO J=1,JSUB
           DO K=1,KB
            TEMP(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)
            if(abs(TEMP(I,J,K,ICOUNT)) .GE. 99.0)
     &	    TEMP(I,J,K,ICOUNT)=-99999.9
           ENDDO	
           ENDDO	
           ENDDO	
           ENDDO	
           STATUS = NF_INQ_VARID(NCID,'salinity',IDVAR)
           STATUS = NF_GET_VAR_REAL(NCID,IDVAR,tmp4d)
           DO N=1,NT
           DO I=1,ISUB
           DO J=1,JSUB
           DO K=1,KB
            SALT(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)
            if(abs(SALT(I,J,K,ICOUNT)) .GE. 99.0)
     &	    SALT(I,J,K,ICOUNT)=-99999.9
           ENDDO	
           ENDDO	
           ENDDO	
           ENDDO	
           STATUS = NF_INQ_VARID(NCID,'u',IDVAR)
           STATUS = NF_GET_VAR_REAL(NCID,IDVAR,tmp4d)
           DO N=1,NT
           DO I=1,ISUB
           DO J=1,JSUB
           DO K=1,KB
            U(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)
            if(abs(U(I,J,K,ICOUNT)) .GE. 99.0)
     &	    U(I,J,K,ICOUNT)=-99999.9
           ENDDO	
           ENDDO	
           ENDDO	
           ENDDO	
           STATUS = NF_INQ_VARID(NCID,'v',IDVAR)
           STATUS = NF_GET_VAR_REAL(NCID,IDVAR,tmp4d)
!           ICOUNT=N0
           DO N=1,NT
!	     ICOUNT=ICOUNT+1
           DO I=1,ISUB
           DO J=1,JSUB
           DO K=1,KB
            V(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)
            if(abs(V(I,J,K,ICOUNT)) .GE. 99.0)
     &	    V(I,J,K,ICOUNT)=-99999.9
           ENDDO	
           ENDDO	
           ENDDO	
           ENDDO	
	   NREC=ICOUNT
           STATUS=NF_CLOSE(NCID)
         ENDDO      
	 NT=NREC 
	 write(*,*)'number of RTOFS data NREC= ',NREC
      ENDIF

C-----------------------------------------------------------------------
C     Open and read all HYCOM netCDF files available during start time to end time       
C-----------------------------------------------------------------------
      IF ( TRIM(DBASE_TS) .EQ. 'HYCOM')THEN
         NCOM_FILE=TRIM(DBASE_TS)//"_FILE"
!         NCOM_FILE='HYCOM_FILE'
         IFILE_NCOM=0
         INQUIRE(FILE=trim(NCOM_FILE),EXIST=FEXIST)
         IF(FEXIST)THEN
           CLOSE(60)
           OPEN(60,file=trim(NCOM_FILE))
           DO I=1,999
              READ(60,'(a100)',END=325)BUFFER
              FILE_NCOM(I)=trim(adjustL(BUFFER))
           ENDDO
325        CLOSE(60)
	   IFILE_NCOM=I-1
         ENDIF
         WRITE(*,*)'Total number of HYCOM FILES= ',IFILE_NCOM
	 IF(IFILE_NCOM .LT. 1)THEN
	      WRITE(*,*)'No HYCOM file is available in this period'
              WRITE(*,*)'use WOA05 climatological dataset to replace'
              DBASE_TS='WOA05'
	      IF ( TRIM(DBASE_WL) .EQ. 'HYCOM')DBASE_WL='ETSS'
              WRITE(ICORMS,'(a)')'HYCOM FILE IS NOT FOUND' 
              WRITE(ICORMS,'(a)')'USE CLIMATOLOGIC BACKUP WOA05' 
              GOTO 30
	 ENDIF  
	 DO I=1,IFILE_NCOM
	    WRITE(*,*)'HYCOM I=',I,TRIM(FILE_NCOM(I))
	 ENDDO
C-----------------------------------------------------------------------
C     Open and read all HYCOM netCDF files available during start time to end time       
C-----------------------------------------------------------------------
         FIN=TRIM(FILE_NCOM(1))
         DO I=1,4
          DIMS(I)=1
         ENDDO	
         VNAME='salinity'
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IM=DIMS(1)
	 JM=DIMS(2)
	 KB=DIMS(3)
	 NT=DIMS(4)
	 WRITE(*,*)'HYCOM IM=',IM,'JM= ',JM,'KB= ',KB,'NT= ',NT
         IF (ALLOCATED(lon)) DEALLOCATE(lon)
         IF (ALLOCATED(lat)) DEALLOCATE(lat)
         IF (ALLOCATED(depth)) DEALLOCATE(depth)
         IF (ALLOCATED(zeta_time)) DEALLOCATE(zeta_time)
         IF (ALLOCATED(ts_time)) DEALLOCATE(ts_time)
         ALLOCATE(lon(IM,JM) )
         ALLOCATE(lat(IM,JM) )
         ALLOCATE(depth(KB) )
         ALLOCATE(zeta_time(NMAX) )
         ALLOCATE(ts_time(NMAX) )
         DO I=1,4
          DIMS(I)=1
         ENDDO	
	 VNAME='lon'
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
         ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
         WRITE(*,*)'DIM Number of Longitude ',NDIM
         IF(NDIM .EQ. 2)THEN
           DO I=1,IM
           DO J=1,JM
             lon(i,j)=TMP4D(i,j,1,1)
 	     if(lon(i,j) .gt. 180)lon(i,j)=lon(i,j)-360.
           ENDDO
           ENDDO
         ELSEIF(NDIM .EQ. 1)THEN
           DO I=1,IM
           DO J=1,JM
             lon(i,j)=TMP4D(i,1,1,1)
 	     if(lon(i,j) .gt. 180)lon(i,j)=lon(i,j)-360.
           ENDDO
           ENDDO
	 ENDIF  
         DO I=1,4
          DIMS(I)=1
         ENDDO	
	 VNAME='lat'
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
         ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
         WRITE(*,*)'DIM Number of Latitude ',NDIM
         IF(NDIM .EQ. 2)THEN
           DO I=1,IM
           DO J=1,JM
             lat(i,j)=TMP4D(i,j,1,1)
           ENDDO
           ENDDO
         ELSEIF(NDIM .EQ. 1)THEN
           DO I=1,IM
           DO J=1,JM
             lat(i,j)=TMP4D(J,1,1,1)
           ENDDO
           ENDDO
         ENDIF
         DO I=1,4
          DIMS(I)=1
         ENDDO	
	 VNAME='depth'
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
         ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
         DO K=1,KB
            depth(K)=TMP4D(K,1,1,1)
         ENDDO
!	 CLOSE(79)
!	 OPEN(79,file='lonlat_HYCOM.dat')
!         DO I=1,IM
!         DO J=1,JM
!	    write(79,'(2F12.4,2I6)')lon(i,j),lat(i,j),I,J
!         ENDDO
!         ENDDO  
!	 close(79) 
C-----------------------------------------------------------------------
C        select subdomain I and J index        
C-----------------------------------------------------------------------
         IMIN=99999
         IMAX=-9999
         JMIN=99999
         JMAX=-9999
         DO I=1,IM
         DO J=1,JM
           IF(lon(i,j) .GE. minlon .and. lon(i,j) .LE. maxlon
     &    .and. lat(i,j) .GE. minlat .and. lat(i,j) .LE. maxlat)then
             IF(I .LT. IMIN)IMIN=I 	  
             IF(I .GT. IMAX)IMAX=I 	  
             IF(J .LT. JMIN)JMIN=J 	  
             IF(J .GT. JMAX)JMAX=J
	   ENDIF
         ENDDO
         ENDDO	  	  
         ISUB=IMAX-IMIN+1	 
         JSUB=JMAX-JMIN+1	 
         write(*,*)'subdomain I J=',ISUB,JSUB,IMIN,IMAX,JMIN,JMAX
C-----------------------------------------------------------------------
C allocate sizes of arrays for HYCOM products 
C-----------------------------------------------------------------------
         NNSUB=ISUB*JSUB
         IF (ALLOCATED(lonsub)) DEALLOCATE(lonsub)
         IF (ALLOCATED(latsub)) DEALLOCATE(latsub)
         IF (ALLOCATED(temp)) DEALLOCATE(temp)
         IF (ALLOCATED(salt)) DEALLOCATE(salt)
         IF (ALLOCATED(u)) DEALLOCATE(u)
         IF (ALLOCATED(v)) DEALLOCATE(v)
         IF (ALLOCATED(WL)) DEALLOCATE(WL)
         allocate(lonsub(ISUB,JSUB))
         allocate(latsub(ISUB,JSUB))
         allocate(temp(ISUB,JSUB,KB,NT*100))
         allocate(salt(ISUB,JSUB,KB,NT*100))
         allocate(u(ISUB,JSUB,KB,NT*100))
         allocate(v(ISUB,JSUB,KB,NT*100))
         allocate(WL(ISUB,JSUB,NT*100))
         allocate(tide_NCOM(ISUB,JSUB,NT*100))
         DO N=1,NT*100
         DO J=1,JSUB
         DO I=1,ISUB
	    WL(I,J,N)=0.0
	    tide_NCOM(I,J,N)=0.0
         ENDDO
         ENDDO
         ENDDO   
	 CLOSE(79)
	 OPEN(79,file='lonlat_HYCOM.dat')
         DO J=1,JSUB
         DO I=1,ISUB
            lonsub(i,j)=lon(IMIN+I-1,JMIN+J-1)
            latsub(i,j)=lat(IMIN+I-1,JMIN+J-1)
	    write(79,'(2F12.4,2I6)')lonsub(i,j),latsub(i,j),I,J
         ENDDO
         ENDDO  
	 close(79) 
         ICOUNT=0
	 NREC=0
         TIMELAST=-9999.0	 
	 DO IFILE=1,IFILE_NCOM
	   write(*,*)'reading HYCOM NetCDF file...',IFILE
           FIN=TRIM(FILE_NCOM(IFILE))
	   VNAME='time'
	   ANAME='units'
           DO I=1,4
             DIMS(I)=1
           ENDDO	
           CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
           IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
           ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
	   NT=DIMS(1)
           CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,6)
	   ANAME=trim(adjustL(ANAME))
	   LEN=LEN_TRIM(ANAME)
           LL=INDEX(ANAME,'minute')         
	   IF(LL .GT. 0)scale_time=1.0/1440.0
           LL=INDEX(ANAME,'hour')         
	   IF(LL .GT. 0)scale_time=1.0/24.0
           LL=INDEX(ANAME,'day')         
	   IF(LL .GT. 0)scale_time=1.0
           LL=INDEX(ANAME,'since')         
	   IF(LL .GT. 0)then
             read(ANAME(LL+6:LEN),80)IYR,IMM,IDD,IHH
             write(*,*)'basetime=',IYR,IMM,IDD,IHH
	   else
	     write(*,*)'there is error while reading base date'
	     stop
	   endif    
           yearb=IYR
           monthb=IMM
           dayb=IDD
           hourb=IHH
           jday=JULIAN(yearb,monthb,dayb,hourb)
           CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,7)
           TIME_NCOM=TMP4D(1,1,1,1)*scale_time+jday-jbase_date
           IF(TIMELAST .LT. TIME_NCOM )THEN
               ICOUNT=ICOUNT+1
	       TS_TIME(ICOUNT)=TIME_NCOM
	       TIMELAST=TIME_NCOM 
           ENDIF

           STATUS = NF_OPEN(trim(FIN),NF_NOWRITE, NCID)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     stop 'open RTOFS netCDF file of failed'
           ENDIF  
           IF (ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
           ALLOCATE(tmp1d(IM) )
           IF (ALLOCATED(tmp3d)) DEALLOCATE(tmp3d)
           ALLOCATE(tmp3d(IM,JM,NT) )
           IF (ALLOCATED(itmp3d)) DEALLOCATE(itmp3d)
           ALLOCATE(itmp3d(IM,JM,NT) )
           IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
           ALLOCATE(tmp4d(IM,JM,KB,NT) )

           STATUS = NF_INQ_VARID(NCID,'surf_el',IDVAR)
           STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
           STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
           STATUS = NF_GET_ATT_INT(NCID,IDVAR,'missing_value',IATT)
           STATUS = NF_GET_VAR_REAL(NCID,IDVAR,tmp3d)
           print *,'first data=',tmp3d(1,1,1),tmp3d(2,1,1)

	   missvalue=IATT*scale+offset
           print *,'factor=',scale,offset,missvalue
           DO N=1,NT
           DO I=1,ISUB
           DO J=1,JSUB
             WL(I,J,ICOUNT)=TMP3D(IMIN+I-1,JMIN+J-1,N)*scale+offset
             if(abs(WL(I,J,ICOUNT)-missvalue) .LE. 0.001)
     &	     WL(I,J,ICOUNT)=-99999.9
           ENDDO	
           ENDDO	
           ENDDO	

           STATUS = NF_INQ_VARID(NCID,'water_temp',IDVAR)
           STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
           STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
           STATUS = NF_GET_ATT_INT(NCID,IDVAR,'missing_value',IATT)
	   missvalue=IATT*scale+offset
	   print *,'scale of temp=',scale,offset,missvalue
           STATUS = NF_GET_VAR_REAL(NCID,IDVAR,tmp4d)
           DO N=1,NT
           DO I=1,ISUB
           DO J=1,JSUB
           DO K=1,KB
            TEMP(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)*scale+offset
            if(abs(TEMP(I,J,K,ICOUNT)-missvalue) .LE. 0.001)
     &	    TEMP(I,J,K,ICOUNT)=-99999.9
            if(TEMP(I,J,K,ICOUNT) .LT. 0.0)TEMP(I,J,K,ICOUNT)=-99999.9
           ENDDO	
           ENDDO	
           ENDDO	
           ENDDO
           STATUS = NF_INQ_VARID(NCID,'salinity',IDVAR)
           STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
           STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
           STATUS = NF_GET_VAR_REAL(NCID,IDVAR,TMP4D)
           DO N=1,NT
           DO I=1,ISUB
           DO J=1,JSUB
           DO K=1,KB
            SALT(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)*scale+offset
            if(abs(SALT(I,J,K,ICOUNT)-missvalue) .LE. 0.001)
     &	    SALT(I,J,K,ICOUNT)=-99999.9
            if(SALT(I,J,K,ICOUNT) .LT. 0.0)SALT(I,J,K,ICOUNT)=-99999.9
           ENDDO	
           ENDDO	
           ENDDO	
           ENDDO	
           STATUS = NF_INQ_VARID(NCID,'water_u',IDVAR)
           STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
           STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
           STATUS = NF_GET_VAR_REAL(NCID,IDVAR,TMP4D)
           DO N=1,NT
           DO I=1,ISUB
           DO J=1,JSUB
           DO K=1,KB
            U(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)*scale+offset
            if(abs(U(I,J,K,ICOUNT)-missvalue) .LE. 0.001)
     &	    U(I,J,K,ICOUNT)=-99999.9
            if(abs(U(I,J,K,ICOUNT)) .GE. 99.0)
     &	    U(I,J,K,ICOUNT)=-99999.9
           ENDDO	
           ENDDO	
           ENDDO	
           ENDDO	
           STATUS = NF_INQ_VARID(NCID,'water_v',IDVAR)
           STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'scale_factor',scale)
           STATUS = NF_GET_ATT_REAL(NCID,IDVAR,'add_offset',offset)
           STATUS = NF_GET_VAR_REAL(NCID,IDVAR,TMP4D)
           DO N=1,NT
           DO I=1,ISUB
           DO J=1,JSUB
           DO K=1,KB
            v(I,J,K,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,K,N)*scale+offset
            if(abs(V(I,J,K,ICOUNT)-missvalue) .LE. 0.001)
     &	    V(I,J,K,ICOUNT)=-99999.9
            if(abs(V(I,J,K,ICOUNT)) .GE. 99.0)
     &	    V(I,J,K,ICOUNT)=-99999.9
           ENDDO	
           ENDDO	
           ENDDO	
           ENDDO	
	   NREC=ICOUNT
           STATUS=NF_CLOSE(NCID)
         ENDDO      
	 NT=NREC 
	 write(*,*)'number of HYCOM data NREC= ',NREC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         CLOSE(79)
	 OPEN(79,file='salt_HYCOM.dat')
         DO N=1,1   !NT
         DO I=1,ISUB
         DO J=1,JSUB
	  IF (SALT(I,J,1,N) .GE. 0.0)THEN
            WRITE(79,'(F10.4,2I5,100F9.3)')TS_TIME(N),I,J,
     &	   lonsub(i,j),latsub(i,j),(SALT(I,J,K,N),K=1,KB)
          ENDIF
         ENDDO	
         ENDDO	
         ENDDO	
	 
         CLOSE(79)
	 OPEN(79,file='temp_HYCOM.dat')
         DO N=1,1 !NT
         DO I=1,ISUB
         DO J=1,JSUB
	  IF (TEMP(I,J,1,N) .GE. 0.0)THEN
            WRITE(79,'(F10.4,2I5,100F9.3)')TS_TIME(N),I,J,
     &	   lonsub(i,j),latsub(i,j),(TEMP(I,J,K,N),K=1,KB)
          ENDIF
         ENDDO	
         ENDDO	
         ENDDO
         CLOSE(79)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!	   	
      ENDIF

C-----------------------------------------------------------------------
C  End of reading date from WOA05 or NCOM, or RTOFS, or HYCOM products 
C-----------------------------------------------------------------------
      
      
      NSTR=1
      DO N=1,NT
         write(*,*)'time= ',N,ts_time(N)
         IF(TS_TIME(N) .gt. day_start)then
	    NSTR=N-1
	    goto 130
	 ENDIF
      ENDDO	
130   continue
      IF(NSTR .LE. 0 .OR. NSTR .GT. NT)THEN  !AJ 08/13/2010 extrapolation
!	 write(*,*)' Time period is not covered by dataset'
!         WRITE(ICORMS,'(a)')TRIM(DBASE_TS)//' DATASET IS NOT CORRECT' 
!         WRITE(ICORMS,'(a)')'USE CLIMATOLOGIC BACKUP WOA05' 
!         DBASE_TS='WOA05'
!         GOTO 30
         DO NN=NT,1,-1
	    TS_TIME(NN+1)=TS_TIME(NN)
            DO I=1,ISUB
            DO J=1,JSUB
	      WL(I,J,NN+1)=WL(I,J,NN) 
            DO K=1,KB
     	      TEMP(I,J,K,NN+1)=TEMP(I,J,K,NN)
     	      SALT(I,J,K,NN+1)=SALT(I,J,K,NN)
!     	      u(I,J,K,NN+1)=u(I,J,K,NN)
!     	      v(I,J,K,NN+1)=v(I,J,K,NN)
            ENDDO	
            ENDDO	
            ENDDO
         ENDDO
	 TS_TIME(1)=day_start
	 NT=NT+1
	 NSTR=1
      ENDIF
      IF (TS_TIME(NT) .LT. day_end)THEN
          diff=day_end-TS_TIME(NT)
          IF (diff .LT. 5.0)THEN 
C AJ ZHANG 03/18/2010, allow persistence for the maximum 5 days. 
	    TS_TIME(NT+1)=day_end
            DO I=1,ISUB
            DO J=1,JSUB
	      WL(I,J,NT+1)=WL(I,J,NT) 
            DO K=1,KB
     	      TEMP(I,J,K,NT+1)=TEMP(I,J,K,NT)
     	      SALT(I,J,K,NT+1)=SALT(I,J,K,NT)
!     	      u(I,J,K,NT+1)=u(I,J,K,NT)
!     	      v(I,J,K,NT+1)=v(I,J,K,NT)
            ENDDO	
            ENDDO	
            ENDDO
	    NT=NT+1
	  ELSE  
	    write(*,*)TRIM(DBASE_TS)//' data is shorter than day_end'
	    write(*,*)'The forcast time period is not covered by RTOFS'	  
            WRITE(*,*)'USE CLIMATOLOGIC BACKUP WOA05' 
            WRITE(ICORMS,'(a)')TRIM(DBASE_TS)//' is too short'
            WRITE(ICORMS,'(a)')'USE CLIMATOLOGIC BACKUP WOA05' 
            DBASE_TS='WOA05'
            GOTO 30
	  ENDIF  
      ENDIF    
      DO N=1,NT
         if(TS_TIME(N) .GE. day_end)then
	    NEND=n
	    goto 140
	 ENDIF
      ENDDO	
140   continue
      IF(NEND .LT. NT)NEND=NEND+1
      
      IF(N .GT. NT)THEN
	write(*,*)' time period is not covered by dataset'	  
        WRITE(ICORMS,'(a)')' time period is not covered by dataset'
	WRITE(ICORMS,'(a)')'Generation of OBC failed'
	WRITE(ICORMS,'(a)')'stop in nos_ofs_create_OBC.f'
	stop 'stop in nos_ofs_create_OBC.f'
      ENDIF  	  
      write(*,*)'start and end time of = ',
     &   TS_time(NSTR),TS_time(NEND),NSTR,NEND,day_start,day_end 		
      NTMAX=NEND-NSTR+1
      IF (ALLOCATED(TIMEOBC)) DEALLOCATE(TIMEOBC)
      ALLOCATE(TIMEOBC(NTMAX) )
      DO N=1,NTMAX
           TIMEOBC(N)=TS_TIME(NSTR+N-1)
      ENDDO
      IF (TRIM(DBASE_WL) .EQ. 'NCOM' .OR. 
     &    TRIM(DBASE_WL) .EQ. 'HYCOM')THEN
         NTMAX_WL=NTMAX
         IF (ALLOCATED(zeta_time)) DEALLOCATE(zeta_time)
         ALLOCATE(zeta_time(NTMAX_WL) )
         DO N=1,NTMAX_WL
           zeta_time(N)=TIMEOBC(N)
         ENDDO
      ENDIF
      KMAX=KB	
      DO K=1,KB
        IF(depth(k) .GT. hmax)THEN
	   KMAX=K
	   GOTO 150
	ENDIF
      ENDDO	    
150   CONTINUE
      WRITE(*,*)'MAX Vertical Levels needed is ',KMAX,depth(KMAX),hmax
C-----------------------------------------------------------------------
C  End of computing time coverage and maximum vertical layer 
C-----------------------------------------------------------------------
!            CLOSE(1)
!	    OPEN(1,file='NCOM_Watercells.dat')
!               DO I=1,ISUB
!               DO J=1,JSUB
!                  IF( WL(I,J,1) .GT. -9999.0 )THEN
!	             WRITE(1,'(5F10.4,2I6)')lonsub(i,j),latsub(i,j)
!     1	               ,WL(I,J,1),TEMP(I,J,1,1),SALT(I,J,1,1),I,J   	     
!                  ENDIF	
!               ENDDO	
!               ENDDO
!	    CLOSE(1)   

C-----------------------------------------------------------------------
C    begin to process lateral open boundary conditions 
C-----------------------------------------------------------------------
      IF (ALLOCATED(WLOBC)) DEALLOCATE(WLOBC)
      IF (ALLOCATED(tempOBC)) DEALLOCATE(tempOBC)
      IF (ALLOCATED(saltOBC)) DEALLOCATE(saltOBC)
      IF (ALLOCATED(uOBC)) DEALLOCATE(uOBC)
      IF (ALLOCATED(vOBC)) DEALLOCATE(vOBC)
      ALLOCATE(WLOBC(NOBC,NTMAX_WL) )
      ALLOCATE(tempOBC(NOBC,KB,NTMAX) )
      ALLOCATE(saltOBC(NOBC,KB,NTMAX) )
!      ALLOCATE(uOBC(NOBC,KB,NTMAX) )
!      ALLOCATE(vOBC(NOBC,KB,NTMAX) )
      IF (ALLOCATED(tempOBC_M)) DEALLOCATE(tempOBC_M)
      IF (ALLOCATED(saltOBC_M)) DEALLOCATE(saltOBC_M)
!      IF (ALLOCATED(uOBC_M)) DEALLOCATE(uOBC_M)
!      IF (ALLOCATED(vOBC_M)) DEALLOCATE(vOBC_M)
!      IF (ALLOCATED(ueOBC_M)) DEALLOCATE(ueOBC_M)
!      IF (ALLOCATED(veOBC_M)) DEALLOCATE(veOBC_M)
!      IF (ALLOCATED(ubarOBC)) DEALLOCATE(ubarOBC)
!      IF (ALLOCATED(vbarOBC)) DEALLOCATE(vbarOBC)
      ALLOCATE(tempOBC_M(NOBC,KBm,NTMAX) )
      ALLOCATE(saltOBC_M(NOBC,KBm,NTMAX) )
!      ALLOCATE(uOBC_M(NOBC,KBm,NTMAX) )
!      ALLOCATE(vOBC_M(NOBC,KBm,NTMAX) )
!      ALLOCATE(ueOBC_M(NEOBC,KBm,NTMAX) )
!      ALLOCATE(veOBC_M(NEOBC,KBm,NTMAX) )
!      ALLOCATE(ubarOBC(NEOBC,NTMAX) )
!      ALLOCATE(vbarOBC(NEOBC,NTMAX) )
      IF( IGRD .EQ. 1 .OR. IGRD .EQ. 4)THEN
         IF (ALLOCATED(XINP)) DEALLOCATE(XINP)
         IF (ALLOCATED(YINP)) DEALLOCATE(YINP)
         IF (ALLOCATED(ZINP)) DEALLOCATE(ZINP)
         IF (ALLOCATED(XOUT)) DEALLOCATE(XOUT)
         IF (ALLOCATED(YOUT)) DEALLOCATE(YOUT)
         IF (ALLOCATED(ZOUT)) DEALLOCATE(ZOUT)
         IF (ALLOCATED(weightnodes)) DEALLOCATE(weightnodes)
         IF (ALLOCATED(weights)) DEALLOCATE(weights)
         allocate(XINP(NNSUB) )
         allocate(YINP(NNSUB) )
         allocate(ZINP(NNSUB) )
         allocate(XOUT(NOBC) )
         allocate(YOUT(NOBC) )
         allocate(ZOUT(NOBC) )
         allocate(weightnodes(NOBC,3))
         allocate(weights(NOBC,3))
      ENDIF
c------------------------------------------------------------------
C  processing Water Level open boundary
c------------------------------------------------------------------
      IF (TRIM(DBASE_WL) .EQ. 'NCOM' .OR. 
     &    TRIM(DBASE_WL) .EQ. 'HYCOM')THEN
        IF (ALLOCATED(WLOBC)) DEALLOCATE(WLOBC)
        IF (ALLOCATED(tmp2d)) DEALLOCATE(tmp2d)
        NOBC=NOBC_ORI
        ALLOCATE(WLOBC(NOBC,NTMAX_WL) )
        ALLOCATE(tmp2d(ISUB,JSUB) )
        DO N=NSTR,NEND
	 N0=N-NSTR+1
         IF (IGRD .EQ. 2 .or. IGRD .EQ. 3)THEN   !! spatial interpolation using bicubic or bilinear
            DO I=1,ISUB
            DO J=1,JSUB
                tmp2d(I,J)=WL(I,J,N)
            ENDDO
            ENDDO
            if( IGRD .eq. 2)iflag=1
            if( IGRD .eq. 3)iflag=0
            CALL INTERP_REGRID(1,ISUB,JSUB,lonsub,latsub,TMP2D,
     &           IROMS,JROMS,lonm,latm,outm,Iout,Jout,1)
            DO I=1,NOBC
               WLOBC(I,N0)=outm(IOBC(I),JOBC(I))
	    ENDDO   
 	 ELSEIF (IGRD .EQ. 1 .OR. IGRD .EQ. 4 )THEN   !! using remesh routine
            NDUM=0                                    !! nature neighbors spatial interpolation 
            DO I=1,ISUB
            DO J=1,JSUB
               IF( WL(I,J,N) .GT. -9999.0 )THEN
                     NDUM=NDUM+1
	             XINP(ndum)=lonsub(i,j)
                     YINP(ndum)=latsub(i,j)
	             ZINP(ndum)=WL(I,J,N)
               ENDIF	
            ENDDO	
            ENDDO	
	    NDATA=NDUM
            IF (IGRD .EQ. 1 )THEN                                  !! using remesh
                call INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     *               NOBC,lonOBC,latOBC,ZOUT,weightnodes,weights,0)
                call INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     *               NOBC,lonOBC,latOBC,ZOUT,weightnodes,weights,1)
    	    ELSEIF (IGRD .EQ. 4)THEN                               !! using nature neighbors
                call INTERP_NNEIGHBORS(NDATA,XINP,YINP,ZINP,
     *               NOBC,lonOBC,latOBC,ZOUT)
            ENDIF
            DO I=1,NOBC
               WLOBC(I,N0)=ZOUT(I)
	    ENDDO   
         ENDIF
        ENDDO  ! end of N LOOP
      ENDIF
!      DO N=1,NTMAX
!	    WRITE(45,'(I5,50F9.3)')N,TIMEOBC(N),(WLOBC(I,N),I=1,50)
!      ENDDO
!      CLOSE(45)  
c------------------------------------------------------------------
C  processing Temperature and salinity open boundary
c------------------------------------------------------------------
300   CONTINUE
      print *,'Begin horizontal interpolation'
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IF (trim(OCEAN_MODEL) .EQ. "SELFE" .OR. 
     1    trim(OCEAN_MODEL) .EQ. "selfe")THEN
 	NOBC=NUM_T_NUDGE
!        allocate (lon_nudge(NUM_T_NUDGE) 
!        allocate (lat_nudge(NUM_T_NUDGE) 
        ndummy=0
        do n=1,NODEm
	  if(weight_t_nudge(n) .GT. 0.0)THEN
	    ndummy=ndummy+1
	    lon_nudge(ndummy)=lonm(n)
	    lat_nudge(ndummy)=latm(n)
	    node_t_nudge(ndummy)=n
	    if(lon_nudge(ndummy) .GT. 180.0)
     1	      lon_nudge(ndummy)=lon_nudge(ndummy)-360.
	  endif  
        enddo
	IF(ndummy .NE. NUM_T_NUDGE)then
	  write(*,*)'ndummy is not equal to NUM_T_NUDGE'
	  write(*,*)'ndummy= ',ndummy,'NUM_T_NUDGE=',NUM_T_NUDGE
	  stop
	endif  
        IF (ALLOCATED(tempOBC)) DEALLOCATE(tempOBC)
        IF (ALLOCATED(saltOBC)) DEALLOCATE(saltOBC)
        IF (ALLOCATED(uOBC)) DEALLOCATE(uOBC)
        IF (ALLOCATED(vOBC)) DEALLOCATE(vOBC)
        ALLOCATE(tempOBC(NOBC,KB,NTMAX) )
        ALLOCATE(saltOBC(NOBC,KB,NTMAX) )
!        ALLOCATE(uOBC(NOBC,KB,NTMAX) )
 !       ALLOCATE(vOBC(NOBC,KB,NTMAX) )
        IF (ALLOCATED(tempOBC_M)) DEALLOCATE(tempOBC_M)
        IF (ALLOCATED(saltOBC_M)) DEALLOCATE(saltOBC_M)
!        IF (ALLOCATED(uOBC_M)) DEALLOCATE(uOBC_M)
!        IF (ALLOCATED(vOBC_M)) DEALLOCATE(vOBC_M)
!        IF (ALLOCATED(ueOBC_M)) DEALLOCATE(ueOBC_M)
!        IF (ALLOCATED(veOBC_M)) DEALLOCATE(veOBC_M)
!        IF (ALLOCATED(ubarOBC)) DEALLOCATE(ubarOBC)
!        IF (ALLOCATED(vbarOBC)) DEALLOCATE(vbarOBC)
        ALLOCATE(tempOBC_M(NOBC,KBm,NTMAX) )
        ALLOCATE(saltOBC_M(NOBC,KBm,NTMAX) )
!        ALLOCATE(uOBC_M(NOBC,KBm,NTMAX) )
!        ALLOCATE(vOBC_M(NOBC,KBm,NTMAX) )
!        ALLOCATE(ueOBC_M(NEOBC,KBm,NTMAX) )
!        ALLOCATE(veOBC_M(NEOBC,KBm,NTMAX) )
!        ALLOCATE(ubarOBC(NEOBC,NTMAX) )
!        ALLOCATE(vbarOBC(NEOBC,NTMAX) )
        IF( IGRD .EQ. 1 .OR. IGRD .EQ. 4)THEN
           IF (ALLOCATED(XINP)) DEALLOCATE(XINP)
           IF (ALLOCATED(YINP)) DEALLOCATE(YINP)
           IF (ALLOCATED(ZINP)) DEALLOCATE(ZINP)
           IF (ALLOCATED(XOUT)) DEALLOCATE(XOUT)
           IF (ALLOCATED(YOUT)) DEALLOCATE(YOUT)
           IF (ALLOCATED(ZOUT)) DEALLOCATE(ZOUT)
           IF (ALLOCATED(weightnodes)) DEALLOCATE(weightnodes)
           IF (ALLOCATED(weights)) DEALLOCATE(weights)
           allocate(XINP(NNSUB) )
           allocate(YINP(NNSUB) )
           allocate(ZINP(NNSUB) )
           allocate(XOUT(NOBC) )
           allocate(YOUT(NOBC) )
           allocate(ZOUT(NOBC) )
           allocate(weightnodes(NOBC,3))
           allocate(weights(NOBC,3))
        ENDIF


      ENDIF

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO K=1,KMAX
         IF (IGRD .EQ. 1 .OR. IGRD .EQ. 4)THEN  
             NDUM=0
             DO I=1,ISUB
             DO J=1,JSUB
                IF( TEMP(I,J,K,NSTR) .GT. -29.0 )THEN
                   NDUM=NDUM+1
	           XINP(ndum)=lonsub(i,j)
                   YINP(ndum)=latsub(i,j)
!		   write(88,'(10f12.4)')XINP(ndum),YINP(ndum)
                ENDIF	
             ENDDO	
             ENDDO
!	      close(88)
	     NDATA=NDUM
	     print *,'Data number at vertical layer ',K,' is ',NDATA
             IF( (NDATA .LT. 10) .AND. (K .GT. 1) )THEN
               DO N=NSTR,NEND
                 N0=N-NSTR+1
                 DO I=1,NOBC
                  tempOBC(I,K,N0)=tempOBC(I,K-1,N0)
                  saltOBC(I,K,N0)=saltOBC(I,K-1,N0)
                ENDDO
              ENDDO
              GOTO 778
             ENDIF 
    	     IF (IGRD .EQ. 1 )THEN                                  
                write(*,*)'COMPUTE WEIGHTS AND NODES FOR REMESH !!!'
                 call INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     &           NOBC,lon_nudge,lat_nudge,ZOUT,weightnodes,weights,0)
             ENDIF
         ENDIF 
         DO N=NSTR,NEND
	     N0=N-NSTR+1
  	     IF (IGRD .EQ. 1 .OR. IGRD .EQ. 4 )THEN   !! using remesh routine
                NDUM=0                                     !! nature neighbors spatial interpolation 
                DO I=1,ISUB
                DO J=1,JSUB
                   IF( TEMP(I,J,K,N) .GT. -9999.0 )THEN
                        NDUM=NDUM+1
	                XINP(ndum)=lonsub(i,j)
                        YINP(ndum)=latsub(i,j)
	                ZINP(ndum)=TEMP(I,J,K,N)
                   ENDIF	
                ENDDO	
                ENDDO
	        IF(NDUM .NE. NDATA)THEN
	           WRITE(*,*)'NDATA is not equal to NDUM TEMP!'
	           WRITE(*,*)'NDATA=',NDATA,'NDUM=',NDUM
	           WRITE(*,*)'N=',N,'K=',K
	    	   NDATA=NDUM	
  	           IF (IGRD .EQ. 1 )THEN                                  
                       call INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     &             NOBC,lon_nudge,lat_nudge,ZOUT,weightnodes,weights,0)
                   ENDIF

	        ENDIF   
  	        IF (IGRD .EQ. 1 )THEN                                  !! using remesh
                       call INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     &             NOBC,lon_nudge,lat_nudge,ZOUT,weightnodes,weights,1)
    	        ELSEIF (IGRD .EQ. 4)THEN                               !! using nature neighbors
                       call INTERP_NNEIGHBORS(NDATA,XINP,YINP,ZINP,
     &                 NOBC,lon_nudge,lat_nudge,ZOUT)
                ENDIF
                DO I=1,NOBC
                     tempOBC(I,K,N0)=ZOUT(I)
	        ENDDO   
!	        WRITE(*,*)'TEMP:NDATA & NDUM =',NDATA,NDUM
                
		NDUM=0                                     
                DO I=1,ISUB
                DO J=1,JSUB
                     IF( SALT(I,J,K,N) .GT. -9999.0 )THEN
                        NDUM=NDUM+1
	                ZINP(ndum)=SALT(I,J,K,N)
                     ENDIF	
                ENDDO	
                ENDDO	
	        IF(NDUM .NE. NDATA)THEN
	            WRITE(*,*)'NDATA is not equal to NDUM in SALT!'
	            WRITE(*,*)'NDATA=',NDATA,'NDUM=',NDUM
	            WRITE(*,*)'N=',N,'K=',K
	  	    NDUM=0                                     
                    DO I=1,ISUB
                    DO J=1,JSUB
                      IF( SALT(I,J,K,N) .GT. -29.0 )THEN
                         NDUM=NDUM+1
	                 XINP(ndum)=lonsub(i,j)
                         YINP(ndum)=latsub(i,j)
	                 ZINP(ndum)=SALT(I,J,K,N)
                      ENDIF	
                    ENDDO	
                    ENDDO	
	    	    NDATA=NDUM	
  	            IF (IGRD .EQ. 1 )                                  
     &              call INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     &           NOBC,lon_nudge,lat_nudge,ZOUT,weightnodes,weights,0)
	        ENDIF   
  	        IF (IGRD .EQ. 1 )THEN                                  !! using remesh
                  call INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     *           NOBC,lon_nudge,lat_nudge,ZOUT,weightnodes,weights,1)
    	        ELSEIF (IGRD .EQ. 4)THEN                               !! using nature neighbors
                   call INTERP_NNEIGHBORS(NDATA,XINP,YINP,ZINP,
     *             NOBC,lon_nudge,lat_nudge,ZOUT)
                ENDIF
                DO I=1,NOBC
                     saltOBC(I,K,N0)=ZOUT(I)
	        ENDDO   
!	        WRITE(*,*)'SALT NDATA & NDUM =',NDATA,NDUM
           ENDIF
         ENDDO
778      CONTINUE
      ENDDO                 !! end loop of K

      OPEN(45,file='TS_NCOM.dat')
      DO K=1,KMAX
           WRITE(45,'(I5,50F9.3)')K,DEPTH(K)
      ENDDO	   
      DO N=1,NTMAX
         WRITE(45,'(I5,2x,10F9.3)')N,TIMEOBC(N)
         DO I=1,NOBC
	    WRITE(45,'(I5,50F9.3)')I,(tempOBC(I,K,N),K=1,KMAX)
	    WRITE(45,'(I5,50F9.3)')I,(saltOBC(I,K,N),K=1,KMAX)
	 ENDDO
      ENDDO
      CLOSE(45)  
      print *,'T min & max= ',minval(tempOBC),maxval(tempOBC)
      print *,'S min & max= ',minval(saltOBC),maxval(saltOBC)
c------------------------------------------------------------------
C  vertically interpolating to model sigma coordinate from z-coordinate of WOA05 or NCOM
C  For FVCOM, K=1 for surface and K=KBm for bottom
C  For SLEFE, K=1 for bottom and K=KBm for surface
c------------------------------------------------------------------
      print *,'Begin vertical interpolation'
      IF (ALLOCATED(oned1)) DEALLOCATE(oned1)
      IF (ALLOCATED(oned2)) DEALLOCATE(oned2)
      IF (ALLOCATED(oned3)) DEALLOCATE(oned3)
      IF (ALLOCATED(oned4)) DEALLOCATE(oned4)
      allocate(oned1(KBm) )
      allocate(oned2(KMAX) )
      allocate(oned3(KMAX) )
      allocate(oned4(KMAX) )
!      hmin=minval(hm)
!      hmax=maxval(hm)
!      hc=min(max(hmin,0.0),TCLINE)
      DO K=1,KMAX
         oned2(K)=depth(K)
      ENDDO	 
      DO N=1,NTMAX
         DO I=1,NOBC
	  h0=0.01   !min. depth for wet/dry;
          eta=0.0   !elevation zeta
	  dp=hm(node_t_nudge(I))  ! water depth
          call zcor_SZ(dp,eta,h0,h_s,h_c,theta_b,theta_f,kz,KBm
     1    ,ztot,sigma,zcor,idry,kbp)
!          IF (ALLOCATED(zsigma)) DEALLOCATE(zsigma)
          DO K=kbp,KBm
	    zsigma(K)=zcor(K)
	    if(zsigma(K) .lt. 0.0)zsigma(K)=-zsigma(K)
	  ENDDO  
          DO K=1,kbp-1
	   zsigma(K)=ztot(K)
	   if(zsigma(K) .lt. 0.0)zsigma(K)=-zsigma(K)
	  ENDDO  
          DO K=1,KBm
	    KR=KBm-K+1
	    oned1(k)=zsigma(KR)
	  ENDDO  
          DO K=1,KBm
	    zsigma(K)=oned1(k)
	  ENDDO  

           DO K=1,KMAX
             oned3(K)=TEMPOBC(I,K,N)
             oned4(K)=SALTOBC(I,K,N)
           ENDDO	 
           CALL lineararray(KBm,zsigma,ONED1,KMAX,oned2,ONED3)
           DO K=1,KBm
	      KR=KBm-K+1
	      TEMPOBC_M(I,K,N)=ONED1(KR)
           ENDDO
!           DO K=kbp,KBm
!	       TEMPOBC_M(I,K,N)=ONED1(K)
!           ENDDO
           DO K=1,kbp-1
	       TEMPOBC_M(I,K,N)=TEMPOBC_M(I,kbp,N)
	   ENDDO  
           CALL lineararray(KBm,zsigma,ONED1,KMAX,oned2,ONED4)
           DO K=1,KBm
	       KR=KBm-K+1
	       SALTOBC_M(I,K,N)=ONED1(KR)
           ENDDO
           DO K=1,kbp-1
	       SALTOBC_M(I,K,N)=SALTOBC_M(I,kbp,N)
	   ENDDO  
           if(I .EQ. 1)THEN
            write(77,*)(zsigma(K),k=1,KBm)
            write(77,*)(tempobc_M(I,K,N),k=1,KBm)
           endif
         ENDDO
      ENDDO
      CLOSE(77)
      print *,'end vertical interpolation'
      OPEN(45,file='TS_NCOM_SELFE.dat')
      DO I=1,NOBC
         DO N=1,NTMAX
            WRITE(45,'(I5,2x,10F9.3)')N,TIMEOBC(N)
	    WRITE(45,*)I,(tempOBC_M(I,K,N),K=1,KBm)
	    WRITE(45,*)I,(saltOBC_M(I,K,N),K=1,KBm)
 !           WRITE(45,'(I5,50F9.3)')I,(tempOBC_M(I,K,N),K=1,KBm)
 !           WRITE(45,'(I5,50F9.3)')I,(saltOBC_M(I,K,N),K=1,KBm)

	 ENDDO
      ENDDO
      CLOSE(45)  
        
c------------------------------------------------------------------
C  end of vertical interpolation onto model vertical coordinate
C   Begin interpolating in time to DELT	
c------------------------------------------------------------------
!      DELT_TS=3.0
!      IF (TRIM(DBASE_TS) .EQ. 'NCOM' )DELT_TS=3.0
!      NREC_TS=INT( (jdaye-jdays)*24/DELT_TS)+1
      NREC_TS=INT( (day_end-day_start)*86400/DELT_TS)+1
      IF (ALLOCATED(ts_time)) DEALLOCATE(ts_time)
      ALLOCATE(ts_time(NTMAX) )
      DO N=1,NTMAX
         ts_time(N)=TIMEOBC(N)
      ENDDO
!      IF (TRIM(DBASE_TS) .EQ. 'WOA05' )THEN  ! temp
        IF (ALLOCATED(oned1)) DEALLOCATE(oned1)
        IF (ALLOCATED(oned2)) DEALLOCATE(oned2)
        IF (ALLOCATED(oned3)) DEALLOCATE(oned3)
        IF (ALLOCATED(oned4)) DEALLOCATE(oned4)
        IF (ALLOCATED(TMP4D)) DEALLOCATE(TMP4D)
        allocate(oned1(NREC_TS) )
        allocate(oned2(NREC_TS) )
        allocate(oned3(NTMAX) )
        allocate(oned4(NTMAX) )
        allocate(TMP4D(4,NOBC,KBm,NREC_TS))
	DO N=1,NREC_TS
	  oned1(N)=day_start+(N-1)*DELT_TS/86400.
	ENDDO  
        DO I=1,NOBC
        DO K=1,KBm
          DO N=1,NTMAX
            oned3(N)=TEMPOBC_M(I,K,N)
            oned4(N)=SALTOBC_M(I,K,N)
          ENDDO       
          CALL lineararray(NREC_TS,ONED1,ONED2,NTMAX,TS_TIME,ONED3)
          DO N=1,NREC_TS
            TMP4D(1,I,K,N)=ONED2(N)
          ENDDO
          CALL lineararray(NREC_TS,ONED1,ONED2,NTMAX,TS_TIME,ONED4)
          DO N=1,NREC_TS
            TMP4D(2,I,K,N)=ONED2(N)
          ENDDO
!         DO N=1,NTMAX
!            oned3(N)=uOBC_M(I,K,N)
!            oned4(N)=vOBC_M(I,K,N)
!         ENDDO       
!         CALL lineararray(NREC,TIME_M,ONED2,NTMAX,TS_TIME,ONED3)
!         DO N=1,NREC
!           TMP4D(3,I,K,N)=ONED2(N)
!         ENDDO
!         CALL lineararray(NREC,TIME_M,ONED2,NTMAX,TS_TIME,ONED4)
!         DO N=1,NREC
!           TMP4D(4,I,K,N)=ONED2(N)
!         ENDDO

        ENDDO
        ENDDO
        IF (ALLOCATED(TEMPOBC_M)) DEALLOCATE(TEMPOBC_M)
        IF (ALLOCATED(SALTOBC_M)) DEALLOCATE(SALTOBC_M)
!      IF (ALLOCATED(uOBC_M)) DEALLOCATE(uOBC_M)
!      IF (ALLOCATED(vOBC_M)) DEALLOCATE(vOBC_M)
!      IF (ALLOCATED(ueOBC_M)) DEALLOCATE(ueOBC_M)
!      IF (ALLOCATED(veOBC_M)) DEALLOCATE(veOBC_M)
!      IF (ALLOCATED(ubarOBC)) DEALLOCATE(ubarOBC)
!      IF (ALLOCATED(vbarOBC)) DEALLOCATE(vbarOBC)
        IF (ALLOCATED(TS_TIME)) DEALLOCATE(TS_TIME)
        allocate(TEMPOBC_M(NOBC,KBm,NREC_TS))
        allocate(SALTOBC_M(NOBC,KBm,NREC_TS))
!      allocate(uOBC_M(NOBC,KBm,NREC))
!      allocate(vOBC_M(NOBC,KBm,NREC))
!      allocate(ueOBC_M(NEOBC,KBm,NREC))
!      allocate(veOBC_M(NEOBC,KBm,NREC))
!      allocate(ubarOBC(NEOBC,NREC))
!      allocate(vbarOBC(NEOBC,NREC))
        allocate(TS_TIME(NREC_TS))
        DO N=1,NREC_TS
          ts_time(N)=ONED1(N)
        DO I=1,NOBC
        DO K=1,KBm
          TEMPOBC_M(I,K,N)=TMP4D(1,I,K,N) 
          SALTOBC_M(I,K,N)=TMP4D(2,I,K,N) 
!         uOBC_M(I,K,N)=TMP4D(3,I,K,N) 
!         vOBC_M(I,K,N)=TMP4D(4,I,K,N) 
        ENDDO
        ENDDO
        ENDDO
        NTMAX=NREC_TS
!      ENDIF	    
      tmin=9999.9
      smin=999.
      DO N=1,NTMAX
      DO I=1,NOBC
      DO K=1,KBm
        if(TEMPOBC_M(I,K,N) .lt. tmin)then
           tmin=TEMPOBC_M(I,K,N)
           Nmin=N
           Imin=I
           Kmin=K
        endif
         if(SALTOBC_M(I,K,N) .lt. smin)then
           smin=SALTOBC_M(I,K,N)
           Nmin0=N
           Imin0=I
           Kmin0=K
        endif
      ENDDO
      ENDDO
      ENDDO
      print *,'tmin =',tmin,Nmin,Imin,Kmin
      print *,'smin= ',smin,Nmin0,Imin0,Kmin0
c------------------------------------------------------------------
C  deallocate unused arrays
c------------------------------------------------------------------
      IF (ALLOCATED(lon)) DEALLOCATE(lon)
      IF (ALLOCATED(lat)) DEALLOCATE(lat)
      IF (ALLOCATED(lonsub)) DEALLOCATE(lonsub)
      IF (ALLOCATED(latsub)) DEALLOCATE(latsub)
      IF (ALLOCATED(masksub)) DEALLOCATE(masksub)
      IF (ALLOCATED(depth)) DEALLOCATE(depth)
      IF (ALLOCATED(temp)) DEALLOCATE(temp)
      IF (ALLOCATED(salt)) DEALLOCATE(salt)
      IF (ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
      IF (ALLOCATED(tmp2d)) DEALLOCATE(tmp2d)
      IF (ALLOCATED(tmp3d)) DEALLOCATE(tmp3d)
      IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
      IF (ALLOCATED(oned1)) DEALLOCATE(oned1)
      IF (ALLOCATED(oned2)) DEALLOCATE(oned2)
      IF (ALLOCATED(oned3)) DEALLOCATE(oned3)
      IF (ALLOCATED(oned4)) DEALLOCATE(oned4)

c------------------------------------------------------------------
C  processing subtidal Water Level open boundary from RTOFS
c------------------------------------------------------------------
      
      IF (TRIM(DBASE_WL) .EQ. 'RTOFS')THEN
         IF( (trim(OCEAN_MODEL) .EQ. "SELFE") .OR. 
     1       (trim(OCEAN_MODEL) .EQ. "selfe") )NOBC=NOBC_ORI
         IFILE_NCOM=0
	 NCOM_FILE='RTOFS_FILE_WL'
         INQUIRE(FILE=trim(NCOM_FILE),EXIST=FEXIST)
         IF(FEXIST)THEN
           CLOSE(60)
           OPEN(60,file=trim(NCOM_FILE))
           DO I=1,999
              READ(60,'(a100)',END=3335)BUFFER
              FILE_NCOM(I)=trim(adjustL(BUFFER))
           ENDDO
3335       CLOSE(60)
	   IFILE_NCOM=I-1
         ENDIF
         WRITE(*,*)'Total number of RTOFS WL FILES= ',IFILE_NCOM
	 IF(IFILE_NCOM .LT. 1)THEN
	      WRITE(*,*)'No RTOFS WL file is available in this period'
              DBASE_TS='ETSS'
             WRITE(ICORMS,'(a)')'RTOFS FILE IS NOT FOUND' 
             WRITE(ICORMS,'(a)')'USE ETSS BACKUP' 
	 ENDIF  
	 DO I=1,IFILE_NCOM
	    WRITE(*,*)'RTOFS I=',I,TRIM(FILE_NCOM(I))
	 ENDDO
C-----------------------------------------------------------------------
C     Open and read all RTOFS WL netCDF files available during start time to end time       
C-----------------------------------------------------------------------
         FIN=TRIM(FILE_NCOM(1))
         DO I=1,4
          DIMS(I)=1
         ENDDO	
         VNAME='ssh'
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IM=DIMS(1)
	 JM=DIMS(2)
	 NT=DIMS(3)
	 WRITE(*,*)'RTOFS IM=',IM,'JM= ',JM,'KB= ',KB,'NT= ',NT
         IF (ALLOCATED(lon)) DEALLOCATE(lon)
         IF (ALLOCATED(lat)) DEALLOCATE(lat)
         IF (ALLOCATED(zeta_time)) DEALLOCATE(zeta_time)
         ALLOCATE(lon(IM,JM) )
         ALLOCATE(lat(IM,JM) )
         ALLOCATE(zeta_time(NMAX) )
         DO I=1,4
          DIMS(I)=1
         ENDDO	
	 VNAME='Longitude'
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
         IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
         ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
         IF(NDIM .EQ. 2)THEN
           DO I=1,IM
           DO J=1,JM
             lon(i,j)=TMP4D(i,j,1,1)
 	     if(lon(i,j) .gt. 180)lon(i,j)=lon(i,j)-360.
           ENDDO
           ENDDO
         ELSEIF(NDIM .EQ. 1)THEN
           DO I=1,IM
           DO J=1,JM
             lon(i,j)=TMP4D(i,1,1,1)
 	     if(lon(i,j) .gt. 180)lon(i,j)=lon(i,j)-360.
           ENDDO
           ENDDO
	 ENDIF  
	 VNAME='Latitude'
         CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
         IF(NDIM .EQ. 2)THEN
           DO I=1,IM
           DO J=1,JM
             lat(i,j)=TMP4D(i,j,1,1)
           ENDDO
           ENDDO
         ELSEIF(NDIM .EQ. 1)THEN
           DO I=1,IM
           DO J=1,JM
             lat(i,j)=TMP4D(J,1,1,1)
           ENDDO
           ENDDO
         ENDIF
C-----------------------------------------------------------------------
C        select subdomain I and J index        
C-----------------------------------------------------------------------
         IMIN=99999
         IMAX=-9999
         JMIN=99999
         JMAX=-9999
         DO I=1,IM
         DO J=1,JM
           IF(lon(i,j) .GE. minlon .and. lon(i,j) .LE. maxlon
     &    .and. lat(i,j) .GE. minlat .and. lat(i,j) .LE. maxlat)then
             IF(I .LT. IMIN)IMIN=I 	  
             IF(I .GT. IMAX)IMAX=I 	  
             IF(J .LT. JMIN)JMIN=J 	  
             IF(J .GT. JMAX)JMAX=J
	   ENDIF
         ENDDO
         ENDDO	  	  
         ISUB=IMAX-IMIN+1	 
         JSUB=JMAX-JMIN+1	 
         write(*,*)'subdomain I J=',ISUB,JSUB,IMIN,IMAX,JMIN,JMAX
C-----------------------------------------------------------------------
C allocate sizes of arrays for NCOM products 
C-----------------------------------------------------------------------
         NNSUB=ISUB*JSUB
         IF (ALLOCATED(lonsub)) DEALLOCATE(lonsub)
         IF (ALLOCATED(latsub)) DEALLOCATE(latsub)
         IF (ALLOCATED(WL)) DEALLOCATE(WL)
         allocate(lonsub(ISUB,JSUB))
         allocate(latsub(ISUB,JSUB))
         allocate(WL(ISUB,JSUB,NT*1000))
         DO N=1,NT*1000
         DO J=1,JSUB
         DO I=1,ISUB
	    WL(I,J,N)=0.0
         ENDDO
         ENDDO
         ENDDO   
         DO J=1,JSUB
         DO I=1,ISUB
            lonsub(i,j)=lon(IMIN+I-1,JMIN+J-1)
            latsub(i,j)=lat(IMIN+I-1,JMIN+J-1)
	    write(77,'(2F12.4,2I6)')lonsub(i,j),latsub(i,j),I,J
         ENDDO
         ENDDO 
	 close(77)  
         ICOUNT=0
	 NREC=0
         TIMELAST=-9999.0	 
	 DO IFILE=1,IFILE_NCOM
	   write(*,*)'reading RTOFS NetCDF file...',IFILE
           FIN=TRIM(FILE_NCOM(IFILE))
	   VNAME='MT'
	   ANAME='units'
           DO I=1,4
             DIMS(I)=1
           ENDDO	
           CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
           IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
           ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
	   NT=DIMS(1)
           CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,6)
	   ANAME=trim(adjustL(ANAME))
	   LEN=LEN_TRIM(ANAME)
           LL=INDEX(ANAME,'minute')         
	   IF(LL .GT. 0)scale_time=1.0/1440.0
           LL=INDEX(ANAME,'hour')         
	   IF(LL .GT. 0)scale_time=1.0/24.0
           LL=INDEX(ANAME,'day')         
	   IF(LL .GT. 0)scale_time=1.0
           LL=INDEX(ANAME,'since')         
	   IF(LL .GT. 0)then
             read(ANAME(LL+6:LEN),80)IYR,IMM,IDD,IHH
             write(*,*)'basetime=',IYR,IMM,IDD,IHH
	   else
	     write(*,*)'there is error while reading base date'
	     stop
	   endif    
           yearb=IYR
           monthb=IMM
           dayb=IDD
           hourb=IHH
           jday=JULIAN(yearb,monthb,dayb,hourb)
           CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,7)
           TIME_RTOFS=TMP4D(1,1,1,1)*scale_time+jday-jbase_date
           IF(TIMELAST .LT. TIME_RTOFS )THEN
               ICOUNT=ICOUNT+1
	       ZETA_TIME(ICOUNT)=TIME_RTOFS
	       TIMELAST=TIME_RTOFS 
           ENDIF
!	   N0=NREC
!          DO N=1,NREC
!            IF(ZETA_TIME(N) .GE. DUMMY)THEN
!	       N0=N-1 
!	       GOTO 992
!	     ENDIF  
!           ENDDO	
!992        CONTINUE	   	
!           IF( (N .GT. NREC) .AND. (NREC .EQ. 1) )N0=0 
!           ICOUNT=N0
!           DO N=1,NT
!             TIME_NCOM=TMP4D(N,1,1,1)*scale_time+jday-jbase_date
!             if(TIME_NCOM .GT. TS_TIME(ICOUNT) )THEN
!               ICOUNT=ICOUNT+1
!	       ZETA_TIME(ICOUNT)=TIME_NCOM
!	       print *,'RTOFS time=',ICOUNT,ZETA_TIME(ICOUNT)
 !            endif 
!	   ENDDO   
	   VNAME='ssh'
           CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0)
           IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
           ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
           CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1)
!           ICOUNT=N0
           DO N=1,NT
!	     ICOUNT=ICOUNT+1
           DO J=1,JSUB
           DO I=1,ISUB
            WL(i,j,ICOUNT)=TMP4D(IMIN+I-1,JMIN+J-1,N,1)
            if(abs(WL(I,J,ICOUNT)) .GE. 99.0)THEN
	      WL(I,J,ICOUNT)=-99999.9
	    ELSE
	      WL(I,J,ICOUNT)=WL(I,J,ICOUNT)+0.25   !! nontidal WL is about 25 cm lower
	    ENDIF  
           ENDDO
           ENDDO   
           ENDDO   
	   NREC=ICOUNT
           STATUS=NF_CLOSE(NCID)
         ENDDO      
	 NT=NREC 
	 write(*,*)'number of RTOFS WL data NREC= ',NREC
C-----------------------------------------------------------------------
C  End of reading date from RTOFS WL products 
C-----------------------------------------------------------------------
         NSTR=1
         DO N=1,NT
            write(*,*)'time= ',N,zeta_time(N)
            IF(zeta_TIME(N) .gt. day_start)then
	      NSTR=N-1
	      goto 1330
	    ENDIF
         ENDDO	
1330     continue
         IF(NSTR .LE. 0 .OR. NSTR .GT. NT)THEN  !AJ 08/13/2010 extrapolation
           DO NN=NT,1,-1
	      ZETA_TIME(NN+1)=ZETA_TIME(NN)
              DO I=1,ISUB
              DO J=1,JSUB
	        WL(I,J,NN+1)=WL(I,J,NN) 
              ENDDO	
              ENDDO
           ENDDO
	   ZETA_TIME(1)=day_start
	   NT=NT+1
	   NSTR=1
         ENDIF
         IF (ZETA_TIME(NT) .LT. day_end)THEN
            diff=day_end-ZETA_TIME(NT)
            IF (diff .LT. 5.0)THEN 
C AJ ZHANG 03/18/2010, allow persistence for the maximum 5 days. 
	      ZETA_TIME(NT+1)=day_end
              DO I=1,ISUB
              DO J=1,JSUB
	        WL(I,J,NT+1)=WL(I,J,NT) 
              ENDDO	
              ENDDO
	      NT=NT+1
	    ELSE  
	      write(*,*)' RTOFS WL data is too shorter than day_end'
	      write(*,*)' The time period is not covered by RTOFS'	  
              WRITE(ICORMS,'(a)')TRIM(DBASE_WL)//' is too short'
              WRITE(ICORMS,'(a)')'USE ETSS BACKUP' 
              DBASE_WL='ETSS'
	    ENDIF  
         ENDIF    
         DO N=1,NT
           if(ZETA_TIME(N) .GE. day_end)then
	     NEND=n
	     goto 1440
	   ENDIF
         ENDDO	
1440     continue
         IF(N .GT. NT)THEN
  	   write(*,*)' time period is not covered by dataset'	  
           WRITE(ICORMS,'(a)')' time period is not covered'
	   WRITE(ICORMS,'(a)')'Generation of WL OBC failed'
	   WRITE(ICORMS,'(a)')'stop in nos_ofs_create_OBC.f'
	   stop 'stop in nos_ofs_create_OBC.f'
         ENDIF  	  
         write(*,*)'start and end time of RTOFS WL= ',
     &   ZETA_time(NSTR),ZETA_time(NEND),NSTR,NEND,day_start,day_end 		
         NTMAX_WL=NEND-NSTR+1
         IF (ALLOCATED(TIMEOBC)) DEALLOCATE(TIMEOBC)
         ALLOCATE(TIMEOBC(NTMAX_WL) )
         DO N=1,NTMAX_WL
             TIMEOBC(N)=ZETA_TIME(NSTR+N-1)
         ENDDO
         IF (ALLOCATED(zeta_time)) DEALLOCATE(zeta_time)
         ALLOCATE(zeta_time(NTMAX_WL) )
         DO N=1,NTMAX_WL
           zeta_time(N)=TIMEOBC(N)
         ENDDO
c------------------------------------------------------------------
C  spatial interpolation for Water Level open boundary
c------------------------------------------------------------------

         IF (ALLOCATED(WLOBC)) DEALLOCATE(WLOBC)
         ALLOCATE(WLOBC(NOBC,NTMAX_WL) )
         IF( IGRD .EQ. 1 .OR. IGRD .EQ. 4)THEN
           IF (ALLOCATED(XINP)) DEALLOCATE(XINP)
           IF (ALLOCATED(YINP)) DEALLOCATE(YINP)
           IF (ALLOCATED(ZINP)) DEALLOCATE(ZINP)
           IF (ALLOCATED(XOUT)) DEALLOCATE(XOUT)
           IF (ALLOCATED(YOUT)) DEALLOCATE(YOUT)
           IF (ALLOCATED(ZOUT)) DEALLOCATE(ZOUT)
           IF (ALLOCATED(weightnodes)) DEALLOCATE(weightnodes)
           IF (ALLOCATED(weights)) DEALLOCATE(weights)
           allocate(XINP(NNSUB) )
           allocate(YINP(NNSUB) )
           allocate(ZINP(NNSUB) )
           allocate(XOUT(NOBC) )
           allocate(YOUT(NOBC) )
           allocate(ZOUT(NOBC) )
           allocate(weightnodes(NOBC,3))
           allocate(weights(NOBC,3))
         ENDIF
         DO N=NSTR,NEND
	   N0=N-NSTR+1
           IF (IGRD .EQ. 2 .or. IGRD .EQ. 3)THEN   !! spatial interpolation using bicubic or bilinear
             WRITE(*,*)'This Option has been deactivated'
	     WRITE(*,*)'Please choose IGRD=1 or IGRD=4'
	     STOP
 	   ELSEIF (IGRD .EQ. 1 .OR. IGRD .EQ. 4 )THEN   !! using remesh routine
             NDUM=0                                    !! nature neighbors spatial interpolation 
             DO I=1,ISUB
             DO J=1,JSUB
               IF( WL(I,J,N) .GT. -9999.0 )THEN
                     NDUM=NDUM+1
	             XINP(ndum)=lonsub(i,j)
                     YINP(ndum)=latsub(i,j)
	             ZINP(ndum)=WL(I,J,N)
               ENDIF	
             ENDDO	
             ENDDO	
	     NDATA=NDUM
             IF (IGRD .EQ. 1 )THEN                                  !! using remesh
                call INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     *               NOBC,lonOBC,latOBC,ZOUT,weightnodes,weights,0)
                call INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     *               NOBC,lonOBC,latOBC,ZOUT,weightnodes,weights,1)
    	     ELSEIF (IGRD .EQ. 4)THEN                               !! using nature neighbors
                call INTERP_NNEIGHBORS(NDATA,XINP,YINP,ZINP,
     *               NOBC,lonOBC,latOBC,ZOUT)
             ENDIF
             DO I=1,NOBC
               WLOBC(I,N0)=ZOUT(I)
	     ENDDO   
           ENDIF
         ENDDO  ! end of N LOOP
      ENDIF
c------------------------------------------------------------------
C  Generate WL OBC from ETSS gridded products if DBASE_WL = ETSS
C  Read in ETSS from a ASCII file generated using wgrib2 utility
c------------------------------------------------------------------
C       
      IF( (trim(OCEAN_MODEL) .EQ. "SELFE") .OR. 
     1    (trim(OCEAN_MODEL) .EQ. "selfe") )NOBC=NOBC_ORI
      IF(TRIM(DBASE_WL) .EQ. 'ETSS' )THEN
         WRITE(*,*)'GENERATE SUBTIDAL WL OBC FROM ETSS'      
         CLOSE(1)
         OPEN(1,FILE=TRIM(ETSSFILE),STATUS='OLD' )
         NT=0
         NETSS=0
         irec=0
2000     read(1,'(a200)',end=2002)BUFFER
         LEN1=LEN_TRIM(trim(BUFFER)) 
         IF(LEN1 .GT. 0)THEN
            LL=INDEX(BUFFER,'ETSRG',BACK=.TRUE.)         
            IF(LL .GT. 0)THEN
                NT=NT+1
                if(irec .GT. NETSS)then
                    NETSS=irec
                    NMAX0=NT
                endif
                IF(NT .GT. 1)ITMP1D(NT-1)=irec
                irec=0
            ELSE
                irec=irec+1  
            ENDIF
         ENDIF
         GOTO 2000
2002     CONTINUE
         if(irec .GT. NETSS)then
            NETSS=irec
            NMAX0=NT
         endif
         IF(NT .GT. 1)ITMP1D(NT)=irec
         REWIND(1)
	 WRITE(*,*)'ETSS SUB DIMS NETSS=',NETSS,'NT=',NT
         IF (ALLOCATED(lon_ETSS)) DEALLOCATE(lon_ETSS)
         IF (ALLOCATED(lat_ETSS)) DEALLOCATE(lat_ETSS)
         IF (ALLOCATED(WL_ETSS)) DEALLOCATE(WL_ETSS)
         IF (ALLOCATED(TIMEOBC)) DEALLOCATE(TIMEOBC)
         IF (ALLOCATED(TMP2D)) DEALLOCATE(TMP2D)
         allocate(lon_ETSS(NETSS,NT))
         allocate(lat_ETSS(NETSS,NT))
         allocate(WL_ETSS(NETSS,NT))
         ALLOCATE(TIMEOBC(NT) )
         DO N=1,NT
         DO I=1,NETSS
            WL_ETSS(I,N)=9999.0
         ENDDO
         ENDDO
         NREC=0
         TIMEOBC(1)=-99999.0
         DO N=1,NT
            read(1,'(a200)')BUFFER
            BUFFER=trim(adjustL(BUFFER))
            WRITE(*,*)trim(BUFFER)
CC processing output from degrib
 !           LEN1=LEN_TRIM(trim(BUFFER))
 !           LL=INDEX(BUFFER,'ETSRG_',BACK=.TRUE.)
 !           READ(BUFFER(LL+6:LEN1),'(I4,3I2)')IYR,IMM,IDD,IHH
CC processing output from wgrib2
            LEN1=LEN_TRIM(trim(BUFFER))
            LL=INDEX(BUFFER,'=',BACK=.TRUE.)
            if(BUFFER(LEN1-2:LEN1) .EQ. 'anl')then
               READ(BUFFER(LL+1:LEN1),'(I4,3I2)')IYR,IMM,IDD,IHH
            elseif(BUFFER(LEN1-3:LEN1) .EQ. 'fcst')then 
               READ(BUFFER(LL+1:LEN1),'(I4,3I2,1x,I2)')IYR,IMM,IDD
     &	       ,ICYC,IHH0
	       IHH=ICYC+IHH0
            endif
            print *,'ETSS forecast time= ',IYR,IMM,IDD,IHH,ICYC,IHH0 
            yearb=IYR
            monthb=IMM
            dayb=IDD
            hourb=IHH   !! do not need minutes to construct ETSS file name
            jday=JULIAN(yearb,monthb,dayb,hourb)
            day0=jday-jbase_date
	    DO K=1,NREC
	      IF(TIMEOBC(K) .GE. day0)THEN
		NREC=K-1
	        GOTO 500
	      ENDIF  
	    ENDDO  	 
500	    CONTINUE
	    NREC=NREC+1
            TIMEOBC(NREC)=day0
            DO I=1,ITMP1D(N)  !!   NETSS
              read(1,'(a200)')BUFFER
	      BUFFER=trim(adjustL(BUFFER))
              READ(BUFFER,*) alo,alt,swl      
	      WL_ETSS(I,NREC)=swl
	      if(alo .gt. 180.0)alo=alo-360.0
 	      lon_ETSS(I,NREC)=alo
	      lat_ETSS(I,NREC)=alt
            ENDDO
         ENDDO
	 CLOSE(1)
	 NT=NREC    
c------------------------------------------------------------------
C  End of Reading in ETSS, and find a time period to cover from start time to end time 
c------------------------------------------------------------------
         IF(TIMEOBC(1) .gt. day_start)TIMEOBC(1)=day_start
         IF(TIMEOBC(NT) .lt. day_end)TIMEOBC(NT)=day_end
         NSTR=1
         DO N=1,NT
            IF(TIMEOBC(N) .gt. day_start)then
	      NSTR=N-1
	      goto 630
	    ENDIF
         ENDDO	
630      continue
         IF(NSTR .LE. 0 .OR. NSTR .GT. NT)THEN
	   write(*,*)'Time of first data is later than start time'
	   write(*,*)' time period is not covered by dataset'
          WRITE(ICORMS,'(a)')TRIM(DBASE_WL)//' does not cover forecast'
          WRITE(ICORMS,'(a)')'CRITICAL FAULURE IN GENERATING WL OBC'
	   stop 'stop in nos_ofs_create_OBC.f '
         ENDIF  	  
         DO N=1,NT
            if(TIMEOBC(N) .ge. day_end)then
	       NEND=n
	       goto 640
	    ENDIF
         ENDDO	
640      continue	  
         IF(N .GT. NT)THEN
	    write(*,*)' time period is not covered by dataset'	  
          WRITE(ICORMS,'(a)')TRIM(DBASE_WL)//' does not cover forecast'
          WRITE(ICORMS,'(a)')'CRITICAL FAULURE IN GENERATING WL OBC'
	  stop 'stop in nos_ofs_create_OBC.f'
         ENDIF  	  
         write(*,*)'start and end time of = ',
     &   TIMEOBC(NSTR),TIMEOBC(NEND),NSTR,NEND,day_start,day_end 		
         NTMAX_WL=NEND-NSTR+1
C -------------------------------------------------------------------
C  print for testing
!         CLOSE(1)
!	 OPEN(1,file='ETSS_Watercells.dat')
!         DO I=1,IETSS
!         DO J=1,JETSS
!            IF( WL(I,J,1) .LT. 999.0 )THEN
!	        WRITE(1,'(2F10.4,2I6)')lonsub(i,j),latsub(i,j),I,J
!            ENDIF	
!         ENDDO	
!         ENDDO
!	 CLOSE(1)  

C -------------------------------------------------------------------
C  End of reading ETSS ASCII file and begin to interpolate onto 
C  open boundary model grid points
C ----------------------------------------------------------------------------
         IF (trim(OCEAN_MODEL) .EQ. "SELFE")NOBC=NOBC_ORI
         IF (ALLOCATED(zeta_time)) DEALLOCATE(zeta_time)
         ALLOCATE(ZETA_TIME(NTMAX_WL) )
         DO N=1,NTMAX_WL
           ZETA_TIME(N)=TIMEOBC(NSTR+N-1)
         ENDDO
         IF (ALLOCATED(WLOBC)) DEALLOCATE(WLOBC)
         ALLOCATE(WLOBC(NOBC,NTMAX_WL) )
	 DO I=1,NOBC
	 DO N=1,NTMAX_WL
	     WLOBC(I,N)=0.0
	 ENDDO
	 ENDDO   
	 
         IF( IGRD .EQ. 1 .OR. IGRD .EQ. 4)THEN
            IF (ALLOCATED(XINP)) DEALLOCATE(XINP)
            IF (ALLOCATED(YINP)) DEALLOCATE(YINP)
            IF (ALLOCATED(ZINP)) DEALLOCATE(ZINP)
            IF (ALLOCATED(XOUT)) DEALLOCATE(XOUT)
            IF (ALLOCATED(YOUT)) DEALLOCATE(YOUT)
            IF (ALLOCATED(ZOUT)) DEALLOCATE(ZOUT)
            IF (ALLOCATED(weightnodes)) DEALLOCATE(weightnodes)
            IF (ALLOCATED(weights)) DEALLOCATE(weights)
            allocate(XINP(NETSS) )
            allocate(YINP(NETSS) )
            allocate(ZINP(NETSS) )
            allocate(XOUT(NOBC) )
            allocate(YOUT(NOBC) )
            allocate(ZOUT(NOBC) )
            allocate(weightnodes(NOBC,3))
            allocate(weights(NOBC,3))
         ELSEIF (IGRD .EQ. 2 .or. IGRD .EQ. 3)THEN   !! spatial interpolation using bicubic or bilinear
             WRITE(*,*)'This option is not available anymore!!!!'
	     WRITE(*,*)'Please redefine IGRD=1 or 4 !!'
	     STOP
!            CALL INTERP_REGRID(1,IETSS,JETSS,lonsub,latsub,TMP2D,
!     &      IROMS,JROMS,lonm,latm,outm,Iout,Jout,0)
         ENDIF 
         NDATA=NETSS
         DO N=NSTR,NEND
            NDATA=ITMP1D(N)
	    N0=N-NSTR+1
    	    IF (IGRD .EQ. 1 .OR. IGRD .EQ. 4 )THEN        !! using remesh routine
               NDUM=0                                     !! nature neighbors spatial interpolation 
               DO I=1,NDATA
                  IF( WL_ETSS(I,N) .LT. 999.0 )THEN
                     NDUM=NDUM+1
	             XINP(ndum)=lon_ETSS(I,N)
                     YINP(ndum)=lat_ETSS(I,N)
	             ZINP(ndum)=WL_ETSS(I,N)
                  ENDIF	
               ENDDO	

!               IF(NDUM .NE. NDATA .AND. IGRD .EQ. 1)THEN
!                  WRITE(*,*)'NDATA=',NDATA,'NDUM=',NDUM,'N=',N 
!                  NDATA=NDUM
!               ENDIF
               WRITE(*,*)'NDATA=',NDATA,'NDUM=',NDUM,'N=',N 
               NDATA=NDUM
  	       IF (IGRD .EQ. 1 )THEN                                  !! using remesh
                   call INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     &             NOBC,lonOBC,latOBC,ZOUT,weightnodes,weights,0)

                   call INTERP_REMESH(NDATA,XINP,YINP,ZINP,
     &             NOBC,lonOBC,latOBC,ZOUT,weightnodes,weights,1)
    	       ELSEIF (IGRD .EQ. 4)THEN                               !! using nature neighbors
                   call INTERP_NNEIGHBORS(NDATA,XINP,YINP,ZINP,
     &             NOBC,lonOBC,latOBC,ZOUT)
               ENDIF
               DO I=1,NOBC
                  WLOBC(I,N0)=ZOUT(I)
               ENDDO
            ENDIF
         ENDDO
	 WRITE(*,*)'end of horizontal interpolation of ETSS'
      ENDIF
C -------------------------------------------------------------------
C   End of processing for WL OBC from ETSS forecasts
C begin time interpolation to DELT	
c------------------------------------------------------------------
!      NREC=NINT( (jdaye-jdays)*86400/DELT)+1
      IF( (trim(OCEAN_MODEL) .EQ. "SELFE") .OR. 
     1    (trim(OCEAN_MODEL) .EQ. "selfe") )NOBC=NOBC_ORI
      NREC=NINT( (day_end-day_start)*86400/DELT)+1
      IF (ALLOCATED(oned1)) DEALLOCATE(oned1)
      IF (ALLOCATED(oned2)) DEALLOCATE(oned2)
      IF (ALLOCATED(oned3)) DEALLOCATE(oned3)
      IF (ALLOCATED(oned4)) DEALLOCATE(oned4)
      IF (ALLOCATED(TMP2D)) DEALLOCATE(TMP2D)
      allocate(oned2(NREC) )
      allocate(oned3(NTMAX_WL) )
      allocate(oned4(NTMAX_WL) )
      allocate(TMP2D(NOBC,NREC))
      DO I=1,NOBC
         DO N=1,NTMAX_WL
            oned3(N)=WLOBC(I,N)
         ENDDO       
         CALL lineararray(NREC,TIME_M,ONED2,NTMAX_WL,ZETA_TIME,ONED3)
         DO N=1,NREC
           TMP2D(I,N)=ONED2(N)
         ENDDO
      ENDDO
      IF (ALLOCATED(WLOBC)) DEALLOCATE(WLOBC)
      allocate(WLOBC(NOBC,NREC))
      IF (ALLOCATED(zeta_time)) DEALLOCATE(zeta_time)
      ALLOCATE(ZETA_TIME(NREC) )
      DO N=1,NREC
        zeta_time(N)=TIME_M(N)
      DO I=1,NOBC
         WLOBC(I,N)=TMP2D(I,N) 
      ENDDO
      ENDDO
      NTMAX_WL=NREC
      WRITE(*,*)'end of time interpolation of WL'
C -------------------------------------------------------------------
C   End of processing for WL OBC from ETSS forecasts
C   Print OBC for evaluation
      WRITE(*,*)'NOBC=',NOBC,'NUM_T_NUDGE=',NUM_T_NUDGE
      OPEN(33,file='WL_OBC.dat')
      DO N=1,NTMAX_WL
       write(33,35)ZETA_TIME(N),(WLOBC(I,N),I=1,NOBC,10),WLOBC(NOBC,N)
      ENDDO
      CLOSE(33)
      OPEN(33,file='TEMP_OBC.dat')
      DO N=1,NTMAX
 !      write(33,35)TS_TIME(N),(TEMPOBC_M(I,2,N),I=1,NOBC,10)  ! K=KBm for surface
 !    &  ,TEMPOBC_M(NOBC,KBm,N)
        DO I=1,NUM_T_NUDGE
           write(33,35)TS_TIME(N),(TEMPOBC_M(I,K,N),K=KBm,1,-1)
        enddo
      ENDDO
      CLOSE(33)
      OPEN(33,file='SALT_OBC.dat')
      DO N=1,NTMAX
       write(33,35)TS_TIME(N),(SALTOBC_M(I,KBm,N),I=1,NUM_T_NUDGE,10)
     &  ,SALTOBC_M(NUM_T_NUDGE,KBm,N)
      ENDDO
!      CLOSE(33)
!      OPEN(33,file='U_OBC.dat')
!      DO N=1,NTMAX
!       write(33,35)TS_TIME(N),(uOBC_M(I,2,N),I=1,NOBC,10)
!     &  ,uOBC_M(NOBC,KBm,N)
!      ENDDO
!      CLOSE(33)
!      OPEN(33,file='V_OBC.dat')
!      DO N=1,NTMAX
!       write(33,35)TS_TIME(N),(vOBC_M(I,2,N),I=1,NOBC,10)
!     &  ,vOBC_M(NOBC,KBm,N)
!      ENDDO
!      CLOSE(33)
      
35    FORMAT(500F10.4)	
C -------------------------------------------------------------------
C   End of processing for WL OBC from ETSS forecasts
C   Begin to process real time observations and tidal prediction
C ----------------------------------------------------------------------------
      IF (ALLOCATED(oned1)) DEALLOCATE(oned1)
      IF (ALLOCATED(oned2)) DEALLOCATE(oned2)
      allocate(oned1(NMAX) )
      allocate(oned2(NMAX) )
      KINDAT=2
      CONV=1.0
      XMAJOR=0.0
      DELT_PRD=DELT/3600.0
!      NREC=NINT( (jdaye-jdays)*24/DELT)+1
!      NREC=NINT( (day_end-day_start)*86400/DELT)+1  
      NREC=NINT( (day_end-day_start+2)*86400/DELT)+1  !one more day for prediction
      DO N=1,NREC
!        TIME_PRD(N)=TIME_M(N)
	TIME_PRD(N)=day_start-2+(N-1)*DELT/86400.0
      ENDDO	
        jday=jdays-2
        call GREGORIAN(jday,yearb,monthb,dayb,hourb)
        IYR=INT(yearb)
        IMM=int(monthb+0.001)
        IDD=INT(dayb+0.001)
        IHH=INT(hourb+0.001)
        IMN=INT((hourb-IHH)*60+0.1)
        ISEC=0
        IF(ISEC .EQ. 60)THEN
           ISEC=0
           IMN=IMN+1
        ENDIF
        IF(IMN .EQ. 60)THEN
           IMN=0
           IHH=IHH+1
        ENDIF
        IF(IHH .EQ. 24)THEN
          IHH=0
          IDD=IDD+1
          IF(MOD(IYR,4) .EQ. 0)DAYS_PER_MONTH(2)=29   !!    Leap Year
          IF(IDD .GT. DAYS_PER_MONTH(IMM) )THEN
             IDD=IDD - DAYS_PER_MONTH(IMM)
             IMM=IMM+1
             IF(IMM .GT. 12)THEN
                IMM=IMM-12
                IYR=IYR+1
             ENDIF
          ENDIF
        ENDIF
        WRITE(START_TIMEm1,'(I4.4,4I2.2)')IYR,IMM,IDD,IHH,0
      DO I=1,NSTA
         NTR(I)=0
         NTR_T(I)=0
         NTR_S(I)=0
      DO N=1,NMAX
	 RTIME(i,N)=-99999.9
	 RTIME_T(i,N)=-99999.9
	 RTIME_S(i,N)=-99999.9
	 WL_OBS(i,N)=-99999.9
	 WL_PRD(I,N)=-99999.9
	 SWL_OBS(i,N)=-99999.9
	 T_OBS(I,N)=-99999.9
	 S_OBS(I,N)=-99999.9
      ENDDO
      ENDDO	 

      DO I=1,NSTA
          BUFFER=TRIM(adjustL(NOS_ID(I)))
          L1=LEN_TRIM(BUFFER)
          DO N=1,NWLON_STA
	     L2=size(stationID,1)
  	     WRITE(BUFFER1,*)stationID(1:L2,N)
             BUFFER1=TRIM(adjustL(BUFFER1))
 	     IF(BUFFER(1:L1) .EQ. BUFFER1(1:L1) )GOTO 110
	  ENDDO
110       CONTINUE
          PRINT *,'Conduct tide prediction at STATION: ',BUFFER(1:L1)
	  IF(N .GT. NWLON_STA)THEN 
	     WRITE(*,*)'STATION ID IS NOT FOUND IN THE HC NetCDF FILE'
             IF( WL_FLAG(I) .EQ. 0 )THEN
  	        WRITE(*,*)'PLEASE ADD HC AT STATION '//TRIM(NOS_ID(I))
	        WRITE(*,*)TRIM(OFS)//' STOP HERE'
	        STOP
             ENDIF 		
             DO N=1,NREC
	       WL_PRD(I,N)=-99999.9
	     ENDDO
	  ELSE   
              FOUT=TRIM(NOS_ID(I))//'.prd'
	      DO K=1,37
	        AMP(K)=tide_amp(N,K)
		EPOC(K)=tide_epoc(N,K)
	      ENDDO
!            CALL NOS_PRD(START_TIME,END_TIME,KINDAT,DELT_PRD,CONV,
            CALL NOS_PRD(START_TIMEm1,END_TIME,KINDAT,DELT_PRD,CONV,
     &      XMAJOR,AMP,EPOC,FOUT,ONED1,ONED2)
            DO N=1,NREC
	      WL_PRD(I,N)=ONED1(N)
	    ENDDO
	  ENDIF   
      ENDDO	   
C ----------------------------------------------------------------------------
C    process real time observations in BUFR files of NCEP data tank at NWLON stations
C ----------------------------------------------------------------------------
      WRITE(FOUT,201)'DATA_OBC_',IYRS,IMMS,IDDS,IHHS,
     &   '.dat'  
201     FORMAT(A9,I4.4,3I2.2,A4)
      FOUT=TRIM(OFS)//'_'//TRIM(adjustL(FOUT))
      CLOSE(15) 
      OPEN(15,file=TRIM(FOUT) )
      write(15,*) 'ID        lat     lon   year  mon day hour
     *  Disch   Tem SST   Salinity'
      DO IZ=INT(day_start),INT(day_end)
        jday=IZ+jbase_date
        call GREGORIAN(jday,yearb,monthb,dayb,hourb)
        IYR=INT(yearb)
        IMM=int(monthb+0.001)
        IDD=INT(dayb+0.001)
        IHH=INT(hourb+0.001)
        IMN=INT((hourb-IHH)*60+0.1)
        ISEC=0
        IF(ISEC .EQ. 60)THEN
           ISEC=0
           IMN=IMN+1
        ENDIF
        IF(IMN .EQ. 60)THEN
           IMN=0
           IHH=IHH+1
        ENDIF
        IF(IHH .EQ. 24)THEN
          IHH=0
          IDD=IDD+1
          IF(MOD(IYR,4) .EQ. 0)DAYS_PER_MONTH(2)=29   !!    Leap Year
          IF(IDD .GT. DAYS_PER_MONTH(IMM) )THEN
             IDD=IDD - DAYS_PER_MONTH(IMM)
             IMM=IMM+1
             IF(IMM .GT. 12)THEN
                IMM=IMM-12
                IYR=IYR+1
             ENDIF
          ENDIF
        ENDIF
        WRITE(BUFRFILE,700)'/',IYR,IMM,IDD,'/b001/'  
!     &   '/b001/xx005'  
700     FORMAT(A1,I4.4,2I2.2,A6)
        BUFRFILE=TRIM(NOSWLDIR)//trim(BUFRFILE)//trim(NOSBUFR) 
        INQUIRE(FILE=trim(BUFRFILE),EXIST=FEXIST)
        IF(.NOT.FEXIST)GOTO 860
        print *,'BUFR FILE= ',trim(BUFRFILE)
        CMD='cp '//trim(BUFRFILE)//'  '//TRIM(NOSBUFR)
        call system(trim(CMD) )
        BUFRFILE=trim(NOSBUFR)
!         write(*,*)'BUFR FILE= ',trim(BUFRFILE)
        LUNIN=11
	CLOSE(LUNIN)
        OPEN(LUNIN,file=trim(BUFRFILE),FORM='UNFORMATTED')
C  -------------------OPEN AND READ THRU THE INPUT BUFR FILE ------------------
       
        CALL OPENBF(LUNIN,'IN',LUNIN)                                     

C  --------------READ THROUGH THE MESSAGES/SUBSETS IN THE FILE------------------
C  
        DO WHILE(IREADMG(LUNIN,SUBSET,IDATE).EQ.0)                       
        DO WHILE(IREADSB(LUNIN).EQ.0)                                    
c  --------------READ THE INTERNAL DATE AND CHECK FOR REALISM---------------------
C                           
          CALL UFBINT(LUNIN,DATES,5,1,IRET,'YEAR MNTH DAYS HOUR MINU')
          IYR = NINT(DATES(1))
          IMM = NINT(DATES(2))
          IDD = NINT(DATES(3))                                           
          IHH = NINT(DATES(4))                                            
          IMN = NINT(DATES(5))                                            
          yearb=IYR
          monthb=IMM
          dayb=IDD
          hourb=REAL(IHH)+REAL(IMN/60.0)   
          dayj=JULIAN(yearb,monthb,dayb,hourb)-jbase_date
!          IF (dayj .LT. day_start)goto 800  
!          IF (dayj .GT. day_end)goto 850  
C  --------------READ THE TIDE GAUGE STATION INFO FROM BUFR FILE------------------
c  AJ 09/15/11 Use different CALL routine in order to handle long station IDs 
          CALL READLC(LUNIN, stnbufrid, 'RPID')
!          CALL UFBINT(LUNIN,rpid,1,1,iret,'RPID')
          CALL UFBINT(LUNIN,DATES,5,1,IRET,'CLAT CLON SELV')
          clat=DATES(1)
          clon=DATES(2)
          selv=DATES(3)
          DO I=1,NSTA 
             IF(trim(stnbufrid) .EQ. TRIM(NWS_ID(I)) )THEN
!                   write(15,35)name,clon,clat,selv,IYR,MM,IDD,IHH,MIN
C  ----------------------------------------------------------------------------
C  GET SEA SURFACE TEMPERATURE DATA ALONG WITH DATA CHECK AND
C  TIME INCREMENT AND DISPLACEMENT INFORMATION
C  SST1 -- Sea Surface Temperature
C  QMST -- Sea Surface Data Check Flag
C  AWCK -- Tide Station Automated Water Data Check Flag
C  MWCK -- Tide Station Manual Water Data Check Flag
C  TPMI -- Time Period or Replacement
C  ----------------------------------------------------------------------------
               LLEN=LEN_TRIM(BUFRFILE)
               IF(BUFRFILE(LLEN-4:LLEN) .EQ. 'xx005')THEN      
                 CALL UFBINT(LUNIN,DATES,5,1,IRET,
     &	               'SST1 TMDB AWCK MWCK TPMI')
               ELSEIF(BUFRFILE(LLEN-4:LLEN) .EQ. 'xx012')THEN 
C   GET AIR TEMPERATURE AND SEA SURFACE TEMPERATURE DATA
                 CALL UFBINT(LUNIN,DATES,5,1,IRET,'WATM TMDB')
	       ENDIF	 
                 if(DATES(1).eq.bmiss)then
                    SST=-99999.99
	         else	  
                    SST=DATES(1)-273.15  !! Convert from Kelvin to deg C
                 endif
                 if(DATES(2).eq.bmiss)then
                    ATMP=-99999.99
	         else	  
                    ATMP=DATES(2)   
                 endif
	         IF (SST .EQ. -99999.99)GOTO 710
                 IF(    (NTR_T(I) .GT. 1)
     &	             .and. (dayj .LE. RTIME_T(I,NTR_T(I)) ) )GOTO 710 
	         NTR_T(I)=NTR_T(I)+1
                 RTIME_T(I,NTR_T(I) )=dayj
	         T_OBS(I,NTR_T(I))=sst
710              CONTINUE         

C  ----------------------------------------------------------------------------
C  GET CONDUCTIVITY AND SALINITY
C  ----------------------------------------------------------------------------
                 CALL UFBINT(LUNIN,DATES,5,1,IRET,'SALN WACN')
                 if(DATES(1).eq.bmiss)then
                    SALN=-99999.9
	         else	  
                    SALN=DATES(1)
                 endif
                 if(DATES(2).eq.bmiss)then
                    COND=-99999.9        !     Unit of COND is mS/cm for NOS station
	         else	  
                    COND=DATES(2)   
                 endif
		 IF(SALN .EQ. -99999.9)THEN
		   IF( (SST .NE.-99999.9) .AND. (COND .NE. -99999.9) )THEN
		      SALN=SAL(COND,SST,0.0)
		   ENDIF   
		 ENDIF  
	         IF (SALN .EQ. -99999.9)GOTO 715
                 IF(    (NTR_S(I) .GT. 1)
     &	             .and. (dayj .LE. RTIME_S(I,NTR_S(I)) ) )GOTO 715 
	         NTR_S(I)=NTR_S(I)+1
                 RTIME_S(I,NTR_S(I) )=dayj
	         S_OBS(I,NTR_S(I))=SALN
715              CONTINUE         


C  -----------------------------------------------------------------------------
C  GET TIDAL ELEVATION WITH RESPECT TO CHART AND METEOROLOGICAL RESIDUAL TIDAL 
C  ELEVATION
C  ----------------------------------------------------------------------------
               IF(BUFRFILE(LLEN-4:LLEN) .EQ. 'xx005')THEN      
                 CALL UFBINT(LUNIN,DATES,5,1,IRET,'TERC TIDER')
               ELSEIF(BUFRFILE(LLEN-4:LLEN) .EQ. 'xx012')THEN 
                 CALL UFBINT(LUNIN,DATES,5,1,IRET,'TLLW TIDER')
	       ENDIF	 
                 if(DATES(1).eq.bmiss)then
                    EL=-99999.9
	         else	  
                    EL=DATES(1)  
                 endif
                 if(DATES(2).eq.bmiss)then
                    SWL=-99999.9
	         else	  
                    SWL=DATES(2)   
                 endif
	         IF (EL .EQ. -99999.9)GOTO 777
                 IF(    (NTR(I) .GT. 1)
     &	             .and. (dayj .LE. RTIME(I,NTR(I)) ) )GOTO 777 
	         NTR(I)=NTR(I)+1
                 RTIME(I,NTR(I) )=dayj
!	         T_OBS(I,NTR(I))=sst
	         WL_OBS(I,NTR(I))=EL-DATUM(I)
	         SWL_OBS(I,NTR(I))=SWL
777              CONTINUE         
                 write(15,780)trim(stnbufrid),clon,clat,IYR,IMM,IDD,
     &		 IHH,IMN,EL,ATMP,SST,SALN,COND
             ENDIF 
 	  ENDDO
780       FORMAT(a20,1x,2F10.4,I5,4i3,10F12.4)
C  ------------------------------------------------------------------------------
800       CONTINUE
        ENDDO
        ENDDO
850     CONTINUE
        CALL CLOSBF(LUNIN)
        CLOSE(LUNIN)
860     CONTINUE
      ENDDO
c  -------------------------------------------------------------------------------
C   decoding USGS BURF file if used
C  ------------------------------------------------------------------------------
      IF( .NOT. USGS_L)GOTO 9000
      DO IZ=INT(day_start),INT(day_end)
        jday=IZ+jbase_date
        call GREGORIAN(jday,yearb,monthb,dayb,hourb)
        IYR=INT(yearb)
        IMM=int(monthb+0.001)
        IDD=INT(dayb+0.001)
        IHH=INT(hourb+0.001)
        IMN=INT((hourb-IHH)*60+0.1)
        ISEC=0
        IF(ISEC .EQ. 60)THEN
           ISEC=0
           IMN=IMN+1
        ENDIF
        IF(IMN .EQ. 60)THEN
           IMN=0
           IHH=IHH+1
        ENDIF
        IF(IHH .EQ. 24)THEN
          IHH=0
          IDD=IDD+1
          IF(MOD(IYR,4) .EQ. 0)DAYS_PER_MONTH(2)=29   !!    Leap Year
          IF(IDD .GT. DAYS_PER_MONTH(IMM) )THEN
             IDD=IDD - DAYS_PER_MONTH(IMM)
             IMM=IMM+1
             IF(IMM .GT. 12)THEN
                IMM=IMM-12
                IYR=IYR+1
             ENDIF
          ENDIF
        ENDIF
        WRITE(BUFRFILE,700)'/',IYR,IMM,IDD,'/b001/'  
        BUFRFILE=TRIM(NOSWLDIR)//trim(BUFRFILE)//trim(USGSBUFR) 
        INQUIRE(FILE=trim(BUFRFILE),EXIST=FEXIST)
        IF(.NOT.FEXIST)GOTO 8860
        print *,'BUFR FILE= ',trim(BUFRFILE)
        CMD='cp -p '//trim(BUFRFILE)//'  '//TRIM(USGSBUFR)
        call system(trim(CMD) )
        BUFRFILE=trim(USGSBUFR)
        LUNIN=11
	CLOSE(LUNIN)
        OPEN(LUNIN,file=trim(BUFRFILE),FORM='UNFORMATTED')
        CALL OPENBF(LUNIN,'IN',LUNIN)                                     
C  --------------READ THROUGH THE MESSAGES/SUBSETS IN THE FILE------------------
        DO WHILE(IREADMG(LUNIN,SUBSET,IDATE).EQ.0)                       
        DO WHILE(IREADSB(LUNIN).EQ.0)                                    
c  --------------READ THE INTERNAL DATE AND CHECK FOR REALISM---------------------
          CALL UFBINT(LUNIN,DATES,5,1,IRET,'YEAR MNTH DAYS HOUR MINU')
          IYR = NINT(DATES(1))
          IMM = NINT(DATES(2))
          IDD = NINT(DATES(3))                                           
          IHH = NINT(DATES(4))                                            
          IMN = NINT(DATES(5))                                            
          yearb=IYR
          monthb=IMM
          dayb=IDD
          hourb=REAL(IHH)+REAL(IMN/60.0)   
          dayj=JULIAN(yearb,monthb,dayb,hourb)-jbase_date
!          IF (dayj .LT. day_start)goto 8800  
!          IF (dayj .GT. day_end)goto 8850  
C  --------------READ THE TIDE GAUGE STATION INFO FROM BUFR FILE------------------
c  AJ 09/15/11 Use different CALL routine in order to handle long station IDs 
          CALL READLC(LUNIN, stnbufrid, 'RPID')
!          CALL UFBINT(LUNIN,rpid,1,1,iret,'RPID')
          CALL UFBINT(LUNIN,DATES,5,1,IRET,'CLAT CLON SELV')
          clat=DATES(1)
          clon=DATES(2)
          selv=DATES(3)
          DO I=1,NSTA
             IF(trim(stnbufrid) .EQ. TRIM(NOS_ID(I)) )THEN
C  ----------------------------------------------------------------------------
C   GET AIR TEMPERATURE AND SEA SURFACE TEMPERATURE DATA
                 CALL UFBINT(LUNIN,DATES,5,1,IRET,'SST1 TMDB')
                 if(DATES(1).eq.bmiss)then
                    SST=-99999.9
	         else	  
                    SST=DATES(1)-273.15  !! Convert from Kelvin to deg C
                 endif
                 if(DATES(2).eq.bmiss)then
                    ATMP=-99999.9
	         else	  
                    ATMP=DATES(2)-273.15  !! Convert from Kelvin to deg C
                 endif
	         IF (SST .EQ. -99999.9)GOTO 8710
                 IF( (NTR_T(I) .GT. 1)
     &	             .and. (dayj .LE. RTIME_T(I,NTR_T(I)) ) )GOTO 8710 
	         NTR_T(I)=NTR_T(I)+1
                 RTIME_T(I,NTR_T(I) )=dayj
	         T_OBS(I,NTR_T(I))=sst
8710             CONTINUE         
c  ------------------------------------------------------------------------------
C  GET RIVER STAGE DATA INCLUDING INFO ON USGS SENSOR TYPE 
c   Stage height is in meters
               CALL UFBINT(LUNIN,DATES,5,1,IRET,'RSH29 STRV')
                 if(DATES(1).eq.bmiss)then
                    rsh29=-99999.9
	         else	  
                    rsh29=DATES(1)  
                 endif
                 if(DATES(2).eq.bmiss)then
                    strv=-99999.9
	         else	  
                    strv=DATES(2)  
                 endif


C  -----------------------------------------------------------------------------
C GET RIVER STAGE HEIGHT ABOVE NGVD 1929, STREAM VELOCITY, AND SALINITY
C  ----------------------------------------------------------------------------
                 CALL UFBINT(LUNIN,DATES,5,1,IRET,'RSHM DDRS')
                 if(DATES(1).eq.bmiss)then
                    EL=-99999.9
	         else	  
                    EL=DATES(1)  
                 endif
	         IF (EL .EQ. -99999.9)GOTO 8720
                 IF(    (NTR(I) .GT. 1)
     &	             .and. (dayj .LE. RTIME(I,NTR(I)) ) )GOTO 8720 
	         NTR(I)=NTR(I)+1
                 RTIME(I,NTR(I) )=dayj
	         WL_OBS(I,NTR(I))=EL-DATUM(I)
8720             CONTINUE         
C  ----------------------------------------------------------------------------
C  GET CONDUCTIVITY AND SALINITY
C  convert specific conductance C25,0 of USGS into conductivity. See Sea Bird Electronics, Inc
C    Fresh Water Conductivity Measurements using SBE-19 SEACAT Profiler
C    specific C25,0 [us/cm]=(C/[1+0.02*(Temperature-25)]  where C is in ms/cm 
C  ----------------------------------------------------------------------------
                 CALL UFBINT(LUNIN,DATES,5,1,IRET,'SALN WACN')
                 if(DATES(1).eq.bmiss)then
                    SALN=-99999.9
	         else	  
                    SALN=DATES(1)
                 endif
                 if(DATES(2).eq.bmiss)then
                    COND=-99999.9        !     Unit of COND is uS/cm for USGS station
	         else	  
                    COND=DATES(2)   
		    
                 endif
		 IF(SALN .EQ. -99999.9)THEN
		   IF( (SST .NE.-99999.9) .AND. (COND .NE. -99999.9) )THEN
		      COND=COND*(1.0+0.02*(SST-25.0))  !! convert from uS/cm to mS/cm
		      SALN=SAL(COND,SST,0.0)
		   ENDIF   
		 ENDIF  
	         IF (SALN .EQ. -99999.9)GOTO 8730
                 IF(    (NTR_S(I) .GT. 1)
     &	             .and. (dayj .LE. RTIME_S(I,NTR_S(I)) ) )GOTO 8730 
	         NTR_S(I)=NTR_S(I)+1
                 RTIME_S(I,NTR_S(I) )=dayj
	         S_OBS(I,NTR_S(I))=SALN
8730             CONTINUE         
                 write(15,780)trim(stnbufrid),clon,clat,IYR,IMM,IDD,
     &		 IHH,IMN,EL,ATMP,SST,SALN,COND
             ENDIF 
 	  ENDDO
8800      CONTINUE
        ENDDO
        ENDDO
8850    CONTINUE
        CALL CLOSBF(LUNIN)
        CLOSE(LUNIN)
8860    CONTINUE
      ENDDO
C  ------------------------------------------------------------------------------
9000  CONTINUE      
      CLOSE(15)
      INQUIRE(FILE=TRIM(FOUT),EXIST=FEXIST)
      IF(FEXIST)THEN
         CMD='sort '//TRIM(FOUT)//' > tmp.dat'
         call system(trim(CMD) )
         CMD='uniq tmp.dat > tmp1.dat'
         call system(trim(CMD) )
         CMD='cp tmp1.dat '//TRIM(FOUT)
         call system(trim(CMD) )
  !       call system('rm -f tmp.dat tmp1.dat' )
      ENDIF	 
C  -----------------------------------------------------------------------------
C  End of reading WL BUFR FILES and begin WL QC procedures
C  ----------------------------------------------------------------------------
      IF (ALLOCATED(oned1)) DEALLOCATE(oned1)
      IF (ALLOCATED(oned2)) DEALLOCATE(oned2)
      IF (ALLOCATED(oned3)) DEALLOCATE(oned3)
      IF (ALLOCATED(oned4)) DEALLOCATE(oned4)
      allocate(oned1(NMAX) )
      allocate(oned2(NMAX) )
      allocate(oned3(NMAX) )
      allocate(oned4(NMAX) )
      DO I=1,NSTA
        IF (NTR(I) .GE. 20)THEN
          avg=0.0
	  NTMP=0
	  DO N=1,NTR(I)
	    IF (ABS(WL_OBS(I,N)) .LE. 10.0)THEN
!	    IF (WL_OBS(I,N) .NE. -99999.9)THEN
	       NTMP=NTMP+1
	       AVG=AVG+WL_OBS(I,N)
	       ONED1(NTMP)=WL_OBS(I,N)
	    ENDIF
	  ENDDO 
	  IF(NTMP .GT. 0)AVG=AVG/NTMP
	  SD=0.0    
          IF(NTMP .GT. 20)THEN
  	    DO N=1,NTMP
	      SD=SD+(ONED1(N)-AVG)**2
	    ENDDO 
	    SD=SQRT(SD/(NTMP-1))
	  ELSE
	    SD=1.0
	  ENDIF  
	  BOUND_L=AVG-3.0*SD
	  BOUND_U=AVG+3.0*SD 
          write(*,*)'bound= ',BOUND_L,BOUND_U,AVG,NOS_ID(I)
	  NTMP=0
	  DO N=1,NTR(I)
	    IF ( (WL_OBS(I,N) .GE. BOUND_L) .AND. 
     &	        (WL_OBS(I,N) .LE. BOUND_U)  )THEN
	       NTMP=NTMP+1
	       ONED1(NTMP)=WL_OBS(I,N)
	       ONED2(NTMP)=RTIME(I,N)
	    ENDIF
	  ENDDO
  	  NTR(I)= NTMP
	  DO N=1,NTMP
             WL_OBS(I,N)=ONED1(N) 
	     RTIME(I,N)=ONED2(N)    
	  ENDDO
	ENDIF   
C gradient change checking  Dh/Dt < 0.7 meters/hour, and assume first data is good
!        NREC=NINT( (day_end-day_start)*86400/DELT)+1
        IF (NTR(I) .GE. 20)THEN
	  NTMP=1
          ONED1(NTMP)=WL_OBS(I,1)
          ONED2(NTMP)=RTIME(I,1)
	  DO N=2,NTR(I)  
!	   GD=(WL_OBS(I,N)-WL_OBS(I,N-1))/(RTIME(I,N)-RTIME(I,N-1))/24.
	   GD=(WL_OBS(I,N)-ONED1(NTMP))/(RTIME(I,N)- ONED2(NTMP))/24.
	   IF ( ABS(GD) .LE. 0.7  )THEN
	      NTMP=NTMP+1
	      ONED1(NTMP)=WL_OBS(I,N)
	      ONED2(NTMP)=RTIME(I,N)
	   ENDIF
	  ENDDO
	  DO N=1,NTMP
             WL_OBS(I,N)=ONED1(N) 
	     RTIME(I,N)=ONED2(N)    
	  ENDDO
	  NTR(I)= NTMP
	  WRITE(*,*)I,'NTMP OF WL= ',NTMP,'NTR= ',NTR(I)
!          CALL lineararray(NREC,TIME_M,ONED4,NTMP,ONED2,ONED1)
!	  NTR(I)= NREC
!	  DO N=1,NREC
!             WL_OBS(I,N)=ONED4(N) 
!	     RTIME(I,N)=TIME_M(N)    
!	  ENDDO
	ENDIF  
        BUFFER='NUMBER OF WL AT '//TRIM(NOS_ID(I))
        BUFFER=TRIM(BUFFER)//' FROM '//TRIM(START_TIME)//' TO '
        BUFFER=TRIM(BUFFER)//' '//TRIM(END_TIME)//' = '
       WRITE(ICORMS,*)TRIM(BUFFER),NTR(I)


C  -----------------------------------------------------------------------------
C  begin QC procedures for Temperature
C  ----------------------------------------------------------------------------
        IF (NTR_T(I) .GE. 20)THEN
          avg=0.0
	  NTMP=0
	  DO N=1,NTR_T(I)
	    IF (ABS(T_OBS(I,N)) .LE. 40.0)THEN  !Deg. C
!	    IF (T_OBS(I,N) .NE. -99999.9)THEN
	      NTMP=NTMP+1
	      AVG=AVG+T_OBS(I,N)
	      ONED1(NTMP)=T_OBS(I,N)
	    ENDIF
	  ENDDO 
	  IF(NTMP .GT. 0)AVG=AVG/NTMP
	  IF(NTMP .GT. 20)THEN
	    SD=0.0    
	    DO N=1,NTMP
	      SD=SD+(ONED1(N)-AVG)**2
	    ENDDO 
	    SD=SQRT(SD/(NTMP-1))
	  ELSE
	    SD=1.0
	  ENDIF    
	  BOUND_L=AVG-3.0*SD
	  BOUND_U=AVG+3.0*SD 
          write(*,*)'bound of T = ',BOUND_L,BOUND_U,AVG,NOS_ID(I)
	  NTMP=0
	  DO N=1,NTR_T(I)
	    IF ( (T_OBS(I,N) .GE. BOUND_L) .AND. 
     &	        (T_OBS(I,N) .LE. BOUND_U)  )THEN
	       NTMP=NTMP+1
	       ONED1(NTMP)=T_OBS(I,N)
	       ONED2(NTMP)=RTIME_T(I,N)
	    ENDIF
	  ENDDO
!	  WRITE(*,*)I,'NTMP of T= ',NTMP,'NTR= ',NTR_T(I)
	  NTR_T(I)= NTMP
	  DO N=1,NTMP
             T_OBS(I,N)=ONED1(N) 
	     RTIME_T(I,N)=ONED2(N)    
	  ENDDO 
	ENDIF  
C gradient change checking  DT/Dt < 2.0 deg./hour, and assume first data is good
        IF (NTR_T(I) .GE. 20)THEN
	  NTMP=1
          ONED1(NTMP)=T_OBS(I,1)
          ONED2(NTMP)=RTIME_T(I,1)
	  DO N=2,NTR_T(I)  
!	  GD=(T_OBS(I,N)-T_OBS(I,N-1))/(RTIME_T(I,N)-RTIME_T(I,N-1))/24.
	   GD=(T_OBS(I,N)-ONED1(NTMP))/(RTIME_T(I,N)- ONED2(NTMP))/24.
	   IF ( ABS(GD) .LE. 2.0  )THEN
	      NTMP=NTMP+1
	      ONED1(NTMP)=T_OBS(I,N)
	      ONED2(NTMP)=RTIME_T(I,N)
	   ENDIF
	  ENDDO
	  WRITE(*,*)I,'NTMP of TEMP= ',NTMP,'NTR= ',NTR_T(I)
	  NTR_T(I)= NTMP
	  DO N=1,NTMP
             T_OBS(I,N)=ONED1(N) 
	     RTIME_T(I,N)=ONED2(N)    
	  ENDDO
	ENDIF  
C  -----------------------------------------------------------------------------
C  begin QC procedures for Salinity
C  ----------------------------------------------------------------------------
        IF (NTR_S(I) .GE. 20)THEN
          avg=0.0
	  NTMP=0
	  DO N=1,NTR_S(I)
	    IF (S_OBS(I,N) .NE. -99999.9)THEN
	       NTMP=NTMP+1
	       AVG=AVG+S_OBS(I,N)
	       ONED1(NTMP)=S_OBS(I,N)
	    ENDIF
	  ENDDO 
	  IF(NTMP .GT. 0)AVG=AVG/NTMP
	  IF(NTMP .GT. 20)THEN
	    SD=0.0    
	    DO N=1,NTMP
	      SD=SD+(ONED1(N)-AVG)**2
	    ENDDO 
	    SD=SQRT(SD/(NTMP-1))
	  ELSE
	    SD=2.0
	  ENDIF    
	  BOUND_L=AVG-3.0*SD
	  BOUND_U=AVG+3.0*SD 
          write(*,*)'bound of SALT = ',BOUND_L,BOUND_U,AVG,NOS_ID(I)
 	  NTMP=0
	  DO N=1,NTR_S(I)
	    IF ( (S_OBS(I,N) .GE. BOUND_L) .AND. 
     &	        (S_OBS(I,N) .LE. BOUND_U)  )THEN
	      NTMP=NTMP+1
	      ONED1(NTMP)=S_OBS(I,N)
	      ONED2(NTMP)=RTIME_S(I,N)
	    ENDIF
	  ENDDO
!	  WRITE(*,*)I,'NTMP of SALT= ',NTMP,'NTR_S= ',NTR_S(I)
	  NTR_S(I)= NTMP
	  DO N=1,NTMP
             S_OBS(I,N)=ONED1(N) 
	     RTIME_S(I,N)=ONED2(N)    
	  ENDDO 
	ENDIF  
        IF (NTR_S(I) .GE. 2)THEN
C gradient change checking  DS/Dt < 3.0 deg./hour, and assume first data is good
	  NTMP=1
          ONED1(NTMP)=S_OBS(I,1)
          ONED2(NTMP)=RTIME_S(I,1)
	  DO N=2,NTR_S(I)  
!	  GD=(T_OBS(I,N)-T_OBS(I,N-1))/(RTIME_T(I,N)-RTIME_T(I,N-1))/24.
	   GD=(S_OBS(I,N)-ONED1(NTMP))/(RTIME_S(I,N)- ONED2(NTMP))/24.
	   IF ( ABS(GD) .LE. 3.0  )THEN
	      NTMP=NTMP+1
	      ONED1(NTMP)=S_OBS(I,N)
	      ONED2(NTMP)=RTIME_S(I,N)
	   ENDIF
	  ENDDO
	  WRITE(*,*)I,'NTMP of SALT= ',NTMP,'NTR_S= ',NTR_S(I)
	  NTR_S(I)= NTMP
	  DO N=1,NTMP
             S_OBS(I,N)=ONED1(N) 
	     RTIME_S(I,N)=ONED2(N)    
	  ENDDO
	ENDIF  
      ENDDO	
C  -----------------------------------------------------------------------------
C  End of  QC procedures
C  begin detiding SWL=OBS -PRED
C  ----------------------------------------------------------------------------

      DO I=1,NSTA
         FOUT=TRIM(NOS_ID(I))//'.obs'
	 CLOSE(10)
	 OPEN(10,file=TRIM(FOUT))
	 N0=1
	 DO N=1,NTR(I)
	   IF (WL_OBS(I,N) .LE. -99999.9)GOTO 890
           jday=RTIME(I,N)+jbase_date
           call GREGORIAN(jday,yearb,monthb,dayb,hourb)
           IYR=INT(yearb)
           IMM=int(monthb+0.001)
           IDD=INT(dayb+0.001)
           IHH=INT(hourb+0.001)
           IMN=INT((hourb-IHH)*60+0.1)
           ISEC=0
           IF(ISEC .EQ. 60)THEN
             ISEC=0
             IMN=IMN+1
           ENDIF
           IF(IMN .EQ. 60)THEN
              IMN=0
              IHH=IHH+1
           ENDIF
           IF(IHH .EQ. 24)THEN
              IHH=0
              IDD=IDD+1
              IF(MOD(IYR,4) .EQ. 0)DAYS_PER_MONTH(2)=29   !!    Leap Year
              IF(IDD .GT. DAYS_PER_MONTH(IMM) )THEN
                 IDD=IDD - DAYS_PER_MONTH(IMM)
                 IMM=IMM+1
                 IF(IMM .GT. 12)THEN
                   IMM=IMM-12
                   IYR=IYR+1
                 ENDIF
              ENDIF
           ENDIF
	   DO N1=N0,NREC
	     IF( (RTIME(I,N) .GE. TIME_PRD(N1))
     &	        .AND. (RTIME(I,N) .LT. TIME_PRD(N1+1) ) )THEN
                X1=TIME_PRD(N1)
                X2=TIME_PRD(N1+1)
		Y1=WL_PRD(I,N1)
		Y2=WL_PRD(I,N1+1)
		CALL linear(X1,Y1,X2,Y2,RTIME(I,N),Y)
	        SWL_OBS(I,N)=WL_OBS(I,N)-Y
!	        SWL_OBS(I,N)=WL_OBS(I,N)-WL_PRD(I,N1)
	        N0=N1
	        GOTO 870
	     ENDIF



!	     IF(abs(RTIME(I,N)-TIME_PRD(N1)) .LE. 0.01 )THEN
!	        SWL_OBS(I,N)=WL_OBS(I,N)-WL_PRD(I,N1)
!	        N0=N1
!	        GOTO 870
!	     ENDIF
	   ENDDO
	   IF(N1 .GT. NREC)SWL_OBS(I,N)=-9999.0
870        CONTINUE	     		
	   
	   WRITE(10,900)RTIME(I,N),IYR,IMM,IDD,IHH
     &	   ,IMN,WL_PRD(I,N1),WL_OBS(I,N),SWL_OBS(I,N),T_OBS(I,N)
 890       CONTINUE    
         ENDDO
        write(*,*)'STA= ',trim(NOS_ID(I)),' Number of WL obs= ',NTR(I)
      ENDDO	
900   FORMAT(f10.4,1x,I5,4i3,4F12.4)




!-----------------------------------------------------------------------
C   if there is no WL obs at primary station, then looking for its backup station
!-----------------------------------------------------------------------
      IF (ALLOCATED(oned1)) DEALLOCATE(oned1)
      IF (ALLOCATED(oned2)) DEALLOCATE(oned2)
      IF (ALLOCATED(oned3)) DEALLOCATE(oned3)
      IF (ALLOCATED(oned4)) DEALLOCATE(oned4)
      IF (ALLOCATED(AVGERR)) DEALLOCATE(AVGERR)
      IF (ALLOCATED(AVGERR_T)) DEALLOCATE(AVGERR_T)
      IF (ALLOCATED(AVGERR_S)) DEALLOCATE(AVGERR_S)
      allocate(oned1(NMAX) )
      allocate(oned2(NMAX) )
      allocate(oned3(NMAX) )
      allocate(oned4(NMAX) )
      allocate(AVGERR(NSTA) )
      allocate(AVGERR_T(NSTA) )
      allocate(AVGERR_S(NSTA) )
      DO I=1,NSTA
        IF( WL_FLAG(I) .EQ. 0 )THEN
          BUFFER='NUMBER OF WL AT '//TRIM(NOS_ID(I))
          BUFFER=TRIM(BUFFER)//' FROM '//TRIM(START_TIME)//' TO '
          BUFFER=TRIM(BUFFER)//' '//TRIM(END_TIME)//' = '
          WRITE(ICORMS,*)TRIM(BUFFER),NTR(I)
          IF( NTR(I) .LE. 20 )THEN  !!  2 can be changed depending upon how many data are required to be available
	     IBKP=BACKUP_SID(I)
	     IF(IBKP .GT. 0)THEN
               IF (NTR(IBKP) .GT. 20)THEN
	          write(*,*)'using backup ',IBKP, 'for I= ',I
	          NTR(I)=NTR(IBKP)
  	          DO N=1,NTR(I)
	            RTIME(I,N)=RTIME(IBKP,N)
	            WL_OBS(I,N)=AS(IBKP)*WL_OBS(IBKP,N)
	            SWL_OBS(I,N)=AS(IBKP)*SWL_OBS(IBKP,N)
	          ENDDO  
               ELSE
	          IBKP1=BACKUP_SID(IBKP)
	          IF( (IBKP1 .GT. 0) .AND.(NTR(IBKP1) .GT. 20) )THEN
	              write(*,*)'use second backup ',IBKP, 'for I= ',I
	              NTR(I)=NTR(IBKP1)
  	              DO N=1,NTR(I)
	                 RTIME(I,N)=RTIME(IBKP1,N)
	                 WL_OBS(I,N)=AS(IBKP1)*WL_OBS(IBKP1,N)
	                 SWL_OBS(I,N)=AS(IBKP1)*SWL_OBS(IBKP1,N)
	              ENDDO
!	          ELSE
!	              NTR(I)=NTMAX_WL
!  	              DO N=1,NTR(I)
!	                 RTIME(I,N)=zeta_time(N)
!	                 WL_OBS(I,N)=0.0
!	                 SWL_OBS(I,N)=-99999.9
!	              ENDDO
	          ENDIF     	   
	       ENDIF
!	     ELSE
!	         write(*,*)'no backup SWL=0.0 for I= ',I
!	          NTR(I)=NTMAX_WL
!  	          DO N=1,NTR(I)
!	             RTIME(I,N)=zeta_time(N)
!	             WL_OBS(I,N)=0.0
!	             SWL_OBS(I,N)=-99999.9
!	          ENDDO
	     ENDIF     	   
	       
	  ENDIF  
!-----------------------------------------------------------------------
C use 6-hour ramping up	
C interpolate into same time intreval as zeta_time  
C  filling missing values using linear interplolation 
!-----------------------------------------------------------------------
          IF (NTR(I) .GT. 20)THEN
             FOUT=TRIM(NOS_ID(I))//'.swl'
	     CLOSE(10)
	     OPEN(10,file=TRIM(FOUT))
             DO N=1,NTR(I)
	       WRITE(10,35)RTIME(I,N),SWL_OBS(I,N)
             ENDDO
	     CLOSE(10)
!-----------------------------------------------------------------------
C  separate bias as mean error + time varying difference
!-----------------------------------------------------------------------
             N0=0
             DO N1=1,NTR(I)
               IF (SWL_OBS(I,N1).GT. -10.0 )THEN
                 N0=N0+1
                 ONED1(N0)= RTIME(I,N1)
                 ONED2(N0)= SWL_OBS(I,N1)
               ENDIF     
             ENDDO  
             NTR(I)=N0
             DO N1=1,NTR(I)
               RTIME(I,N1)=ONED1(N1)
               SWL_OBS(I,N1)=ONED2(N1)
             ENDDO     
             TIME1=RTIME(I,1)
             TIME2=RTIME(I,NTR(I))

             N0=0
             IF (TIME1 .GT. zeta_time(1) )THEN
	       N0=N0+1
	       ONED1(N0)= zeta_time(1)
	       ONED2(N0)= SWL_OBS(I,1)
	     ENDIF
	     DO N1=1,NTR(I)
	       N0=N0+1
	       ONED1(N0)= RTIME(I,N1)
	       ONED2(N0)= SWL_OBS(I,N1)
	     ENDDO  
	     Print *,'N0=',N0,'NTR(I)=',NTR(I)
	     DO N1=2,NTMAX_WL
  	        IF( (zeta_time(N1) .GE. TIME2) )THEN
	          N0=N0+1
	          ONED1(N0)= zeta_time(N1)
	          ONED2(N0)= SWL_OBS(I,NTR(I))
                  goto 903
	        ENDIF
	     ENDDO
903          CONTINUE
	     Print *,'N0=',N0,'NTR(I)=',NTR(I)
	     NTR(I)=N0
             DO N1=1,NTR(I)
               RTIME(I,N1)=ONED1(N1)
	       SWL_OBS(I,N1)=ONED2(N1)
             ENDDO     
             TIME1=RTIME(I,1)
	     TIME2=RTIME(I,NTR(I))
             WRITE(*,*)'TIME1=',TIME1,'TIME2=',TIME2,zeta_time(1),
     &	       zeta_time(NTMAX_WL),'NTR=',NTR(I),NTMAX_WL
             N0=0
	     DO N1=1,NTMAX_WL
  	        IF( (zeta_time(N1) .GE. TIME1) .AND. 
     &	            (zeta_time(N1) .LE. TIME2) )THEN
	             N0=N0+1
	             ONED3(N0)=zeta_time(N1)
	        ENDIF
	     ENDDO
	     IF(N0 .LT. 1)THEN
	       WRITE(*,*)'real time water level data is insufficient'
	       WRITE(*,*)'Please wait for 20 min and then check '
	       WRITE(*,*)'file size of b001/xx012 and b001/xx009'
	       WRITE(*,*)'rerun prep job' 
	       WRITE(*,*)'TIME1=',TIME1,'TIME2=',TIME2,zeta_time(1)
	       STOP
	     ENDIF  
	     NTMP=0 
	     DO N=1,NTR(I)
	        IF(ABS(SWL_OBS(I,N)) .LE. 3.0)THEN
		  NTMP=NTMP+1
	          ONED1(NTMP)=RTIME(I,N)
	          ONED2(NTMP)=SWL_OBS(I,N)
		ENDIF  
	     ENDDO  
	     IF(NTMP .GT. 2)THEN
	       CALL lineararray(N0,ONED3,oned4,NTMP,ONED1,ONED2)
               DO N=1,N0
                  write(86,*)ONED3(N),ONED4(N)
               ENDDO
	     ELSE
	       DO N=1,N0
	          oned4(N)=0.0
	       ENDDO
	     ENDIF  	    
!	     CALL lineararray(N0,ONED3,oned4,NTR(I),ONED1,ONED2)
  	     AVG=0.0
             DO N=1,N0
                DO N1=1,NTMAX_WL
	          IF(ONED3(N) .EQ. zeta_time(N1) )THEN
	            ONED1(N)=ONED4(N)-WLOBC(GRIDID_STA(I),N1)! avg=SWL-ETSS at the corresponding grid
                    write(89,*)ONED1(N),ONED4(N),WLOBC(GRIDID_STA(I),N1)
                    AVG=AVG+ONED1(N)
		    GOTO 905
	          ENDIF	  
                ENDDO
905             CONTINUE
             ENDDO
             write(88,*)'avg=',avg/n0,'n0=',n0
	     IF(N0 .GT. 0)AVG=AVG/N0
	     AVGERR(I)=AVG
             DO N=1,N0
	       ONED4(N)=ONED1(N)-AVG   !! err = avg + err'
	     ENDDO  
	     IF(ONED3(N0) .LT. day_end)THEN
               N0=N0+1
	       ONED3(N0)=ONED3(N0-1)+6.0/24.0  ! use 6-hour ramping up from last err' to zero	
	       ONED4(N0)=0.0
	     ENDIF
	     CALL lineararray(NTMAX_WL,zeta_time,oned1,N0,ONED3,ONED4)
               write(88,*)'N0=',N0
               DO N=1,N0
                  write(88,*)ONED3(N),ONED4(N)
               ENDDO
               WRITE(87,*)'NTMAX_WL=',NTMAX_WL
               DO N=1,NTMAX_WL
                  write(87,*)zeta_time(N),ONED1(N)
               ENDDO

             DO N=1,NTMAX_WL
	       SWL_OBS(I,N)=oned1(N)
	       IF(zeta_time(N) .GT.ONED3(N0) )SWL_OBS(I,N)=0.0 
             ENDDO
	  ELSE
	     AVGERR(I)=0.0
             DO N=1,NTMAX_WL
	        SWL_OBS(I,N)=0.0
             ENDDO
          ENDIF	
	ELSE
	  AVGERR(I)=0.0
          DO N=1,NTMAX_WL
	     SWL_OBS(I,N)=0.0
          ENDDO
        ENDIF	 
C-----------------------------------------------------------------------
C     process water temperature for stations TS_FLAG > 0
C-----------------------------------------------------------------------
        IF( TS_FLAG(I) .EQ. 0 )THEN !! no correction is needed
	    AVGERR_T(I)=0.0
            DO N=1,NTMAX
	       T_OBS(I,N)=0.0
            ENDDO

        ELSEIF( TS_FLAG(I) .EQ. 1 )THEN !! need real time T & S observations
          IF( NTR_T(I) .GT. 5 )THEN  !!  2 can be changed depending upon how many data are required to be available
            BUFFER='NUMBER OF TEMP AT '//TRIM(NOS_ID(I))
            BUFFER=TRIM(BUFFER)//' FROM '//TRIM(START_TIME)//' TO '
            BUFFER=TRIM(BUFFER)//' '//TRIM(END_TIME)//' = '
            WRITE(ICORMS,*)TRIM(BUFFER),NTR_T(I)
          ELSE  
	    IBKP=BACKUP_SID(I)
	    IF(IBKP .GT. 0)THEN
              IF (NTR_T(IBKP) .GT. 5)THEN
	         BUFFER='USING BACKUP STATION OF '//TRIM(NOS_ID(IBKP))
                 WRITE(ICORMS,*)TRIM(BUFFER)
	         write(*,*)'using first backup ',IBKP,'for T at I= ',I
	         NTR_T(I)=NTR_T(IBKP)
  	         DO N=1,NTR_T(I)
	           RTIME_T(I,N)=RTIME_T(IBKP,N)
!	           T_OBS(I,N)=AS(IBKP)*T_OBS(IBKP,N)
	           T_OBS(I,N)=T_OBS(IBKP,N)
	         ENDDO  
              ELSE
	         IBKP1=BACKUP_SID(IBKP)
	         IF( (IBKP1 .GT. 0) .AND.(NTR_T(IBKP1) .GT. 5) )THEN
	           BUFFER='USING BACKUP STATION OF '//TRIM(NOS_ID(IBKP1))
                   WRITE(ICORMS,*)TRIM(BUFFER)
	            write(*,*)'use second backup ',IBKP, 'for I= ',I
	            NTR_T(I)=NTR_T(IBKP1)
  	            DO N=1,NTR_T(I)
	               RTIME_T(I,N)=RTIME_T(IBKP1,N)
	               T_OBS(I,N)=T_OBS(IBKP1,N)
	            ENDDO
		 ELSE   
		    write(*,*)'no observation is at second backup'
	            write(*,*)'use climatology for ',trim(NOS_ID(I))
	           BUFFER='NO OBSERVATIONS, USE CLIMATOLOGIC DATASET'
                   WRITE(ICORMS,*)TRIM(BUFFER)
	            NTR_T(I)=NT_CLIM
  	            DO N=1,NTR_T(I)
	               RTIME_T(I,N)=TIME_CLIM(N)
	               T_OBS(I,N)=T_CLIM(I,N,1)    !! use SST only
	            ENDDO
	         ENDIF     	   
	      ENDIF
	    ELSE     !! use climatology 
	       write(*,*)'use climatology for I= ',I,trim(NOS_ID(I))
	       NTR_T(I)=NT_CLIM
  	       DO N=1,NTR_T(I)
	           RTIME_T(I,N)=TIME_CLIM(N)
	           T_OBS(I,N)=T_CLIM(I,N,1)    !! use SST only
	       ENDDO
	    ENDIF     	   
	  ENDIF
        ELSEIF( TS_FLAG(I) .EQ. 2 )THEN !! correction using climatological dataset
               BUFFER='USE CLIMATOLOGIC DATASET'
               WRITE(ICORMS,*)TRIM(BUFFER)
	       NTR_T(I)=NT_CLIM
  	       DO N=1,NTR_T(I)
	           RTIME_T(I,N)=TIME_CLIM(N)
	           T_OBS(I,N)=T_CLIM(I,N,1)    !! use SST only
	       ENDDO
	ENDIF

        IF( TS_FLAG(I) .GT. 0 )THEN     !! correction is needed
             FOUT=TRIM(NOS_ID(I))//'.temp'
	     CLOSE(10)
	     OPEN(10,file=TRIM(FOUT))
             DO N=1,NTR_T(I)
	       WRITE(10,35)RTIME_T(I,N),T_OBS(I,N)
             ENDDO
	     CLOSE(10)
!-----------------------------------------------------------------------
C  separate bias as mean error + time varying difference
!-----------------------------------------------------------------------
             N0=0
             DO N1=1,NTR_T(I)
               IF (T_OBS(I,N1).GT. -10.0 )THEN
                 N0=N0+1
                 ONED1(N0)= RTIME_T(I,N1)
                 ONED2(N0)= T_OBS(I,N1)
               ENDIF     
             ENDDO  
             NTR_T(I)=N0
             DO N1=1,NTR_T(I)
               RTIME_T(I,N1)=ONED1(N1)
               T_OBS(I,N1)=ONED2(N1)
             ENDDO     

             TIME1=RTIME_T(I,1)
	     TIME2=RTIME_T(I,NTR_T(I))
             N0=0
             IF (TIME1 .GT. TS_time(1) )THEN
	       N0=N0+1
	       ONED1(N0)= TS_time(1)
	       ONED2(N0)= T_OBS(I,1)
	     ENDIF
	     DO N1=1,NTR_T(I)
	       N0=N0+1
	       ONED1(N0)= RTIME_T(I,N1)
	       ONED2(N0)= T_OBS(I,N1)
	     ENDDO  
	     Print *,'N0=',N0,'NTR_T(I)=',NTR_T(I)
	     DO N1=2,NTMAX
  	        IF( (TS_time(N1) .GE. TIME2) )THEN
	          N0=N0+1
	          ONED1(N0)= TS_time(N1)
	          ONED2(N0)= T_OBS(I,NTR_T(I))
                  goto 923
	        ENDIF
	     ENDDO
923          CONTINUE
	     Print *,'N0=',N0,'NTR_T(I)=',NTR_T(I)
	     NTR_T(I)=N0
             DO N1=1,NTR_T(I)
               RTIME_T(I,N1)=ONED1(N1)
	       T_OBS(I,N1)=ONED2(N1)
             ENDDO     
             TIME1=RTIME_T(I,1)
	     TIME2=RTIME_T(I,NTR_T(I))
             WRITE(*,*)'TIME1=',TIME1,'TIME2=',TIME2,TS_time(1),
     &	       TS_time(NTMAX),'NTR_T=',NTR_T(I),NTMAX
             N0=0
	     DO N1=1,NTMAX
  	        IF( (TS_time(N1) .GE. TIME1) .AND. 
     &	            (TS_time(N1) .LE. TIME2) )THEN
	             N0=N0+1
	             ONED3(N0)=TS_time(N1)
	        ENDIF
	     ENDDO
	     IF(N0 .LT. 1)THEN
	       WRITE(*,*)'real time temperature data is insufficient'
	       WRITE(*,*)'Please wait for 20 minutes and then check '
	       WRITE(*,*)'file size of b001/xx012 and b001/xx009'
	       WRITE(*,*)'rerun prep job' 
	       write(*,*)'I=',I,TIME1,TIME2,TS_time(1),TS_time(NTMAX)
	       STOP
	     ENDIF  
	     DO N=1,NTR_T(I)
	          ONED1(N)=RTIME_T(I,N)
	          ONED2(N)=T_OBS(I,N)
	     ENDDO  
	     CALL lineararray(N0,ONED3,oned4,NTR_T(I),ONED1,ONED2)
  	     AVG=0.0
             DO N=1,N0
                DO N1=1,NTMAX
	          IF(ONED3(N) .EQ. TS_time(N1) )THEN
	            ONED1(N)=ONED4(N)-TEMPOBC_M(GRIDID_STA(I),KBm,N1) ! avg=Tobs - NCOM at surface at the corresponding grid
                    AVG=AVG+ONED1(N)                                ! For Selfe, K=1 for bottom, K=KBm for surface   
		    GOTO 925
	          ENDIF	  
                ENDDO
925             CONTINUE
             ENDDO
	     IF(N0 .GT. 0)AVG=AVG/N0
	     AVGERR_T(I)=AVG
	     WRITE(*,*)'mean err of SST=',I,AVGERR_T(I)
             DO N=1,N0
	       ONED4(N)=ONED1(N)-AVG   !! err = avg + err'
	     ENDDO  
	     IF(ONED3(N0) .LT. day_end)THEN
               N0=N0+1
	       ONED3(N0)=ONED3(N0-1)+6.0/24.0  ! use 6-hour ramping up from last err' to zero	
	       ONED4(N0)=0.0
	     ENDIF
	     CALL lineararray(NTMAX,TS_time,oned1,N0,ONED3,ONED4)
             DO N=1,NTMAX
	       T_OBS(I,N)=oned1(N)
	       IF(TS_time(N) .GT.ONED3(N0) )T_OBS(I,N)=0.0 
             ENDDO
	ENDIF  

C-----------------------------------------------------------------------
C     process salinity for stations TS_FLAG > 0
C-----------------------------------------------------------------------
        IF( TS_FLAG(I) .EQ. 0 )THEN !! no correction is needed
	    AVGERR_S(I)=0.0
            DO N=1,NTMAX
	       S_OBS(I,N)=0.0
            ENDDO

        ELSEIF( TS_FLAG(I) .EQ. 1 )THEN !! need real time T & S observations
          IF( NTR_S(I) .GT. 5 )THEN  !!  2 can be changed depending upon how many data are required to be available
            BUFFER='NUMBER OF SALT AT '//TRIM(NOS_ID(I))
            BUFFER=TRIM(BUFFER)//' FROM '//TRIM(START_TIME)//' TO '
            BUFFER=TRIM(BUFFER)//' '//TRIM(END_TIME)//' = '
            WRITE(ICORMS,*)TRIM(BUFFER),NTR_S(I)

          ELSE 
	    IBKP=BACKUP_SID(I)
	    IF(IBKP .GT. 0)THEN
              IF (NTR_S(IBKP) .GT. 5)THEN
	         BUFFER='USING BACKUP STATION OF '//TRIM(NOS_ID(IBKP))
                 WRITE(ICORMS,*)TRIM(BUFFER)
	         write(*,*)'using backup ',IBKP, 'for SAL at I= ',I
	         NTR_S(I)=NTR_S(IBKP)
  	         DO N=1,NTR_S(I)
	           RTIME_S(I,N)=RTIME_S(IBKP,N)
	           S_OBS(I,N)=S_OBS(IBKP,N)
	         ENDDO  
              ELSE
	         IBKP1=BACKUP_SID(IBKP)
	         IF( (IBKP1 .GT. 0) .AND.(NTR_S(IBKP1) .GT. 5) )THEN
	         BUFFER='USING BACKUP STATION OF '//TRIM(NOS_ID(IBKP1))
                 WRITE(ICORMS,*)TRIM(BUFFER)
	            write(*,*)'use second backup ',IBKP, 'for I= ',I
	            NTR_S(I)=NTR_S(IBKP1)
  	            DO N=1,NTR_S(I)
	               RTIME_S(I,N)=RTIME_S(IBKP1,N)
	               S_OBS(I,N)=S_OBS(IBKP1,N)
	            ENDDO
		 ELSE 
	           write(*,*)'use sal climatology for ',trim(NOS_ID(I))
	           NTR_S(I)=NT_CLIM
  	           DO N=1,NTR_S(I)
	              RTIME_S(I,N)=TIME_CLIM(N)
	              S_OBS(I,N)=S_CLIM(I,N,1)    !! use SST only
	           ENDDO
	         ENDIF     	   
	      ENDIF
	    ELSE     !! use climatology 
	      write(*,*)'use sal climatology for I= ',I,trim(NOS_ID(I))
	      NTR_S(I)=NT_CLIM
  	      DO N=1,NTR_S(I)
	          RTIME_S(I,N)=TIME_CLIM(N)
	          S_OBS(I,N)=S_CLIM(I,N,1)    !! use SST only
	      ENDDO
	    ENDIF     	   
	  ENDIF
        ELSEIF( TS_FLAG(I) .EQ. 2 )THEN !! correction using climatological dataset
	  NTR_S(I)=NT_CLIM
  	  DO N=1,NTR_S(I)
	      RTIME_S(I,N)=TIME_CLIM(N)
	      S_OBS(I,N)=S_CLIM(I,N,1)    !! use SST only
	  ENDDO

	ENDIF  
	    
        IF( TS_FLAG(I) .GT. 0 )THEN !!  correction is needed
             FOUT=TRIM(NOS_ID(I))//'.salt'
	     CLOSE(10)
	     OPEN(10,file=TRIM(FOUT))
             DO N=1,NTR_S(I)
	       WRITE(10,35)RTIME_S(I,N),S_OBS(I,N)
             ENDDO
	     CLOSE(10)
!-----------------------------------------------------------------------
C  separate bias as mean error + time varying difference
!-----------------------------------------------------------------------
             N0=0
             DO N1=1,NTR_S(I)
               IF (S_OBS(I,N1).GT. 0.0 )THEN
                 N0=N0+1
                 ONED1(N0)= RTIME_S(I,N1)
                 ONED2(N0)= S_OBS(I,N1)
               ENDIF     
             ENDDO  
             NTR_S(I)=N0
             DO N1=1,NTR_S(I)
               RTIME_S(I,N1)=ONED1(N1)
               S_OBS(I,N1)=ONED2(N1)
             ENDDO     

             TIME1=RTIME_S(I,1)
	     TIME2=RTIME_S(I,NTR_S(I))
             N0=0
             IF (TIME1 .GT. TS_time(1) )THEN
	       N0=N0+1
	       ONED1(N0)= TS_time(1)
	       ONED2(N0)= S_OBS(I,1)
	     ENDIF
	     DO N1=1,NTR_S(I)
	       N0=N0+1
	       ONED1(N0)= RTIME_S(I,N1)
	       ONED2(N0)= S_OBS(I,N1)
	     ENDDO  
	     Print *,'N0=',N0,'NTR_S(I)=',NTR_S(I)
	     DO N1=2,NTMAX
  	        IF( (TS_time(N1) .GE. TIME2) )THEN
	          N0=N0+1
	          ONED1(N0)= TS_time(N1)
	          ONED2(N0)= S_OBS(I,NTR_S(I))
                  goto 8923
	        ENDIF
	     ENDDO
8923         CONTINUE
	     Print *,'N0=',N0,'NTR_S(I)=',NTR_S(I)
	     NTR_S(I)=N0
             DO N1=1,NTR_S(I)
               RTIME_S(I,N1)=ONED1(N1)
	       S_OBS(I,N1)=ONED2(N1)
             ENDDO     
             TIME1=RTIME_S(I,1)
	     TIME2=RTIME_S(I,NTR_S(I))
             WRITE(*,*)'TIME1=',TIME1,'TIME2=',TIME2,TS_time(1),
     &	       TS_time(NTMAX),'NTR_S=',NTR_S(I),NTMAX
             N0=0
	     DO N1=1,NTMAX
  	        IF( (TS_time(N1) .GE. TIME1) .AND. 
     &	            (TS_time(N1) .LE. TIME2) )THEN
	             N0=N0+1
	             ONED3(N0)=TS_time(N1)
	        ENDIF
	     ENDDO
	     IF(N0 .LT. 1)THEN
	       WRITE(*,*)'real time salinity data is insufficient'
	       WRITE(*,*)'Please wait for 20 minutes and then check'
	       WRITE(*,*)'file size of b001/xx012 and b001/xx009'
	       WRITE(*,*)'rerun prep job' 
	       write(*,*)'I=',I,TIME1,TIME2,TS_time(1),TS_time(NTMAX)
	       STOP
	     ENDIF  
	     DO N=1,NTR_S(I)
	          ONED1(N)=RTIME_S(I,N)
	          ONED2(N)=S_OBS(I,N)
	     ENDDO  
	     CALL lineararray(N0,ONED3,oned4,NTR_S(I),ONED1,ONED2)
  	     AVG=0.0
             DO N=1,N0
                DO N1=1,NTMAX
	          IF(ONED3(N) .EQ. TS_time(N1) )THEN
	            ONED1(N)=ONED4(N)-SALTOBC_M(GRIDID_STA(I),KBm,N1) ! avg=Sobs - NCOM at the corresponding grid
                    AVG=AVG+ONED1(N)                                ! For Selfe, K=1 for bottom, K=KBm for surface
		    GOTO 8925
	          ENDIF	  
                ENDDO
8925            CONTINUE
             ENDDO
	     IF(N0 .GT. 0)AVG=AVG/N0
	     AVGERR_S(I)=AVG
	     WRITE(*,*)'mean err of SSS=',I,AVGERR_S(I)
             DO N=1,N0
	       ONED4(N)=ONED1(N)-AVG   !! err = avg + err'
	     ENDDO  
	     IF(ONED3(N0) .LT. day_end)THEN
               N0=N0+1
	       ONED3(N0)=ONED3(N0-1)+6.0/24.0  ! use 6-hour ramping up from last err' to zero	
	       ONED4(N0)=0.0
	     ENDIF
	     CALL lineararray(NTMAX,TS_time,oned1,N0,ONED3,ONED4)
             DO N=1,NTMAX
	       S_OBS(I,N)=oned1(N)
	       IF(TS_time(N) .GT.ONED3(N0) )S_OBS(I,N)=0.0 
             ENDDO
	ENDIF  
      ENDDO
!-----------------------------------------------------------------------
C  correcting OBC by the difference between obs - ETSS/NCOM
!-----------------------------------------------------------------------
      DO I=1,NOBC
	 ID1=WL_SID_1(I)
	 ID2=WL_SID_2(I)
	 sc1=WL_S_1(I)
	 sc2=WL_S_2(I)
         IF (WL_STA(I) .EQ. 1)THEN
	   IF(ID1 .GT. 0)THEN
	    DO N=1,NTMAX_WL
	      WLOBC(I,N)=WLOBC(I,N)+sc1*(AVGERR(ID1)+SWL_OBS(ID1,N))
	    ENDDO
	   ENDIF 
         ELSEIF (WL_STA(I) .EQ. 2)THEN
	   IF(ID1 .GT. 0 .AND. ID2 .GT. 0 )THEN
	    DO N=1,NTMAX_WL
	      WLOBC(I,N)=WLOBC(I,N)+sc1*(AVGERR(ID1)+SWL_OBS(ID1,N))
     &       	                   +sc2*(AVGERR(ID2)+SWL_OBS(ID2,N))
	    ENDDO
	   ENDIF 
	 ENDIF
	 
	 ID1=TS_SID_1(I)
	 ID2=TS_SID_2(I)
	 sc1=TS_S_1(I)
	 sc2=TS_S_2(I)
!         IF (TS_STA(I) .EQ. 1)THEN
!	   IF(ID1 .GT. 0)THEN
!	     DO N=1,NTMAX
!	     DO K=1,KBm
!	        TEMPOBC_M(I,K,N)=TEMPOBC_M(I,K,N)
!     &	        +sc1*(AVGERR_T(ID1)+T_OBS(ID1,N))
!
!	        SALTOBC_M(I,K,N)=SALTOBC_M(I,K,N)
!     &	        +sc1*(AVGERR_S(ID1)+S_OBS(ID1,N))
!	     ENDDO
!	     ENDDO
!	   ENDIF 
!         ELSEIF (TS_STA(I) .EQ. 2)THEN
!	   IF(ID1 .GT. 0 .AND. ID2 .GT. 0 )THEN
!	     DO N=1,NTMAX
!	     DO K=1,KBm
!	        TEMPOBC_M(I,K,N)=TEMPOBC_M(I,K,N)
!     &	        +sc1*(AVGERR_T(ID1)+T_OBS(ID1,N))
!     &          +sc2*(AVGERR_T(ID2)+T_OBS(ID2,N))

!	        SALTOBC_M(I,K,N)=SALTOBC_M(I,K,N)
!     &	        +sc1*(AVGERR_S(ID1)+S_OBS(ID1,N))
!     &          +sc2*(AVGERR_S(ID2)+S_OBS(ID2,N))
!	     ENDDO
!	     ENDDO
!	   ENDIF 
	 !ENDIF
      ENDDO 	 
     	    
C -------------------------------------------------------------------
C   Print OBC for evaluation
      OPEN(33,file='WL_OBC_ajdusted.dat')
      DO N=1,NTMAX_WL
       write(33,35)ZETA_TIME(N),(WLOBC(I,N),I=1,NOBC,10),WLOBC(NOBC,N)
      ENDDO
      CLOSE(33)
!      OPEN(33,file='TEMP_OBC_ajdusted.dat')
!      DO N=1,NTMAX
!        DO I=1,NOBC
!           write(33,35)TS_TIME(N),(TEMPOBC_M(I,K,N),K=1,KBm)
!        enddo
!      ENDDO
!      CLOSE(33)
!      OPEN(33,file='SALT_OBC_ajdusted.dat')
!      DO N=1,NTMAX
!        DO I=1,NOBC
!          write(33,35)TS_TIME(N),(SALTOBC_M(I,K,N),K=1,KBm)
!        ENDDO
!      ENDDO
!      CLOSE(33)
	    
!-----------------------------------------------------------------------
C   assign to corresponding open boundary variables, and then write into a netcdf file
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
C  Set global attributes string of the NetCDF
!-----------------------------------------------------------------------
1     format(I2.2,a1,I2.2,2x,I2.2,a1,i2.2,a1,I4)
      CALL DATE_AND_TIME(BIG_BEN(1),BIG_BEN(2),BIG_BEN(3),DATE_TIME)
      WRITE(CURRENT_TIME,1)DATE_TIME(5),':',DATE_TIME(6),
     &DATE_TIME(2),'/',DATE_TIME(3),'/',DATE_TIME(1)
      globalstr(1)= trim(OCEAN_MODEL)
     & //' lateral open boundary forcing netCDF file'
      globalstr(2)= trim(OFS)//' lateral open boundary netCDF file'
      globalstr(3)= 'Water level OBC from '//trim(DBASE_WL)
     & //' data source'
      globalstr(4)= 'T and S OBC from '//trim(DBASE_TS)
     & //' data source'
      globalstr(5)= trim(DBASE_TS)//' data file: '//trim(FILE_TS)
      if(IGRD  .EQ. 0)then
        globalstr(6)= 'On native '//trim(DBASE_TS)
     &               //' grid, No spatial interpolation'
      elseif(IGRD  .EQ. 1)then
        globalstr(6)= 'On '//trim(OCEAN_MODEL)
     &               //' grid, using remesh spatial interpolation'
      elseif(IGRD  .EQ. 2)then
        globalstr(6)= 'On '//trim(OCEAN_MODEL)
     &               //' grid, using bicubic spatial interpolation'
      elseif(IGRD  .EQ. 3)then
        globalstr(6)= 'On '//trim(OCEAN_MODEL)
     &               //' grid, using bilinear spatial interpolation'
      elseif(IGRD  .EQ. 4)then
        globalstr(6)= 'On '//trim(OCEAN_MODEL)
     &      //' grid, using nature neighbors spatial interpolation'
      endif
      globalstr(7)= 'GRID file: '//trim(GRIDFILE)
      
      globalstr(8)= 'Created at time '//trim(CURRENT_TIME)
      globalstr(9)='Created by Aijun Zhang, OD/CO-OPS/NOS/NOAA'
	  
!-----------------------------------------------------------------------
!   reading in tide constituents at nodes along open boundary and conduct tidal prediction
!-----------------------------------------------------------------------
      INQUIRE(FILE=trim(HC_FILE_OFS),EXIST=FEXIST)
      IF(.NOT. FEXIST)THEN
          WRITE(*,*)'Harmonic Constant NetCDF file is not found'
	  WRITE(*,*)'Provide correct Harmonic Constant File Name:'
	  WRITE(*,*)trim(HC_FILE_OFS)
	  WRITE(*,*)TRIM(OFS)//' stop here'
          WRITE(ICORMS,'(a)')'CRITICAL FAILURE IN CREATING OBC' 
          STOP
      ELSE  
        DO I=1,4
          DIMS(I)=1
        ENDDO	
        IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
        ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )

        IF (ALLOCATED(constituents)) DEALLOCATE(constituents)
        STATUS = NF_OPEN(trim(HC_FILE_OFS), NF_NOWRITE, NCID)
        STATUS = NF_INQ(NCID,NDIMS,NVARS,NGATTS,UNLIMDIMID)

        STATUS = NF_INQ_VARID(NCID,'tide_names',IDVAR)
        STATUS = NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
        status = NF_INQ_VARDIMID(NCID,IDVAR,dimids)
        do i=1,NDIMS
             STATUS = NF_INQ_DIMLEN(NCID,dimids(i),DIMS(i))
        enddo
        allocate(constituents(DIMS(1),DIMS(2)) )
        STATUS = NF_GET_VAR_TEXT(NCID,IDVAR,constituents)
        DO I=1,4
          DIMS(I)=1
        ENDDO	

        STATUS = NF_INQ_VARID(NCID,'tide_Eamp',IDVAR)
        STATUS = NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
        status = NF_INQ_VARDIMID(NCID,IDVAR,dimids)
        do i=1,NDIMS
             STATUS = NF_INQ_DIMLEN(NCID,dimids(i),DIMS(i))
             write(*,*) TRIM(VNAME),' dim ',i,' = ',DIMS(i)
        enddo
        IF (ALLOCATED(tide_amp)) DEALLOCATE(tide_amp)
        IF (ALLOCATED(tide_epoc)) DEALLOCATE(tide_epoc)
        allocate(tide_amp(DIMS(1),DIMS(2)) )
        allocate(tide_epoc(DIMS(1),DIMS(2)))
        STATUS = NF_INQ_VARID(NCID,'tide_Eamp',IDVAR)
        STATUS = NF_GET_VAR_REAL(NCID,IDVAR,tide_amp)
	WRITE(*,*)'min & max E_amp=',minval(tide_amp),maxval(tide_amp)
        STATUS = NF_INQ_VARID(NCID,'tide_Ephase',IDVAR)
        STATUS = NF_GET_VAR_REAL(NCID,IDVAR,tide_epoc)
	WRITE(*,*)'min & max E_pha=',minval(tide_epoc),
     1	maxval(tide_epoc)

C -------------------------------------------------------------------
C   conduct  tidal prediction at open boundary nodes
C ----------------------------------------------------------------------------
        IF (trim(OCEAN_MODEL) .EQ. "SELFE")NOBC=NOBC_ORI
        IF (ALLOCATED(oned1)) DEALLOCATE(oned1)
        IF (ALLOCATED(oned2)) DEALLOCATE(oned2)
!      IF (ALLOCATED(WL_PRD)) DEALLOCATE(WL_PRD)
        allocate(oned1(NMAX) )
        allocate(oned2(NMAX) )
!      allocate(WL_PRD(NOBC,NMAX) )
        KINDAT=2
        CONV=1.0
        XMAJOR=0.0
        FOUT='WL_PRED.out'
        DELT_PRD=DELT/3600.0
!        NREC=NINT( (jdaye-jdays)*24/DELT)+1
        NREC=NINT( (day_end-day_start)*86400/DELT)+1
        DO I=1,NOBC
!          WRITE(*,*)'tidal prediction at node= ',I,NREC
          DO K=1,37
            AMP(K)=tide_amp(I,K)
            EPOC(K)=tide_epoc(I,K)
          ENDDO
          FOUT='OBC.prd'
!          if(I .eq. 1)FOUT='node1.prd'
!          if(I .eq. NOBC)FOUT='node_last.prd'
          CALL NOS_PRD(START_TIME,END_TIME,KINDAT,DELT_PRD,CONV,
     &      XMAJOR,AMP,EPOC,FOUT,ONED1,ONED2)
          DO N=1,NREC
             WLOBC(I,N)=WLOBC(I,N)+ONED1(N)   ! non-tide + tidal prediction
          ENDDO
          IF(I .eq. 1)THEN
            CLOSE(45)
            OPEN(45,file='node1.dat')
!            DO K=1,37
!              WRITE(45,*)K,AMP(K),EPOC(K)
!            ENDDO
            DO N=1,NREC
             write(45,'(3F12.5)')Time_M(N),WLOBC(I,N),ONED1(N)   ! non-tide + tidal prediction
            ENDDO
          ELSEIF(I .eq. NOBC_ORI)THEN
            CLOSE(45)
            OPEN(45,file='node_last.dat')
!            DO K=1,37
!              WRITE(45,*)K,AMP(K),EPOC(K)
!            ENDDO
            DO N=1,NREC
	       write(45,'(3F12.5)')Time_M(N),WLOBC(I,N),ONED1(N)   ! non-tide + tidal prediction
	    ENDDO
	  ENDIF
        ENDDO
	write(*,*)'WL OBC is done'	   
      ENDIF

      WRITE(*,*)'min & max WLOBC=',minval(WLOBC),maxval(WLOBC)
      WRITE(*,*)'min & max T=',minval(tempOBC_M),maxval(tempOBC_M)
      WRITE(*,*)'min & max S=',minval(saltOBC_M),maxval(saltOBC_M)
      WRITE(ICORMS,*)'min & max WLOBC=',minval(WLOBC),maxval(WLOBC)
      WRITE(ICORMS,*)'min & max T=',minval(tempOBC_M),maxval(tempOBC_M)
      WRITE(ICORMS,*)'min & max S=',minval(saltOBC_M),maxval(saltOBC_M)
        tmin=999.
	smin=9999
	
	DO N=1,NTMAX
	DO I=1,NUM_T_NUDGE
	DO K=1,KBm
          if(tempOBC_M(I,K,N) .LT. tmin)then
	    tmin=tempOBC_M(I,K,N)
	    Nmin=N
	    Imin=I
	    Kmin=K
	  endif  
          if(saltOBC_M(I,K,N) .LT. smin)then
	    smin=saltOBC_M(I,K,N)
	    Nmin0=N
	    Imin0=I
	    Kmin0=K
	  endif
          if(tempOBC_M(I,K,N) .LT. -50)then
	   print *,'T N,I,K=',N,I,K,tempOBC_M(I,K,N)
	  endif 
          if(saltOBC_M(I,K,N) .LT. -50)then
	   print *,'S N,I,K=',N,I,K,saltOBC_M(I,K,N)
	  endif 
	enddo
	enddo
	enddo 
	print *,'tmin= ',Nmin,Kmin,Imin,tmin   
	print *,'smin= ',Nmin0,Kmin0,Imin0,smin   
      write(*,*)'write OBC forcing files'

        CLOSE(80)
        CLOSE(81)
        CLOSE(82)
 	OPEN(80,file='temp_nu.dat')
 	OPEN(81,file='salt_nu.dat')
 	OPEN(82,file='elev3D.dat')
	
        CLOSE(37)
        CLOSE(35)
        CLOSE(54)
	nrecl_et=1*(NOBC_ORI+1)
!AJ for CCS nrecl_et=4*(NOBC_ORI+1)
 	OPEN(37,file='temp_nu.in',form='unformatted')
 	OPEN(35,file='salt_nu.in',form='unformatted')
 	OPEN(54,file='elev3D.th',access='direct',recl=nrecl_et)
        IF (ALLOCATED(tmp2d)) DEALLOCATE(tmp2d)
	allocate(tmp2d(nodem,KBm))
	DO N=1,NTMAX
	  time_seconds=INT(ts_time(N)*86400.0/DELT_TS+0.1)*DELT_TS  !DELT_TS is in seconds
	  WRITE(37)time_seconds
	  WRITE(35)time_seconds
!	  WRITE(37)ts_time(N)*86400.0
!	  WRITE(35)ts_time(N)*86400.0
	  DO I=1,NODEM
	  DO K=1,KBm
	     tmp2d(I,K)=0.0
	  ENDDO
	  ENDDO
	  DO I=1,NUM_T_NUDGE
	  DO K=1,KBm
	     tmp2d(NODE_T_NUDGE(I),K)=tempOBC_M(I,K,N)
	  ENDDO
	  ENDDO
	  DO I=1,NODEm
	     WRITE(37)(tmp2d(I,K),K=1,KBm)
	  ENDDO
	  If( N .EQ. 5)THEN
	  WRITE(80,*)time_seconds
	    DO I=1,NODEm
	     WRITE(80,555)(tmp2d(I,K),K=1,KBm,10),tmp2d(I,KBm)
	    ENDDO
	  endif   
	  DO I=1,NODEM
	  DO K=1,KBm
	     tmp2d(I,K)=0.0
	  ENDDO
	  ENDDO
	  DO I=1,NUM_T_NUDGE
	  DO K=1,KBm
	     tmp2d(NODE_T_NUDGE(I),K)=saltOBC_M(I,K,N)
	  ENDDO
	  ENDDO
	  DO I=1,NODEm
	     WRITE(35)(tmp2d(I,K),K=1,KBm)
	  ENDDO
	  If( N .EQ. 5)THEN
	  WRITE(81,*)time_seconds
	    DO I=1,NODEm
	     WRITE(81,555)(tmp2d(I,K),K=1,KBm,10),tmp2d(I,KBm)
	    ENDDO
	  endif   
	ENDDO
555     FORMAT(100F10.4)
	IREC=0
	DO N=1,NTMAX_WL
	  time_seconds=INT(zeta_time(N)*86400.0/DELT+0.1)*DELT  !DELT in seconds
	  if(time_seconds .GT. 0.01)then
	    IREC=IREC+1
!	    print *,'time of WL=',time_seconds,IREC
            write(54,rec=IREC)time_seconds,(WLOBC(I,N),I=1,NOBC_ORI)
	  endif  
          WRITE(82,556)time_seconds,(WLOBC(I,N),I=1,NOBC_ORI)
	ENDDO
	CLOSE(37)
	CLOSE(35)
        CLOSE(54)
556     format(f12.1/,10(10f8.4)/)
        CLOSE(80)
        CLOSE(81)
        CLOSE(82)
      write(*,*)'OBC Forcing file is COMPLETED SUCCESSFULLY'
      WRITE(ICORMS,'(a)')'WL OBC SOURCE IS' //trim(DBASE_WL)
      WRITE(ICORMS,'(a)')'T&S OBC SOURCE IS '//trim(DBASE_TS)

      WRITE(ICORMS,'(a)')'END SECTION OF GENERATING OBC FILE' 
      CLOSE(ICORMS)
      
      STOP
      END
       
      SUBROUTINE READ_NETCDF(FIN,VNAME,ANAME,NDIMS,DIMS,TMP4D,ATT,MODE)
C -------------------------------------------------------------------
C  This Fortran subroutine is to read a variable or an attribute of 
C  a variable in a NetCDF file
C  Mode =0 : read dimension sizes of a variable
C  mode =1:  read a real variable
C  mode =2:  read an integer variable, but return a real variable
C  mode =3:  read a string variable, return with VNAME
C  mode =4:  read real type attribute of a variable, return with ATT
C  mode =5:  read an integer type attribute of a variable, return with ATT
C  mode =6:  read a string type attribute of a variable, return with ANAME      
C  mode =7:  read a double precision variable

C -------------------------------------------------------------------
      include 'netcdf.inc'
      character*200 FIN,VNAME,ANAME,BUFFER
      INTEGER DIMS(4),MODE,dimids(5),COUNT(4)
      REAL TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4) )
      LOGICAL FEXIST
      integer, allocatable :: ITMP4D(:,:,:,:)
      REAL*8, allocatable :: DTMP4D(:,:,:,:)

      IF (MODE .EQ. 0)THEN
         DO I=1,4
            DIMS(I)=1
         ENDDO
         INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
         IF(.NOT. FEXIST)THEN
           print *,trim(FIN)//' does not exist'
         ELSE	
           STATUS = NF_OPEN(trim(FIN),NF_NOWRITE, NCID)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     stop 'open netCDF file failed'
           ENDIF  
           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     print *,'required variable ',trim(VNAME), ' is not found'
	     stop
           ENDIF  

           STATUS = NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
           status = NF_INQ_VARDIMID(NCID,IDVAR,dimids)
           do i=1,NDIMS
             STATUS = NF_INQ_DIMLEN(NCID,dimids(i),DIMS(i))
             write(*,*) TRIM(VNAME),' dim ',i,' = ',DIMS(i)
           enddo
           STATUS=NF_CLOSE(NCID)
         ENDIF
      ELSEIF (MODE .EQ. 1)THEN
         INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
         IF(.NOT. FEXIST)THEN
           print *,trim(FIN)//' does not exist'
         ELSE	
           STATUS = NF_OPEN(trim(FIN),NF_NOWRITE, NCID)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     stop 'open netCDF file failed'
           ENDIF  
           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     print *,'required variable ',trim(VNAME), ' is not found'
	     stop
           ENDIF  

           STATUS = NF_INQ_VARNDIMS(NCID,IDVAR,ndims)
           status =NF_INQ_VARDIMID(NCID,IDVAR,dimids)
           do i=1,ndims
             STATUS = NF_INQ_DIMLEN(NCID,dimids(i),COUNT(i))
             IF (COUNT(i) .NE. DIMS(I) )THEN
	       WRITE(*,*)'Dimension of array does not match' 
               write(*,*) TRIM(VNAME),' dim ',i,' = ',COUNT(i)
               write(*,*)'DIMS(',I,')= ',DIMS(I),ndims
	     ENDIF  
           enddo
           STATUS = NF_GET_VAR_REAL(NCID,IDVAR,TMP4D)
           STATUS=NF_CLOSE(NCID)
         ENDIF

      ELSEIF (MODE .EQ. 7)THEN
         allocate(DTMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4) ))
         INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
         IF(.NOT. FEXIST)THEN
           print *,trim(FIN)//' does not exist'
         ELSE	
           STATUS = NF_OPEN(trim(FIN),NF_NOWRITE, NCID)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     stop 'open netCDF file failed'
           ENDIF  
           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     print *,'required variable ',trim(VNAME), ' is not found'
	     stop
           ENDIF  
!           STATUS = NF_INQ_VARNDIMS(NCID,IDVAR,ndims)
!           status =NF_INQ_VARDIMID(NCID,IDVAR,dimids)
!           do i=1,ndims
!             STATUS = NF_INQ_DIMLEN(NCID,dimids(i),COUNT(i))
!             IF (COUNT(i) .NE. DIMS(I) )THEN
!	       WRITE(*,*)'Dimension of array does not match' 
!               write(*,*) TRIM(VNAME),' dim ',i,' = ',COUNT(i)
!               write(*,*)'DIMS(',I,')= ',DIMS(I),ndims
!	     ENDIF  
!           enddo
           STATUS = NF_GET_VAR_DOUBLE(NCID,IDVAR,DTMP4D)
           DO I1=1,DIMS(1)
           DO I2=1,DIMS(2)
           DO I3=1,DIMS(3)
           DO I4=1,DIMS(4)
	    TMP4D(I1,I2,I3,I4)=DTMP4D(I1,I2,I3,I4)
	   ENDDO
	   ENDDO
	   ENDDO
	   ENDDO
           IF (ALLOCATED(Dtmp4d)) DEALLOCATE(Dtmp4d)
           STATUS=NF_CLOSE(NCID)
         ENDIF
      ELSEIF (MODE .EQ. 2)THEN
         IF (ALLOCATED(itmp4d)) DEALLOCATE(itmp4d)
         allocate(ITMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4) ))
         INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
         IF(.NOT. FEXIST)THEN
           print *,trim(FIN)//' does not exist'
         ELSE	
           STATUS = NF_OPEN(trim(FIN),NF_NOWRITE, NCID)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     stop 'open netCDF file failed'
           ENDIF  
           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
           STATUS = NF_GET_VAR_INT(NCID,IDVAR,ITMP4D)
	   DO I=1,DIMS(1)
	   DO J=1,DIMS(2)
	   DO K=1,DIMS(3)
	   DO N=1,DIMS(4)
	     TMP4D(I,j,k,N)=ITMP4D(I,J,K,N)
	   ENDDO
	   ENDDO
	   ENDDO
	   ENDDO  
           IF (ALLOCATED(itmp4d)) DEALLOCATE(itmp4d)
           STATUS=NF_CLOSE(NCID)
         ENDIF
      ELSEIF (MODE .EQ. 3)THEN
         allocate(ITMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4) ))
         INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
         IF(.NOT. FEXIST)THEN
           print *,trim(FIN)//' does not exist'
         ELSE	
           STATUS = NF_OPEN(trim(FIN),NF_NOWRITE, NCID)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     stop 'open netCDF file failed'
           ENDIF  
           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
           STATUS = NF_GET_VAR_TEXT(NCID,IDVAR,BUFFER)
	   VNAME=TRIM(BUFFER)
           STATUS=NF_CLOSE(NCID)
         ENDIF
      ELSEIF (MODE .EQ. 4)THEN
         INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
         IF(.NOT. FEXIST)THEN
           print *,trim(FIN)//' does not exist'
         ELSE	
           STATUS = NF_OPEN(trim(FIN),NF_NOWRITE, NCID)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     stop 'open netCDF file failed'
           ENDIF  
           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
           STATUS = NF_GET_ATT_REAL(NCID,IDVAR,TRIM(ANAME),ATT)
           STATUS=NF_CLOSE(NCID)
         ENDIF
      ELSEIF (MODE .EQ. 5)THEN
         INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
         IF(.NOT. FEXIST)THEN
           print *,trim(FIN)//' does not exist'
         ELSE	
           STATUS = NF_OPEN(trim(FIN),NF_NOWRITE, NCID)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     stop 'open netCDF file failed'
           ENDIF  
           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
           STATUS = NF_GET_ATT_INT(NCID,IDVAR,TRIM(ANAME),IATT)
	   ATT=IATT
           STATUS=NF_CLOSE(NCID)
         ENDIF
      ELSEIF (MODE .EQ. 6)THEN
         INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
         IF(.NOT. FEXIST)THEN
           print *,trim(FIN)//' does not exist'
         ELSE	
           STATUS = NF_OPEN(trim(FIN),NF_NOWRITE, NCID)
           IF(STATUS .NE. NF_NOERR)then
	     print *,'error message= ',status
	     stop 'open netCDF file failed'
           ENDIF  
           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
           STATUS = NF_GET_ATT_TEXT(NCID,IDVAR,TRIM(ANAME),BUFFER)
	   ANAME=TRIM(BUFFER)
           STATUS=NF_CLOSE(NCID)
         ENDIF
      ENDIF	
      RETURN
      END        
!     Routine to compute z coordinates for SELFE's SZ vertical system
!     Inputs:
!             dp: depth;
!             eta: elevation;
!             h0: min. depth;
!             h_s: transition depth between S and Z layers;
!             h_c: transition depth between S and sigma
!             theta_b, theta_f: spacing const. in S coordinate system;
!             nvrt: total # of vertical levels (S+Z);
!             kz: # of Z levels (1 if pure S);
!             ztot(1:kz):  z coordinates for Z levels; note that ztot(kz)=-h_s;
!             sigma(1:nvrt-kz+1): sigma coordinates for S (or sigma) levels; note that sigma(1)=-1, sigma(nvrt-kz+1)=0;
!     Outputs:
!             idry: wet (0) or dry (1) flag;
!             kbp: bottom index (0 if dry);
!             zcor(kbp:nvrt): z coordinates (undefined if dry);    
      subroutine zcor_SZ(dp,eta,h0,h_s,h_c,theta_b,theta_f,kz,nvrt,
     1 ztot,sigma,zcor,idry,kbp)
      implicit real*4(a-h,o-z)
      integer, intent(in) :: kz,nvrt
      real*4, intent(in) :: dp,eta,h0,h_s,h_c,theta_b,theta_f,
     1 ztot(nvrt),sigma(nvrt)
      integer, intent(out) :: idry,kbp
      real*4, intent(out) :: zcor(nvrt)

      real*4 :: cs(nvrt)
!      print *,'input para=',dp,eta,h0,h_s,h_c,theta_b,theta_f,kz,nvrt
!      print *,'ztot=',ztot
!      print *,'sig=',sigma
!     Sanity check
      if(nvrt<3) stop 'nvrt too small'
      if(kz<1.or.kz>nvrt-2) stop 'kz wrong'
      if(h_c<5.or.h_c>=h_s) then !large h_c to avoid 2nd type abnormaty
        print*, 'h_c needs to be larger:',h_c; stop
      endif
      if(theta_b<0.or.theta_b>1) then
        print*, 'Wrong theta_b:',theta_b; stop
      endif
      if(theta_f<=0) then
        print*, 'Wrong theta_f:',theta_f; stop
      endif

!     Pre-compute constants
      s_con1=sinh(theta_f)
      nsig=nvrt-kz+1 !# of S levels 
      do k=1,nsig
        cs(k)=(1-theta_b)*sinh(theta_f*sigma(k))/sinh(theta_f)+ 
     & theta_b*(tanh(theta_f*(sigma(k)+0.5))-
     & tanh(theta_f*0.5))/2/tanh(theta_f*0.5)
      enddo !k

      if(eta<=h0-h_s) then
        stop 'Deep depth dry'
      else if(eta+dp<=h0) then
        idry=1; kbp=0
      else !wet
!       S-levels
        idry=0
        hmod=min(h_s,dp)
        do k=kz,nvrt
          kin=k-kz+1
          if(hmod<=h_c) then
            zcor(k)=sigma(kin)*(hmod+eta)+eta
          else if(eta<=-h_c-(hmod-h_c)*theta_f/s_con1) then !hmod(i)>h_c>=0
            print*, 'Pls choose a larger h_c (2):',eta,h_c
            stop
          else
          zcor(k)=eta*(1+sigma(kin))+h_c*sigma(kin)+(hmod-h_c)*cs(kin)
          endif
        enddo !k=kz,nvrt

!         z-levels
        if(dp<=h_s) then
          kbp=kz
        else !bottom index 
          kbp=0 !flag
          do k=1,kz-1
            if(-dp>=ztot(k).and.-dp<ztot(k+1)) then
              kbp=k
              exit
            endif
          enddo !k
          if(kbp==0) then
            print*, 'Cannot find a bottom level for node (3):',i
            stop
          endif

          if(kbp>=kz.or.kbp<1) then
            print*, 'Impossible 92:',kbp,kz
            stop
          endif
          zcor(kbp)=-dp
          do k=kbp+1,kz-1
            zcor(k)=ztot(k)
          enddo !k
        endif

        do k=kbp+1,nvrt
          if(zcor(k)-zcor(k-1)<=0) then
            write(*,*)'Inverted z-levels at:',i,k,zcor(k)-zcor(k-1),
     1	    eta,hmod
            stop
          endif
        enddo !k
      endif !wet ot dry

      end subroutine zcor_SZ
