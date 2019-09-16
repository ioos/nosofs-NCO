C xlf nos_ofs_read_restart.f  -I/gpfs/c2/home/wx21az/netcdf-3.6.2/include 
C -L/gpfs/c2/home/wx21az/netcdf-3.6.2/lib -lnetcdf -o nos_ofs_read_restart

C The julian subroutine does not work correctly while Year < 1900
C A new subroutine from Jianhua Qi is used 
      include 'netcdf.inc'
      character*120 OFS,OCEAN_MODEL*10,COLD_START*10
      character*120 FIN,FOUT,GRIDFILE,FIXnos,netcdf_file
      character*120 BUFFER,CMD*132,VNAME,ANAME
      character*120 START_TIME, END_TIME,CTIME*26
      CHARACTER globalstr(9)*120
      real*8 jday_start,jbase_date,JULIAN,yearb,monthb,dayb,hourb
      real*8 jday,jday0,jdays,jdaye
      real, allocatable :: zeta  (:,:)
      real, allocatable :: ubar  (:,:)
      real, allocatable :: vbar  (:,:)
      real, allocatable :: u  (:,:,:)
      real, allocatable :: v  (:,:,:)
      real, allocatable :: temp  (:,:,:)
      real, allocatable :: salt  (:,:,:)
!      CHARACTER, allocatable :: CTIME (:,:)

      integer  Vtransform
      integer  Vstretching
      real*8  theta_s
      real*8  theta_b
      real*8  Tcline
      real*8  hc
      real, allocatable ::   s_rho(:)
      real, allocatable ::   s_w(:)
      real, allocatable ::   Cs_r(:)
      real, allocatable ::   Cs_w(:)
      real, allocatable ::   h(:)
      real, allocatable :: lat_rho (:)
      real, allocatable :: lon_rho (:)
      real, allocatable :: lat_u (:)
      real, allocatable :: lon_u (:)
      real, allocatable :: lat_v (:)
      real, allocatable :: lon_v (:)
      
! temporary arrays
      real, allocatable :: tmp1d  (:)
      real, allocatable :: tmp2d  (:,:)
      real, allocatable :: tmp3d  (:,:,:)
      real, allocatable :: tmp4d  (:,:,:,:)
      real, allocatable :: tmp5d  (:,:,:,:,:)
      integer dimids(5),COUNT(5),DIMS(5),STATUS
      LOGICAL FEXIST,CHANGE_TIME
      CHARACTER (LEN=10) BIG_BEN(3),CURRENT_TIME*20
      INTEGER DATE_TIME(8)
      INTEGER BASE_DATE(4)
      INTEGER DAYS_PER_MONTH(12)
      DATA (DAYS_PER_MONTH(i),I=1,12) /
     &31,28,31,30,31,30,31,31,30,31,30,31/
      CHANGE_TIME=.FALSE.
      read(5,'(a120)')OFS
      read(5,'(a10)')OCEAN_MODEL
      read(5,'(a10)')COLD_START
      read(5,'(a120)')GRIDFILE
      read(5,'(a120)')FIN
      read(5,'(a120)')FOUT
      read(5,'(a120)')BUFFER
      START_TIME=trim(adjustL(BUFFER))
      read(START_TIME,'(I4,4I2)')IYRS,IMMS,IDDS,IHHS
      read(5,'(a120)')BUFFER
      do i=1,len_trim(BUFFER)
         if(BUFFER(i:I) .eq. "'" .or. BUFFER(i:I) .eq. '"')then
            BUFFER(i:I)=' '
	 endif    
      enddo
      BUFFER=trim(adjustL(BUFFER))
      read(BUFFER,'(I4,3i2)')base_date
      yearb=base_date(1)
      monthb=base_date(2)
      dayb=base_date(3)
      hourb=base_date(4)
      jbase_date=JULIAN(yearb,monthb,dayb,hourb)
      
      yearb=IYRS
      monthb=IMMS
      dayb=IDDS
      hourb=IHHS 
      jdays=JULIAN(yearb,monthb,dayb,hourb)  
      day_start=jdays-jbase_date
      OPEN(10,file=trim(FOUT))
      print *,'FIN=',TRIM(FIN)
      INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
      IF(.NOT. FEXIST)THEN
         print *,trim(FIN)//' does not exist'
	 PRINT *,'hot restart file is not found'
	 STOP
      ENDIF
      DO I=1,4
        DIMS(I)=1
      ENDDO	
      IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
      ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
      IF (TRIM(OCEAN_MODEL) .eq. "ROMS" )THEN
         VNAME='ocean_time'
      ELSEIF (TRIM(OCEAN_MODEL) .eq. "FVCOM" )THEN	 
         VNAME='time'
      ENDIF
      IF ( (TRIM(OCEAN_MODEL) .eq. "ROMS") .or.
     1     (TRIM(OCEAN_MODEL) .eq. "FVCOM") )THEN
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0,STATUS)
        IF(STATUS .ne. NF_NOERR)THEN
           WRITE(*,*)'There is error to read: ',trim(VNAME)
           ocean_time=-999.99
           GOTO 20
        ENDIF	
        IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
        ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,1,STATUS)
        ocean_time=-999.99
        DO I1=1,DIMS(1)
        DO I2=1,DIMS(2)
        DO I3=1,DIMS(3)
        DO I4=1,DIMS(4)
          if(tmp4d(I1,I2,I3,I4) .GT. ocean_time)then
	     ocean_time=tmp4d(I1,I2,I3,I4)
	  endif   
        ENDDO
        ENDDO
        ENDDO
        ENDDO
!      if(ocean_time .GT. 0.0)ocean_time=ocean_time/86400.0
        ANAME='units'
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,6,STATUS)
        print *,'ANAME= ',trim(ANAME)
        BUFFER=TRIM(ADJUSTL(ANAME))
        print *,'buffer=',trim(buffer)
        LEN1=LEN_TRIM(BUFFER)
        LL=INDEX(BUFFER,'since',BACK=.TRUE.)
        IF(LL .EQ. 0)THEN
          WRITE(*,*)'since is not found in ocean_time attributes'
	  WRITE(*,*)'There is problem to process restart time '
	  WRITE(*,*)'in reading restart file, and '//TRIM(OFS)//' stop'
	  STOP
 !       GOTO 20
        ENDIF
      	
        print *,'buffer=',BUFFER(LL+6:LEN1)
        READ(BUFFER(LL+6:LEN1),'(I4,3(1x,I2))')IYR,IMM,IDD,IHH
        LUNITS=INDEX(BUFFER,'seconds')
        IF(LUNITS .GT. 0)THEN
          if(ocean_time .GT. 0.0)ocean_time=ocean_time/86400.0
        ENDIF 	
        LUNITS=INDEX(BUFFER,'SECONDS')
        IF(LUNITS .GT. 0)THEN
          if(ocean_time .GT. 0.0)ocean_time=ocean_time/86400.0
        ENDIF 	
        IF( (IYR .NE. base_date(1)) .OR. (IMM .NE. base_date(2) ) .OR.
     &    (IDD .NE. base_date(3)) .OR. (IHH .NE. base_date(4) ) )THEN
          WRITE(*,*)'time origin of ocean_time in the initial file '
	  WRITE(*,*)' does not equal to basedate'
	  WRITE(*,*)'time origin is ', IYR,IMM,IDD,IHH
	  WRITE(*,*)'basedate is    ',base_date
          yearb=IYR
          monthb=IMM
          dayb=IDD
          hourb=IHH   
          ocean_time=JULIAN(yearb,monthb,dayb,hourb)+ocean_time
     &       -jbase_date
	  CHANGE_TIME=.TRUE.
	  print *,'ocean_time=',ocean_time
        ENDIF	
20      CONTINUE
        IF (TRIM(OCEAN_MODEL) .eq. "ROMS" )THEN
          VNAME='ntimes'
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0,STATUS)
          IF(STATUS .ne. NF_NOERR)THEN
            WRITE(*,*)'There is error to read: ',trim(VNAME)
            print *, nf_strerror(status)
            ntimes=0
            GOTO 30
          ENDIF	 
          IF (ALLOCATED(tmp4d)) DEALLOCATE(tmp4d)
          ALLOCATE(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)) )
        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,2,STATUS)
          NTIMES=INT(tmp4d(DIMS(1),DIMS(2),DIMS(3),DIMS(4)))
        ENDIF
30      CONTINUE
      ENDIF
!      PRINT *,trim(VNAME),ocean_time,NTIMES
      day_hotrestart=ocean_time 
      print *,'Time in initial file=',day_hotrestart
      print *,'Time of nowcastend=  ',day_start
      IF ( (day_start-day_hotrestart) .LE. 0.0 )THEN
        write(*,*)'restart time is greater than current time'
	write(*,*)'initial time= ',day_hotrestart
	write(*,*)'current time= ',day_start
	write(*,*)'check initial file and time_nowcastend'
	write(*,*)TRIM(OFS)//' stop in nos_ofs_read_restart.f'
	stop
      ELSEIF ( (day_start-day_hotrestart) .GT. 2.0 )THEN
        write(*,*)'restart file is too old'
	write(*,*)'initial time= ',day_hotrestart
	write(*,*)'current time= ',day_start
	write(*,*)'redefine restart time= day_start - 2'
        day_hotrestart=day_start-2.0  !allow to search 2 days backward
	CHANGE_TIME=.TRUE.
	COLD_START="T"
         ntimes=0
      ENDIF
      jday=day_hotrestart+jbase_date
      call GREGORIAN(jday,yearb,monthb,dayb,hourb)
      IYR=INT(yearb)
      IMM=int(monthb+0.001)
      IDD=INT(dayb+0.001)
      IHH=INT(hourb+0.001)
      IMN=INT((hourb-IHH)*60+0.1)
      ISEC=0
!      ISEC=INT(((hours-IHH)*60-IMN)*60+0.001)
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
      IF (TRIM(OCEAN_MODEL) .eq. "FVCOM" )THEN	 
         WRITE(CTIME,'(I4.4,a1,I2.2,a1,I2.2,a1,I2.2,a1,I2.2,a10)')
     1        IYR,'-',IMM,'-',IDD,'T',IHH,':',IMN,'00.000000 '	
         print *,'CTIME=',trim(CTIME)
      ENDIF    
      IF(CHANGE_TIME)THEN
         STATUS=NF_CLOSE(NCID)
         STATUS = NF_OPEN(trim(FIN),NF_WRITE, NCID)
         IF (TRIM(OCEAN_MODEL) .eq. "ROMS" )THEN
           VNAME='ocean_time'
         ELSEIF (TRIM(OCEAN_MODEL) .eq. "FVCOM" )THEN	 
           VNAME='time'
         ENDIF	 
         STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
 !        if (status .ne. NF_NOERR)return 
         STATUS = NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
 !        if (status .ne. NF_NOERR)return
         status =NF_INQ_VARDIMID(NCID,IDVAR,dimids)
         if (status .ne. NF_NOERR) then
             print *,'status=',status
             print *, nf_strerror(status)
!	     return
         endif
         do i=1,NDIMS
            STATUS = NF_INQ_DIMLEN(NCID,dimids(i),DIMS(i))
            write(*,*) TRIM(VNAME),' dim ',i,' = ',DIMS(i)
         enddo
         IF (ALLOCATED(tmp1d)) DEALLOCATE(tmp1d)
         ALLOCATE(TMP1D(DIMS(1)) )
	 STATUS = NF_GET_VAR_REAL(NCID,IDVAR,TMP1D)
	 IF(DIMS(1) .GT. 1)THEN
	   DO I=DIMS(1)-1,1,-1
	     TMP1D(I)=TMP1D(I+1)-TMP1D(I)
	   ENDDO
	 ENDIF    
         ANAME='units'
!        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,6,STATUS)
         STATUS = NF_GET_ATT_TEXT(NCID,IDVAR,TRIM(ANAME),BUFFER)
	 ANAME=TRIM(BUFFER)
         print *,'ANAME= ',trim(ANAME)
         BUFFER=TRIM(ADJUSTL(ANAME))
         LEN1=LEN_TRIM(BUFFER)
         LL=INDEX(BUFFER,'seconds')
         IF(LL .GT. 0)THEN
	   TMP1D(DIMS(1))=day_hotrestart*86400.0
	   WRITE(BUFFER,202)'seconds since ',base_date(1),'-',
     1	   base_date(2),'-',base_date(3),base_date(4),':00:00'
 202       format(a14,I4.4,a1,I2.2,a1,I2.2,1x,I2.2,a6)
           print *,'buff=',trim(buffer)    
         ENDIF
         LL=INDEX(BUFFER,'days')
         IF(LL .GT. 0)THEN
	   TMP1D(DIMS(1))=day_hotrestart
	   WRITE(BUFFER,204)'days since ',base_date(1),'-',
     1	   base_date(2),'-',base_date(3),base_date(4),':00:00'
 204       format(a11,I4.4,a1,I2.2,a1,I2.2,1x,I2.2,a6)
            print *,'buff=',trim(buffer)    
        ENDIF
        IF(DIMS(1) .GT. 1)THEN
	   DO I=DIMS(1)-1,1,-1
	     TMP1D(I)=TMP1D(I+1)+TMP1D(I)
	   ENDDO
	ENDIF    
        STATUS = NF_REDEF(NCID)
        BUFFER=TRIM(ADJUSTL(BUFFER))
        LEN1=LEN_TRIM(BUFFER)
        STATUS = NF_PUT_ATT_TEXT(NCID,IDVAR,'units',LEN1,BUFFER)
        VNAME='Itime'
        STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
        STATUS = NF_PUT_ATT_TEXT(NCID,IDVAR,'units',LEN1,BUFFER)
	STATUS = NF_ENDDEF(NCID)
        IF (TRIM(OCEAN_MODEL) .eq. "ROMS" )THEN
           VNAME='ocean_time'
        ELSEIF (TRIM(OCEAN_MODEL) .eq. "FVCOM" )THEN	 
           VNAME='time'
        ENDIF	 
        STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
 !       if (status .ne. NF_NOERR)return 
        STATUS = NF_PUT_VAR_REAL(NCID,IDVAR,TMP1D)

        if (status .ne. NF_NOERR) then
             print *,'status=',status
             print *, nf_strerror(status)
!	     return
        endif
        IF (TRIM(OCEAN_MODEL) .eq. "FVCOM" )THEN	 
           VNAME='Times'
           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
           if (status .ne. NF_NOERR) then
             print *,'status=',status
             print *, nf_strerror(status)
!	     return
           endif
	   STATUS = NF_PUT_VAR_TEXT(NCID,IDVAR,CTIME)
           VNAME='Itime'
           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
           if (status .ne. NF_NOERR) then
             print *,'status=',status
             print *, nf_strerror(status)
!	     return
           endif
           Itime=INT(day_hotrestart+0.1)
           STATUS = NF_PUT_VAR_INT(NCID,IDVAR,Itime)
!           VNAME='Itime2'
!           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
!           if (status .ne. NF_NOERR) then
!             print *,'status=',status
!             print *, nf_strerror(status)
!	     return
!           endif
!           Itime2=INT((day_hotrestart-Itime)*86400)*1000
!           STATUS = NF_PUT_VAR_REAL(NCID,IDVAR,Itime2)
	   
        ENDIF	
	 
        IF(TRIM(COLD_START) .eq. "T")THEN  !! forced from cold start
	   NTIMES=0   
	   DO I=1,5
	     DIMS(I)=1
	   ENDDO   
           VNAME='zeta'
           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
 !          if (status .ne. NF_NOERR)return 
           STATUS = NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
 !          if (status .ne. NF_NOERR)return
           status =NF_INQ_VARDIMID(NCID,IDVAR,dimids)
           if (status .ne. NF_NOERR) then
             print *,'status=',status
             print *, nf_strerror(status)
!	     return
           endif
           do i=1,NDIMS
             STATUS = NF_INQ_DIMLEN(NCID,dimids(i),DIMS(i))
             write(*,*) TRIM(VNAME),' dim ',i,' = ',DIMS(i)
           enddo
           IF(STATUS .ne. NF_NOERR)THEN
             WRITE(*,*)'There is error to read: ',trim(VNAME)
             print *, nf_strerror(status)
	     goto 200
           ENDIF	 
           IF (ALLOCATED(tmp5d)) DEALLOCATE(tmp5d)
           ALLOCATE(tmp5d(DIMS(1),DIMS(2),DIMS(3),DIMS(4),DIMS(5)) )
           DO I1=1,DIMS(1)	
           DO I2=1,DIMS(2)	
           DO I3=1,DIMS(3)	
           DO I4=1,DIMS(4)	
           DO I5=1,DIMS(5)	
             TMP5D(I1,I2,I3,I4,I5)=0.0
           ENDDO
           ENDDO
           ENDDO
           ENDDO
           ENDDO
           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
	   STATUS = NF_PUT_VAR_REAL(NCID,IDVAR,tmp5d)
200        VNAME='ubar'
           IF (TRIM(OCEAN_MODEL) .eq. "FVCOM" )VNAME='ua'	 
           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
 !          if (status .ne. NF_NOERR)return 
           STATUS = NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
 !          if (status .ne. NF_NOERR)return
           status =NF_INQ_VARDIMID(NCID,IDVAR,dimids)
           if (status .ne. NF_NOERR) then
             print *,'status=',status
             print *, nf_strerror(status)
!	     return
           endif
           do i=1,NDIMS
             STATUS = NF_INQ_DIMLEN(NCID,dimids(i),DIMS(i))
             write(*,*) TRIM(VNAME),' dim ',i,' = ',DIMS(i)
           enddo
           IF(STATUS .ne. NF_NOERR)THEN
             WRITE(*,*)'There is error to read: ',trim(VNAME)
             print *, nf_strerror(status)
	     goto 200
           ENDIF
	   print *,'test'	 
!        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0,STATUS)
           IF(STATUS .ne. NF_NOERR)THEN
             WRITE(*,*)'There is error to read: ',trim(VNAME)
             print *, nf_strerror(status)
           ENDIF	 
           IF (ALLOCATED(tmp5d)) DEALLOCATE(tmp5d)
           ALLOCATE(tmp5d(DIMS(1),DIMS(2),DIMS(3),DIMS(4),DIMS(5)) )
           DO I1=1,DIMS(1)	
           DO I2=1,DIMS(2)	
           DO I3=1,DIMS(3)	
           DO I4=1,DIMS(4)	
           DO I5=1,DIMS(5)	
             TMP5D(I1,I2,I3,I4,I5)=0.0
           ENDDO
           ENDDO
           ENDDO
           ENDDO
           ENDDO
           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
	   STATUS = NF_PUT_VAR_REAL(NCID,IDVAR,tmp5d)
220        VNAME='vbar'
           IF (TRIM(OCEAN_MODEL) .eq. "FVCOM" )VNAME='va'	 
           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
 !          if (status .ne. NF_NOERR)return 
           STATUS = NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
 !1          if (status .ne. NF_NOERR)return
           status =NF_INQ_VARDIMID(NCID,IDVAR,dimids)
           if (status .ne. NF_NOERR) then
             print *,'status=',status
             print *, nf_strerror(status)
!	     return
           endif
           do i=1,NDIMS
             STATUS = NF_INQ_DIMLEN(NCID,dimids(i),DIMS(i))
             write(*,*) TRIM(VNAME),' dim ',i,' = ',DIMS(i)
           enddo
!        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0,STATUS)
           IF(STATUS .ne. NF_NOERR)THEN
             WRITE(*,*)'There is error to read: ',trim(VNAME)
             print *, nf_strerror(status)
	     goto 240
           ENDIF	 
           IF (ALLOCATED(tmp5d)) DEALLOCATE(tmp5d)
           ALLOCATE(tmp5d(DIMS(1),DIMS(2),DIMS(3),DIMS(4),DIMS(5)) )
           DO I1=1,DIMS(1)	
           DO I2=1,DIMS(2)	
           DO I3=1,DIMS(3)	
           DO I4=1,DIMS(4)	
           DO I5=1,DIMS(5)	
             TMP5D(I1,I2,I3,I4,I5)=0.0
           ENDDO
           ENDDO
           ENDDO
           ENDDO
           ENDDO
           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
	   STATUS = NF_PUT_VAR_REAL(NCID,IDVAR,tmp5d)
240        VNAME='u'
           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
 !          if (status .ne. NF_NOERR)return 
           STATUS = NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
 !          if (status .ne. NF_NOERR)return
           status =NF_INQ_VARDIMID(NCID,IDVAR,dimids)
           if (status .ne. NF_NOERR) then
             print *,'status=',status
             print *, nf_strerror(status)
!	     return
           endif
           do i=1,NDIMS
             STATUS = NF_INQ_DIMLEN(NCID,dimids(i),DIMS(i))
             write(*,*) TRIM(VNAME),' dim ',i,' = ',DIMS(i)
           enddo
!        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0,STATUS)
           IF(STATUS .ne. NF_NOERR)THEN
             WRITE(*,*)'There is error to read: ',trim(VNAME)
             print *, nf_strerror(status)
	     goto 260
           ENDIF	 
           IF (ALLOCATED(tmp5d)) DEALLOCATE(tmp5d)
           ALLOCATE(tmp5d(DIMS(1),DIMS(2),DIMS(3),DIMS(4),DIMS(5)) )
           DO I1=1,DIMS(1)	
           DO I2=1,DIMS(2)	
           DO I3=1,DIMS(3)	
           DO I4=1,DIMS(4)	
           DO I5=1,DIMS(5)	
             TMP5D(I1,I2,I3,I4,I5)=0.0
           ENDDO
           ENDDO
           ENDDO
           ENDDO
           ENDDO
           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
	   STATUS = NF_PUT_VAR_REAL(NCID,IDVAR,tmp5d)
260        VNAME='v'
           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
 !          if (status .ne. NF_NOERR)return 
           STATUS = NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
 !          if (status .ne. NF_NOERR)return
           status =NF_INQ_VARDIMID(NCID,IDVAR,dimids)
           if (status .ne. NF_NOERR) then
             print *,'status=',status
             print *, nf_strerror(status)
!	     return
           endif
           do i=1,NDIMS
             STATUS = NF_INQ_DIMLEN(NCID,dimids(i),DIMS(i))
             write(*,*) TRIM(VNAME),' dim ',i,' = ',DIMS(i)
           enddo
!        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0,STATUS)
           IF(STATUS .ne. NF_NOERR)THEN
             WRITE(*,*)'There is error to read: ',trim(VNAME)
             print *, nf_strerror(status)
	     goto 280
           ENDIF	 
           IF (ALLOCATED(tmp5d)) DEALLOCATE(tmp5d)
           ALLOCATE(tmp5d(DIMS(1),DIMS(2),DIMS(3),DIMS(4),DIMS(5)) )
           DO I1=1,DIMS(1)	
           DO I2=1,DIMS(2)	
           DO I3=1,DIMS(3)	
           DO I4=1,DIMS(4)	
           DO I5=1,DIMS(5)	
             TMP5D(I1,I2,I3,I4,I5)=0.0
           ENDDO
           ENDDO
           ENDDO
           ENDDO
           ENDDO
           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
	   STATUS = NF_PUT_VAR_REAL(NCID,IDVAR,tmp5d)
280        VNAME='w'
           IF (TRIM(OCEAN_MODEL) .eq. "FVCOM" )VNAME='ww'	 
           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
 !          if (status .ne. NF_NOERR)return 
           STATUS = NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
 !          if (status .ne. NF_NOERR)return
           status =NF_INQ_VARDIMID(NCID,IDVAR,dimids)
           if (status .ne. NF_NOERR) then
             print *,'status=',status
             print *, nf_strerror(status)
!	     return
           endif
           do i=1,NDIMS
             STATUS = NF_INQ_DIMLEN(NCID,dimids(i),DIMS(i))
             write(*,*) TRIM(VNAME),' dim ',i,' = ',DIMS(i)
           enddo
!        CALL READ_NETCDF(FIN,VNAME,ANAME,NDIM,DIMS,TMP4D,ATT,0,STATUS)
           IF(STATUS .ne. NF_NOERR)THEN
             WRITE(*,*)'There is error to read: ',trim(VNAME)
             print *, nf_strerror(status)
	     goto 300
           ENDIF	 
           IF (ALLOCATED(tmp5d)) DEALLOCATE(tmp5d)
           ALLOCATE(tmp5d(DIMS(1),DIMS(2),DIMS(3),DIMS(4),DIMS(5)) )
           DO I1=1,DIMS(1)	
           DO I2=1,DIMS(2)	
           DO I3=1,DIMS(3)	
           DO I4=1,DIMS(4)	
           DO I5=1,DIMS(5)	
             TMP5D(I1,I2,I3,I4,I5)=0.0
           ENDDO
           ENDDO
           ENDDO
           ENDDO
           ENDDO
           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
	   STATUS = NF_PUT_VAR_REAL(NCID,IDVAR,tmp5d)
300        CONTINUE	   
        ENDIF
        STATUS=NF_CLOSE(NCID)
      ENDIF   
      yearb=IYR
      monthb=1.0
      dayb=1.0
      hourb=0.0  
      tide_start=JULIAN(yearb,monthb,dayb,hourb)-jbase_date
      WRITE(10,100)IYR,IMM,IDD,IHH,NTIMES,day_hotrestart,'d0'
     1 ,tide_start,'d0' 
!      WRITE(10,100)IYR,IMM,IDD,IHH,NTIMES,day_hotrestart,'d0'
!     ,base_date(1),base_date(2),base_date(3),base_date(4),'.0d0'  
      CLOSE(10)
      WRITE(*,*)'reading time_hotstart is COMPLETED SUCCESSFULLY'
100   FORMAT(I4.4,3I2.2,2x,I10,f12.4,a2,2x,F12.4,a2)		     
! 100  FORMAT(I4.4,3I2.2,2x,I10,f10.4,a2,2x,I4.4,3I2.2,a4)		     
      END
      SUBROUTINE READ_NETCDF(FIN,VNAME,ANAME,NDIMS,DIMS,TMP4D,ATT,MODE
     & ,STATUS)
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

C -------------------------------------------------------------------
      include 'netcdf.inc'
      character*120 FIN,VNAME,ANAME,BUFFER
      INTEGER DIMS(5),MODE,dimids(5),COUNT(5),STATUS
      REAL TMP4D(DIMS(1),DIMS(2),DIMS(3),DIMS(4) )
      LOGICAL FEXIST
      integer, allocatable :: ITMP4D(:,:,:,:)

      IF (MODE .EQ. 0)THEN
         DO I=1,5
            DIMS(I)=1
         ENDDO
         INQUIRE(FILE=trim(FIN),EXIST=FEXIST)
         IF(.NOT. FEXIST)THEN
           print *,trim(FIN)//' does not exist'
	   return
         ELSE	
           STATUS = NF_OPEN(trim(FIN),NF_NOWRITE, NCID)
           if (status .ne. NF_NOERR) return
           STATUS = NF_INQ_VARID(NCID,TRIM(VNAME),IDVAR)
           if (status .ne. NF_NOERR)return 
           STATUS = NF_INQ_VARNDIMS(NCID,IDVAR,NDIMS)
           if (status .ne. NF_NOERR)return
           status =NF_INQ_VARDIMID(NCID,IDVAR,dimids)
           if (status .ne. NF_NOERR) then
              print *,'status=',status
              print *, nf_strerror(status)
	      return
           endif
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
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine write_netCDF_INIT_ROMS(netcdf_file,ncid,time_len,
     & xi_rho_len,eta_rho_len,s_rho_len,tracer_len,base_date,
     & ocean_time,zeta,ubar,vbar,u,v,temp,salt,
     & lat_rho,lon_rho,lat_u,lon_u,lat_v,lon_v,
     &  Vtransform,Vstretching,theta_s,theta_b,Tcline,hc,
     &	s_rho,s_w,Cs_r,Cs_w,h,globalstr)

      include 'netcdf.inc'
      CHARACTER*80 TEXT,CNAME,netcdf_file
      INTEGER LEN,base_date(4),intval(4),CORNER(4),COUNT(4)
      CHARACTER (LEN=10) BIG_BEN(3),CURRENT_TIME*20
      INTEGER DATE_TIME(8)
      REAL START_TIME,END_TIME      
      character globalstr(9)*120
* error iret return
      integer  iret
* netCDF id
      integer  ncid
* dimension ids
      integer  xi_rho_dim
      integer  xi_u_dim
      integer  xi_v_dim
      integer  eta_rho_dim
      integer  eta_u_dim
      integer  eta_v_dim
      integer  s_rho_dim
      integer  s_w_dim
      integer  tracer_dim
      integer  time_dim
* dimension lengths
      integer  xi_rho_len
      integer  xi_u_len
      integer  xi_v_len
      integer  eta_rho_len
      integer  eta_u_len
      integer  eta_v_len
      integer  s_rho_len
      integer  s_w_len
      integer  tracer_len
      integer  time_len
* variable ids
      integer  spherical_id
      integer  Vtransform_id
      integer  Vstretching_id
      integer  theta_s_id
      integer  theta_b_id
      integer  Tcline_id
      integer  hc_id
      integer  s_rho_id
      integer  s_w_id
      integer  Cs_r_id
      integer  Cs_w_id
      integer  h_id
      integer  lon_rho_id
      integer  lat_rho_id
      integer  lon_u_id
      integer  lat_u_id
      integer  lon_v_id
      integer  lat_v_id
      integer  ocean_time_id
      integer  zeta_id
      integer  ubar_id
      integer  vbar_id
      integer  u_id
      integer  v_id
      integer  temp_id
      integer  salt_id
* data variables
      character*1 spherical
      integer  Vtransform
      integer  Vstretching
      double precision  theta_s
      double precision  theta_b
      double precision  Tcline
      double precision  hc
      double precision  s_rho(s_rho_len)
      double precision  s_w(s_rho_len+1)
      double precision  Cs_r(s_rho_len)
      double precision  Cs_w(s_rho_len+1)  !!s_w_len=s_rho_len+1
      double precision  h(xi_rho_len, eta_rho_len)

      double precision  ocean_time(time_len)
      double precision  zeta(xi_rho_len, eta_rho_len,time_len)
      double precision  ubar(xi_rho_len-1, eta_rho_len,time_len)
      double precision  vbar(xi_rho_len, eta_rho_len-1,time_len)
      double precision  u(xi_rho_len-1,eta_rho_len,s_rho_len,time_len)
      double precision  v(xi_rho_len,eta_rho_len-1,s_rho_len,time_len)
      double precision  temp(xi_rho_len, eta_rho_len, s_rho_len
     1 ,time_len)
      double precision  salt(xi_rho_len, eta_rho_len, s_rho_len
     1 ,time_len)
      double precision  lat_rho(xi_rho_len, eta_rho_len)
      double precision  lon_rho(xi_rho_len, eta_rho_len)
      double precision  lat_u(xi_rho_len-1, eta_rho_len)
      double precision  lon_u(xi_rho_len-1, eta_rho_len)
      double precision  lat_v(xi_rho_len, eta_rho_len-1)
      double precision  lon_v(xi_rho_len, eta_rho_len-1)

      iret = nf_create(trim(netcdf_file), NF_CLOBBER, ncid)
      call check_err(iret)

      xi_u_len=xi_rho_len-1
      xi_v_len=xi_rho_len
      eta_u_len=eta_rho_len
      eta_v_len=eta_rho_len-1
      s_w_len=s_rho_len+1

* define dimensions
      iret = nf_def_dim(ncid, 'xi_rho',xi_rho_len , xi_rho_dim)
      call check_err(iret)
      iret = nf_def_dim(ncid, 'xi_u', xi_u_len, xi_u_dim)
      call check_err(iret)
      iret = nf_def_dim(ncid, 'xi_v',xi_v_len , xi_v_dim)
      call check_err(iret)
      iret = nf_def_dim(ncid, 'eta_rho', eta_rho_len, eta_rho_dim)
      call check_err(iret)
      iret = nf_def_dim(ncid, 'eta_u',eta_u_len , eta_u_dim)
      call check_err(iret)
      iret = nf_def_dim(ncid, 'eta_v',eta_v_len, eta_v_dim)
      call check_err(iret)
      iret = nf_def_dim(ncid, 's_rho',s_rho_len, s_rho_dim)
      call check_err(iret)
      iret = nf_def_dim(ncid, 's_w', s_w_len, s_w_dim)
      call check_err(iret)
      iret = nf_def_dim(ncid, 'tracer', tracer_len, tracer_dim)
      call check_err(iret)
      iret = nf_def_dim(ncid, 'ocean_time', time_len, time_dim)
      call check_err(iret)
* define variables
      iret = nf_def_var(ncid, 'spherical', NF_CHAR, 0, 0, 
     1spherical_id)
      call check_err(iret)
      iret = nf_def_var(ncid, 'Vtransform', NF_INT, 0, 0, 
     1Vtransform_id)
      call check_err(iret)
      iret = nf_def_var(ncid, 'Vstretching', NF_INT, 0, 0
     1, Vstretching_id)
      call check_err(iret)
      iret = nf_def_var(ncid, 'theta_s', NF_DOUBLE, 0, 0, 
     1theta_s_id)
      call check_err(iret)
      iret = nf_def_var(ncid, 'theta_b', NF_DOUBLE, 0, 0, 
     1theta_b_id)
      call check_err(iret)
      iret = nf_def_var(ncid, 'Tcline', NF_DOUBLE, 0, 0, 
     1Tcline_id)
      call check_err(iret)
      iret = nf_def_var(ncid, 'hc', NF_DOUBLE, 0, 0, hc_id)
      call check_err(iret)
      intval(1) = s_rho_dim
      iret = nf_def_var(ncid, 's_rho', NF_DOUBLE, 1, intval
     1, s_rho_id)
      call check_err(iret)
      intval(1) = s_w_dim
      iret = nf_def_var(ncid, 's_w', NF_DOUBLE, 1, intval,
     1s_w_id)
      call check_err(iret)
      intval(1) = s_rho_dim
      iret = nf_def_var(ncid, 'Cs_r', NF_DOUBLE, 1, intval, 
     1Cs_r_id)
      call check_err(iret)
      intval(1) = s_w_dim
      iret = nf_def_var(ncid, 'Cs_w', NF_DOUBLE, 1, intval, 
     1Cs_w_id)
      call check_err(iret)
      intval(2) = eta_rho_dim
      intval(1) = xi_rho_dim
      iret = nf_def_var(ncid, 'h', NF_DOUBLE, 2, intval, h_id)
      call check_err(iret)




      intval(2) = eta_rho_dim
      intval(1) = xi_rho_dim
      iret = nf_def_var(ncid, 'lat_rho', NF_DOUBLE, 2, intval,
     & lat_rho_id)
      iret = nf_def_var(ncid, 'lon_rho', NF_DOUBLE, 2, intval,
     & lon_rho_id)
      intval(2) = eta_u_dim
      intval(1) = xi_u_dim
      iret = nf_def_var(ncid, 'lat_u', NF_DOUBLE, 2, intval,
     & lat_u_id)
      iret = nf_def_var(ncid, 'lon_u', NF_DOUBLE, 2, intval,
     & lon_u_id)
      intval(2) = eta_v_dim
      intval(1) = xi_v_dim
      iret = nf_def_var(ncid, 'lat_v', NF_DOUBLE, 2, intval,
     & lat_v_id)
      iret = nf_def_var(ncid, 'lon_v', NF_DOUBLE, 2, intval,
     & lon_v_id)
      iret = nf_def_var(ncid, 'ocean_time', NF_DOUBLE, 1, 
     1time_dim, ocean_time_id)
      call check_err(iret)
      intval(3) = time_dim
      intval(2) = eta_rho_dim
      intval(1) = xi_rho_dim
      iret = nf_def_var(ncid, 'zeta', NF_DOUBLE, 3, intval,zeta_id)
      call check_err(iret)
      intval(3) = time_dim
      intval(2) = eta_u_dim
      intval(1) = xi_u_dim
      iret = nf_def_var(ncid, 'ubar', NF_DOUBLE, 3,intval , ubar_id)
      call check_err(iret)
      intval(3) = time_dim
      intval(2) = eta_v_dim
      intval(1) = xi_v_dim
      iret = nf_def_var(ncid, 'vbar', NF_DOUBLE, 3,intval, vbar_id)
      call check_err(iret)
      intval(4) = time_dim
      intval(3) = s_rho_dim
      intval(2) = eta_u_dim
      intval(1) = xi_u_dim
      iret = nf_def_var(ncid, 'u', NF_DOUBLE, 4, intval, u_id)
      call check_err(iret)
      intval(4) = time_dim
      intval(3) = s_rho_dim
      intval(2) = eta_v_dim
      intval(1) = xi_v_dim
      iret = nf_def_var(ncid, 'v', NF_DOUBLE, 4, intval, v_id)
      call check_err(iret)
      intval(4) = time_dim
      intval(3) = s_rho_dim
      intval(2) = eta_rho_dim
      intval(1) = xi_rho_dim
      iret = nf_def_var(ncid, 'temp', NF_DOUBLE,4,intval, temp_id)
      call check_err(iret)
      iret = nf_def_var(ncid, 'salt', NF_DOUBLE,4,intval, salt_id)
      call check_err(iret)


* assign attributes
!      iret = nf_put_att_text(ncid, spherical_id, 'long_name', 24, 'grid 
!     1type logical switch')
!      call check_err(iret)
!      iret = nf_put_att_text(ncid, spherical_id, 'flag_values', 4, 'T, F
!     1')
!      call check_err(iret)
!      iret = nf_put_att_text(ncid, spherical_id, 'flag_meanings', 19, 's
!     1pherical Cartesian')
!      call check_err(iret)
      iret = nf_put_att_text(ncid, ocean_time_id, 'long_name', 25, 'time
     1 since initialization')
      call check_err(iret)
150   format(a14,I4.4,a1,I2.2,a1,I2.2,1x,I2.2,a6)      
        WRITE(TEXT,150)'seconds since ',base_date(1),'-',
     &  base_date(2),'-',base_date(3),base_date(4),':00:00'
        LEN=LEN_TRIM(TEXT)
	print *,TRIM(TEXT)
        iret=nf_put_att_text(ncid, ocean_time_id,'units',
     &       LEN,TRIM(TEXT))
      call check_err(iret)
      iret = nf_put_att_text(ncid, ocean_time_id, 'calendar', 24, '365.0
     1 days in every year')
!        iret=nf_put_att_int(ncid,ocean_time_id,'base_date',NF_INT,
!     &  4,base_date)

     
      call check_err(iret)
      iret = nf_put_att_text(ncid, zeta_id, 'long_name', 12, 'free-surfa
     1ce')
      call check_err(iret)
      iret = nf_put_att_text(ncid, zeta_id, 'units', 5, 'meter')
      call check_err(iret)
      iret = nf_put_att_text(ncid, zeta_id, 'time', 10, 'ocean_time')
      call check_err(iret)
  !    iret = nf_put_att_text(ncid, zeta_id, 'coordinates', 15,
  !   &  'lat_rho lon_rho')
      call check_err(iret)
      iret = nf_put_att_text(ncid, ubar_id, 'long_name', 42, 'vertically
     1 integrated u-momentum component')
      call check_err(iret)
      iret = nf_put_att_text(ncid, ubar_id, 'units', 14, 'meter second-1
     1')
      call check_err(iret)
      iret = nf_put_att_text(ncid, ubar_id, 'time', 10, 'ocean_time')
      call check_err(iret)
  !    iret = nf_put_att_text(ncid, ubar_id, 'coordinates', 11,
  !   &  'lat_u lon_u')
      call check_err(iret)
      iret = nf_put_att_text(ncid, vbar_id, 'long_name', 42, 'vertically
     1 integrated v-momentum component')
      call check_err(iret)
      iret = nf_put_att_text(ncid, vbar_id, 'units', 14, 'meter second-1
     1')
      call check_err(iret)
      iret = nf_put_att_text(ncid, vbar_id, 'time', 10, 'ocean_time')
      call check_err(iret)
  !    iret = nf_put_att_text(ncid, vbar_id, 'coordinates', 11,
  !   &  'lat_v lon_v')
      call check_err(iret)
      iret = nf_put_att_text(ncid, u_id, 'long_name', 20, 'u-momentum co
     1mponent')
      call check_err(iret)
      iret = nf_put_att_text(ncid, u_id, 'units', 14, 'meter second-1')
      call check_err(iret)
      iret = nf_put_att_text(ncid, u_id, 'time', 10, 'ocean_time')
      call check_err(iret)
  !    iret = nf_put_att_text(ncid, u_id, 'coordinates', 11,
  !   &  'lat_u lon_u')
      call check_err(iret)
      iret = nf_put_att_text(ncid, v_id, 'long_name', 20, 'v-momentum co
     1mponent')
      call check_err(iret)
      iret = nf_put_att_text(ncid, v_id, 'units', 14, 'meter second-1')
      call check_err(iret)
      iret = nf_put_att_text(ncid, v_id, 'time', 10, 'ocean_time')
      call check_err(iret)
  !    iret = nf_put_att_text(ncid, v_id, 'coordinates', 11,
  !   &  'lat_v lon_v')
      call check_err(iret)
      iret = nf_put_att_text(ncid, temp_id, 'long_name', 21, 'potential 
     1temperature')
      call check_err(iret)
      iret = nf_put_att_text(ncid, temp_id, 'units', 7, 'Celsius')
      call check_err(iret)
      iret = nf_put_att_text(ncid, temp_id, 'time', 10, 'ocean_time')
      call check_err(iret)
  !    iret = nf_put_att_text(ncid, temp_id, 'coordinates', 15,
  !   &'lat_rho lon_rho')
      call check_err(iret)
      iret = nf_put_att_text(ncid, salt_id, 'long_name', 8, 'salinity')
      call check_err(iret)
      iret = nf_put_att_text(ncid, salt_id, 'time', 10, 'ocean_time')
      call check_err(iret)
 !     iret = nf_put_att_text(ncid, salt_id, 'coordinates', 15,
 !    & 'lat_rho lon_rho')
      call check_err(iret)

      TEXT='Latitude location of RHO-points'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, lat_rho_id, 'long_name',
     &  LEN,TRIM(TEXT)) 
      TEXT='degrees'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, lat_rho_id, 'units',
     &  LEN,TRIM(TEXT)) 

      TEXT='Longitude location of RHO-points'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, lon_rho_id, 'long_name',
     &  LEN,TRIM(TEXT)) 
      TEXT='degrees'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, lon_rho_id, 'units',
     &  LEN,TRIM(TEXT)) 

      TEXT='Latitude location of U-points'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, lat_u_id, 'long_name',
     &  LEN,TRIM(TEXT)) 
      TEXT='degrees'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, lat_u_id, 'units',
     &  LEN,TRIM(TEXT)) 
      TEXT='Longitude location of U-points'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, lon_u_id, 'long_name',
     &  LEN,TRIM(TEXT)) 
      TEXT='degrees'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, lon_u_id, 'units',
     &  LEN,TRIM(TEXT)) 
      TEXT='Latitude location of V-points'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, lat_v_id, 'long_name',
     &  LEN,TRIM(TEXT)) 
      TEXT='degrees'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, lat_v_id, 'units',
     &  LEN,TRIM(TEXT)) 
      TEXT='Longitude location of V-points'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, lon_v_id, 'long_name',
     &  LEN,TRIM(TEXT)) 
      TEXT='degrees'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, lon_v_id, 'units',
     &  LEN,TRIM(TEXT)) 


C Global Attributes
      TEXT=trim(globalstr(1))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid,NF_GLOBAL ,'type', 
     &       LEN,TRIM(TEXT))
      TEXT=trim(globalstr(2))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, NF_GLOBAL,'title', 
     &       LEN,TRIM(TEXT))
      TEXT=trim(globalstr(3))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, NF_GLOBAL,'WL_Velocity_source', 
     &       LEN,TRIM(TEXT))
      TEXT=trim(globalstr(4))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, NF_GLOBAL,'T_S_source', 
     &       LEN,TRIM(TEXT))

      TEXT=trim(globalstr(5))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, NF_GLOBAL,'history', 
     &       LEN,TRIM(TEXT))
!      TEXT='Created by Aijun Zhang, OD/CO-OPS/NOS/NOAA'
      TEXT=trim(globalstr(6))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, NF_GLOBAL,'reference', 
     &       LEN,TRIM(TEXT))
! * leave define mode
      iret = nf_enddef(ncid)
      call check_err(iret)
       
!* Write record variables
      CORNER(1) = 1
      CORNER(2) = 1
      CORNER(3) = 1
      CORNER(4) = 1
      spherical="T"
      Vtransform=1
      Vstretching=1
      
      CORNER(1) = 1
      COUNT(1)=1
      iret=nf_put_vara_text(ncid,spherical_id,CORNER,COUNT,spherical)
      call check_err(iret)
      COUNT(1)=1
      status=nf_put_vara_INT(ncid,Vtransform_id,CORNER,COUNT,
     &	Vtransform)
      call check_err(iret)
      COUNT(1)=1
      status=nf_put_vara_INT(ncid,Vstretching_id,CORNER,COUNT,
     &	Vstretching)
      call check_err(iret)
      COUNT(1)=1
      print *,'theta_s= ',theta_s,theta_b,Tcline,hc
      status=nf_put_vara_double(ncid,theta_s_id,CORNER,COUNT,
     &	theta_s)
      call check_err(iret)
      COUNT(1)=1
      status=nf_put_vara_double(ncid,theta_b_id,CORNER,COUNT,
     &	theta_b)
      call check_err(iret)
      COUNT(1)=1
      status=nf_put_vara_double(ncid,Tcline_id,CORNER,COUNT,
     &	Tcline)
      call check_err(iret)
      COUNT(1)=1
      status=nf_put_vara_double(ncid,hc_id,CORNER,COUNT,
     &	hc)
      call check_err(iret)
        COUNT(1)=s_rho_len
        status=nf_put_vara_double(ncid,s_rho_id,CORNER,COUNT,
     &	s_rho)
      call check_err(iret)
        COUNT(1)=s_w_len
        status=nf_put_vara_double(ncid,s_w_id,CORNER,COUNT,
     &	s_w)
      call check_err(iret)
        COUNT(1)=s_rho_len
        status=nf_put_vara_double(ncid,Cs_r_id,CORNER,COUNT,
     &	Cs_r)
      call check_err(iret)
        COUNT(1)=s_w_len
        status=nf_put_vara_double(ncid,Cs_w_id,CORNER,COUNT,
     &	Cs_w)
      call check_err(iret)
        COUNT(1)=xi_rho_len
        COUNT(2)=eta_rho_len
        status=nf_put_vara_real(ncid,h_id,CORNER,COUNT,h)




        CORNER(1) = 1
        COUNT(1)=time_len
        status=nf_put_vara_real(ncid,ocean_time_id,CORNER,COUNT,
     &	ocean_time)
      call check_err(iret)
        CORNER(1) = 1
        CORNER(2) = 1
        CORNER(3) = 1
        CORNER(4) = 1
	
        COUNT(1)=xi_rho_len
        COUNT(2)=eta_rho_len
        status=nf_put_vara_real(ncid,lat_rho_id,CORNER,COUNT,lat_rho)
        status=nf_put_vara_real(ncid,lon_rho_id,CORNER,COUNT,lon_rho)
        COUNT(1)=xi_u_len
        COUNT(2)=eta_u_len
        status=nf_put_vara_real(ncid,lat_u_id,CORNER,COUNT,lat_u)
        status=nf_put_vara_real(ncid,lon_u_id,CORNER,COUNT,lon_u)
        COUNT(1)=xi_v_len
        COUNT(2)=eta_v_len
        status=nf_put_vara_real(ncid,lat_v_id,CORNER,COUNT,lat_v)
        status=nf_put_vara_real(ncid,lon_v_id,CORNER,COUNT,lon_v)

        COUNT(1)=xi_rho_len
        COUNT(2)=eta_rho_len
        COUNT(3)=time_len
        status=nf_put_vara_real(ncid,zeta_id,CORNER,COUNT,zeta)
      call check_err(iret)
        COUNT(1)=xi_u_len
        COUNT(2)=eta_u_len
        COUNT(3)=time_len
        status=nf_put_vara_real(ncid,ubar_id,CORNER,COUNT,ubar)
      call check_err(iret)
        COUNT(1)=xi_v_len
        COUNT(2)=eta_v_len
        COUNT(3)=time_len
        status=nf_put_vara_real(ncid,vbar_id,CORNER,COUNT,vbar)
      call check_err(iret)
        COUNT(1)=xi_u_len
        COUNT(2)=eta_u_len
        COUNT(3)=s_rho_len
        COUNT(4)=time_len
        status=nf_put_vara_real(ncid,u_id,CORNER,COUNT,u)
        COUNT(1)=xi_v_len
        COUNT(2)=eta_v_len
        COUNT(3)=s_rho_len
        COUNT(4)=time_len
        status=nf_put_vara_real(ncid,v_id,CORNER,COUNT,v)

        COUNT(1)=xi_rho_len
        COUNT(2)=eta_rho_len
        COUNT(3)=s_rho_len
        COUNT(4)=time_len
        status=nf_put_vara_real(ncid,temp_id,CORNER,COUNT,temp)
        status=nf_put_vara_real(ncid,salt_id,CORNER,COUNT,salt)

      iret = nf_close(ncid)
      call check_err(iret)
      return
      end
      subroutine check_err(iret)
      integer iret
      include 'netcdf.inc'
      if (iret .ne. NF_NOERR) then
      print *, nf_strerror(iret)
      stop
      endif
      end


