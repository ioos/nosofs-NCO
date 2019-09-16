C
C   Documentation  for nos_pred.f 
C
C----------------------------------------------------------------------------------
C
C Fortran Program Name: nos_pred.f
C
C Directory Location:     /COMF/oqcs/sorc
C
C Technical Contact(s):   Name:  Aijun Zhang             Org:  NOS/CO-OPS
C                         Phone: 301-713-2890x127      E-Mail: aijun.zhang@noaa.gov
C
C Abstract:
C               This program is modified from pred.f of Chris Zervas so that 
C		it can make prediction of multiple years. 
C		Change call CONCTJ and CONJTC to call julian.
C  		Also call equarg.f to calculate XODE and VPU, instead of reading 
C		from data file 'yr'.
C
C
C Usage:   nos_pred "$BEGINDATE" "$ENDDATE" $KINDAT $DELT $CONV $XMAJOR $filein $fileout
C
C	   Called by 
C
C Input Parameters:  
C		BEGINDATE="200801011230"
C         	ENDDATE=  "200812311230"
C         	KINDAT=1, for current prediction; =2 for water level prediction
C         	DELT is time interval of output time series in hours
C         	CONV: Units convertion of predicted variable
C         	XMAJOR is principle current direction in degrees
C         	filein is input file name which includes tide constituents
C         	fileout is output name which contains predicted water level or current time series
C		
C                              		wl
C    		2003 01 02 00 00 00     0.5085
C    		2003 01 02 00 06 00     0.5169
C                            		sp        dir  
C   		2003 01 02 00 00 00     0.7091   258.0500
C   		2003 01 02 00 06 00     0.7237   258.3601
C	   
C    		1    2    1.   0.    0              ! nsta ipredk conv tconv il2
C   		tss.out                                 ! Output time series file
C   		0  4  15 0  6 30 0 0.1 1998 1998 106.0   IEL,IMMS,IDDS,TIME,IMME,IDDE,TIMEL,DELT,IYRS,IYRE,XMAJOR
C   		Harmonic Analysis of Data in  325j4b05.dat                                      
C   		29-Day H.A.  Beginning  4-15-1998  at Hour 17.30  along 106 degrees             
C   		12718
C       	1931621828140022115186641641117973376 68733224 81743495 5868 163
C       	2    0   0  804  92    0   0 36211666  4431590    0   0 24821455
C       	3  3513257  6521961    0   0  5803436  6463317    0   0    0   0
C       	0   0    0   0    0   0  3113546 15863554  8262104  1122127
C       	5  213  13 39053385    0   0    0   0 26691641    0   0 38082138
C       	6 28911704    0   0
C   		Harmonic Analysis of Data in  325j4b05.dat                                      
C   		29-Day H.A.  Beginning  4-15-1998  at Hour 17.30  along 196 degrees             
C 		-5820
C       	1 57222694 14073452 22192976 1203 154 47261638 1292 478 26243445
C       	2    0   0  658 796    0   0  4302938  6232349    0   0  2953258
C       	3   563430   403046    0   0   92 316  1023593    0   0    0   0
C       	4    0   0    0   0    0   0   49 617  251 639   833422   113482
C       	5   34 800  398 178    0   0    0   0  3172976    0   0  3833513
C       	6 12661394    0   0C
C
C Language:     lf95         
C
C Compiling/Linking Syntax:    ncepxlf nos_pred.f -o nos_pred.x
C
C Target Computer:   COMF machine, DEW/MIST at NCEP
C
C Estimated Execution Time: 
C
C Input Files:
C  Name             Directory  Location                   Description
C
C Output Files:
C  Name            Directory Location                    Description
C
C Libraries Used:     
C
C Error Conditions:
C
C Revisions:
C         Date                  Author                Description
C       09-26-2008              A ZHANG         put subroutines of "julian.f and gregorian.f in 
C                                               and commentted out the following include statements
C                                               include './library/julian.f'
C                                               include './library/gregorian.f'
C                                               For thansition to NCEP 
C Remarks: 
C
C -----------------------------------------------------------------------------------------------------




!      PROGRAM PREDK7

C          Purpose:
C                 1. Calculates residuals
C                 2. Calculates a predicted series
C **************************************************************
C
C               UNIT=10 is the output file written in ASCII format.
C               UNIT=11 is the input observations in CDF or ASCII format.
C
C
C
C      UNIT=         PATH NAME=                    INPUT      OUTPUT
C
C       5       Redirected std. input (< pathname)   X
C       6                                                       X
C      10                                                       X
C      11                                            X
C ************************************************************************************
C      NSTA =     NUMBER OF STATIONS TO PREDICT
C      CONV =     FACTOR FOR CONVERTING PREDICTED TIME SERIES TO NEW UNITS
C **** Conversion options for time ***
C
C     TCONV = 0   NO CONVERSION OF PREDICTED TIMES
C     TCONV = TIME MERIDIAN FOR WHICH THE KAPPA PRIMES IN THE HARMONIC
C             CONSTANTS WERE DERIVED. THIS OPTION IS USED TO CONVERT
C             THE TIMES TO GREENWICH IF THE CONSTANTS WERE CALCULATED
C             FOR A LOCAL TIME MERIDIAN. A REASON FOR USING THIS OPTION
C             IS IF COMPARISONS WITH ACTUAL DATA IS REQUIRED WHICH IS
C             IN GREENWICH TIME AND THE HARMONIC CONSTANTS WERE OBTAINED
C             FROM THE PREDICTION BRANCH WHERE HARMONIC CONSTANTS ARE
C             FOR LOCAL MERIDIANS ALWAYS
C
C     TCONV changed to hours to shift predicted time series 
C     (positive = later) --- Chris Zervas (7/97)
C
c **** conversion option for using 2mn2 in the harmonic constants  
c     file versus the standard L2
c
c     ***note*** it is node (33) which is re-calculated
c
c     IL2 = 0 --- use the standard <L2> harmonic constants
c           1 --- use the <2MN2> harmonic constants
c
C
C*****************************************************************
C
C     **NOTE(1)** USE FORMAT NO. 531 FOR HARMONIC CONSTANTS
C        TEMPORARY FORMULATION FORMAT 532 IS USED
C
C******************************************************************
C
C
C     IPREDK = 0 -- USED TO CALCULATE THE DIFFERENCES BETWEEN A SET OF
C                   PREDICTED AND OBSERVED SERIES. THIS CALCULATION IS
C                   DONE USING AN INPUT OF OBSERVED VALUES IN ASCII
C                   FORMAT AND OUTPUT IS WRITTEN IN AN ASCII FILE
C
C     IPREDK = 1 -- USED TO CALCULATE THE DIFFERENCES BETWEEN A SET OF
C                   PREDICTED AND OBSERVED SERIES. THIS CALCULATION IS
C                   DONE USING AN INPUT OF OBSERVED VALUES IN CDF
C                   FORMAT AND OUTPUT IS WRITTEN IN AN ASCII FILE
C
C     **NOTE(1)** STARTING TIME FOR PREDICTIONS IS SET EQUAL TO
C                 THE STARTING TIME DEFINED BY THE VALUE OF ISTART
C     **NOTE(2)** NOS1 IS CALCULATED BY THE TIMES OF THE OBSERVED
C                 SERIES
C
C
C      IPRED = 2 -- USED TO CALCULATE A PREDICTED SERIES
C
C     **NOTE(3)** THE PREDICTIONS AND DIFFERENCES CAN NOT BE PERFORMED
C                 ACROSS 2 YEARS
C     **NOTE(4)** THE YEARS FOR EACH NSTA MUST BE THE SAME
C     **NOTE(5)** USE FORMAT NO. 532 FOR HARMONIC CONSTANTS
C ***************************** modified IEL to match KINDAT
C     IEL =    THE ELEMENT OF THE DATA SERIES TO PERFORM CALCULATIONS
C              1   MAJOR/MINOR COMPONENTS OF VECTOR VARIABLE (I.E. CURRENT)
C              2   SCALAR VARIABLE (I.E. TIDAL HEIGHT)
C              3   TEMPERATURE (CDF INPUT FIELD)
C              4   CONDUCTIVITY (CDF INPUT FIELD)
C              6   PRESSURE (CDF INPUT FIELD)
C     IYRS=YEAR OF THE FIRST DATA POINT(CAN NOT GO ACROSS YRS)
C     IMMS=MONTH OF FIRST DATA POINT
C     IDDS=DAY OF FIRST DATA POINT
C     TIME=TIME OF FIRST DATA POINT
C     MON=MONTH OF LAST DATA POINT
C     IDDE=DAY OF LAST DATA POINT
C     TIMEL=TIME OF LAST DATA POINT
C     DELT= desired time interval in hours, delt=0.1 for 6 minutes data
C     NOS1=NUMBER OF DATA POINTS PER HOUR
C     XMAJOR ---- AXIS FOR MAJOR/MINOR COMPONENTS (MAKE SURE HARMONIC
C              CONSTANTS WERE DERIVED ALONG THIS AXIS)
C                       IF A 0.0 IS READ --- 0 DEGRESS TRUE IS
C                       ASSUMED. ALWAYS READ IN THE CONSTITUENTS
C                       FOR THE MAJOR AXIS FIRST.
C///////////////////////////////////////////////////////////////
C
C 
C 
      SUBROUTINE NOS_PRD(START_TIME,END_TIME,IEL,DELT,CONV,
     & XMAJOR,AMP9,EPOC9,cdfout,STORN,STORX)  
      PARAMETER (MXDIM=9000)
      PARAMETER (XMISS=999.)
      character*80 cdfin,cdfout,BUFFER*100,START_TIME,END_TIME
      CHARACTER*10   ALIST
      CHARACTER*80   HEAD(2)
      DIMENSION   XDATA(14,50),ALIST(37),IHEAD(34),DAT(6),SUM(6),SMN(6),
     #TIM(MXDIM),SPEED(MXDIM),DIREC(MXDIM),STORX(MXDIM),STORN(MXDIM)
      DIMENSION   AMP9(37),EPOC9(37)
      common/virt1/
     1            A(37),AMP(37),EPOC(37),XODE(114),VPU(114),
     2            XCOS(1025),ARG(37),TABHR(24),ANG(37),SPD0(37),
     3            EPOCH(37),AMPA(37),IYR(15),NUM(15),ISTA(6),NO(6),
     4            JODAYS(12),C(37)
C
      real*8 jday,jday0,jday1,jbase_date,JULIAN,yearb,monthb,dayb,hourb
      COMMON /XDATA/XDATA
      DIMENSION TSTART(4)
CCCCCCCCCCCCCC
      common /speeds/spd
      common /names/lable
      real*8 spd(180),fff(180),vau(180)
      character*10 lable(180)
      integer order(37)
      data order/1,3,2,5,21,4,22,28,24,30,13,25,12,8,16,11,27,15,14,
     1  35,37,36,34,33,20,18,10,9,19,17,32,26,7,29,6,23,31/

CCCCCCCCCCCCCC
      REAL NOS1
C     DATA (ALIST(I),I=1,37) /'M(2)','S(2)','N(2)','K(1)','M(4)','O(1)',
C    1'M(6)','MK(3)','S(4)','MN(4)','NU(2)','S(6)','MU(2)','2N(2)','OO(1
C    2)','LAMDA(2)','S(1)','M(1)','J(1)','MM','SSA','SA','MSF','MF','RHO
C    3(1)','Q(1)','T(2)','R(2)','2Q(1)','P(1)','2SM(2)','M(3)','L(2)','2
C    4MK(3)','K(2)','M(8)','MS(4)'/
      DATA (ALIST(I),I=1,37) /'M(2)      ','S(2)      ','N(2)      ',
     1                        'K(1)      ','M(4)      ','O(1)      ',
     2                        'M(6)      ','MK(3)     ','S(4)      ',
     3                        'MN(4)     ','NU(2)     ','S(6)      ',
     4                        'MU(2)     ','2N(2)     ','OO(1)     ',
     5                        'LAMDA(2)  ','S(1)      ','M(1)      ',
     6                        'J(1)      ','MM        ','SSA       ',
     7                        'SA        ','MSF       ','MF        ',
     8                        'RHO(1)    ','Q(1)      ','T(2)      ',
     9                        'R(2)      ','2Q(1)     ','P(1)      ',
     1                        '2SM(2)    ','M(3)      ','L(2)      ',
     2                        '2MK3(3)   ','K(2)      ','M(8)      ',
     3                        'MS(4)     '/
      LIN = 5
      LOUT = 6
C
!      read (lin,*)   nsta,ipredk,conv,tconv,il2
      NSTA=1
      IPREDK=2
      CONV=1.0
      TCONV=0.0
      IL2=0
C
      IF (IPREDK.GT.2.OR.IPREDK.LT.0)   then
      print*,'Error in IPREDK value'
      stop
      endif
C
C     DEVELOP COSINE TABLE
C
      H = 0.00153398078789
      DO 35 I = 1,1024
      XCOS(I) = COS(H*(I-1))
   35 CONTINUE
      XCOS(1025) = 0.0
      ms0 = 1
      CON = 1024. / 90.
C        ---
      DO 612   JOB=1,NSTA
!      read(lin,16)cdfout
16    format(a80) 
6000  continue
!      READ (LIN,*) IEL,IMMS,IDDS,TIME,IMME,IDDE,TIMEL,DELT,IYRS,IYRE,
!     1  XMAJOR
!      CALL GETARG(1,BUFFER)
      READ(START_TIME,'(i4,4I2)')IYRS,IMMS,IDDS,IHHS,MNS
      TIME=IHHS+MNS/60.
!      CALL GETARG(2,BUFFER)
      READ(END_TIME,'(i4,4I2)')IYRE,IMME,IDDE,IHHE,MNE
      TIMEL=IHHE+MNE/60.
!      CALL GETARG(3,BUFFER)
!      READ(BUFFER,*)IEL
!      CALL GETARG(4,BUFFER)
!      READ(BUFFER,*)DELT
!      CALL GETARG(5,BUFFER)
!      READ(BUFFER,*)CONV
!      CALL GETARG(6,BUFFER)
!      READ(BUFFER,*)XMAJOR
!      CALL GETARG(7,cdfin)
!      CALL GETARG(8,cdfout)
!      call ncrght(cdfin,nct)
      yearb=IYRS
      monthb=1.
      dayb=1.
      hourb=0.
      jbase_date=JULIAN(yearb,monthb,dayb,hourb)
!      stop
      NOS1=1.0/DELT   !!!  data points per hour
  527 FORMAT(2I5,F10.0,2I5,F10.0)
!      write(6,*)'run pred.f from ',IYRS,IMMS,IDDS,IHHS, ' to ',
!     1   IYRE,IMME,IDDE,IHHE
!      CALL COMPIN(MXDIM,NOS1,IYRS,IMMS,IDDS,TIME,IYRE,IMME,IDDE,TIMEL,
!     1DELT,NPTS,TSTART)
      yearb=IYRS
      monthb=IMMS
      dayb=IDDS
      hourb=IHHS+MNS/60.0
      jday0=JULIAN(yearb,monthb,dayb,hourb)
      yearb=IYRE
      monthb=IMME
      dayb=IDDE
      hourb=IHHE+MNE/60.0
      jday1=JULIAN(yearb,monthb,dayb,hourb)
      TSTART(4) = IYRS
      TSTART(3) = jday0-jbase_date
      TSTART(2) = IHHS
      TSTART(1) =MNS
      NPTS=INT((jday1-jday0)*24/DELT+1+0.1)
C****
119   CIND = NOS1
!      open(30,file=trim(cdfin) )
      DO J=1,37
        AMP(J)=AMP9(J)
        EPOC(J)=EPOC9(J)
      ENDDO

111    CONTINUE
!      READ (30,550)  HEAD(1),HEAD(2)
!      READ (30,532)DATUM,ISTA(1),NO(1),(AMP(J),EPOC(J),J=1,7),
!     1 ISTA(2),NO(2),
!     2 (AMP(J),EPOC(J),J=8,14),ISTA(3),NO(3),(AMP(J),EPOC(J),J=15,21),
!     3 ISTA(4),NO(4),(AMP(J),EPOC(J),J=22,28),ISTA(5),NO(5),(AMP(J),
!     4 EPOC(J),J=29,35),ISTA(6),NO(6),(AMP(J),EPOC(J),J=36,37)
!  112 DO 113 L = 1,5
!      IF (ISTA(L).NE.ISTA(L+1)) GO TO 451
!  113 CONTINUE
!      DO 114 L = 1,6
!      IF (NO(L).NE.L) GO TO 450
!  114 CONTINUE
C
C     CONVERT CONSTANTS IF TCONV IS NOT EQUAL TO ZERO
C     TCONV IS THE TIME MERIDIAN
C
      IF (TCONV.EQ.0.0)   GO TO 120
C
      PRINT 5902
      PRINT 550, HEAD(1),HEAD(2)
      PRINT 5902
      WRITE (LOUT,994)TCONV
  994 FORMAT (' Values of the Epochs before ',F6.2,' hour time shift')
c    1       ' THE VALUES OF THE EPOCHS BEFORE THE CONVERSION TO',
c    2       ' GREENWICH TIME'/)
c    3       ' THE VALUE OF THE LOCAL TIME MERIDIAN OF THE ORIGINAL'/
c    4       ' VALUES OF THE EPOCHS (OR THE VALUES OF KAPPA PRIME)',
c    5       ' WAS ',F10.2/)
C
!      PRINT 5902
!      PRINT 5007
!      PRINT 5902
      DO 650   J=1,37
      IF (AMP(J).EQ.0.0)   GO TO 651
!      PRINT 5000, ALIST(J),AMP(J),EPOC(J)
C
      EPOC(J) = EPOC(J) + A(J)*TCONV
  652 IF (EPOC(J).LE.360.)   GO TO 650
      EPOC(J) = EPOC(J) - 360.0
C
      GO TO 652
C
  651 continue   !!!PRINT 998, ALIST(J)
C
  650 CONTINUE

 120  if(ms0.eq.2) goto 150
CCCCCCCC  using equarg replace yrcrds.f
      length=int(jday1-jday0)+1
      call equarg (37,IYRS,1,1,365,lable(1),fff(1),vau(1))
!      call equarg (37,IYRS,IMMS,IDDS,length,lable(1),fff(1),vau(1))
      do j=1,37
!        VPU(J)=VAU(ORDER(J))
!        XODE(J)=FFF(ORDER(J))
         VPU(J)=VAU(J)
         XODE(J)=FFF(J)
!
!       WRITE(6,'(I2,2x,A10,1x,F12.7,2X,F5.1,2X,F6.4,2x,F10.5,2x,F8.2)')
!     1  J, ALIST(J),A(J),VPU(J),XODE(J),fff(j),vau(j)  !!AMP(J),EPOC(j)
      end do

CCCCCCCCCCCCCCC
  150 DO 155 J = 1,37
      C(J) = A(J) * (CON/CIND)
  155 CONTINUE
C     SET UP TABLES FOR NON-ZERO CONSTITUENTS
  160 K = 0
      DO 180  J = 1,37
  161 IF (AMP(J).EQ.0.0) GO TO 180
  170 K = K + 1
      AMPA(K) = AMP(J) * XODE(J)
      TEMX = VPU(J) - EPOC(J)
      IF (TEMX .GE. 0.) GO TO 171
      TEMX = TEMX + 360.
  171 EPOCH(K) = TEMX * CON
      SPD0(K) = C(J)
  180 CONTINUE
      NOCON = K
C
C     OPERATING TABLES NOW STORED AS AMPA(K),EPOCH(K),SPD0(K)
C
C**** CHECK LENGTH OF SERIES
C
  700 IF (NPTS.LE.MXDIM)   GO TO 191
      NPTS = MXDIM
      PRINT 100
      PRINT 1190, NPTS
191   CONTINUE
!  191 WRITE (LOUT,1002)   NPTS
 1002 FORMAT (/' Total number of prediction times = ',I10)
C
C**** DETERMINE FIRST HOUR OF TIME PERIOD  at 00:00 of Jan. 1, first=0.0
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC modified by zaj on May 13, 2004
!      CALL CONCTJ (ILJD,IMMS,IDDS,IYRS)
!      FIRST=((ILJD-1)*24.+TIME)* CIND
      FIRST=(jday0-jbase_date)*24.* CIND
!      write(6,*)'Time of first prediction=',IYRS,IMMS,IDDS,IHHS,MNS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc      
      DO 220   J=1,MXDIM
      STORX(J) = 0.
  220 CONTINUE
      KOUNT = 0
      KT = 0
C
C        ---
   43 DO 380 K = 1,NPTS
C        ---
C
C**** THE PREDICTION/TIME STEP IS STORED IN VARIABLE PRED
C
        PRED = 0.0
C
        IF (KOUNT.GT.0) GO TO 260
        KOUNT = 1
  231   IF (NOCON.EQ.0)   GO TO 375
        DO 250 J = 1,NOCON
          ARGU = SPD0(J) * FIRST + EPOCH(J)
          ARG(J) = AMOD(ARGU,4096.)
  250   CONTINUE
        GO TO 290
  260   IF (NOCON.EQ.0)   GO TO 375
        DO 280 J = 1,NOCON
          ARG(J) = ARG(J) + SPD0(J)
  270     IF (ARG(J).LT.4096.) GO TO 280
          ARG(J) = ARG(J) - 4096.
          GO TO 270
  280   CONTINUE
  290   DO 374 J = 1,NOCON
          IF (ARG(J) - 1024.)  320,320,300
  300     IF (ARG(J) - 2048.)  350,350,310
  310     IF (ARG(J) - 3072.)  360,360,330
  320     ANG(J) = ARG(J)
          GO TO 340
  330     ANG(J) = 4096. - ARG(J)
  340     NP = ANG(J) + 1.5
C     ----   ----   -----------------
          PRED = PRED + AMPA(J) * XCOS(NP)
C     ----   ----   ------------------
          GO TO 374
  350     ANG(J) = 2048. - ARG(J)
          GO TO 370
  360     ANG(J) = ARG(J) - 2048.
  370     NP = ANG(J) + 1.5
          PRED = PRED - AMPA(J) * XCOS(NP)
  374   CONTINUE
  375   IF (K.NE.NPTS) GO TO 381
        IF (IPREDK.EQ.0) GO TO 381
        IF (KT.EQ.1) GO TO 378
        FIRST = FIRST + NPTS - 1.
        KT = 1
        CHECK = PRED
        PRED = 0.0
        GO TO 231
  378   CKSUM = CHECK - PRED
C
C**** CONVERT RESULTS ACCORDING TO CONV
C
  381   PRED = (PRED + DATUM) * CONV
C
C**** STORE THE RESULTS
C
  621   STORX(K) = PRED
C ---
  380 CONTINUE
!      write(6,*)'it is ok'
!      stop
C ---
      NOS2 = 0
      IF(IEL.EQ.1) NOS2 = 1
      IF(IEL.EQ.1.AND.XMAJOR.NE.0) NOS2 = 2
c     PRINT 5902
c     PRINT 550, HEAD(1),HEAD(2)
c     PRINT 5902
!      PRINT 560, IYR1,DATUM,NOCON,CKSUM
      IF (NOS2.EQ.0) GO TO 5004
      IF(ms0.EQ.2) GO TO 4996
      IF(NOS2.EQ.1) GO TO 4995
      PRINT 5006
      GO TO 5004
 4995 PRINT 5008
      GO TO 5004
 4996 IF(NOS2.EQ.1) GO TO 5002
      PRINT 5011
      GO TO 5004
 5002 PRINT 5009
 5004 PRINT 5902
!      IF(TCONV.EQ.0.0) PRINT 5007
!      PRINT 5902
      DO 5005 IZ =1,37
      IF(AMP(IZ).EQ.0.0) GO TO 4990
!      PRINT 5000,ALIST(IZ),AMP(IZ),EPOC(IZ)
      GO TO 5005
 4990 continue     !!!PRINT 998, ALIST(IZ)
  998 FORMAT (' ',A10)
 5005 CONTINUE
      IF (NOS2)400,400,395
  395 IF (ms0.EQ.2) GO TO 410
      ms0 = 2
C
C**** STORES THE MAJOR AXIS
C
  400 DO 1395 I = 1,NPTS
      STORN(I) = STORX(I)
 1395 CONTINUE
  624 IF (NOS2) 410,410,119
  410 CONTINUE
C
C**** FORM NEW DATA ARRAY
C
      call ncrght(cdfout,nct)
      open(10,file=cdfout(1:nct),status='unknown',form='formatted')
      idet = 0
      IPRED = 1
      jday=jday0
      
C ---
  427 CONTINUE
      call GREGORIAN(jday,yearb,monthb,dayb,hourb)
      IYEAR=INT(yearb)
      ICM=int(monthb+0.001)
      ICD=INT(dayb+0.001)
      IHR=INT(hourb+0.001)
      IMN=INT((hourb-IHR)*60+0.01)
      ISEC=0
      dayj=jday-jbase_date+1.
  415 IF(IEL.EQ.1) GO TO 418
  417 continue
       write(10,571)dayj,IYEAR,ICM,ICD,IHR,IMN,storn(ipred)
!      write(10,572)IYEAR,ICM,ICD,IHR,IMN,ISEC,storn(ipred)
      go to 421
C
  418 continue 
      CALL VELDIR(STORN(IPRED),STORX(IPRED),DR,SP)
      DR = DR + XMAJOR
      IF (DR.GT.360.)   DR = DR - 360.
      u=sp*sin(dr*3.1415926/180.)
      v=sp*cos(dr*3.1415926/180.)
!      write(10,571)dayj,IYEAR,ICM,ICD,IHR,IMN,sp,dr,u,v
      write(10,572)IYEAR,ICM,ICD,IHR,IMN,ISEC,sp,dr 
  571 format(f10.5,I5,4I3,4f10.4)
  572 format(i4,5i3.2,2f11.4)
  421 IPRED = IPRED + 1
      IF (IPRED.LT.NPTS)   GO TO 424
      IF (IPRED.EQ.NPTS)   GO TO 424
      GO TO 602
C           ---
424   CONTINUE      
      jday=jday+delt/24. 
      GO TO 427
C           ---
  602 ms0 = 1
  612 CONTINUE
C ---
!      close(30)
      close(10)
!      WRITE (LOUT,1003)
 1003 FORMAT (' '///' ALL FILES CLOSED --- NORMAL TERMINATION')
C
      RETURN
!      STOP
  450 PRINT 501
      RETURN
!      STOP
  451 PRINT 502
      STOP
  452 PRINT 503
      STOP
  453 PRINT 504
      STOP
  500 FORMAT (5I5,F5.0)
  501 FORMAT (27H STATION CARDS OUT OF ORDER)
  502 FORMAT (31H STATION NUMBERS NOT CONSISTENT)
  503 FORMAT (28H YEAR NUMBERS NOT CONSISTENT)
  504 FORMAT (24H YEAR CARDS OUT OF ORDER)
!  525 FORMAT (F12.8,2I2,F10.8)
  526 FORMAT (5I5,2F10.0,i2,f5.0)   
C 531 FORMAT (F6.3,6(/2I4,7(F5.2,F5.2)))
!  532 FORMAT (F6.3,6(/2I4,7(F5.3,F4.1)))
!  533 FORMAT (I4,2I2,F4.3,F4.1,F4.3,F4.1,F4.3,F4.1,F4.3,F4.1,
!     1 F4.3,F4.1,F4.3,F4.1,F4.3,F4.1,F4.3,F4.1)
C 534 FORMAT (36I2)
C 537 FORMAT (9X,10F9.3)
C 538 FORMAT (3I4)
C 545 FORMAT (6F6.3)
C 546 FORMAT (10F6.3)
  550 FORMAT (A80)
C 551 FORMAT (9X,10F9.3)
C 552 FORMAT (1X//1X,15HPREDICTIONS FOR,2X,A9,2X,10HAT STATION,2X,I2)
C 553 FORMAT (1X//1X,15HPREDICTIONS FOR,2X,A9)
C 554 FORMAT (1X,I10,1X,I10,1X,18F6.3)
  560 FORMAT (' Year ',I4,'  Datum ',F6.3,'  No. of ',
     1'Constituents ',I4,'    checksum',F12.7/)
 2534 FORMAT (2I2,1X,2I2,1X,F5.2)
C   5 FORMAT (1X,2(I2,1X),I4,1X,F5.2,3X,I3,1X,F5.2,2X,F6.3,1X,F6.3)
C   6 FORMAT(24I3)
    7 FORMAT (1H1)
C   8 FORMAT (1X,46HLISTING OF PREDICTED NORTH AND EAST COMPONENTS/49H A
C    1ND THE CORRESPONDING VELOCITIES AND DIRECTIONS.)
C  14 FORMAT (4I5,F6.4)
C  15 FORMAT (8A10)
C  16 FORMAT (3I5,I7,I2,I10,I2,2I5)
  100 FORMAT (1H0)
C 101 FORMAT (4X,4HDATE,15X,5HSPEED,4X,10HCOMPONENTS)
C 213 FORMAT (1H+,50X,17HSPEED  IN KNOTS  /51X,25HDIRECTION IN DEGREES T
C    1RUE)
C8101 FORMAT(1X,16HDA MO YEAR  HOUR,3X,10HDIR (KNTS))
C8108 FORMAT(1H+,34X,1HN,6X,1HE/)
C8109 FORMAT (1H+,32X,5HMAJOR,2X,5HMINOR/)
 8110 FORMAT (1X/1X,31HERROR IN EITHER IPREDK OR ITYPE)
 8111 FORMAT (1X/1X,28HERROR IN EITHER IEL OR ITYPE)
 8112 FORMAT (1X/1X,20HEOF IN OBSERVED TAPE)
 8113 FORMAT (1X/1X,38HCalculated time of last data point is ,4F8.0)
 8114 FORMAT (1X/1X,38HCalculated time of first data point is,4F8.0)
 8115 FORMAT(1H1,5X,'NORMAL TERMINATION FOR IPREDK=2. THERE IS 1 FILE
     1IN ',a27)
C8201 FORMAT (2F5.3)
C 900 FORMAT (30X,F6.3,2X,F6.3)
C 901 FORMAT (30X,15HMEANS OF SERIES/33X,1HN,7X,1HE)
C 902 FORMAT(30X,27H(I.E. NONTIDAL  COMPONENTS))
C 903 FORMAT (30X,15HMEANS OF SERIES/33X,3HMAJ,5X,3HMIN)
 5902 FORMAT (1X,4H    )
C8102 FORMAT (4X,4HDATE,12X,10HMAJOR AXIS)
C8103 FORMAT (1X,16HDA MO YEAR  HOUR,3X,3HDIR,2X,5HSPEED)
C3901 FORMAT (27X,14HMEAN OF SERIES)
 5000 FORMAT (1X,A10,2X,F7.3,3X,F6.2)
 5006 FORMAT(1X,38HHarmonic Constants (Major Axis) ------)
 5011 FORMAT(1X,38HHarmonic Constants (Minor Axis) ------)
 5007 FORMAT (1X,11HConstituent,11X,5HKappa/16X,4HH(A),3X,5HPrime)
 5008 FORMAT (1X,26HHarmonic Constants (North))
 5009 FORMAT (1X,25HHarmonic Constants (East))
C5010 FORMAT (1X,16HPREDICTED VALUES)
C3224 FORMAT (47X,16HNONTIDAL CURRENT/51X,3HVEL,4X,3HDIR)
C3225 FORMAT (49X,F5.2,4X,F3.0)
C3223 FORMAT (1X,7HTFAC = ,F10.8)
 1190 FORMAT (' LENGTH OF PREDICTIONS SHORTENED TO',I10)
      RETURN
      END
