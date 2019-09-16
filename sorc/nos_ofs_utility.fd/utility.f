      SUBROUTINE VELDIR (AAVN,AAVE,AANGLE,AVEL)
      REAL*8 RADDEG
      DATA RADDEG /57.295779513802D0/
      IF(AAVN.NE.0.0) GO TO 113
      IF (AAVE) 110,111,112
  110 AANGLE = 270.0
      GO TO 117
  111 AANGLE = 0.0
      GO TO 117
  112 AANGLE = 90.0
      GO TO 117
  113 AANGLE = ATAN (AAVE/AAVN)
      AANGLE = AANGLE * RADDEG
      IF ( AAVN.GT.0.0) GO TO  114
      AANGLE = AANGLE + 180.0
      GO TO 117
  114 IF (AAVE.GT.0.0) GO TO 117
      AANGLE = AANGLE  + 360.0
  117 AVEL = SQRT (AAVE**2 + AAVN**2)
      IF(AANGLE.GE.360.0) AANGLE = AANGLE - 360.0
      RETURN
      END
C-------------------------------------------------------------------------------
      SUBROUTINE ncrght(line,nc)
c -
c - Returns the last non blank character position in a string
c -
      CHARACTER*(*) line
      ilim = LEN (line)
      DO 100 i = 1,ilim
        IF(line(i:i) .ne. ' ') THEN
          nc = i
        END IF
  100 CONTINUE
      RETURN
      END

      SUBROUTINE GREGORIAN(jday,yr,month,day,hour)
      real*8 jday,yr,month,day,hour
      INTEGER NDP(13)
      INTEGER NDM(12)
      DATA NDP/0,31,59,90,120,151,181,212,243,273,304,334,365/
      DATA NDM/31,28,31,30,31,30,31,31,30,31,30,31/
!      ENTRY DMY1(IDD,IMM,IYY,ICC,KD)
      KD=INT(jday)
      hour=(jday-KD)*24.0
      
C   KD=1 CORRESPONDS TO JANUARY 1, 0000
C!
C!  GIVEN THE (GREGORIAN) DAY#, KD, AS CALCULATED ABOVE IN THIS ROUTINE,
C!  ENTRY DMY RETURNS THE (GREGORIAN) DAY, MONTH, YEAR AND CENTURY.
C!
C!  TEST FOR VALID INPUT:
      IF(KD.LE.0) WRITE(11,5040)KD
5040  FORMAT(' KD = ',I7,'  INVALID INPUT. DMY STOP.')
C!
C!  SAVE KD
      KKD=KD
C!  CALCULATE ICC AND SUBTRACT THE NUMBER OF DAYS REPRESENTED BY ICC
C!  FROM KKD
C!  JFH IS THE NUMBER OF 400 YEAR INTERVALS UP TO KKD
C!  JCC IS THE NUMBER OF ADDITIONAL CENTURIES UP TO KKD
      JFH = KKD/146097
      KKD = KKD - JFH*146097
      IF(KKD.LT.36525)THEN
	 JCC = 0
      ELSE
	 KKD = KKD - 36525
	 JCC = 1 + KKD/36524
	 KKD = KKD - (JCC-1)*36524
      END IF
      ICC = 4*JFH + JCC
      IF(KKD.EQ.0)THEN
	 ICC = ICC-1
	 IYY = 99
	 IMM = 12
	 IDD = 31
	 yr=ICC*100+IYY
	 month=IMM
	 day=IDD
	 RETURN
      ENDIF
C!
C!  CALCULATE IYY. JFY IS THE NUMBER OF FOUR YEAR INTERVALS IN THE
C!  CURRENT CENTURY. THE FIRST FOUR YEAR INTERVAL IS SHORT (1460 DAYS
C!  RATHER THAN 1461)IF THE CURRENT CENTURY IS NOT DIVISIBLE BY 4, AND
C!  IN THIS CASE JCC.NE.0 AS CALCULATED ABOVE.
C!
C!  CALCULATE JFY:
      JFY = 0
      IF(JCC.EQ.0)GOTO 10
      IF(KKD.LT.1460)GOTO 10
      JFY = 1
      KKD = KKD - 1460
10    KK = KKD/1461
      JFY = JFY + KK
      KKD = KKD - KK*1461
C!
C!  CALCULATE JYY, THE REMAINING YEARS OF THE CURRENT CENTURY UP TO THE
C!  CURRENT DAY:
      JYY = 0
C!  THE NEXT YEAR IS NOT A LEAP YEAR IF JFY=0 AND JCC.NE.0.
      IF(JFY.EQ.0.AND.JCC.NE.0)GOTO 20
      IF(KKD.LT.366)GOTO 30
      JYY = 1
      KKD = KKD - 366
20    JYYY = KKD/365
      JYY = JYY + JYYY
      KKD = KKD - JYYY*365
30    IYY = 4*JFY + JYY
      IF(KKD.EQ.0) THEN
	 IYY=IYY-1
	 IMM=12
	 IDD=31
	 yr=ICC*100+IYY
	 month=IMM
	 day=IDD
	 RETURN
      END IF
C!
C!  SET L=1 IF WE HAVE A LEAP YEAR.
      L=0
      IF(IYY-(IYY/4)*4.NE.0)GOTO 40
      IF(IYY.EQ.0.AND.(ICC-(ICC/4)*4).NE.0)GOTO 40
      L=1
C!
C!  CALCULATE IMM AND IDD
40    IF(KKD.GT.31) GOTO 50
      IMM=1
      IDD=KKD
	 yr=ICC*100+IYY
	 month=IMM
	 day=IDD
      RETURN
C!
50    IF(KKD.GT.59)GOTO 60
      IMM = 2
      IDD = KKD-31
	 yr=ICC*100+IYY
	 month=IMM
	 day=IDD
      RETURN
C!
60    IF(KKD.GT.60)GOTO 70
      IF(L.EQ.0)GOTO 70
      IMM = 2
      IDD = 29
	 yr=ICC*100+IYY
	 month=IMM
	 day=IDD
      RETURN
C!
70    IF(L.EQ.1) KKD=KKD-1
      DO 80 I=4,13
	 IF(KKD.GT.NDP(I))GOTO 80
	 IMM = I-1
	 IDD = KKD - NDP(I-1)
	 yr=ICC*100+IYY
	 month=IMM
	 day=IDD
	 RETURN
C!
80    CONTINUE
90    WRITE(11,5050)
5050  FORMAT(' ERROR IN GREGORIAN')
      RETURN
      END
      FUNCTION JULIAN(yr,monthb,dayb,hourb)
C!
C!  GIVEN DAY,MONTH,YEAR AND CENTURY(EACH 2 DIGITS), GDAY RETURNS
C!  THE DAY#, KD BASED ON THE GREGORIAN CALENDAR.
C!  THE GREGORIAN CALENDAR, CURRENTLY 'UNIVERSALLY' IN USE WAS
C!  INITIATED IN EUROPE IN THE SIXTEENTH CENTURY. NOTE THAT GDAY
C!  IS VALID ONLY FOR GREGORIAN CALENDAR DATES.
C
C   KD=1 CORRESPONDS TO JANUARY 1, 0000 
C	
C 	NOTE THAT THE GREGORIAN REFORM OF THE JULIAN CALENDAR 
C	OMITTED 10 DAYS IN 1582 IN ORDER TO RESTORE THE DATE
C	OF THE VERNAL EQUINOX TO MARCH 21 (THE DAY AFTER
C	OCT 4, 1582 BECAME OCT 15, 1582), AND REVISED THE LEAP 
C	YEAR RULE SO THAT CENTURIAL YEARS NOT DIVISIBLE BY 400
C	WERE NOT LEAP YEARS.
C
C   THIS ROUTINE WAS WRITTEN BY EUGENE NEUFELD, AT IOS, IN JUNE 1990.
C
      real*8 yr,monthb,dayb,hourb
      real*8 Yb,mb,julian
      INTEGER NDP(13)
      INTEGER NDM(12)
      DATA NDP/0,31,59,90,120,151,181,212,243,273,304,334,365/
      DATA NDM/31,28,31,30,31,30,31,31,30,31,30,31/
      IDD=INT(dayb)
      IMM=INT(monthb)
      ICC=INT(yr/100)
      IYY=INT(yr-ICC*100)
C!
      LP = 6
C!  TEST FOR INVALID INPUT:
      IF(ICC.LT.0)THEN
	 WRITE(11,5000)ICC
	 RETURN
      ENDIF
      IF(IYY.LT.0.OR.IYY.GT.99)THEN
	 WRITE(11,5010)IYY
	 RETURN
      ENDIF
      IF(IMM.LE.0.OR.IMM.GT.12)THEN
	 WRITE(11,5020)IMM
	 RETURN
      ENDIF
      IF(IDD.LE.0)THEN
	 WRITE(11,5030)IDD
	 RETURN
      ENDIF
      IF(IMM.NE.2.AND.IDD.GT.NDM(IMM))THEN
	 WRITE(11,5030)IDD
	 STOP
      ENDIF
      IF(IMM.EQ.2.AND.IDD.GT.29)THEN
	 WRITE(11,5030)IDD
	 STOP
      ENDIF
      IF(IMM.EQ.2.AND.IDD.GT.28.AND.((IYY/4)*4-IYY.NE.0.OR.(IYY.EQ.0.AND
     .    .(ICC/4)*4-ICC.NE.0)))THEN
	 WRITE(11,5030)IDD
	 STOP
      ENDIF
5000  FORMAT(' INPUT ERROR. ICC = ',I7)
5010  FORMAT(' INPUT ERROR. IYY = ',I7)
5020  FORMAT(' INPUT ERROR. IMM = ',I7)
5030  FORMAT(' INPUT ERROR. IDD = ',I7)
C!
C!  CALCULATE DAY# OF LAST DAY OF LAST CENTURY:
      KD = ICC*36524 + (ICC+3)/4
C!
C!  CALCULATE DAY# OF LAST DAY OF LAST YEAR:
      KD = KD + IYY*365 + (IYY+3)/4
C!
C!  ADJUST FOR CENTURY RULE:
C!  (VIZ. NO LEAP-YEARS ON CENTURYS EXCEPT WHEN THE 2-DIGIT
C!  CENTURY IS DIVISIBLE BY 4.)
      IF(IYY.GT.0.AND.(ICC-(ICC/4)*4).NE.0) KD=KD-1
C!  KD NOW TRULY REPRESENTS THE DAY# OF THE LAST DAY OF LAST YEAR.
C!
C!  CALCULATE DAY# OF LAST DAY OF LAST MONTH:
      KD = KD + NDP(IMM)
C!
C!  ADJUST FOR LEAP YEARS:
      IF(IMM.GT.2.AND.((IYY/4)*4-IYY).EQ.0.AND.((IYY.NE.0).OR.
     .   (((ICC/4)*4-ICC).EQ.0)))   KD=KD+1
C!  KD NOW TRULY REPRESENTS THE DAY# OF THE LAST DAY OF THE LAST
C!  MONTH.
C!
C!  CALCULATE THE CURRENT DAY#:
      KD = KD + IDD
      julian=KD+hourb/24.
      RETURN
      END
       subroutine linear(X1,Y1,X2,Y2,X,Y)
         slope=(Y2-Y1)/(X2-X1)
         Y=Y1+slope*(X-X1)
         RETURN
         END
       subroutine lineararray(N1,X,Y,N2,X1,Y1)
       DIMENSION X(N1),Y(N1),X1(N2),Y1(N2)
       DO I=1,N1	 
         IF(X(I) .LE. X1(1) )THEN
	   Y(I)=Y1(1)
         ELSE IF(X(I) .GE. X1(N2) )THEN
	   Y(I)=Y1(N2)
	 ELSE
	   DO N=1,N2-1
             IF( (X(I) .GT. X1(N)) .AND. 
     &	         (X(I) .LE. X1(N+1))  )THEN
	         N0=N
		 GOTO 10
	     ENDIF
	   ENDDO
10	   CONTINUE  	     
           slope=(Y1(N0+1)-Y1(N0))/(X1(N0+1)-X1(N0) )
           Y(I)=Y1(N0)+slope*(X(I)-X1(N0))
	 ENDIF
       ENDDO	   
       RETURN
       END
       SUBROUTINE spline(num,x,y,XNEW,YNEW)
	DIMENSION X(NUM),Y(NUM),Y2(NUM),U(NUM)
        YP1=(Y(2)-Y(1))/(X(2)-X(1) )
        YPN=(Y(NUM)-Y(NUM-1))/(X(NUM)-X(NUM-1) ) 
!!!IBC=1 FOR NATURAL CUBIC-SPLINE BOUNDARY CONDITION , WHICH HAS ZERO SECOND DERIVATIVE ON ONE OR BOTH END POINTS
!  IBC=2 SET EITHER OF Y2(1) AND Y2(N) TO VALUES CALCULATED FROM EQUATION (3.3.5) SO AS TO MAKE THE FIRST DERIVATIVE OF THE INTERPOLATING
                !  FUNCTION HAVE A SPECIFIED VALUE ON EITHER OR BOTH END POINTS.
      IBC=1
!	IF (YP1 .GT. 1.0E30) THEN
	IF (IBC .EQ. 1)THEN
	  Y2(1)=0.0
	  U(1)=0.0
	 ELSE
	  Y2(1)=-0.5
	  U(1)=(3.0/(X(2)-X(1)))*((Y(2)-Y(1))/(X(2)-X(1))-YP1)
	ENDIF
	DO I=2,NUM-1
	 SIG=(X(I)-X(I-1))/(X(I+1)-X(I-1))
	 P=SIG*Y2(I-1)+2.0
	 Y2(I)=(SIG-1.0)/P
	 U(I)=(6.0*((Y(I+1)-Y(I))/(X(I+1)-X(I))-(Y(I)-Y(I-1))
     1   /(X(I)-X(I-1)))/(X(I+1)-X(I-1))-SIG*U(I-1))/P
	END DO
!	IF (YPN .GT. 1.0E30)THEN
	IF (IBC .EQ. 1) THEN
	  QN=0.0
	  UN=0.0
	 ELSE
	  QN=0.5
	  UN=(3.0/(X(NUM)-X(NUM-1)))*(YPN-(Y(NUM)-Y(NUM-1))
     1       /(X(NUM)-X(NUM-1)))
	END IF
	Y2(NUM)=(UN-QN*U(NUM-1))/(QN*Y2(NUM-1)+1.0)
	DO K=NUM-1,1,-1
	  Y2(K)=Y2(K)*Y2(K+1)+U(K)
	END DO
        XA=XNEW
        KLO=1
 	KHI=NUM
1	IF (KHI-KLO .GT. 1)THEN
	   K=(KHI+KLO)/2
	   IF (X(K) .GT. XA)THEN
	      KHI=K
           ELSE
              KLO=K
	   END IF
           GOTO 1
	END IF
  	H=X(KHI)-X(KLO)
	IF (ABS(H) .LT. 1.0E-5) THEN
            PRINT *, 'Bad XA input in SPLINE'
            STOP
        END IF
	A=(X(KHI)-XA)/H
	B=(XA-X(KLO))/H
	YNEW=A*Y(KLO)+B*Y(KHI)+((A**3-A)*Y2(KLO)+(B**3-B)*Y2(KHI))*
     1   (H**2)/6.0 
  121   format(f9.5,i5,4i3,3f9.4)
99      CONTINUE
        return
        end
c--------------------------------------------------------------------
      subroutine continuous(z,imx,gap,is,ifrst,ilast)
c        this program to search maximum continuous time duration in a time series
      dimension z(imx),m(imx),ifrst(imx),ilast(imx)
      m(1)=1
      ipr=0
      do i=2,imx
        gp=z(i)-z(i-1)
        m(i)=1
        if(gp.ge.gap)m(i)=0     
      enddo
!      do i=1,imx
!        write(*,*)'time,m=',I,z(i),m(i)
!      enddo
c        look for start of a series of '1's.  ifrst is i of first '1'
      is=0
      do i=1,imx-1
        if(i.eq.1.and.m(i).eq.1)then
          is=is+1
          ifrst(is)=i
        endif
        if(i.ge.1.and.m(i).eq.0.and.m(i+1).eq.1)then
          is=is+1
          ifrst(is)=i+1
        endif
      enddo
      if(ipr.eq.1)then
        write(6,*)' is=',is
        if(is.gt.0)then
          do i=1,is
            write(6,*)' i=',i,' ifrst=',ifrst(i)
          enddo
        endif
      endif

c        look for end of a series of '1's.  ilast is i of last '1'
      ie=0
      do i=1,imx-1
        if(m(i).eq.1.and.m(i+1).eq.0)then
          ie=ie+1
          ilast(ie)=i
        endif
      enddo
      if(m(imx).eq.1)then
        ie=ie+1
        ilast(ie)=i
      endif

      if(ipr.eq.1)then
        write(6,*)' ie=',ie
        if(ie.gt.0)then
          do i=1,ie
            write(6,*)' i=',i,' ilast=',ilast(i)
          enddo
        endif
      endif

c        find longest duration
      if(is.ne.ie)then
        write(6,"(/,3x,'**function tmdo. starts not equal to ends**')")
        write(6,"(5x,'is=',i4,' ie=',i4)")is,ie
        if(is.gt.0)then
          do i=1,is
            write(6,*)' i=',i,' ifrst=',ifrst(i)
          enddo
        endif
        if(ie.gt.0)then
          do i=1,ie
            write(6,*)' i=',i,' ilast=',ilast(i)
          enddo
        endif
        do i=1,imx
          write(6,"(' i=',i4,' m=',i2)")i,m(i)
        enddo
        stop
      else if(is.gt.0)then
        mdo=0
        do i=1,is
          NDIF=ilast(i)-ifrst(i)
          IF (NDIF .GT. mdo)then
            mdo=NDIF
            IMAX=I
          ENDIF  
        enddo
      endif
      Istart=ifrst(imax)
      IEND=ilast(imax)
!      write(*,*)'mdo=',mdo,IMAX,Istart,IEND 
      return
      end
C----------------------------------------------------------------------
C
        SUBROUTINE DIST(XSG,XSL,XLATL,XLONGL,D)
!----------------------------------------------------------------------------
!      function:
!      calculate the arc lenth for given two point on the spherical plane
!      input:
!           XSG,XSL,XLATL,XLONGL :are latitude and longitude of two points
!      output:
!           d:  arc lenth of two points in spherical plane in meters
!-----------------------------------------------------------------------------

        REAL LA0,LA1,LO0,LO1,LB,LL
        HAV(X)=(SIN(.5*X))**2
        AHAV(X)=2.*ASIN(SQRT(X))
 !       SQRU(X)=SQRT(ABS(X))
        DATA CONV/57.29578/
        LA0=XLATL/CONV
        LO0=XLONGL/CONV
        LA1=XSG/CONV
        LO1=XSL/CONV
        LL=LA0+LA1
        LB=LA0-LA1
        R=AHAV(HAV(LB)+COS(LA0)*COS(LA1)*HAV(LO0-LO1))
        IF(R.EQ.0.)GO TO 16
        D=3437.7468*R   ! in nautical miles = 1.852 km
        D=6371.0E03*R  ! in meters
        RETURN
 16     D=0.
        RETURN
        END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       SUBROUTINE sigma2Z_ROMS(cw_1,cw_2,h_cut,c_cut,cff_m,cff_c
     1   ,hc,thetas,sigma,H,ele,KB,zsigma)
       dimension sigma(KB),zsigma(KB)
       thetab=cff_m*tanh(c_cut*(H-h_cut))+cff_c
       DO K=1,KB
         ptheta=sinh(thetas*sigma(k))/sinh(thetas)
         rtheta=tanh(thetas*(sigma(k)+0.5))/(2.0*tanh(thetas*0.5))-0.5
         csigma=cw_1*((1.0-thetab)*ptheta+thetab*rtheta)
     1        +cw_2*ABS((1.0-(1.0+sigma(k))**(2.0*thetas))**thetab)
 !        csigma=(1.0-thetab)*ptheta+thetab*rtheta
         z0=hc*sigma(k)+(H-hc)*csigma
         zsigma(k)=z0+(1.+z0/H)*ele
         IF (zsigma(k) .LT. 0.0)zsigma(k)=-zsigma(k)
       ENDDO
       RETURN
       END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       SUBROUTINE sigma2Z_POM(sigma,H,ele,KB,zsigma)
       dimension sigma(KB),zsigma(KB)
       DO K=1,KB
         zsigma(k)=sigma(K)*(H+ele)+ele !for POM
         IF (zsigma(k) .LT. 0.0)zsigma(k)=-zsigma(k)
       ENDDO
       RETURN
       END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       SUBROUTINE sigma2Z_ROMS_FIX(sigma,H,ele,KB,zsigma
     1 ,hc,thetas,thetab,tc)
       dimension sigma(KB),zsigma(KB)
       DO K=1,KB
         ptheta=sinh(thetas*sigma(k))/sinh(thetas)
         rtheta=tanh(thetas*(sigma(k)+0.5))/(2.0*tanh(thetas*0.5))-0.5
         csigma=(1.0-thetab)*ptheta+thetab*rtheta
         z0=hc*sigma(k)+(H-hc)*csigma
         zsigma(k)=z0+(1.+z0/H)*ele
         IF (zsigma(k) .LT. 0.0)zsigma(k)=-zsigma(k)
       ENDDO
       RETURN
       END

       SUBROUTINE sigma2Z_ROMS_FIX_new(sigma,H,ele,KB,zsigma   
     1 ,hc,thetas,thetab,tc,nvtrans,nvstr)
       dimension sigma(KB),zsigma(KB)

       Vtransform=nvtrans
       Vstretching=nvstr        
       theta_s=thetas
       theta_b=thetab


       if (hc > h .and. Vtransform == 1) then
         write (*,*) "==Error: critical depth exceeds minimum bath=="
         stop
       endif 
        
       if (Vtransform < 1 .or. Vtransform > 2) then
        write (*,*) "==Error: Illegal parameter Vtransform = =="
        stop
       endif

       if (Vstretching < 1 .or. Vstretching > 4) then
        write (*,*) "==Error: Illegal parameter Vstretching = = =="
        stop
       endif

       N=KB

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       if (Vstretching == 1) then

        do k=1,N
          s=sigma(k)
          if (theta_s > 0) then
            Ptheta=sinh(theta_s*s)/sinh(theta_s)
            Rtheta=tanh(theta_s*(s+0.5))/(2.0*tanh(0.5*theta_s))-0.5
            zsigma(k)=(1.0-theta_b)*Ptheta+theta_b*Rtheta
          else
            zsigma(k)=s
          endif
        enddo
       elseif (Vstretching == 2) then 
          alfa=1.0
          beta=1.0
          do k=1,N
           s=sigma(k)
           if (theta_s > 0) then
            Csur=(1.0-cosh(theta_s*s))/(cosh(theta_s)-1.0)
            if (theta_b > 0) then
              Cbot=-1.0+sinh(theta_b*(s+1.0))/sinh(theta_b) 
              weigth=(s+1.0)**alfa*(1.0+(alfa/beta)*(1.0-(s+1.0)**beta))
              zsigma(k)=weigth*Csur+(1.0-weigth)*Cbot
            else
              zsigma(k)=Csur
            endif
           else
            zsigma(k)=s
           endif
          enddo 
       elseif (Vstretching == 3) then  
          do k=1,N
            s=sigma(k)
            if (theta_s > 0) then
              exp_s=theta_s      !  surface stretching exponent
              exp_b=theta_b      !  bottom  stretching exponent
              alpha=3            !  scale factor for all hyperbolic functions
              Cbot=log(cosh(alpha*(s+1)**exp_b))/log(cosh(alpha))-1
              Csur=-log(cosh(alpha*abs(s)**exp_s))/log(cosh(alpha))
              weight=(1-tanh( alpha*(s+.5)))/2
              zsigma(k)=weight*Cbot+(1-weight)*Csur
            else
              zsigma(k)=s
            endif
          enddo
       elseif (Vstretching == 4) then
          do k=1,N
            s=sigma(k)
            if (theta_s > 0) then
              Csur=(1.0-cosh(theta_s*s))/(cosh(theta_s)-1.0)
            else
              Csur=-s**2
            endif
            if (theta_b > 0) then
              Cbot=(exp(theta_b*Csur)-1.0)/(1.0-exp(-theta_b))
              zsigma(k)=Cbot
            else
              zsigma(k)=Csur
            endif
          enddo
       endif 
       if (Vtransform == 1) then
         do k=1,N
           z0=hc*sigma(k)+(h-hc)*zsigma(k)
           zsigma(k)=z0+(1.0+z0/h)*ele
          enddo
       elseif (Vtransform == 2) then
          do k=1,N
           z0=(hc*sigma(k)+h*zsigma(k))/(hc+h);
           zsigma(k)=ele+(h+ele)*z0;
          enddo
       endif
       do k=1,N
          IF (zsigma(k) .LT. 0.0) zsigma(k)=-zsigma(k)
       enddo

       RETURN
       END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       SUBROUTINE sigma2Z_SELFE(sigma,H,ele,KB,zsigma
     1            ,h_s,h_c,thetas,thetab)
       dimension sigma(KB),zsigma(KB),cs(kb),dcs(kb)
       NVRT=kb
       KZ=1
!       h_s=4000.0
!       thetas=4.5
!       thetab=0.95
!       hc=10.0
!       ztot(kz)=-h_s
       nsig=nvrt-kz+1 !# of S levels (including "bottom" & f.s.)
       s_con1=sinh(thetas)
!       sigma(1)=-1 !bottom
!       sigma(nsig)=0 !surface
!     Compute C(s) and C'(s)
       do k=1,nsig
         cs(k)=(1-thetab)*sinh(thetas*sigma(k))/sinh(thetas)+
     1     thetab*(tanh(thetas*(sigma(k)+0.5))-tanh(thetas*0.5))
     2     /2/tanh(thetas*0.5)
         dcs(k)=(1-thetab)*thetas*cosh(thetas*sigma(k))/
     1      sinh(thetas)+ thetab*thetas/2/tanh(thetas*0.5)
     2      /cosh(thetas*(sigma(k)+0.5))**2
       enddo !k=1,nvrt
       do k=kz,nvrt
         kin=k-kz+1
         hmod2=amin1(H,h_s)
         if(hmod2<=h_c) then
           zsigma(K)=sigma(kin)*(hmod2+ele)+ele
         else
           zsigma(K)=ele*(1+sigma(kin))+h_c*sigma(kin)
     1       +(hmod2-h_c)*cs(kin)
         endif
         IF (zsigma(k) .LT. 0.0)zsigma(k)=-zsigma(k)
       enddo !k
       RETURN
       END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         SUBROUTINE ONE2TWOD(a,b,IM,JM)
	 DIMENSION A(IM*JM),B(IM,JM)
	 DO J=1,JM
	 DO I=1,IM
           N=(J-1)*IM+I
           B(I,J)=A(N)
	 ENDDO
	 ENDDO    	       
         RETURN
	 END
         SUBROUTINE TWO2ONED(a,b,IM,JM)
	 DIMENSION A(IM*JM),B(IM,JM)
	 DO J=1,JM
	 DO I=1,IM
           N=(J-1)*IM+I
           A(N)=B(I,J)
	 ENDDO
	 ENDDO    	       
         RETURN
	 END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


