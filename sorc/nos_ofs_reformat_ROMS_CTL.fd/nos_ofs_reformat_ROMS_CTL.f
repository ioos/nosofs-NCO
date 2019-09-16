cccccccccccc Machuan Peng made some changes according to ROMS new version 3.6
cccccccccc they need LBC input variables which has two lines in tracers case

       parameter(NREC=10000)
       CHARACTER*200 FIN,ROMS_CTL,FOUT,BUFFER,VNAME*30
       CHARACTER*200 CROMS(NREC),CINP(NREC),CTMP,CTMP1
       READ(5,'(a)')FIN
       READ(5,'(a)')ROMS_CTL
       READ(5,'(a)')FOUT
       
       OPEN(10,file=TRIM(ROMS_CTL) )
       OPEN(11,file=TRIM(FIN) )
       OPEN(12,file=TRIM(FOUT) )
       I=1
10     READ(10,'(a)',END=20)BUFFER
       CROMS(I)=TRIM(ADJUSTL(BUFFER))
       I=I+1       
       GOTO 10
20     CONTINUE
       NLROMS=I-1
       PRINT *,'Number of total lines= ',NLROMS
    
       I=1
30     READ(11,'(a)',END=50)BUFFER
       CINP(I)=TRIM(ADJUSTL(BUFFER))
       I=I+1       
       GOTO 30
50     CONTINUE
       NINP=I-1
       PRINT *,'Number of total lines= ',NINP
       
       DO N=1,NLROMS
          BUFFER=TRIM(ADJUSTL(CROMS(N)))
          INX=INDEX(BUFFER,'=')
	  IF(BUFFER(1:1) .EQ. '!') THEN
             WRITE(12,'(a)')BUFFER
          ELSE
             INX=INDEX(BUFFER,'=')
	     IF(INX .EQ. 0)GOTO 80
             VNAME=BUFFER(1:INX-1)
	     IF (TRIM(VNAME) .NE. 'FRCNAME' )THEN
               DO I=1,NINP
	         CTMP1=TRIM(ADJUSTL(CINP(I)))
                 CTMP='  '
                 INX=INDEX(CTMP1,'=')
                 CTMP=CTMP1(1:INX-1)
	         IF(TRIM(CTMP) .EQ. TRIM(VNAME) )THEN
	           CTMP1='     '//TRIM(CTMP1)
                   WRITE(12,'(a)')TRIM(CTMP1)
		   GOTO 80
		 ENDIF  
	       ENDDO
c              WRITE(12,'(a)')'     '//TRIM(BUFFER)
	
               IF (TRIM(VNAME).NE.'LBC(isTvar)'.and.TRIM(VNAME).NE.
     &          'ad_LBC(isTvar)')THEN
                 WRITE(12,'(a)')'     '//TRIM(BUFFER)
	       else
                 WRITE(12,'(a)')'     '//TRIM(BUFFER)
                 BUFFER=TRIM(ADJUSTL(CROMS(N+1)))
                 WRITE(12,'(a)')'     '//TRIM(BUFFER)
	        endif 
	     ELSEIF(TRIM(VNAME) .EQ. 'FRCNAME' )THEN
               DO I=1,NINP
	         CTMP1=TRIM(ADJUSTL(CINP(I)))
                 CTMP='  '
                 INX=INDEX(CTMP1,'=')
                 CTMP=CTMP1(1:INX-1)
	         IF(TRIM(CTMP) .EQ. 'NFFILES' )THEN
                    INX=INDEX(CTMP1,'=',BACK=.TRUE.)
		    READ(CTMP1(INX+1:INX+3),'(I3)')NFFILES
		    GOTO 60
		 ENDIF
	       ENDDO
	       WRITE(*,*)'NFFILES is not found'
	       WRITE(*,*)'FRCNAME and NFFILES have to be specified'
	       STOP
60	       CONTINUE	 
               DO I=1,NINP
	         CTMP1=TRIM(ADJUSTL(CINP(I)))
                 CTMP='  '
                 INX=INDEX(CTMP1,'=')
                 CTMP=CTMP1(1:INX-1)
	         IF(TRIM(CTMP) .EQ. TRIM(VNAME) )GOTO 70
	       ENDDO
70	       CONTINUE
               I0=I
	       DO K=1,NFFILES
                 I=I0+K-1
	         CTMP1='     '//TRIM(ADJUSTL(CINP(I)))
                 WRITE(12,'(a)')TRIM(CTMP1)
	       ENDDO 
	     ENDIF    	 
	  ENDIF 
80        CONTINUE	                
	ENDDO       
        STOP
        END
