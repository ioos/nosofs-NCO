!     Code to generate new *3D.th (binary format) from interpolating in time from an old *3D.th
!     Input:
!            (1) th.old (old *3D.th);
!            (2) timeint.in (see also a sample 'timeint.in'):
!                1st line: ndays (total # of days), nvrt (1 for elev; use 2*nvrt for hvel as
!                          it has 2 components; for T,S use nvrt);
!                2nd line: nope nond(1:nope) - nope is the # of boundary
!                          segments that need *3D.th,
!                          nond(1:nope) is the # of nodes on each segment
!                3rd line: new dt 
!     Output: th.new (the new *3D.th)

!     ifort -Bstatic -assume byterecl -O3 -o timeint_3Dth2 timeint_3Dth2.f90
      program riverforcing
      parameter(nbyte=4)
      allocatable :: th(:),th1(:),th2(:),nond(:)

      open(21,file='timeint.in',status='old')
      read(21,*)rndays,nvrt
      read(21,*)nope
      allocate(nond(nope),stat=istat)

      rewind(21)
      read(21,*)
      read(21,*)nope,nond(:)
      nond0=sum(nond(:))
      allocate(th(nond0*nvrt),th1(nond0*nvrt),th2(nond0*nvrt),stat=istat)
      if(istat/=0) stop 'Failed too alloc.'

!      do i=1,nope
!        if(nope>mnope.or.nond(i)>mnond) then
!          print*, 'nope >mnope'
!          stop
!        endif
!      enddo !i
      read(21,*)dt
      close(21)

      irecl=nbyte*(1+nond0*nvrt)
      open(17,file='th.old',access='direct',recl=irecl,status='old')
      open(18,file='th.new',access='direct',recl=irecl,status='replace')
      read(17,rec=1)dt0,th2(:)
      tt0=rndays*86400
      nt0=tt0/dt0+1.e-5
      nt1=tt0/dt+1.e-5
!     Read first step in case dt<dt0
      timeout=dt
      ncount=0
      if(dt<dt0) then
        do
          ncount=ncount+1
          write(18,rec=ncount)timeout,th2(:)
          timeout=timeout+dt
          if(timeout>=dt0) exit
        enddo
      endif !dt.lt.dt0

!     timeout>= dt0
!     Interpolate
!      rewind(17)
      time1=0
      irec=0
      do it=1,nt0
        irec=irec+1
        read(17,rec=irec)time2,th2(:)
        if(abs(time2-it*dt0)>1.e-4) then
          print*, 'Time stamp wrong'
          stop
        endif

        if(it>1.and.timeout>=time1.and.timeout<=time2) then
          do
            rat=(timeout-time1)/dt0
            if(rat<0.or.rat>1) then
              print*, 'ratio out of bound:',rat,it
              stop
            endif
            ncount=ncount+1
            th=th2*rat+th1*(1-rat)
            write(18,rec=ncount)timeout,th(:)
            timeout=timeout+dt
            if(timeout>time2) exit
          enddo
        endif !it.gt.1 etc

        th1=th2
        time1=time2
      enddo !it=1,nt0

101   format(i12,6000(1x,f16.4))
      
      if(ncount/=nt1) then
        print*, 'Miscount:',ncount,nt1
        stop
      endif

      stop
      end 
