      SUBROUTINE wrt_quick (ng, tile)
!
!svn $Id: wrt_quick.F 858 2017-07-31 23:02:30Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine writes requested model fields at requested levels      !
!  into ROMS QUICKSAVE NetCDF file.                                    !
!                                                                      !
!  Notice that only momentum is affected by the full time-averaged     !
!  masks.  If applicable, these mask contains information about        !
!  river runoff and time-dependent wetting and drying variations.      !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_coupling
      USE mod_forces
      USE mod_grid
      USE mod_iounits
      USE mod_mixing
      USE mod_ncparam
      USE mod_netcdf
      USE mod_ocean
      USE mod_scalars
      USE mod_stepping
!
      USE nf_fwrite2d_mod, ONLY : nf_fwrite2d
      USE nf_fwrite3d_mod, ONLY : nf_fwrite3d
      USE omega_mod,       ONLY : scale_omega
      USE uv_rotate_mod,   ONLY : uv_rotate2d
      USE uv_rotate_mod,   ONLY : uv_rotate3d
      USE strings_mod,     ONLY : FoundError
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
      integer :: LBi, UBi, LBj, UBj
      integer :: Fcount, gfactor, gtype, status
      integer :: i, itrc, j, k
      real(r8) :: scale
      real(r8), allocatable :: Ur2d(:,:)
      real(r8), allocatable :: Vr2d(:,:)
      real(r8), allocatable :: Ur3d(:,:,:)
      real(r8), allocatable :: Vr3d(:,:,:)
      real(r8), allocatable :: Wr3d(:,:,:)
!
!-----------------------------------------------------------------------
!  Set lower and upper tile bounds and staggered variables bounds for
!  this horizontal domain partition.  Notice that if tile=-1, it will
!  set the values for the global grid.
!-----------------------------------------------------------------------
!
      integer :: Istr, IstrB, IstrP, IstrR, IstrT, IstrM, IstrU
      integer :: Iend, IendB, IendP, IendR, IendT
      integer :: Jstr, JstrB, JstrP, JstrR, JstrT, JstrM, JstrV
      integer :: Jend, JendB, JendP, JendR, JendT
      integer :: Istrm3, Istrm2, Istrm1, IstrUm2, IstrUm1
      integer :: Iendp1, Iendp2, Iendp2i, Iendp3
      integer :: Jstrm3, Jstrm2, Jstrm1, JstrVm2, JstrVm1
      integer :: Jendp1, Jendp2, Jendp2i, Jendp3
!
      Istr   =BOUNDS(ng) % Istr   (tile)
      IstrB  =BOUNDS(ng) % IstrB  (tile)
      IstrM  =BOUNDS(ng) % IstrM  (tile)
      IstrP  =BOUNDS(ng) % IstrP  (tile)
      IstrR  =BOUNDS(ng) % IstrR  (tile)
      IstrT  =BOUNDS(ng) % IstrT  (tile)
      IstrU  =BOUNDS(ng) % IstrU  (tile)
      Iend   =BOUNDS(ng) % Iend   (tile)
      IendB  =BOUNDS(ng) % IendB  (tile)
      IendP  =BOUNDS(ng) % IendP  (tile)
      IendR  =BOUNDS(ng) % IendR  (tile)
      IendT  =BOUNDS(ng) % IendT  (tile)
      Jstr   =BOUNDS(ng) % Jstr   (tile)
      JstrB  =BOUNDS(ng) % JstrB  (tile)
      JstrM  =BOUNDS(ng) % JstrM  (tile)
      JstrP  =BOUNDS(ng) % JstrP  (tile)
      JstrR  =BOUNDS(ng) % JstrR  (tile)
      JstrT  =BOUNDS(ng) % JstrT  (tile)
      JstrV  =BOUNDS(ng) % JstrV  (tile)
      Jend   =BOUNDS(ng) % Jend   (tile)
      JendB  =BOUNDS(ng) % JendB  (tile)
      JendP  =BOUNDS(ng) % JendP  (tile)
      JendR  =BOUNDS(ng) % JendR  (tile)
      JendT  =BOUNDS(ng) % JendT  (tile)
!
      Istrm3 =BOUNDS(ng) % Istrm3 (tile)            ! Istr-3
      Istrm2 =BOUNDS(ng) % Istrm2 (tile)            ! Istr-2
      Istrm1 =BOUNDS(ng) % Istrm1 (tile)            ! Istr-1
      IstrUm2=BOUNDS(ng) % IstrUm2(tile)            ! IstrU-2
      IstrUm1=BOUNDS(ng) % IstrUm1(tile)            ! IstrU-1
      Iendp1 =BOUNDS(ng) % Iendp1 (tile)            ! Iend+1
      Iendp2 =BOUNDS(ng) % Iendp2 (tile)            ! Iend+2
      Iendp2i=BOUNDS(ng) % Iendp2i(tile)            ! Iend+2 interior
      Iendp3 =BOUNDS(ng) % Iendp3 (tile)            ! Iend+3
      Jstrm3 =BOUNDS(ng) % Jstrm3 (tile)            ! Jstr-3
      Jstrm2 =BOUNDS(ng) % Jstrm2 (tile)            ! Jstr-2
      Jstrm1 =BOUNDS(ng) % Jstrm1 (tile)            ! Jstr-1
      JstrVm2=BOUNDS(ng) % JstrVm2(tile)            ! JstrV-2
      JstrVm1=BOUNDS(ng) % JstrVm1(tile)            ! JstrV-1
      Jendp1 =BOUNDS(ng) % Jendp1 (tile)            ! Jend+1
      Jendp2 =BOUNDS(ng) % Jendp2 (tile)            ! Jend+2
      Jendp2i=BOUNDS(ng) % Jendp2i(tile)            ! Jend+2 interior
      Jendp3 =BOUNDS(ng) % Jendp3 (tile)            ! Jend+3
!
      SourceFile="ROMS/Utility/wrt_quick.F"
!
      LBi=LBOUND(GRID(ng)%h,DIM=1)
      UBi=UBOUND(GRID(ng)%h,DIM=1)
      LBj=LBOUND(GRID(ng)%h,DIM=2)
      UBj=UBOUND(GRID(ng)%h,DIM=2)
!
!-----------------------------------------------------------------------
!  Write out quicksave fields.
!-----------------------------------------------------------------------
!
      IF (FoundError(exit_flag, NoError, 89,                            &
     &               "ROMS/Utility/wrt_quick.F")) RETURN
!
!  Set grid type factor to write full (gfactor=1) fields or water
!  points (gfactor=-1) fields only.
!
      gfactor=1
!
!  Set time record index.
!
      QCK(ng)%Rindex=QCK(ng)%Rindex+1
      Fcount=QCK(ng)%Fcount
      QCK(ng)%Nrec(Fcount)=QCK(ng)%Nrec(Fcount)+1
!
!  Write out model time (s).
!
      CALL netcdf_put_fvar (ng, iNLM, QCK(ng)%name,                     &
     &                      TRIM(Vname(idtime,ng)), time(ng:),          &
     &                      (/QCK(ng)%Rindex/), (/1/),                  &
     &                      ncid = QCK(ng)%ncid,                        &
     &                      varid = QCK(ng)%Vid(idtime))
      IF (FoundError(exit_flag, NoError, 114,                           &
     &               "ROMS/Utility/wrt_quick.F")) RETURN
!
!  Write out wet/dry mask at PSI-points.
!
      scale=1.0_r8
      gtype=gfactor*p2dvar
      status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idPwet),   &
     &                   QCK(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
     &                   GRID(ng) % pmask,                              &
     &                   GRID(ng) % pmask_wet,                          &
     &                   SetFillVal = .FALSE.)
      IF (FoundError(status, nf90_noerr, 157,                           &
     &               "ROMS/Utility/wrt_quick.F")) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idPwet)), QCK(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!  Write out wet/dry mask at RHO-points.
!
      scale=1.0_r8
      gtype=gfactor*r2dvar
      status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idRwet),   &
     &                   QCK(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
     &                   GRID(ng) % rmask,                              &
     &                   GRID(ng) % rmask_wet,                          &
     &                   SetFillVal = .FALSE.)
      IF (FoundError(status, nf90_noerr, 179,                           &
     &               "ROMS/Utility/wrt_quick.F")) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idRwet)), QCK(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!  Write out wet/dry mask at U-points.
!
      scale=1.0_r8
      gtype=gfactor*u2dvar
      status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idUwet),   &
     &                   QCK(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
     &                   GRID(ng) % umask,                              &
     &                   GRID(ng) % umask_wet,                          &
     &                   SetFillVal = .FALSE.)
      IF (FoundError(status, nf90_noerr, 201,                           &
     &               "ROMS/Utility/wrt_quick.F")) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idUwet)), QCK(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!  Write out wet/dry mask at V-points.
!
      scale=1.0_r8
      gtype=gfactor*v2dvar
      status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idVwet),   &
     &                   QCK(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
     &                   GRID(ng) % vmask,                              &
     &                   GRID(ng) % vmask_wet,                          &
     &                   SetFillVal = .FALSE.)
      IF (FoundError(status, nf90_noerr, 223,                           &
     &               "ROMS/Utility/wrt_quick.F")) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idVwet)), QCK(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!  Write time-varying depths of RHO-points.
!
      IF (Qout(idpthR,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idpthR), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
     &                     GRID(ng) % rmask,                            &
     &                     GRID(ng) % z_r)
        IF (FoundError(status, nf90_noerr, 247,                         &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idpthR)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write time-varying depths of U-points.
!
      IF (Qout(idpthU,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*u3dvar
        DO k=1,N(ng)
          DO j=Jstr-1,Jend+1
            DO i=IstrU-1,Iend+1
              GRID(ng)%z_v(i,j,k)=0.5_r8*(GRID(ng)%z_r(i-1,j,k)+        &
     &                                    GRID(ng)%z_r(i  ,j,k))
            END DO
          END DO
        END DO
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idpthU), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
     &                     GRID(ng) % umask,                            &
     &                     GRID(ng) % z_v)
        IF (FoundError(status, nf90_noerr, 278,                         &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idpthU)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write time-varying depths of V-points.
!
      IF (Qout(idpthV,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*v3dvar
        DO k=1,N(ng)
          DO j=JstrV-1,Jend+1
            DO i=Istr-1,Iend+1
              GRID(ng)%z_v(i,j,k)=0.5_r8*(GRID(ng)%z_r(i,j-1,k)+        &

     &                                    GRID(ng)%z_r(i,j  ,k))
            END DO
          END DO
        END DO
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idpthV), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
     &                     GRID(ng) % vmask,                            &
     &                     GRID(ng) % z_v)
        IF (FoundError(status, nf90_noerr, 309,                         &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idpthV)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write time-varying depths of W-points.
!
      IF (Qout(idpthW,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*w3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idpthW), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
     &                     GRID(ng) % rmask,                            &
     &                     GRID(ng) % z_w)
        IF (FoundError(status, nf90_noerr, 332,                         &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idpthW)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out free-surface (m)
!
      IF (Qout(idFsur,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idFsur), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     GRID(ng) % rmask,                            &
     &                     OCEAN(ng) % zeta(:,:,kstp(ng)),              &
     &                     SetFillVal = .FALSE.)
        IF (FoundError(status, nf90_noerr, 361,                         &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idFsur)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D U-momentum component (m/s).
!
      IF (Qout(idUbar,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idUbar), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     GRID(ng) % umask_full,                       &
     &                     OCEAN(ng) % ubar(:,:,kstp(ng)))
        IF (FoundError(status, nf90_noerr, 384,                         &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUbar)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D V-momentum component (m/s).
!
      IF (Qout(idVbar,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idVbar), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     GRID(ng) % vmask_full,                       &
     &                     OCEAN(ng) % vbar(:,:,kstp(ng)))
        IF (FoundError(status, nf90_noerr, 407,                         &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVbar)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D Eastward and Northward momentum components (m/s) at
!  RHO-points.
!
      IF (Qout(idu2dE,ng).and.Qout(idv2dN,ng)) THEN
        IF (.not.allocated(Ur2d)) THEN
          allocate (Ur2d(LBi:UBi,LBj:UBj))
            Ur2d(LBi:UBi,LBj:UBj)=0.0_r8
        END IF
        IF (.not.allocated(Vr2d)) THEN
          allocate (Vr2d(LBi:UBi,LBj:UBj))
            Vr2d(LBi:UBi,LBj:UBj)=0.0_r8
        END IF
        CALL uv_rotate2d (ng, tile, .FALSE., .TRUE.,                    &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    GRID(ng) % CosAngler,                         &
     &                    GRID(ng) % SinAngler,                         &
     &                    GRID(ng) % rmask_full,                        &
     &                    OCEAN(ng) % ubar(:,:,kstp(ng)),               &
     &                    OCEAN(ng) % vbar(:,:,kstp(ng)),               &
     &                    Ur2d, Vr2d)
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idu2dE), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     GRID(ng) % rmask_full,                       &
     &                     Ur2d)
        IF (FoundError(status, nf90_noerr, 450,                         &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idu2dE)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idv2dN), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     GRID(ng) % rmask_full,                       &
     &                     Vr2d)
        IF (FoundError(status, nf90_noerr, 467,                         &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idv2dN)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
        deallocate (Ur2d)
        deallocate (Vr2d)
      END IF
!
!  Write out 3D U-momentum component (m/s).
!
      IF (Qout(idUvel,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*u3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idUvel), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
     &                     GRID(ng) % umask_full,                       &
     &                     OCEAN(ng) % u(:,:,:,nrhs(ng)))
        IF (FoundError(status, nf90_noerr, 494,                         &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUvel)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D V-momentum component (m/s).
!
      IF (Qout(idVvel,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*v3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idVvel), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
     &                     GRID(ng) % vmask_full,                       &
     &                     OCEAN(ng) % v(:,:,:,nrhs(ng)))
        IF (FoundError(status, nf90_noerr, 517,                         &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVvel)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out surface U-momentum component (m/s).
!
      IF (Qout(idUsur,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idUsur), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     GRID(ng) % umask_full,                       &
     &                     OCEAN(ng) % u(:,:,N(ng),nrhs(ng)))
        IF (FoundError(status, nf90_noerr, 540,                         &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUsur)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out surface V-momentum component (m/s).
!
      IF (Qout(idVsur,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idVsur), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     GRID(ng) % vmask_full,                       &
     &                     OCEAN(ng) % v(:,:,N(ng),nrhs(ng)))
        IF (FoundError(status, nf90_noerr, 563,                         &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVsur)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D Eastward and Northward momentum components (m/s) at
!  RHO-points.
!
      IF ((Qout(idu3dE,ng).and.Qout(idv3dN,ng)).or.                     &
     &    (Qout(idUsuE,ng).and.Qout(idVsuN,ng))) THEN
        IF (.not.allocated(Ur3d)) THEN
          allocate (Ur3d(LBi:UBi,LBj:UBj,N(ng)))
          Ur3d(LBi:UBi,LBj:UBj,1:N(ng))=0.0_r8
        END IF
        IF (.not.allocated(Vr3d)) THEN
          allocate (Vr3d(LBi:UBi,LBj:UBj,N(ng)))
          Vr3d(LBi:UBi,LBj:UBj,1:N(ng))=0.0_r8
        END IF
        CALL uv_rotate3d (ng, tile, .FALSE., .TRUE.,                    &
     &                    LBi, UBi, LBj, UBj, 1, N(ng),                 &
     &                    GRID(ng) % CosAngler,                         &
     &                    GRID(ng) % SinAngler,                         &
     &                    GRID(ng) % rmask_full,                        &
     &                    OCEAN(ng) % u(:,:,:,nrhs(ng)),                &
     &                    OCEAN(ng) % v(:,:,:,nrhs(ng)),                &
     &                    Ur3d, Vr3d)
        IF ((Qout(idu3dE,ng).and.Qout(idv3dN,ng))) THEN
          scale=1.0_r8
          gtype=gfactor*r3dvar
          status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid,                    &
     &                       QCK(ng)%Vid(idu3dE),                       &
     &                       QCK(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
     &                       GRID(ng) % rmask_full,                     &
     &                       Ur3d)
          IF (FoundError(status, nf90_noerr, 609,                       &
     &                   "ROMS/Utility/wrt_quick.F")) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idu3dE)), QCK(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
          status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid,                    &
     &                       QCK(ng)%Vid(idv3dN),                       &
     &                       QCK(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
     &                       GRID(ng) % rmask_full,                     &
     &                       Vr3d)
          IF (FoundError(status, nf90_noerr, 627,                       &
     &                   "ROMS/Utility/wrt_quick.F")) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idv3dN)), QCK(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
!
!  Write out surface Eastward and Northward momentum components (m/s) at
!  RHO-points.
!
        IF ((Qout(idUsuE,ng).and.Qout(idVsuN,ng))) THEN
          scale=1.0_r8
          gtype=gfactor*r2dvar
          status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                    &
     &                       QCK(ng)%Vid(idUsuE),                       &
     &                       QCK(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, scale,                 &
     &                       GRID(ng) % rmask_full,                     &
     &                       Ur3d(:,:,N(ng)))
          IF (FoundError(status, nf90_noerr, 652,                       &
     &                   "ROMS/Utility/wrt_quick.F")) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idUsuE)), QCK(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
          status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                    &
     &                       QCK(ng)%Vid(idVsuN),                       &
     &                       QCK(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, scale,                 &
     &                       GRID(ng) % rmask_full,                     &
     &                       Vr3d(:,:,N(ng)))
          IF (FoundError(status, nf90_noerr, 670,                       &
     &                   "ROMS/Utility/wrt_quick.F")) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idVsuN)), QCK(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
        deallocate (Ur3d)
        deallocate (Vr3d)
      END IF
!
!  Write out S-coordinate omega vertical velocity (m/s).
!
      IF (Qout(idOvel,ng)) THEN
        IF (.not.allocated(Wr3d)) THEN
          allocate (Wr3d(LBi:UBi,LBj:UBj,0:N(ng)))
          Wr3d(LBi:UBi,LBj:UBj,0:N(ng))=0.0_r8
        END IF
        scale=1.0_r8
        gtype=gfactor*w3dvar
        CALL scale_omega (ng, tile, LBi, UBi, LBj, UBj, 0, N(ng),       &
     &                    GRID(ng) % pm,                                &
     &                    GRID(ng) % pn,                                &
     &                    OCEAN(ng) % W,                                &
     &                    Wr3d)
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idOvel), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
     &                     GRID(ng) % rmask,                            &
     &                     Wr3d)
        IF (FoundError(status, nf90_noerr, 705,                         &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idOvel)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
        deallocate (Wr3d)
      END IF
!
!  Write out vertical velocity (m/s).
!
      IF (Qout(idWvel,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*w3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idWvel), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
     &                     GRID(ng) % rmask,                            &
     &                     OCEAN(ng) % wvel)
        IF (FoundError(status, nf90_noerr, 729,                         &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idWvel)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out tracer type variables.
!
      DO itrc=1,NT(ng)
        IF (Qout(idTvar(itrc),ng)) THEN
          scale=1.0_r8
          gtype=gfactor*r3dvar
          status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Tid(itrc), &
     &                       QCK(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
     &                       GRID(ng) % rmask,                          &
     &                       OCEAN(ng) % t(:,:,:,nrhs(ng),itrc))
          IF (FoundError(status, nf90_noerr, 753,                       &
     &                   "ROMS/Utility/wrt_quick.F")) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idTvar(itrc))),            &
     &                          QCK(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
!
!  Write out surface tracer type variables.
!
      DO itrc=1,NT(ng)
        IF (Qout(idsurT(itrc),ng)) THEN
          scale=1.0_r8
          gtype=gfactor*r2dvar
          status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                    &
     &                       QCK(ng)%Vid(idsurT(itrc)),                 &
     &                       QCK(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, scale,                 &
     &                       GRID(ng) % rmask,                          &
     &                       OCEAN(ng) % t(:,:,N(ng),nrhs(ng),itrc))
          IF (FoundError(status, nf90_noerr, 780,                       &
     &                   "ROMS/Utility/wrt_quick.F")) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idsurT(itrc))),            &
     &                          QCK(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
!
!  Write out density anomaly.
!
      IF (Qout(idDano,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idDano), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
     &                     GRID(ng) % rmask,                            &
     &                     OCEAN(ng) % rho)
        IF (FoundError(status, nf90_noerr, 805,                         &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idDano)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out vertical viscosity coefficient.
!
      IF (Qout(idVvis,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*w3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idVvis), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
     &                     GRID(ng) % rmask,                            &
     &                     MIXING(ng) % Akv,                            &
     &                     SetFillVal = .FALSE.)
        IF (FoundError(status, nf90_noerr, 879,                         &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVvis)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out vertical diffusion coefficient for potential temperature.
!
      IF (Qout(idTdif,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*w3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idTdif), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
     &                     GRID(ng) % rmask,                            &
     &                     MIXING(ng) % Akt(:,:,:,itemp),               &
     &                     SetFillVal = .FALSE.)
        IF (FoundError(status, nf90_noerr, 903,                         &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idTdif)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out vertical diffusion coefficient for salinity.
!
      IF (Qout(idSdif,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*w3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idSdif), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
     &                     GRID(ng) % rmask,                            &
     &                     MIXING(ng) % Akt(:,:,:,isalt),               &
     &                     SetFillVal = .FALSE.)
        IF (FoundError(status, nf90_noerr, 928,                         &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSdif)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out turbulent kinetic energy.
!
      IF (Qout(idMtke,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*w3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idMtke), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
     &                     GRID(ng) % rmask,                            &
     &                     MIXING(ng) % tke(:,:,:,nrhs(ng)),            &
     &                     SetFillVal = .FALSE.)
        IF (FoundError(status, nf90_noerr, 954,                         &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idMtke)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out turbulent length scale field.
!
      IF (Qout(idMtls,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*w3dvar
        status=nf_fwrite3d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idMtls), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
     &                     GRID(ng) % rmask,                            &
     &                     MIXING(ng) % gls(:,:,:,nrhs(ng)),            &
     &                     SetFillVal = .FALSE.)
        IF (FoundError(status, nf90_noerr, 979,                         &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idMtls)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out surface air pressure.
!
      IF (Qout(idPair,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idPair), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     GRID(ng) % rmask,                            &
     &                     FORCES(ng) % Pair)
        IF (FoundError(status, nf90_noerr, 1004,                        &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idPair)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out surface winds.
!
      IF (Qout(idUair,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idUair), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     GRID(ng) % rmask,                            &
     &                     FORCES(ng) % Uwind)
        IF (FoundError(status, nf90_noerr, 1029,                        &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUair)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
      IF (Qout(idVair,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idVair), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     GRID(ng) % rmask,                            &
     &                     FORCES(ng) % Vwind)
        IF (FoundError(status, nf90_noerr, 1050,                        &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVair)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out surface active traces fluxes.
!
      DO itrc=1,NAT
        IF (Qout(idTsur(itrc),ng)) THEN
          IF (itrc.eq.itemp) THEN
            scale=rho0*Cp                   ! Celsius m/s to W/m2
          ELSE IF (itrc.eq.isalt) THEN
            scale=1.0_r8
          END IF
          gtype=gfactor*r2dvar
          status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid,                    &
     &                       QCK(ng)%Vid(idTsur(itrc)),                 &
     &                       QCK(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, scale,                 &
     &                       GRID(ng) % rmask,                          &
     &                       FORCES(ng) % stflx(:,:,itrc))
          IF (FoundError(status, nf90_noerr, 1084,                      &
     &                   "ROMS/Utility/wrt_quick.F")) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idTsur(itrc))),            &
     &                          QCK(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
!
!  Write out latent heat flux.
!
      IF (Qout(idLhea,ng)) THEN
        scale=rho0*Cp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idLhea), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     GRID(ng) % rmask,                            &
     &                     FORCES(ng) % lhflx)
        IF (FoundError(status, nf90_noerr, 1111,                        &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idLhea)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out sensible heat flux.
!
      IF (Qout(idShea,ng)) THEN
        scale=rho0*Cp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idShea), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     GRID(ng) % rmask,                            &
     &                     FORCES(ng) % shflx)
        IF (FoundError(status, nf90_noerr, 1134,                        &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idShea)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out longwave radiation flux.
!
      IF (Qout(idLrad,ng)) THEN
        scale=rho0*Cp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idLrad), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     GRID(ng) % rmask,                            &
     &                     FORCES(ng) % lrflx)
        IF (FoundError(status, nf90_noerr, 1157,                        &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idLrad)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out E-P (m/s).
!
      IF (Qout(idEmPf,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idEmPf), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     GRID(ng) % rmask,                            &
     &                     FORCES(ng) % EminusP)
        IF (FoundError(status, nf90_noerr, 1181,                        &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idEmPf)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out evaporation rate (kg/m2/s).
!
      IF (Qout(idevap,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idevap), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     GRID(ng) % rmask,                            &
     &                     FORCES(ng) % evap)
        IF (FoundError(status, nf90_noerr, 1204,                        &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idevap)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out precipitation rate (kg/m2/s).
!
      IF (Qout(idrain,ng)) THEN
        scale=1.0_r8
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idrain), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     GRID(ng) % rmask,                            &
     &                     FORCES(ng) % rain)
        IF (FoundError(status, nf90_noerr, 1227,                        &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idrain)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out shortwave radiation flux.
!
      IF (Qout(idSrad,ng)) THEN
        scale=rho0*Cp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idSrad), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     GRID(ng) % rmask,                            &
     &                     FORCES(ng) % srflx)
        IF (FoundError(status, nf90_noerr, 1253,                        &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSrad)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out surface U-momentum stress.
!
      IF (Qout(idUsms,ng)) THEN
        scale=rho0                          ! m2/s2 to Pa
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idUsms), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     GRID(ng) % umask,                            &
     &                     FORCES(ng) % sustr)
        IF (FoundError(status, nf90_noerr, 1278,                        &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUsms)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out surface V-momentum stress.
!
      IF (Qout(idVsms,ng)) THEN
        scale=rho0
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idVsms), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     GRID(ng) % vmask,                            &
     &                     FORCES(ng) % svstr)
        IF (FoundError(status, nf90_noerr, 1301,                        &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVsms)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out bottom U-momentum stress.
!
      IF (Qout(idUbms,ng)) THEN
        scale=-rho0
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idUbms), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     GRID(ng) % umask,                            &
     &                     FORCES(ng) % bustr)
        IF (FoundError(status, nf90_noerr, 1324,                        &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idUbms)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out bottom V-momentum stress.
!
      IF (Qout(idVbms,ng)) THEN
        scale=-rho0
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, iNLM, QCK(ng)%ncid, QCK(ng)%Vid(idVbms), &
     &                     QCK(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     GRID(ng) % vmask,                            &
     &                     FORCES(ng) % bvstr)
        IF (FoundError(status, nf90_noerr, 1347,                        &
     &                 "ROMS/Utility/wrt_quick.F")) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idVbms)), QCK(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  Synchronize quicksave NetCDF file to disk to allow other processes
!  to access data immediately after it is written.
!-----------------------------------------------------------------------
!
      CALL netcdf_sync (ng, iNLM, QCK(ng)%name, QCK(ng)%ncid)
      IF (FoundError(exit_flag, NoError, 2312,                          &
     &               "ROMS/Utility/wrt_quick.F")) RETURN
      IF (Master) WRITE (stdout,20) kstp(ng), nrhs(ng), QCK(ng)%Rindex
!
  10  FORMAT (/,' WRT_QUICK - error while writing variable: ',a,/,13x,  &
     &        'into quicksave NetCDF file for time record: ',i4)
  20  FORMAT (6x,'WRT_QUICK   - wrote quicksave', t39,                  &
     &        'fields (Index=',i1,',',i1,') in record = ',i7.7)
      RETURN
      END SUBROUTINE wrt_quick
