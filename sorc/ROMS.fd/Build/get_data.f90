      SUBROUTINE get_data (ng)
!
!svn $Id: get_data.F 858 2017-07-31 23:02:30Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine reads in forcing, climatology and other data from      !
!  NetCDF files.  If there is more than one time-record,  data is      !
!  loaded into global  two-time  record arrays. The interpolation      !
!  is carried elsewhere.                                               !
!                                                                      !
!  Currently, this routine is only executed in serial mode by the      !
!  main thread.                                                        !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_boundary
      USE mod_clima
      USE mod_forces
      USE mod_grid
      USE mod_iounits
      USE mod_ncparam
      USE mod_scalars
      USE mod_sources
      USE mod_stepping
!
      USE strings_mod, ONLY : FoundError
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng
!
!  Local variable declarations.
!
      logical, dimension(3) :: update =                                 &
     &         (/ .FALSE., .FALSE., .FALSE. /)
      integer :: ILB, IUB, JLB, JUB
      integer :: LBi, UBi, LBj, UBj
      integer :: i, ic, my_tile
!
!  Lower and upper bounds for nontiled (global values) boundary arrays.
!
      my_tile=-1                           ! for global values
      ILB=BOUNDS(ng)%LBi(my_tile)
      IUB=BOUNDS(ng)%UBi(my_tile)
      JLB=BOUNDS(ng)%LBj(my_tile)
      JUB=BOUNDS(ng)%UBj(my_tile)
!
!  Lower and upper bounds for tiled arrays.
!
      LBi=LBOUND(GRID(ng)%h,DIM=1)
      UBi=UBOUND(GRID(ng)%h,DIM=1)
      LBj=LBOUND(GRID(ng)%h,DIM=2)
      UBj=UBOUND(GRID(ng)%h,DIM=2)
!
!-----------------------------------------------------------------------
!  Turn on input data time wall clock.
!-----------------------------------------------------------------------
!
      CALL wclock_on (ng, iNLM, 3, 79, "ROMS/Nonlinear/get_data.F")
!
!=======================================================================
!  Read in forcing data from FORCING NetCDF file.
!=======================================================================
!
!-----------------------------------------------------------------------
!  Point Sources/Sinks time dependent data.
!-----------------------------------------------------------------------
!
!  Point Source/Sink vertically integrated mass transport.
!
      IF (LuvSrc(ng).or.LwSrc(ng)) THEN
        CALL get_ngfld (ng, iNLM, idRtra, SSF(ng)%ncid,                 &
     &                  1, SSF(ng), update(1),                          &
     &                  1, Nsrc(ng), 1, 2, 1, Nsrc(ng), 1,              &
     &                  SOURCES(ng) % QbarG)
        IF (FoundError(exit_flag, NoError, 99,                          &
     &                 "ROMS/Nonlinear/get_data.F")) RETURN
      END IF
!
!  Tracer Sources/Sinks.
!
      DO i=1,NT(ng)
        IF (LtracerSrc(i,ng)) THEN
          CALL get_ngfld (ng, iNLM, idRtrc(i), SSF(ng)%ncid,            &
     &                    1, SSF(ng), update(1),                        &
     &                    1, Nsrc(ng), N(ng), 2, 1, Nsrc(ng), N(ng),    &
     &                    SOURCES(ng) % TsrcG(:,:,:,i))
          IF (FoundError(exit_flag, NoError, 113,                       &
     &                   "ROMS/Nonlinear/get_data.F")) RETURN
        END IF
      END DO
!
!-----------------------------------------------------------------------
!  Surface wind components.
!-----------------------------------------------------------------------
!
      CALL get_2dfld (ng, iNLM, idUair, ncFRCid(idUair,ng),             &
     &                nFfiles(ng), FRC(1,ng), update(1),                &
     &                LBi, UBi, LBj, UBj, 2, 1,                         &
     &                GRID(ng) % rmask,                                 &
     &                FORCES(ng) % UwindG)
      IF (FoundError(exit_flag, NoError, 134,                           &
     &               "ROMS/Nonlinear/get_data.F")) RETURN
      CALL get_2dfld (ng , iNLM, idVair, ncFRCid(idVair,ng),            &
     &                nFfiles(ng), FRC(1,ng), update(1),                &
     &                LBi, UBi, LBj, UBj, 2, 1,                         &
     &                GRID(ng) % rmask,                                 &
     &                FORCES(ng) % VwindG)
      IF (FoundError(exit_flag, NoError, 144,                           &
     &               "ROMS/Nonlinear/get_data.F")) RETURN
!
!-----------------------------------------------------------------------
!  Surface air pressure.
!-----------------------------------------------------------------------
!
      CALL get_2dfld (ng, iNLM, idPair, ncFRCid(idPair,ng),             &
     &                nFfiles(ng), FRC(1,ng), update(1),                &
     &                LBi, UBi, LBj, UBj, 2, 1,                         &
     &                GRID(ng) % rmask,                                 &
     &                FORCES(ng) % PairG)
      IF (FoundError(exit_flag, NoError, 219,                           &
     &               "ROMS/Nonlinear/get_data.F")) RETURN
!
!-----------------------------------------------------------------------
!  Surface solar shortwave radiation.
!-----------------------------------------------------------------------
!
      CALL get_2dfld (ng, iNLM, idSrad, ncFRCid(idSrad,ng),             &
     &                nFfiles(ng), FRC(1,ng), update(1),                &
     &                LBi, UBi, LBj, UBj, 2, 1,                         &
     &                GRID(ng) % rmask,                                 &
     &                FORCES(ng) % srflxG)
      IF (FoundError(exit_flag, NoError, 358,                           &
     &               "ROMS/Nonlinear/get_data.F")) RETURN
!
!-----------------------------------------------------------------------
!  Surface downwelling longwave radiation.
!-----------------------------------------------------------------------
!
      CALL get_2dfld (ng, iNLM, idLdwn, ncFRCid(idLdwn,ng),             &
     &                nFfiles(ng), FRC(1,ng), update(1),                &
     &                LBi, UBi, LBj, UBj, 2, 1,                         &
     &                GRID(ng) % rmask,                                 &
     &                FORCES(ng) % lrflxG)
      IF (FoundError(exit_flag, NoError, 411,                           &
     &               "ROMS/Nonlinear/get_data.F")) RETURN
!
!-----------------------------------------------------------------------
!  Surface air temperature.
!-----------------------------------------------------------------------
!
      CALL get_2dfld (ng, iNLM, idTair, ncFRCid(idTair,ng),             &
     &                nFfiles(ng), FRC(1,ng), update(1),                &
     &                LBi, UBi, LBj, UBj, 2, 1,                         &
     &                GRID(ng) % rmask,                                 &
     &                FORCES(ng) % TairG)
      IF (FoundError(exit_flag, NoError, 430,                           &
     &               "ROMS/Nonlinear/get_data.F")) RETURN
!
!-----------------------------------------------------------------------
!  Surface air humidity.
!-----------------------------------------------------------------------
!
      CALL get_2dfld (ng, iNLM, idQair, ncFRCid(idQair,ng),             &
     &                nFfiles(ng), FRC(1,ng), update(1),                &
     &                LBi, UBi, LBj, UBj, 2, 1,                         &
     &                GRID(ng) % rmask,                                 &
     &                FORCES(ng) % HairG)
      IF (FoundError(exit_flag, NoError, 448,                           &
     &               "ROMS/Nonlinear/get_data.F")) RETURN
!
!-----------------------------------------------------------------------
!  Rain fall rate.
!-----------------------------------------------------------------------
!
      CALL get_2dfld (ng, iNLM, idrain, ncFRCid(idrain,ng),             &
     &                nFfiles(ng), FRC(1,ng), update(1),                &
     &                LBi, UBi, LBj, UBj, 2, 1,                         &
     &                GRID(ng) % rmask,                                 &
     &                FORCES(ng) % rainG)
      IF (FoundError(exit_flag, NoError, 465,                           &
     &               "ROMS/Nonlinear/get_data.F")) RETURN
!
!=======================================================================
!  Read in open boundary conditions from BOUNDARY NetCDF file.
!=======================================================================
!
      IF (LprocessOBC(ng)) THEN
        IF (LBC(iwest,isFsur,ng)%acquire) THEN
          CALL get_ngfld (ng, iNLM, idZbry(iwest), BRY(ng)%ncid,        &
     &                    1, BRY(ng), update(1),                        &
     &                    JLB, JUB, 1, 2, 0, Mm(ng)+1, 1,               &
     &                    BOUNDARY(ng) % zetaG_west)
          IF (FoundError(exit_flag, NoError, 741,                       &
     &                   "ROMS/Nonlinear/get_data.F")) RETURN
        END IF
        IF (LBC(ieast,isFsur,ng)%acquire) THEN
          CALL get_ngfld (ng, iNLM, idZbry(ieast), BRY(ng)%ncid,        &
     &                    1, BRY(ng), update(1),                        &
     &                    JLB, JUB, 1, 2, 0, Mm(ng)+1, 1,               &
     &                    BOUNDARY(ng) % zetaG_east)
          IF (FoundError(exit_flag, NoError, 750,                       &
     &                   "ROMS/Nonlinear/get_data.F")) RETURN
        END IF
        IF (LBC(isouth,isFsur,ng)%acquire) THEN
          CALL get_ngfld (ng, iNLM, idZbry(isouth), BRY(ng)%ncid,       &
     &                    1, BRY(ng), update(1),                        &
     &                    ILB, IUB, 1, 2, 0, Lm(ng)+1, 1,               &
     &                    BOUNDARY(ng) % zetaG_south)
          IF (FoundError(exit_flag, NoError, 759,                       &
     &                   "ROMS/Nonlinear/get_data.F")) RETURN
        END IF
        IF (LBC(inorth,isFsur,ng)%acquire) THEN
          CALL get_ngfld (ng, iNLM, idZbry(inorth), BRY(ng)%ncid,       &
     &                    1, BRY(ng), update(1),                        &
     &                    ILB, IUB, 1, 2, 0, Lm(ng)+1, 1,               &
     &                    BOUNDARY(ng) % zetaG_north)
          IF (FoundError(exit_flag, NoError, 768,                       &
     &                   "ROMS/Nonlinear/get_data.F")) RETURN
        END IF
      END IF
!
      IF (LprocessOBC(ng)) THEN
        IF (LBC(iwest,isUbar,ng)%acquire) THEN
          CALL get_ngfld (ng, iNLM, idU2bc(iwest), BRY(ng)%ncid,        &
     &                    1, BRY(ng), update(1),                        &
     &                    JLB, JUB, 1, 2, 0, Mm(ng)+1, 1,               &
     &                    BOUNDARY(ng) % ubarG_west)
          IF (FoundError(exit_flag, NoError, 782,                       &
     &                   "ROMS/Nonlinear/get_data.F")) RETURN
        END IF
        IF (LBC(iwest,isVbar,ng)%acquire) THEN
          CALL get_ngfld (ng, iNLM, idV2bc(iwest), BRY(ng)%ncid,        &
     &                    1, BRY(ng), update(1),                        &
     &                    JLB, JUB, 1, 2, 1, Mm(ng)+1, 1,               &
     &                    BOUNDARY(ng) % vbarG_west)
          IF (FoundError(exit_flag, NoError, 791,                       &
     &                   "ROMS/Nonlinear/get_data.F")) RETURN
        END IF
        IF (LBC(ieast,isUbar,ng)%acquire) THEN
          CALL get_ngfld (ng, iNLM, idU2bc(ieast), BRY(ng)%ncid,        &
     &                    1, BRY(ng), update(1),                        &
     &                    JLB, JUB, 1, 2, 0, Mm(ng)+1, 1,               &
     &                    BOUNDARY(ng) % ubarG_east)
          IF (FoundError(exit_flag, NoError, 800,                       &
     &                   "ROMS/Nonlinear/get_data.F")) RETURN
        END IF
        IF (LBC(ieast,isVbar,ng)%acquire) THEN
          CALL get_ngfld (ng, iNLM, idV2bc(ieast), BRY(ng)%ncid,        &
     &                    1, BRY(ng), update(1),                        &
     &                    JLB, JUB, 1, 2, 1, Mm(ng)+1, 1,               &
     &                    BOUNDARY(ng) % vbarG_east)
          IF (FoundError(exit_flag, NoError, 809,                       &
     &                   "ROMS/Nonlinear/get_data.F")) RETURN
        END IF
        IF (LBC(isouth,isUbar,ng)%acquire) THEN
          CALL get_ngfld (ng, iNLM, idU2bc(isouth), BRY(ng)%ncid,       &
     &                    1, BRY(ng), update(1),                        &
     &                    ILB, IUB, 1, 2, 1, Lm(ng)+1, 1,               &
     &                    BOUNDARY(ng) % ubarG_south)
          IF (FoundError(exit_flag, NoError, 818,                       &
     &                   "ROMS/Nonlinear/get_data.F")) RETURN
        END IF
        IF (LBC(isouth,isVbar,ng)%acquire) THEN
          CALL get_ngfld (ng, iNLM, idV2bc(isouth), BRY(ng)%ncid,       &
     &                    1, BRY(ng), update(1),                        &
     &                    ILB, IUB, 1, 2, 0, Lm(ng)+1, 1,               &
     &                    BOUNDARY(ng) % vbarG_south)
          IF (FoundError(exit_flag, NoError, 827,                       &
     &                   "ROMS/Nonlinear/get_data.F")) RETURN
        END IF
        IF (LBC(inorth,isUbar,ng)%acquire) THEN
          CALL get_ngfld (ng, iNLM, idU2bc(inorth), BRY(ng)%ncid,       &
     &                    1, BRY(ng), update(1),                        &
     &                    ILB, IUB, 1, 2, 1, Lm(ng)+1, 1,               &
     &                    BOUNDARY(ng) % ubarG_north)
          IF (FoundError(exit_flag, NoError, 836,                       &
     &                   "ROMS/Nonlinear/get_data.F")) RETURN
        END IF
        IF (LBC(inorth,isVbar,ng)%acquire) THEN
          CALL get_ngfld (ng, iNLM, idV2bc(inorth), BRY(ng)%ncid,       &
     &                    1, BRY(ng), update(1),                        &
     &                    ILB, IUB, 1, 2, 0, Lm(ng)+1, 1,               &
     &                    BOUNDARY(ng) % vbarG_north)
          IF (FoundError(exit_flag, NoError, 845,                       &
     &                   "ROMS/Nonlinear/get_data.F")) RETURN
        END IF
      END IF
!
      IF (LprocessOBC(ng)) THEN
        IF (LBC(iwest,isUvel,ng)%acquire) THEN
          CALL get_ngfld (ng, iNLM, idU3bc(iwest), BRY(ng)%ncid,        &
     &                    1, BRY(ng), update(1),                        &
     &                    JLB, JUB, N(ng), 2, 0, Mm(ng)+1, N(ng),       &
     &                    BOUNDARY(ng) % uG_west)
          IF (FoundError(exit_flag, NoError, 860,                       &
     &                   "ROMS/Nonlinear/get_data.F")) RETURN
        END IF
        IF (LBC(iwest,isVvel,ng)%acquire) THEN
          CALL get_ngfld (ng, iNLM, idV3bc(iwest), BRY(ng)%ncid,        &
     &                    1, BRY(ng), update(1),                        &
     &                    JLB, JUB, N(ng), 2, 1, Mm(ng)+1, N(ng),       &
     &                    BOUNDARY(ng) % vG_west)
          IF (FoundError(exit_flag, NoError, 869,                       &
     &                   "ROMS/Nonlinear/get_data.F")) RETURN
        END IF
        IF (LBC(ieast,isUvel,ng)%acquire) THEN
          CALL get_ngfld (ng, iNLM, idU3bc(ieast), BRY(ng)%ncid,        &
     &                    1, BRY(ng), update(1),                        &
     &                    JLB, JUB, N(ng), 2, 0, Mm(ng)+1, N(ng),       &
     &                    BOUNDARY(ng) % uG_east)
          IF (FoundError(exit_flag, NoError, 878,                       &
     &                   "ROMS/Nonlinear/get_data.F")) RETURN
        END IF
        IF (LBC(ieast,isVvel,ng)%acquire) THEN
          CALL get_ngfld (ng, iNLM, idV3bc(ieast), BRY(ng)%ncid,        &
     &                    1, BRY(ng), update(1),                        &
     &                    JLB, JUB, N(ng), 2, 1, Mm(ng)+1, N(ng),       &
     &                    BOUNDARY(ng) % vG_east)
          IF (FoundError(exit_flag, NoError, 887,                       &
     &                   "ROMS/Nonlinear/get_data.F")) RETURN
        END IF
        IF (LBC(isouth,isUvel,ng)%acquire) THEN
          CALL get_ngfld (ng, iNLM, idU3bc(isouth), BRY(ng)%ncid,       &
     &                    1, BRY(ng), update(1),                        &
     &                    ILB, IUB, N(ng), 2, 1, Lm(ng)+1, N(ng),       &
     &                    BOUNDARY(ng) % uG_south)
          IF (FoundError(exit_flag, NoError, 896,                       &
     &                   "ROMS/Nonlinear/get_data.F")) RETURN
        END IF
        IF (LBC(isouth,isVvel,ng)%acquire) THEN
          CALL get_ngfld (ng, iNLM, idV3bc(isouth), BRY(ng)%ncid,       &
     &                    1, BRY(ng), update(1),                        &
     &                    ILB, IUB, N(ng), 2, 0, Lm(ng)+1, N(ng),       &
     &                    BOUNDARY(ng) % vG_south)
          IF (FoundError(exit_flag, NoError, 905,                       &
     &                   "ROMS/Nonlinear/get_data.F")) RETURN
        END IF
        IF (LBC(inorth,isUvel,ng)%acquire) THEN
          CALL get_ngfld (ng, iNLM, idU3bc(inorth), BRY(ng)%ncid,       &
     &                    1, BRY(ng), update(1),                        &
     &                    ILB, IUB, N(ng), 2, 1, Lm(ng)+1, N(ng),       &
     &                    BOUNDARY(ng) % uG_north)
          IF (FoundError(exit_flag, NoError, 914,                       &
     &                   "ROMS/Nonlinear/get_data.F")) RETURN
        END IF
        IF (LBC(inorth,isVvel,ng)%acquire) THEN
          CALL get_ngfld (ng, iNLM, idV3bc(inorth), BRY(ng)%ncid,       &
     &                    1, BRY(ng), update(1),                        &
     &                    ILB, IUB, N(ng), 2, 0, Lm(ng)+1, N(ng),       &
     &                    BOUNDARY(ng) % vG_north)
          IF (FoundError(exit_flag, NoError, 923,                       &
     &                   "ROMS/Nonlinear/get_data.F")) RETURN
        END IF
      END IF
!
      IF (LprocessOBC(ng)) THEN
        DO i=1,NT(ng)
          IF (LBC(iwest,isTvar(i),ng)%acquire) THEN
            CALL get_ngfld (ng, iNLM, idTbry(iwest,i), BRY(ng)%ncid,    &
     &                      1, BRY(ng), update(1),                      &
     &                      JLB, JUB, N(ng), 2, 0, Mm(ng)+1, N(ng),     &
     &                      BOUNDARY(ng) % tG_west(:,:,:,i))
            IF (FoundError(exit_flag, NoError, 938,                     &
     &                     "ROMS/Nonlinear/get_data.F")) RETURN
          END IF
        END DO
        DO i=1,NT(ng)
          IF (LBC(ieast,isTvar(i),ng)%acquire) THEN
            CALL get_ngfld (ng, iNLM, idTbry(ieast,i), BRY(ng)%ncid,    &
     &                      1, BRY(ng), update(1),                      &
     &                      JLB, JUB, N(ng), 2, 0, Mm(ng)+1, N(ng),     &
     &                      BOUNDARY(ng) % tG_east(:,:,:,i))
            IF (FoundError(exit_flag, NoError, 949,                     &
     &                     "ROMS/Nonlinear/get_data.F")) RETURN
          END IF
        END DO
        DO i=1,NT(ng)
          IF (LBC(isouth,isTvar(i),ng)%acquire) THEN
            CALL get_ngfld (ng, iNLM, idTbry(isouth,i), BRY(ng)%ncid,   &
     &                      1, BRY(ng), update(1),                      &
     &                      ILB, IUB, N(ng), 2, 0, Lm(ng)+1, N(ng),     &
     &                      BOUNDARY(ng) % tG_south(:,:,:,i))
            IF (FoundError(exit_flag, NoError, 960,                     &
     &                     "ROMS/Nonlinear/get_data.F")) RETURN
          END IF
        END DO
        DO i=1,NT(ng)
          IF (LBC(inorth,isTvar(i),ng)%acquire) THEN
            CALL get_ngfld (ng, iNLM, idTbry(inorth,i), BRY(ng)%ncid,   &
     &                      1, BRY(ng), update(1),                      &
     &                      ILB, IUB, N(ng), 2, 0, Lm(ng)+1, N(ng),     &
     &                      BOUNDARY(ng) % tG_north(:,:,:,i))
            IF (FoundError(exit_flag, NoError, 971,                     &
     &                     "ROMS/Nonlinear/get_data.F")) RETURN
          END IF
        END DO
      END IF
!
!=======================================================================
!  Read in data from Climatology NetCDF file.
!=======================================================================
!
!  Free-surface.
!
      IF (LsshCLM(ng)) THEN
        CALL get_2dfld (ng, iNLM, idSSHc, CLM(ng)%ncid,                 &
     &                  1, CLM(ng), update(1),                          &
     &                  LBi, UBi, LBj, UBj, 2, 1,                       &
     &                  GRID(ng) % rmask,                               &
     &                  CLIMA(ng) % sshG)
        IF (FoundError(exit_flag, NoError, 995,                         &
     &                 "ROMS/Nonlinear/get_data.F")) RETURN
      END IF
!
!  2D momentum.
!
      IF (Lm2CLM(ng)) THEN
        CALL get_2dfld (ng, iNLM, idUbcl, CLM(ng)%ncid,                 &
     &                  1, CLM(ng), update(1),                          &
     &                  LBi, UBi, LBj, UBj, 2, 1,                       &
     &                  GRID(ng) % umask,                               &
     &                  CLIMA(ng) % ubarclmG)
        IF (FoundError(exit_flag, NoError, 1011,                        &
     &                 "ROMS/Nonlinear/get_data.F")) RETURN
!
        CALL get_2dfld (ng, iNLM, idVbcl, CLM(ng)%ncid,                 &
     &                  1, CLM(ng), update(1),                          &
     &                  LBi, UBi, LBj, UBj, 2, 1,                       &
     &                  GRID(ng) % vmask,                               &
     &                  CLIMA(ng) % vbarclmG)
        IF (FoundError(exit_flag, NoError, 1021,                        &
     &                 "ROMS/Nonlinear/get_data.F")) RETURN
      END IF
!
!  3D momentum.
!
      IF (Lm3CLM(ng)) THEN
        CALL get_3dfld (ng, iNLM, idUclm, CLM(ng)%ncid,                 &
     &                  1, CLM(ng), update(1),                          &
     &                  LBi, UBi, LBj, UBj, 1, N(ng), 2, 1,             &
     &                  GRID(ng) % umask,                               &
     &                  CLIMA(ng) % uclmG)
        IF (FoundError(exit_flag, NoError, 1038,                        &
     &                 "ROMS/Nonlinear/get_data.F")) RETURN
!
        CALL get_3dfld (ng, iNLM, idVclm, CLM(ng)%ncid,                 &
     &                  1, CLM(ng), update(1),                          &
     &                  LBi, UBi, LBj, UBj, 1, N(ng), 2, 1,             &
     &                  GRID(ng) % vmask,                               &
     &                  CLIMA(ng) % vclmG)
        IF (FoundError(exit_flag, NoError, 1048,                        &
     &                 "ROMS/Nonlinear/get_data.F")) RETURN
      END IF
!
!  Tracers.
!
      ic=0
      DO i=1,NT(ng)
        IF (LtracerCLM(i,ng)) THEN
          ic=ic+1
          CALL get_3dfld (ng, iNLM, idTclm(i), CLM(ng)%ncid,            &
     &                    1, CLM(ng), update(1),                        &
     &                    LBi, UBi, LBj, UBj, 1, N(ng), 2, 1,           &
     &                    GRID(ng) % rmask,                             &
     &                    CLIMA(ng) % tclmG(:,:,:,:,ic))
          IF (FoundError(exit_flag, NoError, 1067,                      &
     &                   "ROMS/Nonlinear/get_data.F")) RETURN
        END IF
      END DO
!
!-----------------------------------------------------------------------
!  Turn off input data time wall clock.
!-----------------------------------------------------------------------
!
      CALL wclock_off (ng, iNLM, 3, 1250, "ROMS/Nonlinear/get_data.F")
      RETURN
      END SUBROUTINE get_data
