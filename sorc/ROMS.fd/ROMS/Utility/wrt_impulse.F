#include "cppdefs.h"
#if defined ADJOINT && defined IMPULSE
      SUBROUTINE wrt_impulse (ng, tile, model, INPncname)
!
!svn $Id: wrt_impulse.F 857 2017-07-29 04:05:27Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  These subroutine read in requested adjoint solution from input      !
!  NetCDF (saved at nADJ time-step intervals) and then writes  to      !
!  output  impulse  forcing  NetCDF  file in ascending time order      !
!  since it is processed by the TL and RP models.                      !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ng              Nested grid number.                              !
!     tile            Domain partition.                                !
!     model           Calling model identifier.                        !
!     INPncname       Input adjoint solution NetCDF file name.         !
!                                                                      !
!  Notice that only momentum is affected by the full time-averaged     !
!  masks.  If applicable, these mask contains information about        !
!  river runoff and time-dependent wetting and drying variations.      !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_grid
      USE mod_iounits
      USE mod_ncparam
      USE mod_netcdf
      USE mod_ocean
      USE mod_scalars
!
      USE nf_fread2d_mod,  ONLY : nf_fread2d
# ifdef SOLVE3D
      USE nf_fread3d_mod,  ONLY : nf_fread3d
# endif
      USE nf_fwrite2d_mod, ONLY : nf_fwrite2d
# ifdef SOLVE3D
      USE nf_fwrite3d_mod, ONLY : nf_fwrite3d
# endif
      USE strings_mod,     ONLY : FoundError, find_string
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model

      character (len=*), intent(in) :: INPncname
!
!  Local variable declarations.
!
      integer :: LBi, UBi, LBj, UBj
      integer :: Iinp, Iout, Irec, MyType, Nrec
      integer :: INPncid, INPvid
      integer :: i, gtype, status, varid
      integer :: ibuffer(2), Vsize(4)

      real(r8) :: Fmin, Fmax, scale
      real(r8) :: inp_time(1)

# include "set_bounds.h"
!
      SourceFile=__FILE__
!
      LBi=LBOUND(GRID(ng)%h,DIM=1)
      UBi=UBOUND(GRID(ng)%h,DIM=1)
      LBj=LBOUND(GRID(ng)%h,DIM=2)
      UBj=UBOUND(GRID(ng)%h,DIM=2)
!
!-----------------------------------------------------------------------
!  Determine variables to read and their availability.
!-----------------------------------------------------------------------
!
!  Inquire about the dimensions and check for consistency.
!
      CALL netcdf_check_dim (ng, model, INPncname)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
      Nrec=rec_size
!
!  Inquire about the variables.
!
      CALL netcdf_inq_var (ng, model, INPncname)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN
!
!  Set Vsize to zero to deactivate interpolation of input data to model
!  grid in "nf_fread2d".
!
      DO i=1,4
        Vsize(i)=0
      END DO
!
!-----------------------------------------------------------------------
!  Read adjoint solution and convert to impulse forcing.  Then, write
!  impulse forcing into output NetCDF file.
!-----------------------------------------------------------------------
!
!  Open input NetCDF file.
!
      CALL netcdf_open (ng, model, INPncname, 0, INPncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) THEN
        WRITE (stdout,10) TRIM(INPncname)
        RETURN
      END IF
!
!  Process each record in input adjoint NetCDF except last. Note that
!  the adjoint records are processed backwards (Nrec-1:1) and written
!  in ascending time order (Iout initialized to 0) since the weak
!  constraint forcing will be read by the TL and RP models. Record
!  Nrec is not processed since it is not needed.
!
      Iinp=1
      Iout=0
      scale=1.0_r8

      DO Irec=Nrec-1,1,-1
        Iout=Iout+1
!
!  Process time.
!
        IF (find_string(var_name, n_var, Vname(1,idtime), INPvid)) THEN
          CALL netcdf_get_fvar (ng, model, INPncname, Vname(1,idtime),  &
     &                          inp_time,                               &
     &                          ncid = INPncid,                         &
     &                          start = (/Irec/), total = (/1/))
          IF (FoundError(exit_flag, NoError, __LINE__,                  &
     &                   __FILE__)) RETURN
!
          CALL netcdf_put_fvar (ng, model, TLF(ng)%name,                &
     &                          Vname(1,idtime), inp_time,              &
     &                          (/Iout/), (/1/),                        &
     &                          ncid = TLF(ng)%ncid)
          IF (FoundError(exit_flag, NoError, __LINE__,                  &
     &                   __FILE__)) RETURN
        ELSE
          IF (Master) WRITE (stdout,20) TRIM(Vname(1,idtime)),          &
     &                                  TRIM(INPncname)
          exit_flag=2
        END IF
!
!  Process free-surface weak-constraint impulse forcing.
!
        IF (find_string(var_name, n_var, Vname(1,idZtlf), INPvid)) THEN
          gtype=var_flag(INPvid)*r2dvar
          MyType=gtype
          status=nf_fread2d(ng, model, INPncname, INPncid,              &
     &                      Vname(1,idZtlf), INPvid,                    &
     &                      Irec, MyType, Vsize,                        &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, Fmin, Fmax,                          &
# ifdef MASKING
     &                      GRID(ng) % rmask,                           &
# endif
     &                      OCEAN(ng) % ad_zeta(:,:,Iinp))
          IF (FoundError(status, nf90_noerr, __LINE__,                  &
     &                   __FILE__)) THEN
            IF (Master) THEN
              WRITE (stdout,20) TRIM(Vname(1,idZtlf)), Irec,            &
     &                          TRIM(INPncname)
            END IF
            exit_flag=2
            ioerror=status
            RETURN
          END IF
!
          MyType=gtype
          status=nf_fwrite2d(ng, model, TLF(ng)%ncid,                   &
     &                       TLF(ng)%Vid(idZtlf),                       &
     &                       Iout, MyType,                              &
     &                       LBi, UBi, LBj, UBj, scale,                 &
# ifdef MASKING
     &                       GRID(ng) % rmask,                          &
# endif
     &                       OCEAN(ng) % ad_zeta(:,:,Iinp))
          IF (FoundError(status, nf90_noerr, __LINE__,                  &
     &                   __FILE__)) THEN
            IF (Master) THEN
              WRITE (stdout,30) TRIM(Vname(1,idZtlf)), Irec,            &
     &                          TRIM(TLF(ng)%name)
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        ELSE
          IF (Master) WRITE (stdout,40) TRIM(Vname(1,idZtlf)),          &
     &                                  TRIM(INPncname)
          exit_flag=2
          RETURN
        END IF

# ifndef SOLVE3D
!
!  Process 2D U-momentum weak-constraint impulse forcing.
!
        IF (find_string(var_name, n_var, Vname(1,idUbtf), INPvid)) THEN
          gtype=var_flag(INPvid)*u2dvar
          MyType=gtype
          status=nf_fread2d(ng, model, INPncname, INPncid,              &
     &                      Vname(1,idUbtf), INPvid,                    &
     &                      Irec, MyType, Vsize,                        &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, Fmin, Fmax,                          &
#  ifdef MASKING
     &                      GRID(ng) % umask_full,                      &
#  endif
     &                      OCEAN(ng) % ad_ubar(:,:,Iinp))
          IF (FoundError(status, nf90_noerr, __LINE__,                  &
     &                   __FILE__)) THEN
            IF (Master) THEN
              WRITE (stdout,20) TRIM(Vname(1,idUbtf)), Irec,            &
     &                          TRIM(INPncname)
            END IF
            exit_flag=2
            ioerror=status
            RETURN
          END IF
!
          MyType=gtype
          status=nf_fwrite2d(ng, model, TLF(ng)%ncid,                   &
     &                       TLF(ng)%Vid(idUbtf),                       &
     &                       Iout, MyType,                              &
     &                       LBi, UBi, LBj, UBj, scale,                 &
#  ifdef MASKING
     &                       GRID(ng) % umask_full,                     &
#  endif
     &                       OCEAN(ng) % ad_ubar(:,:,Iinp))
          IF (FoundError(status, nf90_noerr, __LINE__,                  &
     &                   __FILE__)) THEN
            IF (Master) THEN
              WRITE (stdout,30) TRIM(Vname(1,idUbtf)), Irec,            &
     &                          TRIM(TLF(ng)%name)
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        ELSE
          IF (Master) WRITE (stdout,20) TRIM(Vname(1,idUbtf)),          &
     &                                  TRIM(INPncname)
          exit_flag=2
          RETURN
        END IF
!
!  Process 2D V-momentum weak-constraint impulse forcing.
!
        IF (find_string(var_name, n_var, Vname(1,idVbtf), INPvid)) THEN
          gtype=var_flag(INPvid)*v2dvar
          MyType=gtype
          status=nf_fread2d(ng, model, INPncname, INPncid,              &
     &                      Vname(1,idVbtf), INPvid,                    &
     &                      Irec, MyType, Vsize,                        &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, Fmin, Fmax,                          &
#  ifdef MASKING
     &                      GRID(ng) % vmask_full,                      &
#  endif
     &                      OCEAN(ng) % ad_vbar(:,:,Iinp))
          IF (FoundError(status, nf90_noerr, __LINE__,                  &
     &                   __FILE__)) THEN
            IF (Master) THEN
              WRITE (stdout,20) TRIM(Vname(1,idVbtf)), Irec,            &
     &                          TRIM(INPncname)
            END IF
            exit_flag=2
            ioerror=status
            RETURN
          END IF
!
          MyType=gtype
          status=nf_fwrite2d(ng, model, TLF(ng)%ncid,                   &
     &                       TLF(ng)%Vid(idVbtf),                       &
     &                       Iout, MyType,                              &
     &                       LBi, UBi, LBj, UBj, scale,                 &
#  ifdef MASKING
     &                       GRID(ng) % vmask_full,                     &
#  endif
     &                       OCEAN(ng) % ad_vbar(:,:,Iinp))
          IF (FoundError(status, nf90_noerr, __LINE__,                  &
     &                   __FILE__)) THEN
            IF (Master) THEN
              WRITE (stdout,30) TRIM(Vname(1,idVbtf)), Irec,            &
     &                          TRIM(TLF(ng)%name)
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        ELSE
          IF (Master) WRITE (stdout,40) TRIM(Vname(1,idVbtf)),          &
     &                                  TRIM(INPncname)
          exit_flag=2
          RETURN
        END IF
# endif
# ifdef SOLVE3D
!
!  Process 3D U-momentum weak-constraint impulse forcing.
!
        IF (find_string(var_name, n_var, Vname(1,idUtlf), INPvid)) THEN
          gtype=var_flag(INPvid)*u3dvar
          MyType=gtype
          status=nf_fread3d(ng, model, INPncname, INPncid,              &
     &                      Vname(1,idUtlf), INPvid,                    &
     &                      Irec, MyType, Vsize,                        &
     &                      LBi, UBi, LBj, UBj, 1, N(ng),               &
     &                      scale, Fmin, Fmax,                          &
#  ifdef MASKING
     &                      GRID(ng) % umask_full,                      &
#  endif
     &                      OCEAN(ng) % ad_u(:,:,:,Iinp))
          IF (FoundError(status, nf90_noerr, __LINE__,                  &
     &                   __FILE__)) THEN
            IF (Master) THEN
              WRITE (stdout,20) TRIM(Vname(1,idUtlf)), Irec,            &
     &                          TRIM(INPncname)
            END IF
            exit_flag=2
            ioerror=status
            RETURN
          END IF
!
          status=nf_fwrite3d(ng, model, TLF(ng)%ncid,                   &
     &                       TLF(ng)%Vid(idUtlf),                       &
     &                       Iout, MyType,                              &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
#  ifdef MASKING
     &                       GRID(ng) % umask_full,                     &
#  endif
     &                       OCEAN(ng) % ad_u(:,:,:,Iinp))
          IF (FoundError(status, nf90_noerr, __LINE__,                  &
     &                   __FILE__)) THEN
            IF (Master) THEN
              WRITE (stdout,30) TRIM(Vname(1,idUtlf)), Irec,            &
     &                          TRIM(TLF(ng)%name)
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        ELSE
          IF (Master) WRITE (stdout,40) TRIM(Vname(1,idUtlf)),          &
     &                                  TRIM(INPncname)
          exit_flag=2
          RETURN
        END IF
!
!  Process 3D V-momentum weak-constraint impulse forcing.
!
        IF (find_string(var_name, n_var, Vname(1,idVtlf), INPvid)) THEN
          gtype=var_flag(INPvid)*v3dvar
          MyType=gtype
          status=nf_fread3d(ng, model, INPncname, INPncid,              &
     &                      Vname(1,idVtlf), INPvid,                    &
     &                      Irec, MyType, Vsize,                        &
     &                      LBi, UBi, LBj, UBj, 1, N(ng),               &
     &                      scale, Fmin, Fmax,                          &
#  ifdef MASKING
     &                      GRID(ng) % vmask_full,                      &
#  endif
     &                      OCEAN(ng) % ad_v(:,:,:,Iinp))
          IF (FoundError(status, nf90_noerr, __LINE__,                  &
     &                   __FILE__)) THEN
            IF (Master) THEN
              WRITE (stdout,20) TRIM(Vname(1,idVtlf)), Irec,            &
     &                          TRIM(INPncname)
            END IF
            exit_flag=2
            ioerror=status
            RETURN
          END IF
!
          status=nf_fwrite3d(ng, model, TLF(ng)%ncid,                   &
     &                       TLF(ng)%Vid(idVtlf),                       &
     &                       Iout, MyType,                              &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
#  ifdef MASKING
     &                       GRID(ng) % vmask_full,                     &
#  endif
     &                       OCEAN(ng) % ad_v(:,:,:,Iinp))
          IF (FoundError(status, nf90_noerr, __LINE__,                  &
     &                   __FILE__)) THEN
            IF (Master) THEN
              WRITE (stdout,30) TRIM(Vname(1,idVtlf)), Irec,            &
     &                          TRIM(TLF(ng)%name)
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        ELSE
          IF (Master) WRITE (stdout,40) TRIM(Vname(1,idVtlf)),          &
     &                                  TRIM(INPncname)
          exit_flag=2
          RETURN
        END IF
!
!  Process tracer type variables impulses.
!
        DO i=1,NT(ng)
          IF (find_string(var_name, n_var, Vname(1,idTtlf(i)),          &
     &                    INPvid)) THEN
            gtype=var_flag(INPvid)*r3dvar
            MyType=gtype
            status=nf_fread3d(ng, model, INPncname, INPncid,            &
     &                        Vname(1,idTtlf(i)), INPvid,               &
     &                        Irec, MyType, Vsize,                      &
     &                        LBi, UBi, LBj, UBj, 1, N(ng),             &
     &                        scale, Fmin, Fmax,                        &
#  ifdef MASKING
     &                        GRID(ng) % rmask,                         &
#  endif
     &                        OCEAN(ng) % ad_t(:,:,:,Iinp,i))
            IF (FoundError(status, nf90_noerr, __LINE__,                &
     &                     __FILE__)) THEN
              IF (Master) THEN
                WRITE (stdout,20) TRIM(Vname(1,idTtlf(i))), Irec,       &
     &                            TRIM(INPncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            END IF
!
            status=nf_fwrite3d(ng, model, TLF(ng)%ncid, TLF(ng)%Tid(i), &
     &                         Iout, MyType,                            &
     &                         LBi, UBi, LBj, UBj, 1, N(ng), scale,     &
#  ifdef MASKING
     &                         GRID(ng) % rmask,                        &
#  endif
     &                         OCEAN(ng) % ad_t(:,:,:,Iinp,i))
            IF (FoundError(status, nf90_noerr, __LINE__,                &
     &                     __FILE__)) THEN
              IF (Master) THEN
                WRITE (stdout,30) TRIM(Vname(1,idTtlf(i))), Irec,       &
     &                            TRIM(TLF(ng)%name)
              END IF
              exit_flag=3
              ioerror=status
              RETURN
            END IF
          ELSE
            IF (Master) WRITE (stdout,40) TRIM(Vname(1,idTtlf(i))),     &
     &                                    TRIM(INPncname)
            exit_flag=2
            RETURN
          END IF
        END DO
# endif
      END DO
!
!  Close input NetCDF file.
!
      CALL netcdf_close (ng, model, INPncid, INPncname, .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) THEN
        WRITE (stdout,50) TRIM(INPncname)
        RETURN
      END IF
!
!-----------------------------------------------------------------------
!  Synchronize impulse NetCDF file to disk to allow other processes
!  to access data immediately after it is written.
!-----------------------------------------------------------------------
!
      CALL netcdf_sync (ng, model, INPncname, TLF(ng)%ncid)
      IF (FoundError(exit_flag, NoError, __LINE__,                      &
     &               __FILE__)) RETURN

      IF (Master)  WRITE (stdout,60) Nrec-1, TRIM(TLF(ng)%name)
!
  10  FORMAT (/,' WRT_IMPULSE - unable to open input NetCDF file: ',a)
  20  FORMAT (/,' WRT_IMPULSE - error while reading variable: ',a,2x,   &
     &        'at time record = ',i3,/,17x,'in input NetCDF file: ',a)
  30  FORMAT (/,' WRT_IMPULSE - error while writing variable: ',a,2x,   &
     &        'at time record = ',i3,/,17x,'into NetCDF file: ',a)
  40  FORMAT (/,' WRT_IMPULSE - cannot find state variable: ',a,        &
     &        /,12x,'in input NetCDF file: ',a)
  50  FORMAT (/,' WRT_IMPULSE - unable to close input NetCDF file: ',a)
  60  FORMAT (4x,'WRT_IMPULSE - wrote convolved adjoint impulses, ',    &
     &           'records: 001 to ',i3.3,/,18x,'file: ',a)

      RETURN
      END SUBROUTINE wrt_impulse
#else
      SUBROUTINE wrt_impulse
      END SUBROUTINE wrt_impulse
#endif
