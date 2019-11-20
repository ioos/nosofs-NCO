/* Copyright (C) 1991-2012 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */


/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */

/* We do support the IEC 559 math functionality, real and complex.  */

/* wchar_t uses ISO/IEC 10646 (2nd ed., published 2011-03-15) /
   Unicode 6.0.  */

/* We do not support C11 <threads.h>.  */

MODULE MOD_1D
!******************************************************************
!***        DEFINE PARAMETERS RELATED TO PHYTOPLANKTON          ***
!***   PARAMETERS ARE LISTED ALPHABETICALLY FOR EACH CATEGORY   ***
!******************************************************************
     IMPLICIT NONE
     SAVE
     INTEGER, PARAMETER    :: SPP              = SELECTED_REAL_KIND(6,30)
     INTEGER, PARAMETER    :: DPP              = SELECTED_REAL_KIND(12,300)
     INTEGER               :: KBV              ! VERTICAL LAYERS
     INTEGER               :: KBVM1            ! VERTICAL LAYERS MINUS 1
     INTEGER               :: NTT              ! TOTAL NUMBER OF STATE VARIALBES
     INTEGER               :: NNA,INA          ! AUXILLIARY, TEMPORALLY HERE

     CHARACTER(LEN=50)               :: STRUCTURE     ! MODEL STRUCTURE
     CHARACTER(LEN=50), ALLOCATABLE  :: BIO_NAME(:,:) ! VARIABLE NAME FOR NETCDF OUTPUT

!     REAL(SPP), PARAMETER   :: ZERO             = 0.0_SPP
     REAL(SPP), ALLOCATABLE :: BIO_VAR(:,:)     ! ENSEMBLE VARIABLES FOR MIXING
     REAL(SPP), ALLOCATABLE :: DELTA_D(:)       ! LAYER THICKNESS
     REAL(SPP), ALLOCATABLE :: DELTA_Z(:)       ! LAYER BETWEEN SIGMA CENTRE
     REAL(SPP), ALLOCATABLE :: DEPTH_Z(:)       ! DEPTH AT EACH LAYER CENTRE
     REAL(SPP), ALLOCATABLE :: KM_BIO(:)        ! VERTICAL MIXING COEFFICIENT
     REAL(SPP), ALLOCATABLE :: RADIATION(:)     ! SURFACE RATIATION
     REAL(SPP), ALLOCATABLE :: T_BIO(:)         ! TEMPERATURE
     REAL(SPP)              :: T_STEP           ! TIME STEP
END MODULE MOD_1D
