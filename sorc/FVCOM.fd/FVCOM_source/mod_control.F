!/===========================================================================/
! Copyright (c) 2007, The University of Massachusetts Dartmouth 
! Produced at the School of Marine Science & Technology 
! Marine Ecosystem Dynamics Modeling group
! All rights reserved.
!
! FVCOM has been developed by the joint UMASSD-WHOI research team. For 
! details of authorship and attribution of credit please see the FVCOM
! technical manual or contact the MEDM group.
!
! 
! This file is part of FVCOM. For details, see http://fvcom.smast.umassd.edu 
! The full copyright notice is contained in the file COPYRIGHT located in the 
! root directory of the FVCOM code. This original header must be maintained
! in all distributed versions.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
! AND ANY EXPRESS OR  IMPLIED WARRANTIES, INCLUDING,  BUT NOT  LIMITED TO,
! THE IMPLIED WARRANTIES OF MERCHANTABILITY AND  FITNESS FOR A PARTICULAR
! PURPOSE ARE DISCLAIMED.  
!
!/---------------------------------------------------------------------------/
! CVS VERSION INFORMATION
! $Id$
! $Name$
! $Revision$
!/===========================================================================/

!==============================================================================|
!   CONTROL VARIABLES                                                          |
!==============================================================================|

MODULE CONTROL
   USE MOD_TIME
   USE MOD_PREC   
   USE NETCDF
   IMPLICIT NONE
   SAVE

   ! Stuff set inside FVCOM at compile time:
   LOGICAL :: SERIAL         !!TRUE IF SINGLE PROCESSOR
   LOGICAL ::    MSR         !!TRUE IF MASTER PROCESSOR (MYID==1)
   LOGICAL ::    PAR         !!TRUE IF MULTIPROCESSOR RUN
   LOGICAL ::   IOPROC         !!TRUE IF PROCESSOR IS THE IOPROC
   LOGICAL :: IN_MPI_IO_LOOP !!TRUE IF OUTNODE IS ALREADY IN THE LOOP
   CHARACTER(LEN=80) PRG_NAME

   INTEGER MPI_FVCOM_GROUP         !! PROCESSORS WORKING ON FVCOM
!   INTEGER MPI_OTHER_GROUP        !! ADD OTHER GROUPS HERE 

   ! FVCOM info strings
   CHARACTER(LEN=80) FVCOM_VERSION !!STRING DESCRIBING VERSION
   CHARACTER(LEN=80) INSTITUTION   !!STRING DESCRIBING FVCOM MOTHER SHIP
   CHARACTER(LEN=80) FVCOM_WEBSITE !!STRING DESCRIBING WEBSITE FOR FVCOM INFO 

   ! Set at command line
   CHARACTER(LEN=80) CASENAME      !!LETTER ACRONYM SPECIFYING CASE
   CHARACTER(LEN=80) INFOFILE      !!INFO  FILE            

   ! FOR LOADING RUNFILE
   LOGICAL BLANK_NAMELIST
   CHARACTER(LEN=80) NAMELIST_NAME

    !--Parameters in NameList NML_CASE
    CHARACTER(LEN=80) CASE_TITLE     !!CASE TITLE                                 
    CHARACTER(Len=80) DATE_FORMAT    !!DATE FORMAT: ie 2007-11-19 => YMD 
    CHARACTER(Len=80) TIMEZONE       !!Name of TIME ZONE OR NONE
    CHARACTER(Len=80) START_DATE     !!DATE TO START MODEL
    CHARACTER(Len=80) END_DATE       !!DATE TO END MODEL 
    CHARACTER(Len=80) DATE_REFERENCE !!USER DEFINED REFERENCE DATE OR DEFAULT
    
    LOGICAL USE_REAL_WORLD_TIME
    NAMELIST /NML_CASE/        &
         & CASE_TITLE,         &
         & TIMEZONE,           &
         & DATE_FORMAT,        &
	 & DATE_REFERENCE,     &
         & START_DATE,         &
         & END_DATE

    !--Paramters in NameList NML_STARTUP
    CHARACTER(LEN=80) STARTUP_TYPE  !!'hotstart' or 'coldstart' 
    CHARACTER(LEN=80) STARTUP_FILE  !!NAME OF START FILE
    CHARACTER(LEN=80) STARTUP_TS_TYPE  !!TYPE OF TS START
    CHARACTER(LEN=80) STARTUP_UV_TYPE  !!TYPE OF TS START
    CHARACTER(LEN=80) STARTUP_TURB_TYPE  !!TYPE OF TS START
    REAL(SP) :: STARTUP_T_VALS(2)
    REAL(SP) :: STARTUP_S_VALS(2)
    REAL(SP) :: STARTUP_U_VALS
    REAL(SP) :: STARTUP_V_VALS
    REAL(SP) :: STARTUP_DMAX
        
    LOGICAL CMDLN_RESTART

    CHARACTER(LEN=80), parameter :: STARTUP_TYPE_COLDSTART = 'coldstart'
    CHARACTER(LEN=80), parameter :: STARTUP_TYPE_HOTSTART = 'hotstart'
    CHARACTER(LEN=80), parameter :: STARTUP_TYPE_CRASHRESTART = 'crashrestart'
    CHARACTER(LEN=80), parameter :: STARTUP_TYPE_FORECAST = 'forecast'

    LOGICAL FORECAST_MODE

    CHARACTER(LEN=80), parameter :: STARTUP_TYPE_DEFAULT = 'default'
    CHARACTER(LEN=80), parameter :: STARTUP_TYPE_CONSTANT = 'constant'
    CHARACTER(LEN=80), parameter :: STARTUP_TYPE_LINEAR = 'linear'
    CHARACTER(LEN=80), parameter :: STARTUP_TYPE_OBSERVED = 'observed'
    CHARACTER(LEN=80), parameter :: STARTUP_TYPE_SETVALUES = 'set values'


    NAMELIST /NML_STARTUP/     &
         & STARTUP_TYPE,       & ! coldstart, hotstart, crashrestart
         & STARTUP_FILE,       &
         & STARTUP_UV_TYPE,    &  ! default, set values
         & STARTUP_TURB_TYPE,  &  ! default, set values
         & STARTUP_TS_TYPE,    &  ! constant, linear, observed, set values
         & STARTUP_T_VALS,     &
         & STARTUP_S_VALS,     &
         & STARTUP_U_VALS,     &
         & STARTUP_V_VALS,     &
         & STARTUP_DMAX



    !--Parameters in NameList NML_IO
    CHARACTER(LEN=80) INPUT_DIR         !!MAIN   INPUT DIRECTORY
    CHARACTER(LEN=80) OUTPUT_DIR        !!PARENT OUTPUT DIRECTORY
    INTEGER IREPORT                !!INTERVAL (IINT) FOR REPORTING OF FLOWFIELD STATISTICS
    LOGICAL VISIT_ALL_VARS         !!SET THE LEVEL OF COMPLEXITY IN VISIT
    LOGICAL WAIT_FOR_VISIT         !!WAIT FOR VISIT TO CONNECT BEFORE BEGINING INTEGRATION
    LOGICAL USE_MPI_IO_MODE        !!TURN ON THE PRINTER NODE FOR MPI JOBS

    NAMELIST /NML_IO/         &
         & INPUT_DIR,         &
         & OUTPUT_DIR,        &
         & IREPORT,           &
         & VISIT_ALL_VARS,    &
         & WAIT_FOR_VISIT,    &
         & USE_MPI_IO_MODE
        
    !--Parameters in NameList NML_INTEGRATION

! FOR EXPLICIT TIME STEP MODEL
    REAL(DP)  EXTSTEP_SECONDS!!EXTERNAL TIME STEP IN SECONDS;
                             !! THIS VALUE WILL BE TRUNCATED TO MICROSECONDS!!
    INTEGER   ISPLIT         !!NUMBER OF ITERATIONS OF EXTERNAL MODE/INTERNAL STEP

! FOR SEMI-IMPLICIT TIME STEP MODEL
    REAL(DP)  INTSTEP_SECONDS!!INTERNAL TIME STEP IN SECONDS FOR SEMI IMPLICIT;

    INTEGER   IRAMP          !!RAMP FACTOR USED TO EASE STARTUP = f(IINT)
    REAL(SP)  STATIC_SSH_ADJ !!WATER LEVEL CONSTANT ADJUSTMENT
    REAL(SP)  MIN_DEPTH      !!MINIMUM ALLOWABLE DEPTH         

    NAMELIST /NML_INTEGRATION/  &
# if defined(SEMI_IMPLICIT)
         & INTSTEP_SECONDS,     &
# else
         & EXTSTEP_SECONDS,     &
         & ISPLIT,              &
# endif
         & IRAMP,               &
         & MIN_DEPTH,           &
         & STATIC_SSH_ADJ
    


    !--Parameters in NameList NML_RESTART
    LOGICAL RST_ON                  !!TRUE IF OUTPUT RESART FILES
    CHARACTER(LEN=80) RST_FIRST_OUT !!DATE OF FIRST RESTART FILE OUTPUT
    CHARACTER(LEN=80) RST_OUT_INTERVAL       !!INTERVAL IN DECIMAL DAYS FOR CREATING A RESTART FILE
    INTEGER  RST_OUTPUT_STACK       !!NUMBER OF TIMESTEPS PER FILE

    CHARACTER(LEN=80) RESTART_FILE_NAME !!NAME OF RESTART FILE
    ! ONLY VALID TILL THE NAME IS INCRIMENTED IF THE STKCNT IS EXCEEDED

    NAMELIST /NML_RESTART/   &
         & RST_ON,           &
         & RST_FIRST_OUT,    &
         & RST_OUT_INTERVAL, &
         & RST_OUTPUT_STACK

        
    !--Parameters in NameList NML_NETCDF
    LOGICAL NC_ON
    CHARACTER(LEN=80) NC_FIRST_OUT        !! DATE TO START NETCDF OUTPUT
    CHARACTER(LEN=80) NC_OUT_INTERVAL     !! OUTPUT INTERVAL IN DECIMAL DAYS
    INTEGER  NC_OUTPUT_STACK              !! NUMBER OF TIMESTEPS PER FILE
    CHARACTER(LEN=120) NC_SUBDOMAIN_FILES !! DOMAIN OUTPUT SPECS
    ! GRID STUFF
    LOGICAL NC_GRID_METRICS
    LOGICAL NC_FILE_DATE
    ! MODEL DATA
    LOGICAL NC_VELOCITY
    LOGICAL NC_SALT_TEMP
    LOGICAL NC_TURBULENCE
    LOGICAL NC_VERTICAL_VEL
    LOGICAL NC_AVERAGE_VEL
    LOGICAL NC_VORTICITY
    LOGICAL NC_NH_QP
    LOGICAL NC_NH_RHS
    LOGICAL NC_ICE
    
    ! FORCING DATA
    LOGICAL NC_WIND_VEL
    LOGICAL NC_WIND_STRESS
#   if defined (AIR_PRESSURE) || (HEATING_CALCULATED) || (HEATING_CALCULATED_GL)
    LOGICAL NC_ATM_PRESS  
#   endif    
    LOGICAL NC_WAVE_PARA     
    LOGICAL NC_WAVE_STRESS   
    LOGICAL NC_EVAP_PRECIP
    LOGICAL NC_SURFACE_HEAT
    LOGICAL NC_GROUNDWATER
    LOGICAL NC_BIO
    LOGICAL NC_WQM

    CHARACTER(LEN=80) NC_FILE_NAME !!NAME OF NC FILE
    ! ONLY VALID TILL THE NAME IS INCRIMENTED IF THE STKCNT IS EXCEEDED

    NAMELIST /NML_NETCDF/    &
         & NC_ON,            &
         & NC_FIRST_OUT,     &
         & NC_OUT_INTERVAL,  &
         & NC_OUTPUT_STACK,  &
         & NC_SUBDOMAIN_FILES,&
         & NC_GRID_METRICS,  &
         & NC_FILE_DATE,     &
         & NC_VELOCITY,      &
         & NC_SALT_TEMP,     &
         & NC_TURBULENCE,    &
         & NC_AVERAGE_VEL,   &
         & NC_VERTICAL_VEL,  &
# if defined(NH)
         & NC_NH_QP,         &
         & NC_NH_RHS,        &
# endif
         & NC_WIND_VEL,      &
         & NC_WIND_STRESS,   &
# if defined (AIR_PRESSURE) || (HEATING_CALCULATED) || (HEATING_CALCULATED_GL)
         & NC_ATM_PRESS,     &
# endif
# if defined(WAVE_CURRENT_INTERACTION)
         & NC_WAVE_PARA,     &    
         & NC_WAVE_STRESS,   &
# endif
         & NC_EVAP_PRECIP,   &
         & NC_SURFACE_HEAT,  &
         & NC_GROUNDWATER,   &
# if defined(ICE)
         & NC_ICE,           &
# endif
         & NC_BIO,           &
         & NC_WQM,           &
	 & NC_VORTICITY

    !--Parameters in NameList NML_NETCDF_AV
    LOGICAL NCAV_ON                         !! TURN ON NETCDF AVERAGING
    CHARACTER(LEN=80) NCAV_FIRST_OUT        !! DATE TO START NETCDF AVERAGE OUTPUT
    CHARACTER(LEN=80) NCAV_OUT_INTERVAL     !! OUTPUT INTERVAL IN DECIMAL DAYS
    INTEGER  NCAV_OUTPUT_STACK              !! NUMBER OF TIMESTEPS PER FILE
    CHARACTER(LEN=120) NCAV_SUBDOMAIN_FILES !! DOMAIN OUTPUT SPECS
    ! GRID STUFF
    LOGICAL NCAV_GRID_METRICS
    LOGICAL NCAV_FILE_DATE
    ! MODEL DATA
    LOGICAL NCAV_VELOCITY
    LOGICAL NCAV_SALT_TEMP
    LOGICAL NCAV_TURBULENCE
    LOGICAL NCAV_VERTICAL_VEL
    LOGICAL NCAV_AVERAGE_VEL
    LOGICAL NCAV_VORTICITY
    LOGICAL NCAV_NH_QP
    LOGICAL NCAV_NH_RHS
    LOGICAL NCAV_ICE

    ! FORCING DATA
    LOGICAL NCAV_WIND_VEL
    LOGICAL NCAV_WIND_STRESS
# if defined (AIR_PRESSURE) || (HEATING_CALCULATED) || (HEATING_CALCULATED_GL)
    LOGICAL NCAV_ATM_PRESS  
# endif
    LOGICAL NCAV_WAVE_PARA    
    LOGICAL NCAV_WAVE_STRESS  
    LOGICAL NCAV_EVAP_PRECIP
    LOGICAL NCAV_SURFACE_HEAT
    LOGICAL NCAV_GROUNDWATER
    LOGICAL NCAV_BIO
    LOGICAL NCAV_WQM

    CHARACTER(LEN=80) NCAV_FILE_NAME !!NAME OF NCAV FILE
    ! ONLY VALID TILL THE NAME IS INCRIMENTED IF THE STKCNT IS EXCEEDED

    NAMELIST /NML_NETCDF_AV/  &
         & NCAV_ON,           &
         & NCAV_FIRST_OUT,    &
         & NCAV_OUT_INTERVAL, &
         & NCAV_OUTPUT_STACK, &
         & NCAV_SUBDOMAIN_FILES,&
         & NCAV_GRID_METRICS,   &
         & NCAV_FILE_DATE,      &
         & NCAV_VELOCITY,       &
         & NCAV_SALT_TEMP,      &
         & NCAV_TURBULENCE,     &
         & NCAV_AVERAGE_VEL,    &
         & NCAV_VERTICAL_VEL,   &
# if defined(NH)
         & NCAV_NH_QP,          &
         & NCAV_NH_RHS,         &
# endif
         
# if defined(ICE)
         & NCAV_ICE,          &
# endif
         & NCAV_WIND_VEL,       &
         & NCAV_WIND_STRESS,    &
# if defined (AIR_PRESSURE) || (HEATING_CALCULATED) || (HEATING_CALCULATED_GL)
         & NCAV_ATM_PRESS,      &
# endif
# if defined(WAVE_CURRENT_INTERACTION)
         & NCAV_WAVE_PARA,      &    
         & NCAV_WAVE_STRESS,    &
# endif
         & NCAV_EVAP_PRECIP,    &
         & NCAV_SURFACE_HEAT,   &
         & NCAV_GROUNDWATER,    &
         & NCAV_BIO,            &
         & NCAV_WQM,            &
	 & NCAV_VORTICITY

    !--Parameters in NameList NML_PHYSICS
    CHARACTER(LEN=80) HORIZONTAL_MIXING_TYPE
    CHARACTER(LEN=80) HORIZONTAL_MIXING_FILE
    CHARACTER(LEN=80) HORIZONTAL_MIXING_KIND
    REAL(SP)  HORIZONTAL_MIXING_COEFFICIENT
    REAL(SP)  HORIZONTAL_PRANDTL_NUMBER

! REMOVED HORCON, THIS IS NOW STORED IN THE ARRAYS, NN_HVC AND CC_HVC
!    REAL(SP) HORCON ! FVCOM NAME
    REAL(SP) HPRNU  ! FVCOM NAME

    CHARACTER(LEN=80) VERTICAL_MIXING_TYPE
    REAL(SP) VERTICAL_MIXING_COEFFICIENT
    REAL(SP) VERTICAL_PRANDTL_NUMBER

    REAL(SP) UMOL   ! FVCOM NAME
    REAL(SP) VPRNU  ! FVCOM NAME

    CHARACTER(LEN=80) BOTTOM_ROUGHNESS_KIND 
    CHARACTER(LEN=80) BOTTOM_ROUGHNESS_TYPE 
    CHARACTER(LEN=80) BOTTOM_ROUGHNESS_FILE 
    REAL(SP)  BOTTOM_ROUGHNESS_MINIMUM          !!MINIMUM BOTTOM DRAG COEFFICIENT
    REAL(SP)  BOTTOM_ROUGHNESS_LENGTHSCALE      !!BOTTOM FRICTION DEPTH LENGTH SCALE

    REAL(SP) CBCMIN ! FVCOM NAME

    CHARACTER(LEN=80),parameter :: BR_ORIG   = 'orig'
    CHARACTER(LEN=80),parameter :: BR_GOTM   = 'gotm'

    LOGICAL CONVECTIVE_OVERTURNING
    LOGICAL SCALAR_POSITIVITY_CONTROL
    LOGICAL BAROTROPIC
    CHARACTER(LEN=80) SEA_WATER_DENSITY_FUNCTION
    CHARACTER(LEN=80) BAROCLINIC_PRESSURE_GRADIENT
    LOGICAL TEMPERATURE_ACTIVE
    LOGICAL SALINITY_ACTIVE
    LOGICAL SURFACE_WAVE_MIXING

    ! AT PRESENT YOU CAN NOT COMPILE WITH WET DRY WITHOUT USING IT!
    LOGICAL WETTING_DRYING_ON

    LOGICAL  RECALCULATE_RHO_MEAN
    CHARACTER(LEN=80) INTERVAL_RHO_MEAN

    CHARACTER(LEN=80),parameter :: SW_DENS1 = 'dens1'
    CHARACTER(LEN=80),parameter :: SW_DENS2 = 'dens2'
    CHARACTER(LEN=80),parameter :: SW_DENS3 = 'dens3'

    LOGICAL ADCOR_ON
    LOGICAL EQUATOR_BETA_PLANE
    LOGICAL NOFLUX_BOT_CONDITION

    NAMELIST /NML_PHYSICS/               &
         & HORIZONTAL_MIXING_TYPE,       &
         & HORIZONTAL_MIXING_FILE,       &
         & HORIZONTAL_MIXING_KIND,       &
         & HORIZONTAL_MIXING_COEFFICIENT,&
         & HORIZONTAL_PRANDTL_NUMBER,    & 
         & VERTICAL_MIXING_TYPE,         &
         & VERTICAL_MIXING_COEFFICIENT,  &
         & VERTICAL_PRANDTL_NUMBER,      &
         & BOTTOM_ROUGHNESS_TYPE,        &
         & BOTTOM_ROUGHNESS_KIND,        &
         & BOTTOM_ROUGHNESS_FILE,        &
         & BOTTOM_ROUGHNESS_LENGTHSCALE, &
         & BOTTOM_ROUGHNESS_MINIMUM,     &
         & CONVECTIVE_OVERTURNING,       &
         & SCALAR_POSITIVITY_CONTROL,    &
         & BAROTROPIC,                   &
         & BAROCLINIC_PRESSURE_GRADIENT, &
         & SEA_WATER_DENSITY_FUNCTION,   &
         & RECALCULATE_RHO_MEAN,         &
         & INTERVAL_RHO_MEAN,            &
         & TEMPERATURE_ACTIVE,           &
         & SALINITY_ACTIVE,              &
         & SURFACE_WAVE_MIXING,          &
         & WETTING_DRYING_ON,            &
         & ADCOR_ON,                     &
	 & EQUATOR_BETA_PLANE,           &
         & NOFLUX_BOT_CONDITION


    !--Parameters in NameList NML_SURFACE_FORCING
  
    LOGICAL  WIND_ON
    CHARACTER(LEN=80) WIND_TYPE
    CHARACTER(LEN=80) WIND_FILE
    CHARACTER(LEN=80) WIND_KIND
    REAL(SP) WIND_X
    REAL(SP) WIND_Y
    
    LOGICAL HEATING_ON
    CHARACTER(LEN=80) HEATING_TYPE
    CHARACTER(LEN=80) HEATING_FILE
    CHARACTER(LEN=80) HEATING_KIND
    REAL(SP) HEATING_LONGWAVE_PERCTAGE
    REAL(SP) HEATING_LONGWAVE_LENGTHSCALE
    REAL(SP) HEATING_SHORTWAVE_LENGTHSCALE
    REAL(SP) HEATING_RADIATION
    REAL(SP) HEATING_NETFLUX

    !FVCOM NAMES
    REAL(SP) RHEAT ! LONG WAVE PERCENTAGE
    REAL(SP) ZETA1 ! LONG WAVE LENGTH
    REAL(SP) ZETA2 ! SHORT WAVE LENGTH
    

    LOGICAL PRECIPITATION_ON
    CHARACTER(LEN=80) PRECIPITATION_FILE    
    CHARACTER(LEN=80) PRECIPITATION_KIND    
    REAL(SP) :: PRECIPITATION_EVP
    REAL(SP) :: PRECIPITATION_PRC

    LOGICAL AIRPRESSURE_ON
    CHARACTER(LEN=80) AIRPRESSURE_FILE    
    CHARACTER(LEN=80) AIRPRESSURE_KIND
    REAL(SP) :: AIRPRESSURE_VALUE

    LOGICAL WAVE_ON               
    CHARACTER(LEN=80) WAVE_FILE    
    CHARACTER(LEN=80) WAVE_KIND
    REAL(SP) :: WAVE_HEIGHT
    REAL(SP) :: WAVE_LENGTH
    REAL(SP) :: WAVE_DIRECTION
    REAL(SP) :: WAVE_PERIOD
    REAL(SP) :: WAVE_PER_BOT
    REAL(SP) :: WAVE_UB_BOT

    ! FORCING KINDS
    CHARACTER(LEN=80),parameter:: CNSTNT   = "constant"
    CHARACTER(LEN=80),parameter:: STTC     = "static"
    CHARACTER(LEN=80),parameter:: TMDPNDNT = "time dependant"
    CHARACTER(LEN=80),parameter:: PRDC     = "periodic"
    CHARACTER(LEN=80),parameter:: VRBL     = "variable"

    CHARACTER(LEN=80), parameter:: SPEED  = "speed"
    CHARACTER(LEN=80), parameter:: STRESS = "stress"


    NAMELIST /NML_SURFACE_FORCING/          &
         & WIND_ON,                         &
         & WIND_TYPE,                       &
         & WIND_FILE,                       &
         & WIND_KIND,                       &
         & WIND_X,                          &
         & WIND_Y,                          &
         & HEATING_ON,                      &
         & HEATING_TYPE,                    &
         & HEATING_KIND,                    &
         & HEATING_FILE,                    &
         & HEATING_LONGWAVE_LENGTHSCALE,    &
         & HEATING_LONGWAVE_PERCTAGE,       &
         & HEATING_SHORTWAVE_LENGTHSCALE,   & 
         & HEATING_RADIATION,               &
         & HEATING_NETFLUX,                 & 
         & PRECIPITATION_ON,                &
         & PRECIPITATION_KIND,              &
         & PRECIPITATION_FILE,              &
         & PRECIPITATION_PRC,               &
         & PRECIPITATION_EVP,               &
	 & AIRPRESSURE_ON,                  &
	 & AIRPRESSURE_KIND,                &
	 & AIRPRESSURE_FILE,                &
	 & AIRPRESSURE_VALUE,               &
         & WAVE_ON,                         &   
         & WAVE_FILE,                       &
         & WAVE_KIND,                       &
         & WAVE_HEIGHT,                     &
         & WAVE_LENGTH,                     &
         & WAVE_DIRECTION,                  &
         & WAVE_PERIOD,                     &
         & WAVE_PER_BOT,                    &
         & WAVE_UB_BOT  

    !--Parameters in NameList NML_RIVER_TYPE
    CHARACTER(LEN=80) RIVER_TS_SETTING ! METHOD TO CALCULATE T&S AT
    ! THE RIVER MOUTH
    CHARACTER(LEN=80) RIVER_INFLOW_LOCATION ! IS THE FLUX
    CHARACTER(LEN=80) RIVER_KIND ! PERIODIC OR VARIABLE
    CHARACTER(LEN=80) RIVER_INFO_FILE ! RIVERS INFORMATION FILE
    ! SPECIFIED FOR NODES OR EDGES
    INTEGER RIVER_NUMBER ! THE NUMBER OF RIVERS

    NAMELIST /NML_RIVER_TYPE/               &
         & RIVER_NUMBER,                    &
         & RIVER_KIND,                      &
         & RIVER_TS_SETTING,                &
         & RIVER_INFO_FILE,                 &
         & RIVER_INFLOW_LOCATION
    

    !--Parameters in NameList NML_RIVER
    INTEGER, PARAMETER :: RIVER_CHAR_LEN=60

    INTEGER, PARAMETER :: MAX_LAYERS = 100

    CHARACTER(LEN=80) RIVER_NAME
    CHARACTER(LEN=80) RIVER_FILE
    INTEGER RIVER_GRID_LOCATION

# if defined(RIVER_FLOAT)
    ! ASSUMING MAXIMUM NUMBER OF LAYERS!
    REAL(SP) :: RIVER_VERTICAL_DISTRIBUTION(MAX_LAYERS)
# else
    CHARACTER(LEN=RIVER_CHAR_LEN) RIVER_VERTICAL_DISTRIBUTION
# endif

    NAMELIST /NML_RIVER/                    &
         & RIVER_NAME,                      &
         & RIVER_FILE,                      &
         & RIVER_GRID_LOCATION,             &        
         & RIVER_VERTICAL_DISTRIBUTION

    
    ! THIS TYPE PROVIDES INTERMEDIATE STORAGE FOR RIVER INFO UNTILL
    ! THE RIVER FORCING IS SET UP IN MOD_FORCE
    type RIVER
       CHARACTER(LEN=80) NAME
       CHARACTER(LEN=80) FILE
       INTEGER LOCATION
# if defined(RIVER_FLOAT)
       REAL(SP) :: DISTRIBUTION(MAX_LAYERS)
# else
       CHARACTER(LEN=RIVER_CHAR_LEN) DISTRIBUTION
# endif
    end type RIVER

    type(RIVER), Allocatable, DIMENSION(:) :: RIVERS

    !--Parameters in NameList NML_OPEN_BOUNDARY
    LOGICAL OBC_ON
    CHARACTER(LEN=80) OBC_NODE_LIST_FILE
    LOGICAL OBC_ELEVATION_FORCING_ON
    CHARACTER(LEN=80) OBC_ELEVATION_FILE
    INTEGER OBC_TS_TYPE
    LOGICAL OBC_TEMP_NUDGING
    CHARACTER(LEN=80) OBC_TEMP_FILE
    REAL(SP) OBC_TEMP_NUDGING_TIMESCALE
    LOGICAL OBC_SALT_NUDGING
    CHARACTER(LEN=80) OBC_SALT_FILE
    REAL(SP) OBC_SALT_NUDGING_TIMESCALE
#   if defined (WATER_QUALITY)
    LOGICAL OBC_WQM_NUDGING
    CHARACTER(LEN=80) OBC_WQM_FILE
    REAL(SP) OBC_WQM_NUDGING_TIMESCALE
#   endif    
#   if defined (BioGen)
    LOGICAL OBC_BIO_NUDGING
    CHARACTER(LEN=80) OBC_BIO_FILE
    REAL(SP) OBC_BIO_NUDGING_TIMESCALE
#   endif    
    LOGICAL OBC_MEANFLOW
    CHARACTER(LEN=80) OBC_MEANFLOW_FILE
    LOGICAL OBC_LONGSHORE_FLOW_ON
    CHARACTER(LEN=80) OBC_LONGSHORE_FLOW_FILE
    INTEGER OBC_TIDEOUT_INITIAL,OBC_TIDEOUT_INTERVAL
    LOGICAL OBC_DEPTH_CONTROL_ON

    NAMELIST /NML_OPEN_BOUNDARY_CONTROL/    &
         & OBC_ON,                          &
         & OBC_NODE_LIST_FILE,              &
         & OBC_ELEVATION_FORCING_ON,        &
         & OBC_ELEVATION_FILE,              &
         & OBC_TS_TYPE,                     &
         & OBC_TEMP_NUDGING,                &
         & OBC_TEMP_FILE,                   &
         & OBC_TEMP_NUDGING_TIMESCALE,      &
         & OBC_SALT_NUDGING,                &
         & OBC_SALT_FILE,                   &
         & OBC_SALT_NUDGING_TIMESCALE,      &
#   if defined (WATER_QUALITY)
         & OBC_WQM_NUDGING,                 &
         & OBC_WQM_FILE,                    &
         & OBC_WQM_NUDGING_TIMESCALE,       &
#   endif    
#   if defined (BioGen)
         & OBC_BIO_NUDGING,                 &
         & OBC_BIO_FILE,                    &
         & OBC_BIO_NUDGING_TIMESCALE,       &
#   endif    
         & OBC_MEANFLOW,                    &
         & OBC_MEANFLOW_FILE,               &
         & OBC_TIDEOUT_INITIAL,             &
         & OBC_TIDEOUT_INTERVAL,            &
         & OBC_LONGSHORE_FLOW_ON,           &
         & OBC_LONGSHORE_FLOW_FILE,         &
         & OBC_DEPTH_CONTROL_ON


    !--Parameters in NameList GRID_COORDINATES
    CHARACTER(LEN=80) GRID_FILE
    CHARACTER(LEN=80) GRID_FILE_UNITS
    CHARACTER(LEN=200) PROJECTION_REFERENCE
    CHARACTER(LEN=80) SIGMA_LEVELS_FILE
    CHARACTER(LEN=80) DEPTH_FILE
    CHARACTER(LEN=80) CORIOLIS_FILE
    CHARACTER(LEN=80) SPONGE_FILE

    LOGICAL USE_PROJ
! THESE ARE THE FVCOM NAMES FOR 
     !SIGMA_LEVELS = KB
     !SIGMA_LAYERS = KBM1 = KB - 1
     !SIGMA_LEVELS - 2 = KBM2

    NAMELIST /NML_GRID_COORDINATES/        &
         & GRID_FILE,                      &
         & GRID_FILE_UNITS,                &
         & PROJECTION_REFERENCE,           &
         & SIGMA_LEVELS_FILE,              &
         & DEPTH_FILE,                     &
         & CORIOLIS_FILE,                  &
         & SPONGE_FILE

    !--Parameters in NameList NML_GROUNDWATER
    LOGICAL GROUNDWATER_ON
    CHARACTER(LEN=80) GROUNDWATER_KIND
    CHARACTER(LEN=80) GROUNDWATER_FILE
    REAL(SP) GROUNDWATER_FLOW
    REAL(SP) GROUNDWATER_TEMP
    LOGICAL GROUNDWATER_TEMP_ON
    REAL(SP) GROUNDWATER_SALT
    LOGICAL GROUNDWATER_SALT_ON



    NAMELIST /NML_GROUNDWATER/            &
         & GROUNDWATER_ON,                &
         & GROUNDWATER_TEMP_ON,           &
         & GROUNDWATER_SALT_ON,           &
         & GROUNDWATER_KIND,              &
         & GROUNDWATER_FILE,              &
         & GROUNDWATER_FLOW,              &
         & GROUNDWATER_TEMP,              &
         & GROUNDWATER_SALT

    !--Parameters in NameList NML_LAG_PART
    LOGICAL LAG_PARTICLES_ON
    CHARACTER(LEN=80) LAG_START_FILE
    CHARACTER(LEN=80) LAG_OUT_FILE
    CHARACTER(LEN=80) LAG_FIRST_OUT
    CHARACTER(LEN=80) LAG_RESTART_FILE
    CHARACTER(LEN=80) LAG_OUT_INTERVAL 
    CHARACTER(LEN=80) LAG_SCAL_CHOICE

!!$    LOGICAL LAG_TEMPERATURE
!!$    LOGICAL LAG_SALINITY
!!$    LOGICAL LAG_DENSITY
!!$    LOGICAL LAG_EDDY_VISCOSITY
!!$    LOGICAL LAG_DIFFUSIVITY
  
    NAMELIST /NML_LAG/                    &
         & LAG_PARTICLES_ON,              &
         & LAG_START_FILE,                &
         & LAG_OUT_FILE,                  &
         & LAG_FIRST_OUT,                 &
         & LAG_RESTART_FILE,              &
         & LAG_OUT_INTERVAL,              &
         & LAG_SCAL_CHOICE


!!$         & LAG_TEMPERATURE,               &
!!$         & LAG_SALINITY,                  &
!!$         & LAG_DENSITY,                   &
!!$         & LAG_EDDY_VISCOSITY,            &
!!$         & LAG_DIFFUSIVITY

    !--Parameters in NameList NML_ADDITIONAL_MODELS
!    LOGICAL WATER_QUALITY_MODEL
!    CHARACTER(LEN=80) WATER_QUALITY_MODEL_FILE
    LOGICAL DATA_ASSIMILATION
    CHARACTER(LEN=80) DATA_ASSIMILATION_FILE
    LOGICAL BIOLOGICAL_MODEL
!!$    CHARACTER(LEN=80) BIOLOGICAL_MODEL_FILE
    CHARACTER(LEN=80) STARTUP_BIO_TYPE  !!TYPE OF BIO START
    LOGICAL SEDIMENT_MODEL
    CHARACTER(LEN=80) SEDIMENT_MODEL_FILE
# if defined (OFFLINE_SEDIMENT)
    CHARACTER(LEN=80) OFFLINE_SEDIMENT_FILE
# endif
    CHARACTER(LEN=80) SEDIMENT_PARAMETER_TYPE
    CHARACTER(LEN=80) SEDIMENT_PARAMETER_FILE
    CHARACTER(LEN=80) BEDFLAG_TYPE 
    CHARACTER(LEN=80) BEDFLAG_FILE 

    LOGICAL ICING_MODEL
    CHARACTER(LEN=80) ICING_FORCING_FILE
    CHARACTER(LEN=80) ICING_FORCING_KIND
    REAL(SP) ::ICING_AIR_TEMP
    REAL(SP) ::ICING_WSPD
    
    LOGICAL ICE_MODEL
    CHARACTER(LEN=80) ICE_FORCING_FILE
    CHARACTER(LEN=80) ICE_FORCING_KIND
    REAL(SP) ::ICE_SEA_LEVEL_PRESSURE
    REAL(SP) ::ICE_AIR_TEMP
    REAL(SP) ::ICE_SPEC_HUMIDITY
    REAL(SP) ::ICE_CLOUD_COVER
    REAL(SP) ::ICE_SHORTWAVE

    !ADDITIONAL MODEL DATA
    INTEGER            :: N_SED
    INTEGER, PARAMETER :: N_SED_MAX = 10
    CHARACTER(LEN=20)  :: SED_NAMES(N_SED_MAX)
    REAL(SP), ALLOCATABLE :: SEDDIS(:,:)

#   if defined (WATER_QUALITY)
    INTEGER, PARAMETER :: NB     = 8
    CHARACTER(LEN=80) :: WQM_NAME(NB,4)
#   endif

    REAL(SP), ALLOCATABLE :: BIODIS(:,:)


    ! FVCOM RUN MODE PARAMETERS
    CHARACTER(LEN=80) FVCOM_RUN_MODE
    CHARACTER(LEN=80),parameter :: FVCOM_PURE_SIM         = 'pure sim'
!    CHARACTER(LEN=80),parameter :: FVCOM_NUDGE_AVG_SST = 'nudge avg sst'
!    CHARACTER(LEN=80),parameter :: FVCOM_NUDGE_AVG_TSGRD = 'nudge avg ts'
    CHARACTER(LEN=80),parameter :: FVCOM_NUDGE_OI_ASSIM   = 'nudge or OI assim'   
!    CHARACTER(LEN=80),parameter :: FVCOM_OI_ASSIM      = 'OI ASSIM'
!    CHARACTER(LEN=80),parameter :: FVCOM_KALMAN_RRKF   = 'Kalman RRKF'
    CHARACTER(LEN=80),parameter :: FVCOM_RRKF_WITHOUT_SSA = 'RRKF WITHOUT SSH/SST'
    CHARACTER(LEN=80),parameter :: FVCOM_RRKF_WITH_SSA    = 'RRKF WITH SSH/SST'
    CHARACTER(LEN=80),parameter :: FVCOM_ENKF_WITHOUT_SSA = 'ENKF WITHOUT SSH/SST'
    CHARACTER(LEN=80),parameter :: FVCOM_ENKF_WITH_SSA    = 'ENKF WITH SSH/SST'
    CHARACTER(LEN=80),parameter :: FVCOM_KALMAN_4         = 'Kalman 4'

    NAMELIST /NML_ADDITIONAL_MODELS/      &
!         & WATER_QUALITY_MODEL,           &
!         & WATER_QUALITY_MODEL_FILE,      &
         & DATA_ASSIMILATION,             &
         & DATA_ASSIMILATION_FILE,        &
         & BIOLOGICAL_MODEL,              &
!!$         & BIOLOGICAL_MODEL_FILE,         &
         & STARTUP_BIO_TYPE,              &
         & SEDIMENT_MODEL,                &
         & SEDIMENT_MODEL_FILE,           &
# if defined (OFFLINE_SEDIMENT)
         & OFFLINE_SEDIMENT_FILE,         &
# endif
         & SEDIMENT_PARAMETER_TYPE,       &
         & SEDIMENT_PARAMETER_FILE,       &
         & BEDFLAG_TYPE,                  &
         & BEDFLAG_FILE,                  &
         & ICING_MODEL,                   &
         & ICING_FORCING_FILE,            &
         & ICING_FORCING_KIND,            &
         & ICING_AIR_TEMP,                &
         & ICING_WSPD,                    &
         & ICE_MODEL,                     &
         & ICE_FORCING_FILE,              &
         & ICE_FORCING_KIND,              &
         & ICE_SEA_LEVEL_PRESSURE,        &
         & ICE_AIR_TEMP,                  &
         & ICE_SPEC_HUMIDITY,             &
         & ICE_SHORTWAVE,                 &
         & ICE_CLOUD_COVER

    !--Parameters in NameList NML_PROBE
    LOGICAL PROBES_ON
    INTEGER PROBES_NUMBER
    CHARACTER(len=80) PROBES_FILE
    
    NAMELIST /NML_PROBES/      &
         & PROBES_ON,          &
         & PROBES_NUMBER,      &
         & PROBES_FILE

    !--Parameters in NameList NML_BOUNDSCHK
    !=> bounds checking
    LOGICAL  :: FORCE_ARCHIVE = .false.
    LOGICAL  :: BOUNDSCHK_ON = .false.
    INTEGER  :: CHK_INTERVAL
    REAL(SP) :: VELOC_MAG_MAX
    REAL(SP) :: ZETA_MAG_MAX
    REAL(SP) :: TEMP_MAX
    REAL(SP) :: TEMP_MIN
    REAL(SP) :: SALT_MAX
    REAL(SP) :: SALT_MIN
#   if defined (WAVE_CURRENT_INTERACTION)
    REAL(SP) :: HS_MAX = 1e9
#   endif
  
    NAMELIST /NML_BOUNDSCHK/      &
           & BOUNDSCHK_ON,        &
           & CHK_INTERVAL,        &
           & VELOC_MAG_MAX,       &
           & ZETA_MAG_MAX,        &
#  if defined(WAVE_CURRENT_INTERACTION)
           & HS_MAX,              &
#  endif
           & TEMP_MAX,            &
           & TEMP_MIN,            &
           & SALT_MAX,            &
           & SALT_MIN
    !<= bounds checking

!--Time Variables for FVCOM-----------------------------------------!
    TYPE(TIME) :: IntTime
    TYPE(TIME) :: ExtTime
    Type(TIME) :: RKTime
    TYPE(TIME) :: ZEROTIME

    TYPE(TIME) :: EndTime
    Type(TIME) :: StartTime
    Type(TIME) :: RUNFILE_StartTime
    Type(TIME) :: ReferenceDate

    TYPE(TIME) :: DELT_RHO_MEAN
    TYPE(TIME) :: RECALC_RHO_MEAN

    ! ZERO PHASE TIME FOR SPECTRAL (NON JULIAN) TIDE
    TYPE(TIME) :: SPECTIME
    
    REAL(SP)   :: DTE        !!EXTERNAL TIME STEP (Seconds)
    REAL(SP)   :: DTI        !!INTERNAL TIME STEP (Seconds)
    REAL(SP)   :: RAMP

    TYPE(TIME) :: IMDTE      !!EXTERNAL TIME STEP 
    TYPE(TIME) :: IMDTI      !!INTERNAL TIME STEP 
    
    INTEGER(itime) :: IINT     !!INTERNAL TIME STEP ITERATION NUMBER (ISTART => IEND)
    INTEGER(itime) :: IEXT     !!EXTERNAL TIME STEP ITERATION NUMBER (1 => ISPLIT)
    INTEGER(itime) :: ISTART   !!STARTING INTERNAL TIME STEP ITERATION NUMBER
    INTEGER(itime) :: IEND     !!ENDING INTERNAL TIME STEP ITERATION NUMBER
    INTEGER(itime) :: NSTEPS   !!Number OF INTERAL TIME STEPS IN SIMULATION
    
    ! Time variables for File IO
    
    INTEGER, PARAMETER:: TIMEPREC = 6
    !! THIS IS THE LENGHT THAT LOOKS NICE FOR GIVEN TIME PREC
    INTEGER, PARAMETER:: DATESTRLEN = 20+TIMEPREC 

!    CHARACTER(LEN=80) :: IO_FILE_DATE
!    CHARACTER(LEN=80) :: IO_timestr
!    real(SP)          :: IO_days
!    integer           :: IO_mjd
!    integer           :: IO_msec
!    integer           :: IO_IINT



!--Constants-------------------------------------------------------------------!

    ! SELECT WHETHER YOU WANT TO MAKE TIME MORE EXACT (SP CAUSES
    ! ROUND OF ERROR BUT IS THE TRADITIONAL PRECISION)
   REAL(DP), PARAMETER, DIMENSION(4) :: ALPHA_RK = (/0.2500_DP,1.0_DP/3.0_DP,0.5000_DP,1.0_DP/)
!   REAL(SP), PARAMETER, DIMENSION(4) :: ALPHA_RK = (/0.2500_DP,1.0_DP/3.0_DP,0.5000_DP,1.0_DP/)


   REAL(DP), PARAMETER :: GRAV      = 9.81_SP
   REAL(DP), PARAMETER :: PI        = 3.141592653589793238_DP
   REAL(DP), PARAMETER :: PI2       = 2.0_DP * 3.141592653589793238_DP
   REAL(DP), PARAMETER :: ZERO      = 0.0_DP 
   REAL(DP), PARAMETER :: ONE_THIRD = 1.0_DP/3.0_DP 
   REAL(DP), PARAMETER :: REARTH    = 6371.0E03_DP   !!Earth Radius in Meters
   REAL(DP), PARAMETER :: DEG2RAD   = PI2/360.0_DP   !!Radians/Degree
   REAL(DP), PARAMETER :: TPI       = DEG2RAD*REARTH !TPI=pi*rearth/180.=3.14159265/180.0*6371.*1000.
   REAL(DP), PARAMETER :: ROFVROS   = 0.9775171065_DP!!RATIO OF THE DENSITY OF FRESH AND SEA WATER 1000./1023.   
   real(DP), parameter :: SLP0      = 101325.0_sp    !! mean sea surface pressure (Pa)


!--Parameter Controlling Vertical Coordinate Distribution------------
   !----------!
   CHARACTER(LEN=80) :: STYPE
   CHARACTER(LEN=80), PARAMETER :: STYPE_UNIFORM= "UNIFORM"
   CHARACTER(LEN=80), PARAMETER :: STYPE_GEOMETRIC= "GEOMETRIC"
   CHARACTER(LEN=80), PARAMETER :: STYPE_TANH= "TANH"
   CHARACTER(LEN=80), PARAMETER :: STYPE_GENERALIZED= "GENERALIZED"
   CHARACTER(LEN=80), PARAMETER :: STYPE_RESTART= "RESTART"
!--Sigma Level Parameters for case GEOMETRIC OF UNIFORM------------------------!
   REAL(SP) :: P_SIGMA      !!PARAMETER CONTROLLING SIGMA LEVEL DISTRIBUTION

!--General Vertical Level Parameters for case TANH ----------------------------!
   REAL(SP) :: DU2          !!PARAMETER CONTROLLING LEVEL DISTRIBUTION OF SURFACE
   REAL(SP) :: DL2          !!PARAMETER CONTROLLING LEVEL DISTRIBUTION OF BOTTOM

!--General Vertical Level Parameters for case GENERALIZED ---------------------!
   REAL(SP) :: DUU          !!THE UPPER BOUNDARY OF PARALLEL COORDINATE
   REAL(SP) :: DLL          !!THE LOWER BOUNDARY OF PARALLEL COORDINATE
   REAL(SP) :: HMIN1        !!THE MIN DEPTH AT WHICH THE LAYERS ARE CONSTANT
   
   INTEGER  :: KU           !!THE NUMBERS OF LAYERS ABOVE UPPER BOUNDARY
   INTEGER  :: KL           !!THE NUMBVER OF LAYERS BELOW LOWER BOUNDARY
   
   REAL(SP), ALLOCATABLE :: ZKU(:)       !!THE DEPTHS OF PARALLEL LAYERS ABOVE UPPER BOUNDARY
   REAL(SP), ALLOCATABLE :: ZKL(:)       !!THE DEPTHS OF PARALLEL LAYERS BELOW LOWER BOUNDARY

   REAL(SP) :: HMAX         !!GLOBAL MAXIMUM DEPTH IN DEPTH FILE
   REAL(SP) :: HMIN         !!GLOBAL MINIMUM DEPTH IN DEPTH FILE


   ! All file identifiers go here
   INTEGER :: IPT                     !! IUNIT FOR LOG FILE OUTPUT
   INTEGER, PUBLIC, PARAMETER :: IPT_BASE= 7000 ! FOR PAR LOG FILES

   INTEGER, PARAMETER :: TESTUNIT = 200 !! TEST TO SEE IF OUTPUT DIR EXISTS/WRITABLE
   INTEGER, PARAMETER :: NMLUNIT = 10 !! NAMELIST RUN FILE
   INTEGER, PARAMETER :: ITSUNIT=11
   INTEGER, PARAMETER :: OBCUNIT=12
   INTEGER, PARAMETER :: GRIDUNIT=13
   INTEGER, PARAMETER :: SIGMAUNIT=14
   INTEGER, PARAMETER :: DEPTHUNIT=15
   INTEGER, PARAMETER :: CORIOLISUNIT=16
   INTEGER, PARAMETER :: SPONGEUNIT=17
   INTEGER, PARAMETER :: LSFUNIT=18
   INTEGER, PARAMETER :: ASSIMUNIT=19
   INTEGER, PARAMETER :: OIASSIMUNIT=23
   INTEGER, PARAMETER :: PROBEUNIT=20
   INTEGER, PARAMETER :: JULOBCUNIT=21
   INTEGER, PARAMETER :: KFUNIT=22
   INTEGER, PARAMETER :: NESTUNIT=30
   INTEGER, PARAMETER :: SUBDUNIT=31

   INTEGER            :: RIVERNMLUNIT

# if defined (PLBC) 
   INTEGER, PARAMETER :: INTCELL1 =46
   INTEGER, PARAMETER :: INTNODE1 =47
# endif

END MODULE CONTROL

