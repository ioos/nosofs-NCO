#define CURVGRID
#define MASKING
#define NONLIN_EOS
#define SOLVE3D
#define SALINITY

/* output stuff */

#undef NO_WRITE_GRID
#undef OUT_DOUBLE
#undef RST_SINGLE  /*here*/
#undef AVERAGES
#undef AVERAGES2
#ifdef SOLVE3D
# undef AVERAGES_DETIDE
# define AVERAGES_AKT
# define AVERAGES_AKS
# define AVERAGES_AKV
# define AVERAGES_FLUXES
# define AVERAGES_QUADRATIC
# undef DIAGNOSTICS_TS
#endif
#undef DIAGNOSTICS_UV

/* advection, dissipation, pressure grad, etc. */

#ifdef SOLVE3D
# define DJ_GRADPS
#endif

#define UV_ADV
#define UV_COR
#define UV_SADVECTION

#ifdef SOLVE3D
# define TS_U3HADVECTION
# define TS_C4VADVECTION
#endif

#define UV_VIS2
#define UV_SMAGORINSKY
#define VISC_3DCOEF
#define MIX_S_UV

#define VISC_GRID

#ifdef SOLVE3D
# define TS_DIF2
# define MIX_GEO_TS
# define DIFF_GRID
#endif


/* vertical mixing */

#ifdef SOLVE3D
# define SOLAR_SOURCE

# undef LMD_MIXING
# ifdef LMD_MIXING
#  define LMD_RIMIX
#  define LMD_CONVEC
#  define LMD_SKPP
#  undef LMD_BKPP
#  define LMD_NONLOCAL
#  define LMD_SHAPIRO
#  undef LMD_DDMIX
# endif

# define GLS_MIXING
# undef MY25_MIXING

# if defined GLS_MIXING || defined MY25_MIXING
#  define KANTHA_CLAYSON
#  define N2S2_HORAVG
# endif
#endif

/* surface forcing */

#ifdef SOLVE3D
# define CORE_FORCING
# define BULK_FLUXES
# define CCSM_FLUXES
# if defined BULK_FLUXES || defined CCSM_FLUXES
#  define LONGWAVE_OUT
#  define DIURNAL_SRFLUX
#  define EMINUSP
#  define ALBEDO_CURVE
# endif
#endif

/* surface and side corrections */

#ifdef SOLVE3D
# undef SRELAXATION
# undef QCORRECTION
#endif

#ifdef SOLVE3D
# undef TCLIMATOLOGY
# undef TCLM_NUDGING
#endif

/* point sources (rivers, line sources) */

/* Using Runoff instead now */
#ifdef SOLVE3D
#define RUNOFF
# undef UV_PSOURCE
# undef ANA_PSOURCE
# undef TS_PSOURCE
#endif

/* tides */

#define LTIDES
#ifdef LTIDES
# undef FILTERED
# define SSH_TIDES
# define UV_TIDES    /*here*/
# define ADD_FSOBC
# define ADD_M2OBC   /*here*/
# undef RAMP_TIDES
# undef TIDES_ASTRO /*here*/
# undef POT_TIDES 

# define UV_LDRAG
# undef RDRG_GRID
# undef DRAG_LIMITER
# undef UV_QDRAG
#else
# define UV_QDRAG
#endif

/* Boundary conditions...be carefull here */

#define EASTERN_WALL
#define NORTHERN_WALL
#define WESTERN_WALL
#undef SOUTHERN_WALL

#define RADIATION_2D

#ifndef NORTHERN_WALL
# define NORTH_FSCHAPMAN
# define NORTH_M2FLATHER
# ifdef SOLVE3D
#  define NORTH_M3RADIATION
#  define NORTH_M3NUDGING
#  define NORTH_TRADIATION
#  define NORTH_TNUDGING
#  define NORTH_MIGRADIENT
# endif
#endif

#ifndef WESTERN_WALL
# define WEST_FSCHAPMAN
# define WEST_M2FLATHER
# ifdef SOLVE3D
#  define WEST_M3RADIATION
#  define WEST_M3NUDGING
#  define WEST_TRADIATION
#  define WEST_TNUDGING
#  define WEST_MIGRADIENT
# endif
#endif

#ifndef SOUTHERN_WALL
# define SOUTH_FSCHAPMAN
# define SOUTH_M2FLATHER
# ifdef SOLVE3D
#  define SOUTH_M3RADIATION
#  define SOUTH_M3NUDGING
#  define SOUTH_TRADIATION
#  define SOUTH_TNUDGING
#  define SOUTH_MIGRADIENT
# endif
#endif

#ifndef EASTERN_WALL
# define EAST_FSCHAPMAN
# define EAST_M2FLATHER
# ifdef SOLVE3D
#  define EAST_M3RADIATION
#  define EAST_M3NUDGING
#  define EAST_TRADIATION
#  define EAST_TNUDGING
#  define EAST_MIGRADIENT
# endif
#endif

/* roms quirks */

#ifdef SOLVE3D
# define ANA_BSFLUX
# define ANA_BTFLUX
# define ANA_NUDGCOEF
#else
# define ANA_SMFLUX
#endif


