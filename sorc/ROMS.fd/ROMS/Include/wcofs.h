/*
** svn $Id: wcofs.h 585 2014-12-19 11:17:00Z kurapov $
*******************************************************************************
** Copyright (c) 2002-2012 The ROMS/TOMS Group                               **
**   Licensed under a MIT/X style license                                    **
**   See License_ROMS.txt                                                    **
*******************************************************************************
**
** Options for wcofs
**
** Application flag:   WCOFS
*/

#define NONLINEAR
#define SOLVE3D
#define CURVGRID
#define MASKING
#define NONLIN_EOS
#define SALINITY
#define VAR_RHO_2D

#define UV_ADV
#define UV_COR
#define UV_U3HADVECTION
#define UV_C4VADVECTION
#define UV_QDRAG
#undef  UV_PSOURCE
#define UV_VIS2

#define TS_U3HADVECTION
#define TS_C4VADVECTION
#define TS_DIF2
#undef  TS_PSOURCE

#define DJ_GRADPS

#define ADD_FSOBC
#define ADD_M2OBC
#define SSH_TIDES
#define UV_TIDES

#define ANA_BSFLUX
#define ANA_BTFLUX
/*#define ANA_RAIN*/
#define ANA_SSFLUX

#define BULK_FLUXES
#define LONGWAVE_OUT
#define SOLAR_SOURCE
#define EMINUSP
#define HDF5
#define MIX_S_TS
#define MIX_S_UV

#define MY25_MIXING
#define N2S2_HORAVG
#define RI_SPLINE
#define RADIATION_2D
/*
#define LIMIT_VDIFF
#define LIMIT_VVISC
*/
#define LIMIT_STFLX_COOLING

#define AVERAGES
#define STATIONS
#define PERFECT_RESTART
#undef RAMP_TIDES /*for cold start only*/
