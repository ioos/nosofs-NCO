#!/bin/sh
# Script Name:  nos_ofs_create_forcing_obc_tides.sh
#
# Purpose:

#   The missing variables are filled with a missing value of -99999.0.
#
# Directory Location:   ~/scripts 
# Technical Contact:   	Aijun Zhang         Org:  NOS/CO-OPS
#                       Phone: 301-7132890 ext. 127  E-Mail: aijun.zhang@noaa.gov
#
# Usage: ./nos_ofs_create_forcing_obc_tides.sh OFS 
#
# Input Parameters:
#  OFS:         Name of Operational Forecast System, e.g., TBOFS, CBOFS, DBOFS
#  TIME_START:  start time to grab data, e.g., YYYYMMDDHH (2008101500)
#  TIME_END:    end time to grab data, e.g., YYYYMMDDHH (2008101600)
#
# Language:   Bourne Shell Script      
#
# Target Computer:  Super Computer at NCEP
#
# Estimated Execution Time: 300s 
#
# Suboutines/Functions Called:
#
# Input Files:
#   ROMS ocean model grid netCDF file  
# Output Files:
#   nos.ofs.${OFS}.roms.tides.nc
#   Fortran_Tide.log
#
# Libraries Used: see the makefile
#  
# Error Conditions:
#  
# Modification History:
#  (1). Degui Cao
#       Implementation                    01/14/2010
#
# -------------------------------------------------------------------------
set -x

echo 'The script nos_ofs_create_forcing_obc.sh started at UTC' `date`
echo 'The script nos_ofs_create_forcing_obc.sh started at UTC' `date` >> $jlogfile
echo 'The script nos_ofs_create_forcing_obc.sh started at UTC' `date` >> $nosjlogfile

TIME_START=$time_hotstart
TIME_END=$time_forecastend
TIME_NOW=$time_nowcastend
DBASE_TS=$DBASE_TS_NOW

#typeset -Z2 HH HH3 CYCLE
YYYY=`echo $TIME_START | cut -c1-4 `
MM=`echo $TIME_START |cut -c5-6 `
DD=`echo $TIME_START |cut -c7-8 `
HH=`echo $TIME_START |cut -c9-10 `
 
##  create tidal forcing netCDF file for ROMS
if [ $OCEAN_MODEL == "ROMS" -o $OCEAN_MODEL == "roms" ]
then
 if [ $CREATE_TIDEFORCING -gt 0 -o ! -s $HC_FILE_OFS ]; then
  echo 'creating tidal forcing netCDF file for ROMS !!! '
  rm -f Fortran_Tide.ctl Fortran_Tide.log
  echo ${OFS} > Fortran_Tide.ctl
  echo $OCEAN_MODEL >> Fortran_Tide.ctl
  echo ${TIME_START}00 >> Fortran_Tide.ctl
  echo $GRIDFILE >> Fortran_Tide.ctl
  echo $HC_FILE_OBC >> Fortran_Tide.ctl
  echo $HC_FILE_OFS >> Fortran_Tide.ctl
  echo $BASE_DATE >> Fortran_Tide.ctl 

  export pgm=nos_ofs_create_forcing_obc_tides
  . prep_step

  startmsg

  $EXECnos/nos_ofs_create_forcing_obc_tides < Fortran_Tide.ctl > Fortran_Tide.log
  export err=$?

  if [ $err -ne 0 ]
  then
    echo "$pgm did not complete normally, FATAL ERROR!"
    msg="$pgm did not complete normally, FATAL ERROR!"
    postmsg "$jlogfile" "$msg"
    postmsg "$nosjlogfile" "$msg"
    err_chk
  else
    echo "$pgm completed normally"
    msg="$pgm completed normally"
    postmsg "$jlogfile" "$msg"
    postmsg "$nosjlogfile" "$msg"
  fi

## output corms flag information
  if grep "COMPLETED SUCCESSFULLY" Fortran_Tide.log /dev/null 2>&1
  then
     echo "TIDAL FORCING COMPLETED SUCCESSFULLY 100" >> $cormslogfile
     echo "TIDAL_FORCING DONE 100 " >> $cormslogfile

  else
     echo "TIDAL FORCING COMPLETED SUCCESSFULLY 0" >> $cormslogfile
     echo "TIDAL_FORCING DONE 0 " >> $cormslogfile
  fi
 else
  echo "USE OLD TIDAL FORCING FILE 100" >> $cormslogfile
  echo "TIDAL_FORCING DONE 100 " >> $cormslogfile 
 fi
 if [ -f "$HC_FILE_OFS" ]; then
    cp $HC_FILE_OFS $COMOUT/$OBC_TIDALFORCING_FILE 
    if [ $SENDDBN = YES ]; then
      if [ "${OFS,,}" == "gomofs" -o "${OFS,,}" == "wcofs" -o "${OFS,,}" == "ciofs" ]; then
        $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF_LRG $job $COMOUT/$OBC_TIDALFORCING_FILE
      else
        $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job $COMOUT/$OBC_TIDALFORCING_FILE
      fi
    fi
 fi
fi


if [ -f "$HC_FILE_OFS" ]
then
    cp $HC_FILE_OFS $COMOUT/$HC_FILE_OFS 
#    if [ $SENDDBN = YES ]; then
#      $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job $COMOUT/$HC_FILE_OFS
#    fi
else
    echo "NO $HC_FILE_OFS File Found"
fi

if [ -f Fortran_Tide.log ]
then
    cp Fortran_Tide.log $COMOUT/Fortran_Tide.t${cyc}z.log
else
    echo "NO Fortran_Tide.log Found"
fi

echo 'The script nos_ofs_create_forcing_obc_tides.sh ended at UTC' `date`

