#!/bin/sh
#  Script Name:  exnos_ofs_nowcast_forecast.sms.prod
#  Purpose:                                                                   #
#  This script is to make nowcast or forecast simulation after running:       #
#  nos_ofs_create_forcing_river.sh, nos_ofs_create_forcing_obc.sh             #
#  nos_ofs_create_forcing_met_nowcast.sh nos_ofs_create_forcing_met_forecast.sh
#  nos_ofs_reformat_roms_ctl_nowcast.sh nos_ofs_reformat_roms_ctl_forecast.sh 
#                                                                             #
#  Child scripts :                                                            #
#                                                                             #
#  The utililty script used:                                                  #
#                                                                             #
# Remarks :                                                                   #
# - For non-fatal errors output is written to the *.log file.                 #
# - NOTE TO NCO: this script is I/O limited. To get the script to run in      #
#                approximately 1:15, 1 node  and 12 processors are optimal    #
#                                                                             #
# Language:  C shell script
# Nowcast  
# Input:
#     nos.${RUN}.river.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${RUN}.obc.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${RUN}.met.nowcast.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${RUN}.init.nowcast.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${RUN}.hflux.nowcast.$yyyy$mm$dd.t${HH}z.nc
#     ${RUN}_roms_nowcast.in
#     nos.${RUN}.roms.tides.nc
# Output:
#     nos.${RUN}.stations.nowcast.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${RUN}.fields.nowcast.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${RUN}.fields.forecast.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${RUN}.rst.nowcast.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${RUN}.roms.nowcast.$yyyy$mm$dd.t${cyc}z.log
# Forecast  
# Input:
#     nos.${RUN}.river.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${RUN}.obc.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${RUN}.met.forecast.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${RUN}.rst.nowcast.$yyyy$mm$dd.t${cyc}z.nc
#     ${RUN}_roms_forecast.in
#     nos.${RUN}.roms.tides.nc
# Output:
#     nos.${RUN}.stations.forecast.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${RUN}.fields.forecast.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${RUN}.rst.forecast.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${RUN}.roms.forecast.$yyyy$mm$dd.t${cyc}z.log
#
# Technical Contact:    Aijun Zhang       Org:  NOS/CO-OPS                    #
#                       Phone: 240-533-0591                                   #
#                       E-Mail: aijun.zhang@noaa.gov                          #
# Modification History:
#        
#           
# 
#                                                                             #
#                                                                             #
###############################################################################
# --------------------------------------------------------------------------- #
# 0.  Preparations
# 0.a Basic modes of operation

function seton {
  set -x
}
function setoff {
  set +x
}
seton

cd $DATA

echo ' '
echo '  		    ****************************************'
echo '  		    *** NOS OFS  NOWCAST/FORECAST SCRIPT ***'
echo '  		    ****************************************'
echo ' '
echo "Starting nos_ofs_nowcast_forecast.sh at : `date`"


#export MP_PGMMODEL=mpmd
#export MP_CMDFILE=cmdfile

RUNTYPE=$1 
RUN=$OFS 

if [ ${OCEAN_MODEL} == "FVCOM" -o ${OCEAN_MODEL} == "fvcom" ]
then 
  NCSF_OUT_INTERVAL=${NCSF_OUT_INTERVAL:-$NC_OUT_INTERVAL}
  if [ -s ${FIXofs}/nos.${RUN}_brf.nc ]
  then
     cp -p ${FIXofs}/nos.${RUN}_brf.nc $DATA/${RUN}_brf.nc
  fi

  if [ -s ${FIXofs}/nos.${RUN}_cor.dat ]
  then
     cp -p ${FIXofs}/nos.${RUN}_cor.dat $DATA/${RUN}_cor.dat
  fi
  if [ -s ${FIXofs}/nos.${RUN}_dep.dat ]
  then
     cp -p ${FIXofs}/nos.${RUN}_dep.dat $DATA/${RUN}_dep.dat
  fi
  if [ -s ${FIXofs}/nos.${RUN}_grd.dat ]
  then
     cp -p ${FIXofs}/nos.${RUN}_grd.dat $DATA/${RUN}_grd.dat
  fi
  if [ -s ${FIXofs}/nos.${RUN}_obc.dat ]
  then
     cp -p ${FIXofs}/nos.${RUN}_obc.dat $DATA/${RUN}_obc.dat
  fi
  if [ -s ${FIXofs}/nos.${RUN}_sigma.dat ]
  then
     cp -p ${FIXofs}/nos.${RUN}_sigma.dat $DATA/${RUN}_sigma.dat
  fi
  if [ -s ${FIXofs}/nos.${RUN}_spg.dat ]
  then
     cp -p ${FIXofs}/nos.${RUN}_spg.dat $DATA/${RUN}_spg.dat
  fi
  if [ -s ${FIXofs}/nos.${RUN}_station.dat ]
  then
     cp -p ${FIXofs}/nos.${RUN}_station.dat $DATA/${RUN}_station.dat
  fi
  if [ -s ${FIXofs}/nos.${RUN}_rivernamelist.nml ]
  then
     cp -p ${FIXofs}/nos.${RUN}_rivernamelist.nml $DATA/RIVERS_NAMELIST.nml
  fi
  if [ "${OFS,,}" == "ngofs" ]; then
     if [ -s ${FIXofs}/nos_${RUN}_nestnode_negofs.dat ]; then
        cp -p ${FIXofs}/nos_${RUN}_nestnode_negofs.dat $DATA/nos_${RUN}_nestnode_negofs.dat
     fi
     if [ -s ${FIXofs}/nos_${RUN}_nestnode_nwgofs.dat ]; then
        cp -p ${FIXofs}/nos_${RUN}_nestnode_nwgofs.dat $DATA/nos_${RUN}_nestnode_nwgofs.dat
     fi

  fi   
elif [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
then 
  if [ ! -d $DATA/outputs ] 
  then
     mkdir -p $DATA/outputs
  fi
  if [ ! -d $DATA/sflux ] 
  then
     mkdir -p $DATA/sflux
  fi
  if [ -s $COMOUT/time_nowcastend.${cycle} ]; then
    read time_nowcastend < $COMOUT/time_nowcastend.${cycle}
    export time_nowcastend
  fi
  if [ -s $COMOUT/time_hotstart.${cycle} ]; then
    read time_hotstart < $COMOUT/time_hotstart.${cycle}
    export time_hotstart
  fi
  if [ -s $COMOUT/time_forecastend.${cycle} ]; then
    read time_forecastend < $COMOUT/time_forecastend.${cycle}
    export time_forecastend
  fi
  if [ -s $COMOUT/base_date.${cycle} ]; then
    read BASE_DATE < $COMOUT/base_date.${cycle}
    export BASE_DATE
  fi
    
  export ynet=`echo $time_nowcastend |cut -c1-4`
  export mnet=`echo $time_nowcastend |cut -c5-6`
  export dnet=`echo $time_nowcastend |cut -c7-8`
  export hnet=`echo $time_nowcastend |cut -c9-10`
  export nnh=`$NHOUR $time_nowcastend $time_hotstart`
  export tsnh=`expr $nnh \* 3600 / ${DELT_MODEL}`

 # export tsnh=$(echo "(($nnh * 3600) / 90)" | bc)
  export nh=$(echo "scale=4;$nnh / 24.0" | bc)
  export nhfr=$(echo "scale=4;$nh + 2.0" | bc)
  echo "number hours nowcast= $nnh"
  echo "number days nowcast= $nh"

  export yhst=`echo $time_hotstart |cut -c1-4`
  export mhst=`echo $time_hotstart |cut -c5-6`
  export dhst=`echo $time_hotstart |cut -c7-8`
  export hhst=`echo $time_hotstart |cut -c9-10`

#static files

  if [ -s $FIXofs/${NET}.${RUN}.albedo.gr3 ]
  then
  cp -p $FIXofs/${NET}.${RUN}.albedo.gr3 $DATA/albedo.gr3 
  fi

  if [ -s $FIXofs/${NET}.${RUN}.drag.gr3 ]
  then
  cp -p $FIXofs/${NET}.${RUN}.drag.gr3 $DATA/drag.gr3 
  fi

  if [ -s $FIXofs/${NET}.${RUN}.hgrid.ll ]
  then
  cp -p $FIXofs/${NET}.${RUN}.hgrid.ll $DATA/hgrid.ll
  fi

  if [ -s $FIXofs/${STA_OUT_CTL} ]
  then
  cp -p $FIXofs/${STA_OUT_CTL} $DATA/station.in 
#  cp -p $FIXofs/${NET}.${RUN}.station.in $DATA/station.in 
  fi

  if [ -s $FIXofs/${NET}.${RUN}.watertype.gr3 ]
  then
  cp -p $FIXofs/${NET}.${RUN}.watertype.gr3 $DATA/watertype.gr3 
  fi

  if [ -s $FIXofs/${NET}.${RUN}.diffmax.gr3 ]
  then
  cp -p $FIXofs/${NET}.${RUN}.diffmax.gr3  $DATA/diffmax.gr3 
  fi

  if [ -s $FIXofs/${NET}.${RUN}.estuary.gr3 ]
  then
  cp -p $FIXofs/${NET}.${RUN}.estuary.gr3 $DATA/estuary.gr3 
  fi

  if [ -s $FIXofs/${NET}.${RUN}.interpol.gr3 ]
  then
  cp -p $FIXofs/${NET}.${RUN}.interpol.gr3 $DATA/interpol.gr3 
  fi

  if [ -s $FIXofs/${NET}.${RUN}.t_nudge.gr3 ]
  then
  cp -p $FIXofs/${NET}.${RUN}.t_nudge.gr3 $DATA/t_nudge.gr3 
  fi

  if [ -s $FIXofs/${NET}.${RUN}.windrot_geo2proj.gr3 ]
  then
  cp -p $FIXofs/${NET}.${RUN}.windrot_geo2proj.gr3 $DATA/windrot_geo2proj.gr3 
  fi

  if [ -s $FIXofs/${NET}.${RUN}.diffmin.gr3 ]
  then
  cp -p $FIXofs/${NET}.${RUN}.diffmin.gr3   $DATA/diffmin.gr3 
  fi

  if [ -s $FIXofs/${NET}.${RUN}.hgrid.gr3 ]
  then
  cp -p $FIXofs/${NET}.${RUN}.hgrid.gr3 $DATA/hgrid.gr3 
  fi

  if [ -s $FIXofs/${NET}.${RUN}.s_nudge.gr3 ]
  then
  cp -p $FIXofs/${NET}.${RUN}.s_nudge.gr3 $DATA/s_nudge.gr3 
  fi

  if [ -s $FIXofs/${VGRID_CTL} ]
  then
  cp -p $FIXofs/${VGRID_CTL} $DATA/vgrid.in 
  fi

  if [ -s $FIXofs/${NET}.${RUN}.xlsc.gr3 ]
  then
  cp -p $FIXofs/${NET}.${RUN}.xlsc.gr3 $DATA/xlsc.gr3 
  fi
  if [ -s $FIXofs/${NET}.${RUN}.bctides.in ]
  then
  cp -p $FIXofs/${NET}.${RUN}.bctides.in $DATA/bctides.in 
  fi
fi

if [ "${OFS,,}" == "nwgofs" ]; then
     if [ -s ${FIXofs}/nos.${RUN}_dam_cell.dat ]; then
        cp -p ${FIXofs}/nos.${RUN}_dam_cell.dat $DATA/${RUN}_dam_cell.dat
     fi
     if [ -s ${FIXofs}/nos.${RUN}_dam_node.dat ]; then
        cp -p ${FIXofs}/nos.${RUN}_dam_node.dat $DATA/${RUN}_dam_node.dat
     fi
fi

# --------------------------------------------------------------------------- #
# 1.  Get files that are used by most (child) scripts

echo "Preparing input files for ${RUN} $RUNTYPE "
echo '-----------------------'
seton

###############################################################################
###############################################################################
#### NOWCAST
###############################################################################
###############################################################################

if [ $RUNTYPE == "NOWCAST" -o $RUNTYPE == "nowcast" ]
then
  if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
  then 
#CHECK FOR MET CONTROL FILE
    if [ -s ${COMOUT}/$RUNTIME_MET_CTL_NOWCAST ]
    then
      echo "MET control files exist"
      cp -p ${COMOUT}/$RUNTIME_MET_CTL_NOWCAST $DATA/sflux/sflux_inputs.txt
    else
      msg="FATAL ERROR: No MET control file for Nowcast"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '******************************************************'
      echo '*** FATAL ERROR : No MET control file for Nowcast  ***'
      echo '******************************************************'
      echo ' '
      echo $msg
      seton
#      touch err.${RUN}.$PDY1.t${HH}z
      err_exit "No MET control file for nowcast: ${COMOUT}/$RUNTIME_MET_CTL_NOWCAST"
    fi
  fi

# 1.a RIVER FORCING FILE 
  if [ ${OCEAN_MODEL} == "ROMS" -o ${OCEAN_MODEL} == "roms" ]
  then

    if [ -s $DATA/$RIVER_FORCING_FILE ]
    then
      echo " $DATA/$RIVER_FORCING_FILE existed "
    elif [ -s $COMOUT/$RIVER_FORCING_FILE ]
    then
      cp -p $COMOUT/$RIVER_FORCING_FILE $RIVER_FORCING_FILE
    else  
      msg="FATAL ERROR: NO RIVER FORCING FILE $RIVER_FORCING_FILE"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '**********************************************'
      echo '*** FATAL ERROR : NO $RIVER_FORCING_FILE   ***'
      echo '**********************************************'
      echo ' '
      echo $msg
      seton
      touch err.${RUN}.$PDY1.t${HH}z
      err_exit "No river forcing file: $COMOUT/$RIVER_FORCING_FILE"
    fi
  elif [ ${OCEAN_MODEL} == "FVCOM" -o ${OCEAN_MODEL} == "fvcom" ]
  then
    if [ ! -r $DATA/RIVER ]
    then

     if [ -s $DATA/${RIVER_FORCING_FILE} ]
     then
       echo " $DATA/$RIVER_FORCING_FILE existed "
       rm -fr $DATA/RIVER
       tar -xvf $DATA/${RIVER_FORCING_FILE}
     elif [ -s $COMOUT/${RIVER_FORCING_FILE} ]
     then
       rm -fr $DATA/RIVER
       cp -p $COMOUT/${RIVER_FORCING_FILE} $DATA/.
       tar -xvf $DATA/${RIVER_FORCING_FILE}
     else  
      msg="FATAL ERROR: NO RIVER FORCING FILE $RIVER_FORCING_FILE"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '**********************************************'
      echo '*** FATAL ERROR : NO $RIVER_FORCING_FILE   ***'
      echo '**********************************************'
      echo ' '
      echo $msg
      seton
      touch err.${RUN}.$PDY1.t${HH}z
      err_exit "No river forcing file: ${RIVER_FORCING_FILE}"
     fi
    fi
  elif [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
  then 
     if [ -s $DATA/selfe_temp.th -a $DATA/selfe_flux.th -a $DATA/selfe_salt.th ]
     then
      echo "RIVER forcing files exist"
      cp -p $DATA/selfe_temp.th  $DATA/temp.th
      cp -p $DATA/selfe_flux.th  $DATA/flux.th
      cp -p $DATA/selfe_salt.th  $DATA/salt.th
     elif [ -s $COMOUT/${RIVER_FORCING_FILE} ]
     then
       cp -p $COMOUT/${RIVER_FORCING_FILE} $DATA/.
       tar -xvf $DATA/${RIVER_FORCING_FILE}
       cp -p $DATA/selfe_temp.th  $DATA/temp.th
       cp -p $DATA/selfe_flux.th  $DATA/flux.th
       cp -p $DATA/selfe_salt.th  $DATA/salt.th
    else
      msg="FATAL ERROR: No River Forcing For Nowcast/Forecast"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '*************************************************************'
      echo '*** FATAL ERROR : NO River Forcing For Nowcast/Forecast   ***'
      echo '*************************************************************'
      echo ' '
      echo $msg
      seton
      err_exit "No river forcing for nowcast/forecast: $COMOUT/${RIVER_FORCING_FILE}"
    fi
    
  fi
# 1.b OBC FORCING FILE 

  if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
  then
    if [ -s $DATA/elev3D.th -a $DATA/salt_nu.in -a $DATA/temp_nu.in ]
    then
      echo "OBC forcing files exist"
    elif [ -s $COMOUT/$OBC_FORCING_FILE ]
    then
#      cp -p $COMOUT/$OBC_FORCING_FILE $DATA/$OBC_FORCING_FILE
      tar -xvf $COMOUT/$OBC_FORCING_FILE
    else
      msg="FATAL ERROR: No OBC Forcing For Nowcast/Forecast"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '************************************************************'
      echo '*** FATAL ERROR : NO OBC Forcing For Nowcast/Forecast    ***'
      echo '************************************************************'
      echo ' '
      echo $msg
      seton
#      touch err.${RUN}.$PDY1.t${HH}z
      err_exit "No OBC forcing for nowcast/forecast: $COMOUT/$OBC_FORCING_FILE"
    fi
   
  else
    if [ -f $DATA/$OBC_FORCING_FILE ]
    then
      echo "   $DATA/$OBC_FORCING_FILE existed "
    elif [ -s $COMOUT/$OBC_FORCING_FILE ]
    then
      cp -p $COMOUT/$OBC_FORCING_FILE $OBC_FORCING_FILE
# for SFBOFS
    elif [ -s $COMOUT/$OBC_FORCING_FILE_EL ]
    then
      cp -p $COMOUT/$OBC_FORCING_FILE_EL $OBC_FORCING_FILE_EL
      cp -p $COMOUT/$OBC_FORCING_FILE_TS $OBC_FORCING_FILE_TS

    else
      msg="FATAL ERROR: NO OBC FORCING FILE $OBC_FORCING_FILE"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '********************************************'
      echo '*** FATAL ERROR : NO $OBC_FORCING_FILE   ***'
      echo '********************************************'
      echo ' '
      echo $msg
      seton
      touch err.${RUN}.$PDY1.t${HH}z
      err_exit "No OBC forcing file: $COMOUT/$OBC_FORCING_FILE_EL"
    fi

  fi

# 1.c Meteorological Forcing For Nowcast 
  if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
  then
    if [ ! -d $DATA/sflux ]; then
      mkdir -p $DATA/sflux
    else
      rm -f $DATA/sflux/*.nc  
    fi  
    if [ -s $COMOUT/$MET_NETCDF_1_NOWCAST ]
    then
      cd $DATA/sflux
      tar -xvf $COMOUT/$MET_NETCDF_1_NOWCAST 
      cd $DATA
    else
      msg="FATAL ERROR: NO Meteorological Forcing For Nowcast $MET_NETCDF_1_NOWCAST"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '*************************************************************'
      echo '*** FATAL ERROR : NO Meteorological Forcing For Nowcast   ***'
      echo '*************************************************************'
      echo ' '
      echo $msg
      seton
      touch err.${RUN}.$PDY1.t${HH}z
      err_exit "No meteorological forcing for nowcast: $COMOUT/$MET_NETCDF_1_NOWCAST"
    fi  
  else 
    if [ -s $DATA/$MET_NETCDF_1_NOWCAST ]
    then
      echo "   $DATA/$MET_NETCDF_1_NOWCAST existed "
    elif [ -s $COMOUT/$MET_NETCDF_1_NOWCAST ]
    then
      cp -p $COMOUT/$MET_NETCDF_1_NOWCAST $MET_NETCDF_1_NOWCAST
    else
      msg="FATAL ERROR: NO Meteorological Forcing For Nowcast $MET_NETCDF_1_NOWCAST"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '*************************************************************'
      echo '*** FATAL ERROR : NO Meteorological Forcing For Nowcast   ***'
      echo '*************************************************************'
      echo ' '
      echo $msg
      seton
      touch err.${RUN}.$PDY1.t${HH}z
      err_exit "No meteorological forcing for nowcast: $COMOUT/$MET_NETCDF_1_NOWCAST"
    fi
  fi
   
# 1.d Initial forcing For Nowcast
  if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
  then
    if [ -f $DATA/$INI_FILE_NOWCAST ]
    then
      echo "   $DATA/$INI_FILE_NOWCAST exists"
      cp -p $DATA/$INI_FILE_NOWCAST $DATA/hotstart.in
    elif [ -s $COMOUT/$INI_FILE_NOWCAST ]
    then
      cp -p $COMOUT/$INI_FILE_NOWCAST $DATA/hotstart.in
    else
      msg="FATAL ERROR: NO intial forcing file For Nowcast $INI_FILE_NOWCAST"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '**********************************************************'
      echo '*** FATAL ERROR : NO intial forcing file For Nowcast   ***'
      echo '**********************************************************'
      echo ' '
      echo $msg
      seton
      touch err.${RUN}.$PDY1.t${HH}z
      err_exit "No initial forcing file for nowcast: $COMOUT/$INI_FILE_NOWCAST"
    fi
  else
    if [ -f $DATA/$INI_FILE_NOWCAST ]
    then
      echo "   $COMOUT/$INI_FILE_NOWCAST existed "
    elif [ -s $COMOUT/$INI_FILE_NOWCAST ]
    then
      cp -p $COMOUT/$INI_FILE_NOWCAST $INI_FILE_NOWCAST
    else
      msg="FATAL ERROR: NO intial forcing file For Nowcast $INI_FILE_NOWCAST"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '**********************************************************'
      echo '*** FATAL ERROR : NO intial forcing file For Nowcast   ***'
      echo '**********************************************************'
      echo ' '
      echo $msg
      seton
      touch err.${RUN}.$PDY1.t${HH}z
      err_exit "No initial forcing file for nowcast: $COMOUT/$INI_FILE_NOWCAST"
    fi
  fi

# 1.e HFlux For Nowcast
  if [ -f $DATA/$MET_NETCDF_2_NOWCAST ]
  then
      echo "   $DATA/$MET_NETCDF_2_NOWCAST existed "
  elif [ -s $COMOUT/$MET_NETCDF_2_NOWCAST ]
  then
      cp -p $COMOUT/$MET_NETCDF_2_NOWCAST $MET_NETCDF_2_NOWCAST
  fi

# 1.f Nowcast control file
  if [ -f $DATA/${RUN}_${OCEAN_MODEL}_nowcast.in ]
  then
    echo "   $DATA/${RUN}_${OCEAN_MODEL}_nowcast.in is found "
    if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
    then
      cp -p $DATA/${RUN}_${OCEAN_MODEL}_nowcast.in $DATA/param.in
    elif [ ${OCEAN_MODEL} == "FVCOM" -o ${OCEAN_MODEL} == "fvcom" ]
    then
      cp -p $DATA/${RUN}_${OCEAN_MODEL}_nowcast.in $DATA/${RUN}'_run.nml'
    fi  
  elif [ -s $COMOUT/${RUNTIME_CTL_NOWCAST} ]
  then
    echo "$COMOUT/${RUNTIME_CTL_NOWCAST} is found "
    if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
    then
      cp -p $COMOUT/${RUNTIME_CTL_NOWCAST} $DATA/param.in
    elif [ ${OCEAN_MODEL} == "FVCOM" -o ${OCEAN_MODEL} == "fvcom" ]
    then
      cp -p $COMOUT/${RUNTIME_CTL_NOWCAST} $DATA/${RUN}'_run.nml'
    elif [ ${OCEAN_MODEL} == "ROMS" -o ${OCEAN_MODEL} == "roms" ]
    then
      cp -p $COMOUT/${RUNTIME_CTL_NOWCAST} ${RUN}_${OCEAN_MODEL}_nowcast.in
    fi  
  else
     echo "$DATA/${RUN}_${OCEAN_MODEL}_nowcast.in is not found"
     echo "$COMOUT/${RUNTIME_CTL_NOWCAST} is not found "
     msg="FATAL ERROR: MODEL runtime input file for nowcast is not found"
     postmsg "$jlogfile" "$msg"
     postmsg "$nosjlogfile" "$msg"
    setoff
    echo ' '
    echo '**********************************************************************'
    echo '*** FATAL ERROR : ROMS runtime input file for nowcast is not found ***'
    echo '**********************************************************************'
    echo ' '
    echo $msg
    seton
    touch err.${RUN}.$PDY1.t${HH}z
    err_exit "ROMS runtime input file for nowcast is not found: $COMOUT/${RUNTIME_CTL_NOWCAST}"
  fi

#1.i Tide data 
  if [ $CREATE_TIDEFORCING -ge 0 ]; then
   if [ -f $DATA/$HC_FILE_OFS ]; then
      echo "   $DATA/$HC_FILE_OFS linked "
   elif [ -s $COMOUT/$HC_FILE_OFS ]; then
      cp -p $COMOUT/$HC_FILE_OFS $HC_FILE_OFS
   fi
  fi
#1.j Nudging file
  TS_NUDGING=${TS_NUDGING:-0}
  if [ $TS_NUDGING -eq 1 ]; then
    if [ -f $COMOUT/$NUDG_FORCING_FILE ]; then
      cp -p $COMOUT/$NUDG_FORCING_FILE $NUDG_FORCING_FILE
    else
      echo "$COMOUT/$NUDG_FORCING_FILE is not found"
      msg="FATAL ERROR: T/S nudging is on but the forcing file is not found"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
    fi
  fi


  ######## NOWCAST #########
  ##########################
  echo 'Ocean Model run starts at time: ' `date `
# --------------------------------------------------------------------------- #
# 2   Execute ocean model of ROMS; where ${RUN}_roms_nowcast.in is created by nos_ofs_reformat_roms_ctl.sh
  if [ ${OCEAN_MODEL} == "ROMS" -o ${OCEAN_MODEL} == "roms" ]
  then 
    mpirun $EXECnos/${RUN}_roms_mpi ${RUN}_${OCEAN_MODEL}_nowcast.in >> ${MODEL_LOG_NOWCAST}
    export err=$?
    if [ $err -ne 0 ]
    then
      echo "Running ocean model ${RUN}_roms_mpi for $RUNTYPE did not complete normally"
      msg="Running ocean model ${RUN}_roms_mpi for $RUNTYPE did not complete normally"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      err_exit "$msg"
#    else
#      echo "Running ocean model ${RUN}_roms_mpi for $RUNTYPE completed normally"
#     msg="Running ocean model ${RUN}_roms_mpi for $RUNTYPE completed normally"
#      postmsg "$jlogfile" "$msg"
#      postmsg "$nosjlogfile" "$msg"
    fi

    rm -f corms.now corms.fcst 
    if [ -s ${MODEL_LOG_NOWCAST} ]
    then
      grep "ROMS/TOMS - Blows up" ${MODEL_LOG_NOWCAST} > corms.now
      grep "Blowing-up" ${MODEL_LOG_NOWCAST} >> corms.now
      grep "Abnormal termination: BLOWUP" ${MODEL_LOG_NOWCAST} >> corms.now
    fi
    if [ -s  corms.now ]
    then
       echo "${RUN} NOWCAST RUN OF CYCLE t${HH}z ON $PDY FAILED 00"  >> $cormslogfile 
       echo "NOWCAST_RUN DONE 0"  >> $cormslogfile
#for development
       cp -pr $DATA $DATA/../../.
       export err=99; err_chk
    else
       grep "ROMS/TOMS: DONE" ${MODEL_LOG_NOWCAST} > corms.now
       if [ -s  corms.now ]
       then
         echo "${RUN} NOWCAST RUN OF CYCLE t${HH}z ON $PDY COMPLETED SUCCESSFULLY 100" >> $cormslogfile
         echo "NOWCAST_RUN DONE 100"  >> $cormslogfile
       fi
# save new nowcast restart file into archive directory for next cycle run
       if [ -f $DATA/$RST_OUT_NOWCAST ]
       then
         cp -p $DATA/$RST_OUT_NOWCAST $COMOUT/$RST_OUT_NOWCAST
         echo "   $RST_OUT_NOWCAST saved "
       fi
    fi
## separate HIS output file into multiple smaller files
#AJ 02/26/2015       Im1=0
    Im1=0   #for new version of ROMS which doesn't ouput hour=0 (initial time)
    IQm=0
#    NHIS_INTERVAL=`expr $NHIS \* ${DELT_MODEL}`
#    NQCK_INTERVAL=`expr $NQCK \* ${DELT_MODEL}`
#    NHIS_INTERVAL=`expr $NHIS_INTERVAL / 3600`
#    NQCK_INTERVAL=`expr $NQCK_INTERVAL / 3600`

     NHIS_INTERVAL=`expr $NHIS / 3600`
     NQCK_INTERVAL=`expr $NQCK / 3600`
    if [ -f nos.${RUN}.avg.nc ]; then 
      mv nos.${RUN}.avg.nc nos.${RUN}.avg.nowcast.$PDY.t${cyc}z.nc
      cp -p nos.${RUN}.avg.nowcast.$PDY.t${cyc}z.nc ${COMOUT}/
    fi
    I=1
    while (( I < 120 ))
    do
       fhr4=`echo $I |  awk '{printf("%04i",$1)}'`
       file=nos.${RUN}.fields.nowcast.$PDY.t${cyc}z_${fhr4}.nc
       if [ -s $file ]; then
         Im1=`expr $Im1 + $NHIS_INTERVAL`
         fhr3=`echo $Im1 |  awk '{printf("%03i",$1)}'`
         fileout=nos.${RUN}.fields.n${fhr3}.$PDY.t${cyc}z.nc
         mv $file $fileout
#         Im1=`expr $Im1 + 1`
       fi

#       fhr4=`echo $I |  awk '{printf("%04i",$1)}'`
       file=nos.${RUN}.surface.nowcast.$PDY.t${cyc}z_${fhr4}.nc
       if [ -s $file ]; then
         IQm=`expr $IQm + $NQCK_INTERVAL`
         fhr3=`echo $IQm |  awk '{printf("%03i",$1)}'`
         fileout=nos.${RUN}.2ds.n${fhr3}.$PDY.t${cyc}z.nc
         mv $file $fileout
       fi



       (( I = I + 1 ))
    done

  elif [ ${OCEAN_MODEL} == "FVCOM" -o ${OCEAN_MODEL} == "fvcom" ]
  then
    # mpirun $EXECnos/fvcom_${RUN} --casename=$RUN > $MODEL_LOG_NOWCAST
    mpirun -verbose -np $NPP -bind-to core:$NPP $EXECnos/fvcom_${RUN} --casename=$RUN > $MODEL_LOG_NOWCAST
    #mpirun -verbose -np $NPP -bind-to numa:2 -map-by C $EXECnos/fvcom_${RUN} --casename=$RUN > $MODEL_LOG_NOWCAST
    export err=$?
    if [ $err -ne 0 ]
    then
      echo "Running ocean model for $RUNTYPE did not complete normally"
      msg="Running ocean model for $RUNTYPE did not complete normally"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      err_exit "$msg"
    fi

    rm -f corms.now corms.fcst 
    if [ -s ${MODEL_LOG_NOWCAST} ]
    then
      grep "NaN" ${MODEL_LOG_NOWCAST} > corms.now
      grep "STOP" ${MODEL_LOG_NOWCAST} >> corms.now
      grep "failed" ${MODEL_LOG_NOWCAST} >> corms.now
      grep "Failed" ${MODEL_LOG_NOWCAST} >> corms.now
      grep "Blowing-up" ${MODEL_LOG_NOWCAST} >> corms.now
      grep "Abnormal termination: BLOWUP" ${MODEL_LOG_NOWCAST} >> corms.now
      if [ -s  corms.now ]
      then
       echo "${RUN} NOWCAST RUN OF CYCLE t${HH}z ON $PDY FAILED 00"  >> $cormslogfile 
       echo "NOWCAST_RUN DONE 0"  >> $cormslogfile
       export err=99; err_chk
      else
       echo "${RUN} NOWCAST RUN OF CYCLE t${HH}z ON $PDY COMPLETED SUCCESSFULLY 100" >> $cormslogfile
       echo "NOWCAST_RUN DONE 100"  >> $cormslogfile
# save new nowcast restart file into archive directory for next cycle run
       if [ -f $DATA/$RUN'_restart_0001.nc' ]
       then
         mv $DATA/$RUN'_restart_0001.nc' $DATA/$RST_OUT_NOWCAST
         cp -p $DATA/$RST_OUT_NOWCAST $COMOUT/$RST_OUT_NOWCAST
         echo "   $RST_OUT_NOWCAST saved "
       fi
#       if [ -f $DATA/$RUN'_0001.nc' ]
#       then
#          cp -p  $DATA/$RUN'_0001.nc' $DATA/$HIS_OUT_NOWCAST
#       fi
       if [ -f $DATA/$RUN'_station_timeseries.nc' ]
       then
         mv $DATA/$RUN'_station_timeseries.nc' $DATA/$STA_OUT_NOWCAST
       fi
       NC_OUT_INTERVAL=`printf '%.0f' $NC_OUT_INTERVAL` ## convert decimal number to integer, and get the nearest integer
       NCSF_OUT_INTERVAL=${NCSF_OUT_INTERVAL%.*} # just tuncate the integer part and remove the fractional part   
       NHIS_INTERVAL=`expr $NC_OUT_INTERVAL / 3600`
       NQCK_INTERVAL=`expr $NCSF_OUT_INTERVAL / 3600`
       echo $NHIS_INTERVAL $NQCK_INTERVAL 
       Im1=0
       IQm=0
       I=1
       while (( I < 100 ))
       do
          fhr4=`echo $I |  awk '{printf("%04i",$1)}'`
          file=$RUN'_'${fhr4}'.nc'
          if [ -s $file ]; then
            fhr3=`echo $Im1 |  awk '{printf("%03i",$1)}'`
            fileout=nos.${RUN}.fields.n${fhr3}.$PDY.t${cyc}z.nc
            mv $file $fileout
            Im1=`expr $Im1 + $NHIS_INTERVAL`
          fi

          file=$RUN'_surface_'${fhr4}'.nc'
          if [ -s $file ]; then
            fhr3=`echo $IQm |  awk '{printf("%03i",$1)}'`
            fileout=nos.${RUN}.2ds.n${fhr3}.$PDY.t${cyc}z.nc
            mv $file $fileout
            IQm=`expr $IQm + $NQCK_INTERVAL`
          fi
          (( I = I + 1 ))

       done
       
      fi  
      if [ "${OFS,,}" == "ngofs" ]; then
#      if [ ${OFS} == "NGOFS" -o ${OFS} == "ngofs" ]; then
        if [ -s nos_${RUN}_nestnode_negofs.nc ]; then
          cp -p nos_${RUN}_nestnode_negofs.nc $COMOUT/nos.${RUN}.nestnode.negofs.nowcast.$PDY.t${cyc}z.nc
        fi
        if [ -s nos_${RUN}_nestnode_nwgofs.nc ]; then
          cp -p nos_${RUN}_nestnode_nwgofs.nc $COMOUT/nos.${RUN}.nestnode.nwgofs.nowcast.$PDY.t${cyc}z.nc
        fi
      fi

    else
       echo "${RUN} NOWCAST RUN OF CYCLE t${HH}z ON $PDY FAILED 00"  >> $cormslogfile 
       echo "NOWCAST_RUN DONE 0"  >> $cormslogfile
       export err=99; err_chk
    fi  
  elif [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
  then
    echo "nowcast simulation began at:  `date`" >> $nosjlogfile
    mpirun $EXECnos/selfe_${RUN} > $MODEL_LOG_NOWCAST
    export err=$?
    rm -f corms.now corms.fcst 
    if [ -s $DATA/mirror.out ]
    then
      grep "Run completed successfully" $DATA/mirror.out > corms.now
    fi
    if [ ! -s  corms.now ]
    then
       echo "${RUN} NOWCAST RUN OF CYCLE t${HH}z ON $PDY FAILED 00"  >> $cormslogfile 
       echo "NOWCAST_RUN DONE 0"  >> $cormslogfile
       export err=99; err_chk
    else
       echo "${RUN} NOWCAST RUN OF CYCLE t${HH}z ON $PDY COMPLETED SUCCESSFULLY 100" >> $cormslogfile
       echo "NOWCAST_RUN DONE 100"  >> $cormslogfile
    fi

    if [ $err -ne 0 ]
    then
      echo "Running ocean model for $RUNTYPE did not complete normally"
      msg="Running ocean model  for $RUNTYPE did not complete normally"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      err_exit "$msg"
    else
      echo "Running ocean model for $RUNTYPE completed normally"
      msg="Running ocean model  for $RUNTYPE completed normally"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
    fi
    echo "nowcast simulation completed at:  `date`" >> $nosjlogfile
## combine all hotstart files into a single restart file    
    if [ -s ${COMOUT}/$RUNTIME_COMBINE_RST_NOWCAST ]
    then
      echo "combine restart control files exists"
      cp -p ${COMOUT}/$RUNTIME_COMBINE_RST_NOWCAST $DATA/outputs/combine_hotstart.in
    else
      msg="FATAL ERROR: No combine restart control file"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '********************************************************'
      echo '*** FATAL ERROR : No combine restart control file    ***'
      echo '********************************************************'
      echo ' '
      echo $msg
      seton
      err_exit "No combine restart control file: ${COMOUT}/$RUNTIME_COMBINE_RST_NOWCAST"
    fi
#run combine hotstart executable
    cd $DATA/outputs
    $EXECnos/nos_ofs_combine_hotstart_out_selfe < ./combine_hotstart.in 
    export err=$?
    if [ $err -ne 0 ]; then
      echo "Running nos_ofs_combine_hotstart_out_selfe did not complete normally"
      msg="Running nos_ofs_combine_hotstart_out_selfe did not complete normally"
      postmsg "$jlogfile" "$msg"
      err_exit "$msg"
    else
      echo "Running nos_ofs_combine_hotstart_out_selfe completed normally"
      msg="Running nos_ofs_combine_hotstart_out_selfe completed normally"
      postmsg "$jlogfile" "$msg"
    fi
    if [ -s hotstart.in ]
    then
      cp -p hotstart.in  $COMOUT/$RST_OUT_NOWCAST
#      cp -p hotstart.in $DATA
      echo "NOWCAST RESTART FILES HAVE BEEN COMBINED SUCCESSFULLY 100" >> $cormslogfile
    else
      echo "NOWCAST RESTART FILES HAVE BEEN COMBINED SUCCESSFULLY 0" >> $cormslogfile
      msg="FATAL ERROR: No hotstart.in restart file for the next model run"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '************************************************************************'
      echo '*** FATAL ERROR : No hotstart.in restart file for the next model run ***'
      echo '************************************************************************'
      echo ' '
      echo $msg
      seton
      err_exit "No hotstart.in restart file for the next model run"
    fi
# compute water level offset over previous 7 days at NOS NWLON stations
    echo "water level offset correction began at:  `date`" >> $nosjlogfile
    cd $DATA
    CURRENTTIME=$time_nowcastend
##  allow to search back for maximum 5 days
    CURRENTTIMEm5=`$NDATE -120 $CURRENTTIME `
    YYYY=`echo $CURRENTTIME | cut -c1-4 `
      MM=`echo $CURRENTTIME |cut -c5-6 `
      DD=`echo $CURRENTTIME |cut -c7-8 `
      HH=`echo $CURRENTTIME |cut -c9-10 `
    OLDFILE=$COMOUTroot/${OFS}.$YYYY$MM$DD/$WL_OFFSET_OLD
    while [ ! -s $OLDFILE -a $CURRENTTIME -gt $CURRENTTIMEm5 ]
    do
         CURRENTTIME=`$NDATE -1 $CURRENTTIME `
         YYYY=`echo $CURRENTTIME | cut -c1-4 `
           MM=`echo $CURRENTTIME |cut -c5-6 `
           DD=`echo $CURRENTTIME |cut -c7-8 `
           HH=`echo $CURRENTTIME |cut -c9-10 `
         OLDFILE=$COMOUTroot/${OFS}.$YYYY$MM$DD/$WL_OFFSET_OLD
    done
    if [ -s $OLDFILE ]; then
       cp -p $OLDFILE $DATA
    else
       if [ -s $FIXofs/$WL_OFFSET_OLD ]; then
         cp $FIXofs/$WL_OFFSET_OLD $DATA
       else
         echo cannot find $WL_OFFSET_OLD in archive directory
         echo "please copy $WL_OFFSET_OLD in creofs.YYYYMMDD"  
         err_exit "Cannot find $FIXofs/$WL_OFFSET_OLD"
       fi
    fi
    if [ ! -s $CORRECTION_STATION_CTL ]; then
       cp -p $FIXofs/$CORRECTION_STATION_CTL $DATA
    fi

    if [ ! -s $GRIDFILE_LL ]; then
       cp -p $FIXofs/$GRIDFILE_LL $DATA 
    fi
    if [ ! -s $STA_NETCDF_CTL ]; then
       cp -p $FIXofs/$STA_NETCDF_CTL $DATA 
    fi
#    if [ ! -s $WL_OFFSET_OLD ]; then
#       cp -p $FIXofs/$WL_OFFSET_OLD $DATA 
#    fi
    
    rm -f tmpsta.ctl
    echo $COMOUTroot > tmpsta.ctl
    echo $DCOMINports >> tmpsta.ctl
    echo $NOSBUFR >> tmpsta.ctl
    echo $CORRECTION_STATION_CTL >> tmpsta.ctl
    echo $GRIDFILE_LL >> tmpsta.ctl
    echo $STA_NETCDF_CTL >> tmpsta.ctl
    echo $WL_OFFSET_OLD >> tmpsta.ctl
    echo $time_nowcastend >> tmpsta.ctl   #END_TIME='YYYYMMDDHH'
    $EXECnos/nos_creofs_wl_offset_correction < ./tmpsta.ctl > ./Fortran_offset.log
    if [ -s $DATA/Fortran_offset.log ]
    then
      grep "has completed successfully" $DATA/Fortran_offset.log > corms.now
    fi
    if [ ! -s corms.now ]
    then
       echo " Running nos_creofs_wl_offset_correction FAILED 00"  >> $cormslogfile 
#       export err=99; err_chk
    else
       echo " Running nos_creofs_wl_offset_correction COMPLETED SUCCESSFULLY 100"  >> $cormslogfile 
    fi

    if [ -s $WL_OFFSET_OLD ]; then
#       cp -p $WL_OFFSET_OLD $FIXofs/$WL_OFFSET_OLD
        cp -p $WL_OFFSET_OLD $COMOUT 
    fi
    echo "water level correction  completed at:  `date`" >> $nosjlogfile
# combine field outputs into a single NetCDF file
    echo "combine field nowcast simulation began at:  `date`" >> $nosjlogfile 
    cd $DATA/outputs
    rm -f listyes listno
    I=1
#    while [ $I -le 100 ]
     while (( I < 100 ))
    do
 
      file=$I'_0000_elev.61'
      echo $file
      if [ -s $file ]; then
         echo $file >> listyes
      fi	 
      (( I = I + 1 ))
    done

    if [ -s listyes ]; then
#       btmnow=`cat listyes | awk -F_ '{print $1}' | sort | head -1`
#       etmnow=`cat listyes | awk -F_ '{print $1}' | sort | tail -1`
       btmnow=`cat listyes | awk -F_ '{print $1}' |  head -1`
       etmnow=`cat listyes | awk -F_ '{print $1}' |  tail -1`
    fi
    if [ -s $FIXofs/$RUNTIME_COMBINE_NETCDF ]
    then
      if [ -s $FIXofs/$GRIDFILE_LL ]; then
        cp -p $FIXofs/$GRIDFILE_LL $DATA/outputs
      fi
      if [ -s $DATA/param.in ]; then
        cp -p $DATA/param.in $DATA/outputs
      else
        echo "$DATA/param.in does not exist"
        echo 'combine field netcdf code will fail!'
      fi
      if [ $RUNTYPE == "NOWCAST" -o $RUNTYPE == "nowcast" ]; then
         ncastfcast="n"
      fi	 	
      cat $FIXofs/$RUNTIME_COMBINE_NETCDF | sed -e s/BTM/$btmnow/g -e s/ETM/$etmnow/g \
      -e s/GRIDFILE/$GRIDFILE_LL/g \
      -e s/BASE_DATE/$BASE_DATE/g \
      -e s/TIME_NOWCASTEND/${time_nowcastend}/g \
      -e s/RUNTYPE/${ncastfcast}/g > combine_output.in
      cp -p combine_output.in ${COMOUT}/$RUNTIME_COMBINE_NETCDF_NOWCAST
    else
      msg="FATAL ERROR: No control file for combining field outputs of $RUNTYPE"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo " "
      echo "******************************************************************************"
      echo "*** FATAL ERROR : No control file for combining field outputs of $RUNTYPE  ***"
      echo "******************************************************************************"
      echo " "
      echo $msg
      seton
#      touch err.${RUN}.$PDY1.t${HH}z
      err_exit "No control file for combining field outputs of $RUNTYPE: $FIXofs/$RUNTIME_COMBINE_NETCDF"
    fi
 
#run netcdf combine executable
#    cp -p $EXECnos/nos_ofs_combine_field_netcdf_selfe .
     time tar -cvf - . | cat >/dev/null 
     time ${EXECnos}/nos_ofs_combine_field_netcdf_selfe 
     wait
#    ${EXECnos}/nos_ofs_combine_field_netcdf_selfe

    export err=$?
    if [ $err -ne 0 ]; then
      echo "Running nos_ofs_combine_field_netcdf_selfe for $RUNTYPE did not complete normally"
      msg="Running nos_ofs_combine_field_netcdf_selfe for $RUNTYPE did not complete normally"
      postmsg "$jlogfile" "$msg"
      err_exit "$msg"
    else
      echo "Running nos_ofs_combine_field_netcdf_selfe for $RUNTYPE completed normally"
      msg="Running nos_ofs_combine_field_netcdf_selfe for $RUNTYPE completed normally"
      postmsg "$jlogfile" "$msg"
    fi
    echo "combine field nowcast simulation completed at:  `date`" >> $nosjlogfile
# combine station outputs into a single NetCDF file for $RUNTYPE

    if [ -s ${COMOUT}/$RUNTIME_COMBINE_NETCDF_STA_NOWCAST ]
    then
      echo "combine restart control files exists"
      cp -p ${COMOUT}/$RUNTIME_COMBINE_NETCDF_STA_NOWCAST $DATA/outputs/combine_station_output.in
    else
      msg="FATAL ERROR: No combine netcdf station control file"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '**************************************************************'
      echo '*** FATAL ERROR : No combine netcdf station control file   ***'
      echo '**************************************************************'
      echo ' '
      echo $msg
      seton
      err_exit "No combine NetCDF station control file: ${COMOUT}/$RUNTIME_COMBINE_NETCDF_STA_NOWCAST"
    fi
    if [ -s $FIXofs/${STA_NETCDF_CTL} ]
    then
       cp -p $FIXofs/${STA_NETCDF_CTL} $DATA/outputs
    else
       echo "$FIXofs/${STA_NETCDF_CTL} does not rxist !!!"
       echo " combine station netCDF code will not run !!!"
       echo "Provide $FIXofs/${STA_NETCDF_CTL}"
    fi     
#run netcdf combine executable
    $EXECnos/nos_ofs_combine_station_netcdf_selfe <  ./combine_station_output.in
    export err=$?
    if [ $err -ne 0 ]; then
      echo "Running nos_ofs_combine_station_netcdf_selfe for $RUNTYPE did not complete normally"
      msg="Running nos_ofs_combine_station_netcdf_selfe for $RUNTYPE did not complete normally"
      postmsg "$jlogfile" "$msg"
      err_exit "$msg"
    else
      echo "Running nos_ofs_combine_station_netcdf_selfe for $RUNTYPE completed normally"
      msg="Running nos_ofs_combine_station_netcdf_selfe for $RUNTYPE completed normally"
      postmsg "$jlogfile" "$msg"
    fi
#move netcdf files + hotstart files to COMOUT
    if [ -s  Station.nc ]; then 
      mv  Station.nc ${DATA}/$STA_OUT_NOWCAST
#      cp -p  Station.nc ${COMOUT}/$STA_OUT_NOWCAST
    fi
#    if [ -s  combinefields.nc ]; then 
#      cp -p  combinefields.nc ${COMOUT}/$HIS_OUT_NOWCAST
#    fi
#    for combinefields in `ls ${DATA}/nos.creofs.fields.n*.nc`
#    do
#      cp -p ${combinefields} ${COMOUT}/.
#      cp -p ${combinefields} ${DATA}/.
#    done

    cd $DATA
    mv $DATA/outputs $DATA/outputs_nowcast
    mv mirror.out $DATA/outputs_nowcast
  fi
  echo 'Ocean Model run ends at time: ' `date `
#save 3D surface nowcast field output file into COMOUT
  for combinefields in `ls ${DATA}/nos.${RUN}.fields.n*.nc`
  do
    if [ -s ${combinefields} ]; then 
      cp -p ${combinefields} ${COMOUT}/.
    fi
  done
#save 2D surface nowcast field output file into COMOUT
  if [ -s ${DATA}/nos.${RUN}.2ds.n001.$PDY.t${cyc}z.nc ]; then
   for combinefields in `ls ${DATA}/nos.${OFS}.2ds.n*.nc`
   do
    if [ -s ${combinefields} ]; then 
      cp -p ${combinefields} ${COMOUT}/.
    fi
   done
  fi
#save nowcast station output file into COMOUT
  cp -p $DATA/$STA_OUT_NOWCAST  ${COMOUT}/$STA_OUT_NOWCAST
  if [ -s $DATA/nos.${OFS}.avg.nowcast.$PDY.t${cyc}z.nc ]; then
    cp -p $DATA/nos.${OFS}.avg.n*.nc $COMOUT/
  fi 
fi


###############################################################################
###############################################################################
#### FORECAST
###############################################################################
###############################################################################
 
if [ $RUNTYPE == "FORECAST" -o $RUNTYPE == "forecast" ]
then

  if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
  then 
    if [ ! -d $DATA/outputs ]; then
      mkdir -p $DATA/outputs
    fi  
#CHECK FOR MET CONTROL FILE
    if [ -s ${COMOUT}/$RUNTIME_MET_CTL_FORECAST ]
    then
      echo "MET control files exist"
      cp -p ${COMOUT}/$RUNTIME_MET_CTL_FORECAST $DATA/sflux/sflux_inputs.txt
    else
      msg="FATAL ERROR: No MET control file for Forecast"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '*******************************************************'
      echo '*** FATAL ERROR : No MET control file for Forecast  ***'
      echo '*******************************************************'
      echo ' '
      echo $msg
      seton
#      touch err.${RUN}.$PDY1.t${HH}z
      err_exit "No MET control file for forecast: ${COMOUT}/$RUNTIME_MET_CTL_FORECAST"
    fi

  fi
# 1.a RIVER FORCING FILE 
  if [ ${OCEAN_MODEL} == "ROMS" -o ${OCEAN_MODEL} == "roms" ]
  then
    if [ -s $DATA/$RIVER_FORCING_FILE ]
    then
      echo " $DATA/$RIVER_FORCING_FILE existed "
    elif [ -s $COMOUT/$RIVER_FORCING_FILE ]
    then
      cp -p $COMOUT/$RIVER_FORCING_FILE $RIVER_FORCING_FILE
    else  
      msg="FATAL ERROR: NO RIVER FORCING FILE $RIVER_FORCING_FILE"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '*********************************************'
      echo '*** FATAL ERROR : NO RIVER_FORCING_FILE   ***'
      echo '*********************************************'
      echo ' '
      echo $msg
      seton
      touch err.${RUN}.$PDY1.t${HH}z
      err_exit "No river forcing file: $COMOUT/$RIVER_FORCING_FILE"
    fi
  elif [ ${OCEAN_MODEL} == "FVCOM" -o ${OCEAN_MODEL} == "fvcom" ]
  then
    if [ -s $DATA/${RIVER_FORCING_FILE} ]
    then
      echo " $DATA/$RIVER_FORCING_FILE existed "
      rm -fr $DATA/RIVER
      tar -xvf $DATA/${RIVER_FORCING_FILE}
    elif [ -s $COMOUT/${RIVER_FORCING_FILE} ]
    then
      rm -fr $DATA/RIVER
      cp -p $COMOUT/${RIVER_FORCING_FILE} $DATA/.
      tar -xvf $DATA/${RIVER_FORCING_FILE}
    else  
      msg="FATAL ERROR: NO RIVER FORCING FILE $RIVER_FORCING_FILE"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '*********************************************'
      echo '*** FATAL ERROR : NO RIVER_FORCING_FILE   ***'
      echo '*********************************************'
      echo ' '
      echo $msg
      seton
      touch err.${RUN}.$PDY1.t${HH}z
      err_exit "No river forcing file: $COMOUT/${RIVER_FORCING_FILE}"
    fi
  elif [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
  then 
     if [ -s $DATA/selfe_temp.th -a $DATA/selfe_flux.th -a $DATA/selfe_salt.th ]
     then
      echo "RIVER forcing files exist"
      cp -p $DATA/selfe_temp.th  $DATA/temp.th
      cp -p $DATA/selfe_flux.th  $DATA/flux.th
      cp -p $DATA/selfe_salt.th  $DATA/salt.th
     elif [ -s $COMOUT/${RIVER_FORCING_FILE} ]
     then
#       cp -p $COMOUT/${RIVER_FORCING_FILE} $DATA/.
       tar -xvf $COMOUT/${RIVER_FORCING_FILE}
       cp -p $DATA/selfe_temp.th  $DATA/temp.th
       cp -p $DATA/selfe_flux.th  $DATA/flux.th
       cp -p $DATA/selfe_salt.th  $DATA/salt.th
    else
      msg="FATAL ERROR: No River Forcing For Nowcast/Forecast"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '*************************************************************'
      echo '*** FATAL ERROR : NO River Forcing For Nowcast/Forecast   ***'
      echo '*************************************************************'
      echo ' '
      echo $msg
      seton
      err_exit "No river forcing for nowcast/forecast: $COMOUT/${RIVER_FORCING_FILE}"
    fi
  fi

# 1.b OBC FORCING FILE 
  if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
  then
    if [ -s $DATA/elev3D.th -a $DATA/salt_nu.in -a $DATA/temp_nu.in ]
    then
      echo "OBC forcing files exist"
    elif [ -s $COMOUT/$OBC_FORCING_FILE ]
    then
#      cp -p $COMOUT/$OBC_FORCING_FILE $DATA/$OBC_FORCING_FILE
      tar -xvf $COMOUT/$OBC_FORCING_FILE
    else
      msg="FATAL ERROR: No OBC Forcing For Nowcast/Forecast"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '************************************************************'
      echo '*** FATAL ERROR : NO OBC Forcing For Nowcast/Forecast    ***'
      echo '************************************************************'
      echo ' '
      echo $msg
      seton
#      touch err.${RUN}.$PDY1.t${HH}z
      err_exit "No OBC forcing for nowcast/forecast: $COMOUT/$OBC_FORCING_FILE"
    fi
  else
    if [ -f $DATA/$OBC_FORCING_FILE ]
    then
      echo "   $DATA/$OBC_FORCING_FILE existed "
    elif [ -s $COMOUT/$OBC_FORCING_FILE ]
    then
      cp -p $COMOUT/$OBC_FORCING_FILE $OBC_FORCING_FILE
    elif [ -s $COMOUT/$OBC_FORCING_FILE_EL ]
    then
      cp -p $COMOUT/$OBC_FORCING_FILE_EL $OBC_FORCING_FILE_EL
      cp -p $COMOUT/$OBC_FORCING_FILE_TS $OBC_FORCING_FILE_TS
    else
      msg="FATAL ERROR: NO OBC FORCING FILE $OBC_FORCING_FILE"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '*******************************************'
      echo '*** FATAL ERROR : NO OBC_FORCING_FILE   ***'
      echo '*******************************************'
      echo ' '
      echo $msg
      seton
      touch err.${RUN}.$PDY1.t${HH}z
      err_exit "No OBC forcing file: $COMOUT/$OBC_FORCING_FILE_EL"
    fi
  fi
# 1.c Meteorological Forcing For forecast 
  if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
  then
    if [ ! -d $DATA/sflux ]; then
      mkdir -p $DATA/sflux
    else
      rm -f $DATA/sflux/*.nc
    fi  
    if [ -s $COMOUT/$MET_NETCDF_1_FORECAST ]
    then
      cd $DATA/sflux
      tar -xvf $COMOUT/$MET_NETCDF_1_FORECAST
      cd $DATA
    else
      msg="FATAL ERROR: NO Meteorological Forcing For forecast $MET_NETCDF_1_FORECAST"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '*************************************************************'
      echo '*** FATAL ERROR : NO Meteorological Forcing For forecast  ***'
      echo '*************************************************************'
      echo ' '
      echo $msg
      seton
      err_exit "No meteorological forcing for forecast: $COMOUT/$MET_NETCDF_1_FORECAST"
        
    fi  
  else
    if [ -f $DATA/$MET_NETCDF_1_FORECAST ]
    then
      echo "   $DATA/$MET_NETCDF_1_FORECAST existed "
    elif [ -s $COMOUT/$MET_NETCDF_1_FORECAST ]
    then
      cp -p $COMOUT/$MET_NETCDF_1_FORECAST $MET_NETCDF_1_FORECAST
    else
      msg="FATAL ERROR: NO Meteorological Forcing For forecast $MET_NETCDF_1_FORECAST"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '**************************************************************'
      echo '*** FATAL ERROR : NO Meteorological Forcing For forecast   ***'
      echo '**************************************************************'
      echo ' '
      echo $msg
      seton
      err_exit "No meteorological forcing for forecast: $COMOUT/$MET_NETCDF_1_FORECAST"
    fi  
  fi

# 1.g HFlux For Forecast
  if [ -s $DATA/$MET_NETCDF_2_FORECAST ]
  then
      echo "   $DATA/$MET_NETCDF_2_FORECAST existed "
  elif [ -s $COMOUT/$MET_NETCDF_2_FORECAST ]
  then
      cp -p $COMOUT/$MET_NETCDF_2_FORECAST $MET_NETCDF_2_FORECAST
  fi

# 1.h forecast cntl file
  if [ -f $DATA/${RUN}_${OCEAN_MODEL}_forecast.in ]
  then
    echo "   $DATA/${RUN}_${OCEAN_MODEL}_forecast.in existed "
    if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
    then
      cp -p $DATA/${RUN}_${OCEAN_MODEL}_forecast.in $DATA/param.in
    elif [ ${OCEAN_MODEL} == "FVCOM" -o ${OCEAN_MODEL} == "fvcom" ]
    then
      cp -p $DATA/${RUN}_${OCEAN_MODEL}_forecast.in $DATA/${RUN}'_run.nml'
    fi  
  elif [ -s $COMOUT/${RUNTIME_CTL_FORECAST} ]
  then
    if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
    then
      cp -p $COMOUT/${RUNTIME_CTL_FORECAST} $DATA/param.in
    elif [ ${OCEAN_MODEL} == "FVCOM" -o ${OCEAN_MODEL} == "fvcom" ]
    then
      cp -p $COMOUT/${RUNTIME_CTL_FORECAST} $DATA/${RUN}'_run.nml'
    elif [ ${OCEAN_MODEL} == "ROMS" -o ${OCEAN_MODEL} == "roms" ]
    then
      cp -p $COMOUT/${RUNTIME_CTL_FORECAST} ${RUN}_${OCEAN_MODEL}_forecast.in
    fi  

  else
    msg="FATAL ERROR: MODEL runtime input file for forecast is not found"
    postmsg "$jlogfile" "$msg"
    postmsg "$nosjlogfile" "$msg"
    setoff
    echo ' '
    echo '******************************************************************'
    echo '*** FATAL ERROR : ROMS runtime input file for nowcast is not found'
    echo '******************************************************************'
    echo ' '
    echo $msg
    seton
    touch err.${RUN}.$PDY1.t${HH}z
    err_exit "ROMS runtime input file for nowcast is not found: $COMOUT/${RUNTIME_CTL_FORECAST}"
  fi

#1.h Tide data 
  if [ ${OCEAN_MODEL} == "ROMS" -o ${OCEAN_MODEL} == "roms" ]
  then
    if [ -f $DATA/$HC_FILE_OFS ]
    then
       echo "   $DATA/$HC_FILE_OFS existed "
    elif [ -s $COMOUT/$HC_FILE_OFS ]
    then
      cp -p $COMOUT/$HC_FILE_OFS $HC_FILE_OFS
    else
      msg="FATAL ERROR: Tide Constituent file for ROMS OBC is not found"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '*****************************************************************'
      echo '*** FATAL ERROR : Tide Constituent file for ROMS OBC is not found'
      echo '*****************************************************************'
      echo ' '
      echo $msg
      seton
      touch err.${RUN}.$PDY1.t${HH}z
      err_exit "Tide constituent file for ROMS OBC is not found: $COMOUT/$HC_FILE_OFS"
    fi
  fi

#1.i Nowcast RST file 
  if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
  then

    if [ -f $DATA/$RST_OUT_NOWCAST ]
    then
      cp -p  $DATA/$RST_OUT_NOWCAST  $DATA/hotstart.in
    elif [ -s $COMOUT/$RST_OUT_NOWCAST ]
    then
      cp -p $COMOUT/$RST_OUT_NOWCAST $DATA/hotstart.in
    else
      msg="FATAL ERROR: NO Restart file for Forecast"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '**************************************************'
      echo '*** FATAL ERROR : NO Restart file for Forecast ***'
      echo '**************************************************'
      echo ' '
      echo $msg
      seton
      err_exit "No restart file for forecast: $COMOUT/$RST_OUT_NOWCAST"
    fi
  else
    if [ -f $DATA/$RST_OUT_NOWCAST ]
    then
      echo "   $DATA/$RST_OUT_NOWCAST existed " 
    elif [ -s $COMOUT/$RST_OUT_NOWCAST ]
    then
      cp -p $COMOUT/$RST_OUT_NOWCAST $RST_OUT_NOWCAST
    else
      msg="FATAL ERROR: NO Restart file for Forecast"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '**************************************************'
      echo '*** FATAL ERROR : NO Restart file for Forecast ***'
      echo '**************************************************'
      echo ' '
      echo $msg
      seton
      err_exit "No restart file for forecast: $COMOUT/$RST_OUT_NOWCAST"
    fi
  fi 

  ######## FORECAST #########
  ##########################
# --------------------------------------------------------------------------- #
# 2   Execute ocean model of ROMS; where ${RUN}_roms_forecast.in is created by nos_ofs_reformat_roms_ctl.sh
  if [ ${OCEAN_MODEL} == "ROMS" -o ${OCEAN_MODEL} == "roms" ]; then
     mpirun -np $NPP -ppn $PPN -f $HOSTFILE $EXECnos/${RUN}_roms_mpi ./${RUN}_${OCEAN_MODEL}_forecast.in >> ${MODEL_LOG_FORECAST}
     #mpirun $EXECnos/${RUN}_roms_mpi ./${RUN}_${OCEAN_MODEL}_forecast.in >> ${MODEL_LOG_FORECAST}
    export err=$?
    if [ $err -ne 0 ]
    then
      echo "Running ocean model ${RUN}_roms_mpi for $RUNTYPE did not complete normally"
      msg="Running ocean model ${RUN}_roms_mpi for $RUNTYPE did not complete normally"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      err_exit "$msg"
#    else
#      echo "Running ocean model ${RUN}_roms_mpi completed normally"
#      msg="Running ocean model ${RUN}_roms_mpi  completed normally"
#      postmsg "$jlogfile" "$msg"
#      postmsg "$nosjlogfile" "$msg"
    fi

    rm -f corms.fcst
    if [ -s ${MODEL_LOG_FORECAST} ]
    then
      grep "ROMS/TOMS - Blows up" ${MODEL_LOG_FORECAST} > corms.fcst
      grep "Blowing-up" ${MODEL_LOG_FORECAST} >> corms.fcst
      grep "Abnormal termination: BLOWUP" ${MODEL_LOG_FORECAST} >> corms.fcst
    fi
    if [ -s  corms.fcst ]
    then
      echo "${RUN} FORECAST RUN OF CYCLE t${HH}z ON $PDY FAILED 00"  >> $cormslogfile 
      echo "FORECAST_RUN DONE 0"  >> $cormslogfile
      export err=99; err_chk
    else
      echo "${RUN} FORECAST RUN  OF CYCLE t${HH}z ON $PDY COMPLETED SUCCESSFULLY 100" >> $cormslogfile
      echo "FORECAST_RUN DONE 100"  >> $cormslogfile
##    create a status file for CO-OPS side
      rm -f ${RUN}.status
      YYYY=`echo $time_nowcastend | cut -c1-4 `
        MM=`echo $time_nowcastend |cut -c5-6 `
        DD=`echo $time_nowcastend |cut -c7-8 `
        HH=`echo $time_nowcastend |cut -c9-10 `
       echo $YYYY$MM$DD$HH > ${RUN}.status
       cp ${RUN}.status $COMOUT/.
    fi
## separate HIS output file into multiple smaller files
#AJ 02/26/2015       Im1=0
    Im1=0   #for new version of ROMS which doesn't ouput hour=0 (initial time)
    IQm=0
#    I=1
#    while (( I < 168 ))
#    do
#       fhr4=`echo $I |  awk '{printf("%04i",$1)}'`
#       file=nos.${RUN}.fields.forecast.$PDY.t${cyc}z_${fhr4}.nc
#       if [ -s $file ]; then
#         fhr3=`echo $Im1 |  awk '{printf("%03i",$1)}'`
#         fileout=nos.${RUN}.fields.f${fhr3}.$PDY.t${cyc}z.nc
#         mv $file $fileout
#         Im1=`expr $Im1 + 1`
#       fi
#       (( I = I + 1 ))
#    done
    if [ -f nos.${RUN}.avg.nc ]; then
      mv nos.${RUN}.avg.nc nos.${RUN}.avg.forecast.$PDY.t${cyc}z.nc
    fi

    NHIS_INTERVAL=`expr $NHIS / 3600`
    NQCK_INTERVAL=`expr $NQCK / 3600`
    I=1
    while (( I < 168 ))
    do
       fhr4=`echo $I |  awk '{printf("%04i",$1)}'`
       file=nos.${RUN}.fields.forecast.$PDY.t${cyc}z_${fhr4}.nc
       if [ -s $file ]; then
         Im1=`expr $Im1 + $NHIS_INTERVAL`
         fhr3=`echo $Im1 |  awk '{printf("%03i",$1)}'`
         fileout=nos.${RUN}.fields.f${fhr3}.$PDY.t${cyc}z.nc
         mv $file $fileout
#      Im1=`expr $Im1 + 1`
       fi

       file=nos.${RUN}.surface.forecast.$PDY.t${cyc}z_${fhr4}.nc
       if [ -s $file ]; then
         IQm=`expr $IQm + $NQCK_INTERVAL`
         fhr3=`echo $IQm |  awk '{printf("%03i",$1)}'`
         fileout=nos.${RUN}.2ds.f${fhr3}.$PDY.t${cyc}z.nc
#         cp -p $file $COMOUT/$fileout
         mv $file $fileout
       fi
       (( I = I + 1 ))
    done

  ######## FORECAST #########
  ##########################
  elif [ ${OCEAN_MODEL} == "FVCOM" -o ${OCEAN_MODEL} == "fvcom" ]
  then
    rm -f $MODEL_LOG_FORECAST
    #mpirun $EXECnos/fvcom_${RUN} --casename=$RUN > $MODEL_LOG_FORECAST
    mpirun -np $NPP -ppn $PPN $mpiopts -bind-to core $EXECnos/fvcom_${RUN} --casename=$RUN > $MODEL_LOG_FORECAST
    export err=$?
    if [ $err -ne 0 ]
    then
      echo "Running ocean model for $RUNTYPE did not complete normally"
      msg="Running ocean model  for $RUNTYPE did not complete normally"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      err_exit "$msg"
#    else
#      echo "Running ocean model for $RUNTYPE completed normally"
#      msg="Running ocean model  for $RUNTYPE completed normally"
#      postmsg "$jlogfile" "$msg"
#      postmsg "$nosjlogfile" "$msg"
    fi
    rm -f corms.fcst
    if [ -s ${MODEL_LOG_FORECAST} ]
    then
      grep "NaN" ${MODEL_LOG_FORECAST} > corms.fcst
      grep "STOP" ${MODEL_LOG_FORECAST} >> corms.fcst
      grep "failed" ${MODEL_LOG_FORECAST} >> corms.fcst
      grep "Failed" ${MODEL_LOG_FORECAST} >> corms.fcst
      grep "Blowing-up" ${MODEL_LOG_FORECAST} >> corms.fcst
      grep "Abnormal termination: BLOWUP" ${MODEL_LOG_FORECAST} >> corms.fcst
      if [ -s  corms.fcst ]
      then
        echo "${RUN} FORECAST RUN OF CYCLE t${HH}z ON $YYYY$MM$DD FAILED 00"  >> $cormslogfile 
        echo "FORECAST_RUN DONE 0"  >> $cormslogfile
        export err=99; err_chk
      else
        echo "${RUN} FORECAST RUN  OF CYCLE t${HH}z ON $YYYY$MM$DD COMPLETED SUCCESSFULLY 100" >> $cormslogfile
        echo "FORECAST_RUN DONE 100"  >> $cormslogfile
##    create a status file for CO-OPS side
        rm -f ${RUN}.status
        YYYY=`echo $time_nowcastend | cut -c1-4 `
          MM=`echo $time_nowcastend |cut -c5-6 `
          DD=`echo $time_nowcastend |cut -c7-8 `
          HH=`echo $time_nowcastend |cut -c9-10 `
        echo $YYYY$MM$DD$HH > ${RUN}.status
        cp ${RUN}.status $COMOUT/.

#        if [ -f $DATA/${RUN}'_0001.nc' ]
#        then
#         cp -p $DATA/${RUN}'_0001.nc' $DATA/$HIS_OUT_FORECAST
#         echo "  $HIS_OUT_FORECAST  saved "
#       fi
        if [ -f $DATA/$RUN'_station_timeseries.nc' ]
        then
          mv $DATA/$RUN'_station_timeseries.nc' $DATA/$STA_OUT_FORECAST
          echo "  $STA_OUT_FORECAST  saved "
        fi
        NC_OUT_INTERVAL=`printf '%.0f' $NC_OUT_INTERVAL` ## convert decimal number to integer, and get the nearest integer
        NCSF_OUT_INTERVAL=${NCSF_OUT_INTERVAL%.*} # just tuncate the integer part and remove the fractional part   
        NHIS_INTERVAL=`expr $NC_OUT_INTERVAL / 3600`
        NQCK_INTERVAL=`expr $NCSF_OUT_INTERVAL / 3600`
        Im1=0
        IQm=0
        I=1
        while (( I < 169 ))
        do
          fhr4=`echo $I |  awk '{printf("%04i",$1)}'`
          file=$RUN'_'${fhr4}'.nc'
          if [ -s $file ]; then
            fhr3=`echo $Im1 |  awk '{printf("%03i",$1)}'`
            fileout=nos.${RUN}.fields.f${fhr3}.$PDY.t${cyc}z.nc
            mv $file $fileout
            Im1=`expr $Im1 + $NHIS_INTERVAL`
          fi

          file=$RUN'_surface_'${fhr4}'.nc'
          if [ -s $file ]; then
            fhr3=`echo $IQm |  awk '{printf("%03i",$1)}'`
            fileout=nos.${RUN}.2ds.f${fhr3}.$PDY.t${cyc}z.nc
            mv $file $fileout
            IQm=`expr $IQm + $NQCK_INTERVAL`
          fi

          (( I = I + 1 ))
        done
      fi
    else
        echo "${RUN} FORECAST RUN OF CYCLE t${HH}z ON $YYYY$MM$DD FAILED 00"  >> $cormslogfile 
        echo "FORECAST_RUN DONE 0"  >> $cormslogfile
        export err=99; err_chk
    fi  

    if [ ${OFS} == "NGOFS" -o ${OFS} == "ngofs" ]; then
       if [ -s nos_${RUN}_nestnode_negofs.nc ]; then
          cp -p nos_${RUN}_nestnode_negofs.nc $COMOUT/nos.${RUN}.nestnode.negofs.forecast.$PDY.t${cyc}z.nc
       fi
       if [ -s nos_${RUN}_nestnode_nwgofs.nc ]; then
          cp -p nos_${RUN}_nestnode_nwgofs.nc $COMOUT/nos.${RUN}.nestnode.nwgofs.forecast.$PDY.t${cyc}z.nc
       fi
    fi
 

  elif [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
  then
    echo "forecast simulation began at:  `date`" >> $nosjlogfile
    mpirun $EXECnos/selfe_${OFS} > $MODEL_LOG_FORECAST
    export err=$?
    rm -f corms.now corms.fcst 
    if [ -s $DATA/mirror.out ]
    then
      grep "Run completed successfully" $DATA/mirror.out > corms.fcst
    fi
    if [ ! -s  corms.fcst ]
    then
       echo "${OFS} FORECAST RUN OF CYCLE t${HH}z ON $YYYY$MM$DD FAILED 00"  >> $cormslogfile 
       echo "FORECAST_RUN DONE 0"  >> $cormslogfile
       export err=99; err_chk
    else
       echo "${OFS} FORECAST RUN OF CYCLE t${HH}z ON $YYYY$MM$DD COMPLETED SUCCESSFULLY 100" >> $cormslogfile
       echo "FORECAST_RUN DONE 100"  >> $cormslogfile
    fi

    if [ $err -ne 0 ]
    then
      echo "Running ocean model for $RUNTYPE did not complete normally"
      msg="Running ocean model  for $RUNTYPE did not complete normally"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      err_exit "$msg"
    else
      echo "Running ocean model for $RUNTYPE completed normally"
      msg="Running ocean model  for $RUNTYPE completed normally"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
    fi
    echo "forecast simulation completed at:  `date`" >> $nosjlogfile
# combine field outputs into a single NetCDF file for forecast
    echo "combine field forecast simulation began at:  `date`" >> $nosjlogfile
    cd $DATA/outputs
    rm -f listyes listno
    I=1
#    while [ $I -le 100 ]
    while (( I < 100 ))
    do
      file=$I'_0000_elev.61'
      if [ -s $file ]; then
         echo $file >> listyes
      fi	 
      (( I = I + 1 ))
    done
    if [ -s listyes ]; then
       btmnow=`cat listyes | awk -F_ '{print $1}' | head -1`
       etmnow=`cat listyes | awk -F_ '{print $1}' | tail -1`
    fi
    if [ -s $FIXofs/$RUNTIME_COMBINE_NETCDF ]
    then
      if [ -s $FIXofs/$GRIDFILE_LL ]; then
        cp -p $FIXofs/$GRIDFILE_LL $DATA/outputs
      fi
      if [ -s $DATA/param.in ]; then
        cp -p $DATA/param.in $DATA/outputs
      else
        echo $DATA/param.in does not exist
        echo 'combine field netcdf code will fail !!'	
      fi
      if [ $RUNTYPE == "FORECAST" -o $RUNTYPE == "forecast" ]; then
         ncastfcast="f"
      fi	 	
      	
      cat $FIXofs/$RUNTIME_COMBINE_NETCDF | sed -e s/BTM/$btmnow/g -e s/ETM/$etmnow/g \
      -e s/GRIDFILE/$GRIDFILE_LL/g \
      -e s/BASE_DATE/$BASE_DATE/g  \
      -e s/TIME_NOWCASTEND/${time_nowcastend}/g \
      -e s/RUNTYPE/${ncastfcast}/g > combine_output.in
      cp -p combine_output.in ${COMOUT}/$RUNTIME_COMBINE_NETCDF_FORECAST

    else
      msg="FATAL ERROR: No control file for combining field outputs of $RUNTYPE"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo " "
      echo "*****************************************************************************"
      echo "*** FATAL ERROR : No control file for combining field outputs of $RUNTYPE ***"
      echo "*****************************************************************************"
      echo " "
      echo $msg
      seton
#      touch err.${OFS}.$PDY1.t${HH}z
      err_exit "No control file for combining field outputs of $RUNTYPE: $FIXofs/$RUNTIME_COMBINE_NETCDF"
    fi
 
#run netcdf combine executable
#    cp -p $EXECnos/nos_ofs_combine_field_netcdf_selfe $DATA/outputs 
    time tar -cvf - . | cat >/dev/null 
    time $EXECnos/nos_ofs_combine_field_netcdf_selfe
    wait 
    export err=$?
    if [ $err -ne 0 ]; then
      echo "Running nos_ofs_combine_netcdf_out_selfe for $RUNTYPE did not complete normally"
      msg="Running nos_ofs_combine_netcdf_out_selfe for $RUNTYPE did not complete normally"
      postmsg "$jlogfile" "$msg"
      err_exit "$msg"
    else
      echo "Running nos_ofs_combine_netcdf_out_selfe for $RUNTYPE completed normally"
      msg="Running nos_ofs_combine_netcdf_out_selfe for $RUNTYPE completed normally"
      postmsg "$jlogfile" "$msg"
    fi
    echo "combine field forecast simulation completed at:  `date`" >> $nosjlogfile
# combine station outputs
    if [ -s ${COMOUT}/$RUNTIME_COMBINE_NETCDF_STA_FORECAST ]
    then
      echo "combine netcdf station control files exists"
      cp -p ${COMOUT}/$RUNTIME_COMBINE_NETCDF_STA_FORECAST $DATA/outputs/combine_station_output.in
    else
      msg="FATAL ERROR: No combine netcdf station control file"
      postmsg "$jlogfile" "$msg"
      postmsg "$nosjlogfile" "$msg"
      setoff
      echo ' '
      echo '*************************************************************'
      echo '*** FATAL ERROR : No combine netcdf station control file  ***'
      echo '*************************************************************'
      echo ' '
      echo $msg
      seton
      err_exit "No combine NetCDF station control file: ${COMOUT}/$RUNTIME_COMBINE_NETCDF_STA_FORECAST"
    fi
    if [ -s $FIXofs/${STA_NETCDF_CTL} ]
    then
       cp -p $FIXofs/${STA_NETCDF_CTL} $DATA/outputs
    else
       echo "$FIXofs/${STA_NETCDF_CTL} does not rxist !!!"
       echo " combine station netCDF code will not run !!!"
       echo "Provide $FIXofs/${STA_NETCDF_CTL}"
    fi     
 
#run netcdf combine executable
    $EXECnos/nos_ofs_combine_station_netcdf_selfe <  ./combine_station_output.in
    export err=$?
    if [ $err -ne 0 ]; then
      echo "Running nos_ofs_combine_station_netcdf_selfe for $RUNTYPE did not complete normally"
      msg="Running nos_ofs_combine_station_netcdf_selfe for $RUNTYPE did not complete normally"
      postmsg "$jlogfile" "$msg"
      err_exit "$msg"
    else
      echo "Running nos_ofs_combine_station_netcdf_selfe for $RUNTYPE completed normally"
      msg="Running nos_ofs_combine_station_netcdf_selfe for $RUNTYPE completed normally"
      postmsg "$jlogfile" "$msg"
    fi
    if [ -s  Station.nc ]; then 
      cp -p  Station.nc ${DATA}/$STA_OUT_FORECAST
    fi
#    if [ -s  combinefields.nc ]; then 
#      cp -p  combinefields.nc ${COMOUT}/$HIS_OUT_FORECAST
#    fi
#    for combinefields in `ls ${DATA}/nos.creofs.fields.f*.nc`
#    do
#      cp -p ${combinefields} ${COMOUT}/.
#      cp -p ${combinefields} ${DATA}/.
#    done

##    create a status file for CO-OPS side
      rm -f ${OFS}.status
      YYYY=`echo $time_nowcastend | cut -c1-4 `
        MM=`echo $time_nowcastend |cut -c5-6 `
        DD=`echo $time_nowcastend |cut -c7-8 `
        HH=`echo $time_nowcastend |cut -c9-10 `
       echo $YYYY$MM$DD$HH > ${OFS}.status
       cp ${OFS}.status $COMOUT/.
  
  fi
#save 3D forecast field output file into COMOUT
  cd $DATA
  for combinefields in `ls ${DATA}/nos.${OFS}.fields.f*.nc`
  do
    if [ -s ${combinefields} ]; then
      cp -p ${combinefields} ${COMOUT}/.
    fi
  done

#save 2D surface forecast field output file into COMOUT
  if [ -s ${DATA}/nos.${RUN}.2ds.f003.$PDY.t${cyc}z.nc ]; then
   for combinefields in `ls ${DATA}/nos.${OFS}.2ds.f*.nc`
   do
    if [ -s ${combinefields} ]; then
      cp -p ${combinefields} ${COMOUT}/.
    fi
   done
  fi
#save forecast station output file into COMOUT
  cp -p $DATA/$STA_OUT_FORECAST  ${COMOUT}/$STA_OUT_FORECAST
  if [ -s $DATA/nos.${OFS}.avg.forecast.$PDY.t${cyc}z.nc ]; then
    cp -p $DATA/nos.${OFS}.avg.f*.nc $COMOUT/
  fi 

fi 


# --------------------------------------------------------------------------- #
# 4.  Ending output

  setoff
  echo ' '
  echo "Ending nos_ofs_nowcast_forecast.sh at : `date`"
  echo ' '
  echo '                     *** End of NOS OFS NOWCAST/FORECAST SIMULATIONS ***'
  echo ' '

# End of NOS OFS Nowcast script ------------------------------------------- #

