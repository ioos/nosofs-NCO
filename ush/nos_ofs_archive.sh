#!/bin/sh
#  Script Name:  nos_ofs_archive.prod
#  Purpose:                                                                   #
#  This script is to make copy model files to corresonding directories after  #
#  successfully completing nowcast and forecast simulations by running:       #
#  exnos_ofs_nowcast_forecast.sh.sms                                          #
#                                                                             #
#  Child scripts :                                                            #
#                                                                             #
#  The utililty script used:                                                  #
#                                                                             #
# Remarks :                                                                   #
# - For non-fatal errors output is written to the *.log file.                 #
#                                                                             #
# Language:  C shell script
# Nowcast  
# Input:
#     nos.${OFS}.river.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${OFS}.obc.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${OFS}.met.nowcast.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${OFS}.init.nowcast.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${OFS}.hflux.nowcast.$yyyy$mm$dd.t${HH}z.nc
#     ${OFS}_roms_nowcast.in
#     nos.${OFS}.roms.tides.nc
# Output:
#     nos.${OFS}.stations.nowcast.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${OFS}.fields.nowcast.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${OFS}.fields.forecast.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${OFS}.rst.nowcast.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${OFS}.roms.nowcast.$yyyy$mm$dd.t${cyc}z.log
# Forecast  
# Input:
#     nos.${OFS}.river.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${OFS}.obc.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${OFS}.met.forecast.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${OFS}.rst.nowcast.$yyyy$mm$dd.t${cyc}z.nc
#     ${OFS}_roms_forecast.in
#     nos.${OFS}.roms.tides.nc
# Output:
#     nos.${OFS}.stations.forecast.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${OFS}.fields.forecast.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${OFS}.rst.forecast.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${OFS}.roms.forecast.$yyyy$mm$dd.t${cyc}z.log
#
# Technical Contact:    Aijun Zhang         Org:  NOS/CO-OPS                  #
#                       Phone: 301-7132890 ext. 127                           #
#                       E-Mail: aijun.zhang@noaa.gov                          #
#                                                                             #
#                                                                             #
###############################################################################
# --------------------------------------------------------------------------- #
# 0.  Preparations
# 0.a Basic modes of operation

cd $DATA

seton='-x'
setoff='+x'
set $seton

set $setoff
echo ' '
echo '  		    ****************************************'
echo '  		    *** NOS OFS  ARCHIVE SCRIPT  ***        '
echo '  		    ****************************************'
echo ' '
echo "Starting nos_ofs_archive.sh at : `date`"
set $seton
RUNTYPE=$1
#export MP_PGMMODEL=mpmd
#export MP_CMDFILE=cmdfile
###############################################################################
if [ $RUNTYPE == "NOWCAST" -o $RUNTYPE == "nowcast" ]
then

# 1  Save nowcast output 
# 1.1 Nowcast log 
    if [ -f $COMOUT/${MODEL_LOG_NOWCAST} ]
    then
      echo "$COMOUT/${MODEL_LOG_NOWCAST}" existed
      if [ $SENDDBN = YES ]; then
        $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_TEXT $job $COMOUT/${MODEL_LOG_NOWCAST}
      fi
    else  
      if [ -f ${MODEL_LOG_NOWCAST} ]
      then
        cp -p ${MODEL_LOG_NOWCAST} $COMOUT/.  
        if [ $SENDDBN = YES ]; then
          $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_TEXT $job $COMOUT/${MODEL_LOG_NOWCAST}
        fi
      else
        echo "WARNING: ${MODEL_LOG_NOWCAST} does not exist !!"
      fi		
    fi

# 1.2 HIS nowcast 
#    if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
#    then 
     if [ -s $COMOUT/nos.${OFS}.2ds.n003.$PDY.t${cyc}z.nc ]; then
       for combinefields in `ls $COMOUT/nos.${OFS}.2ds.n*t${cyc}z.nc`
       do
         if [ -f ${combinefields} ]
         then
           if [ $SENDDBN = YES ]; then
             $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job ${combinefields}
           fi
         fi
       done
     fi
     if [ -s $DATA/nos.${OFS}.avg.nowcast.$PDY.t${cyc}z.nc ]; then
       for combinefields in `ls $COMOUT/nos.${OFS}.avg.n*.nc`
       do
         if [ -f ${combinefields} ]
         then
           if [ $SENDDBN = YES ]; then
             if [ "${OFS,,}" == "gomofs" -o "${OFS,,}" == "wcofs" -o "${OFS,,}" == "ciofs" ]; then
               $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF_LRG $job ${combinefields}
             else
               $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job ${combinefields}
             fi
           fi
         fi
       done
     fi
       for combinefields in `ls $COMOUT/nos.${OFS}.fields.n*t${cyc}z.nc`
       do
         if [ -f ${combinefields} ]
         then
           if [ $SENDDBN = YES ]; then
             if [ "${OFS,,}" == "gomofs" -o "${OFS,,}" == "wcofs" -o "${OFS,,}" == "ciofs" ]; then
               $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF_LRG $job ${combinefields}
             else
               $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job ${combinefields}
             fi
           fi
         fi
       done
#    else   
      if [ -f $COMOUT/$HIS_OUT_NOWCAST ]
      then
        if [ $SENDDBN = YES ]; then
         $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job $COMOUT/${HIS_OUT_NOWCAST}
        fi
      else
        if [ -f $DATA/$HIS_OUT_NOWCAST ]
        then
          cp -p $DATA/$HIS_OUT_NOWCAST $COMOUT/$HIS_OUT_NOWCAST 
          echo "   $HIS_OUT_NOWCAST saved "
          if [ $SENDDBN = YES ]; then
            $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job $COMOUT/${HIS_OUT_NOWCAST}
          fi
        fi
      fi
#    fi
# 1.3 STA nowcast
    if [ -f $COMOUT/$STA_OUT_NOWCAST ]
    then
      if [ $SENDDBN = YES ]; then
         $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job $COMOUT/${STA_OUT_NOWCAST}
      fi
    else    
      if [ -f $DATA/$STA_OUT_NOWCAST ]
      then
        cp -p $DATA/$STA_OUT_NOWCAST $COMOUT/$STA_OUT_NOWCAST
        echo "   $STA_OUT_NOWCAST saved "
        if [ $SENDDBN = YES ]; then
           $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job $COMOUT/${STA_OUT_NOWCAST}
        fi
      fi	
    fi
   
# 1.4 nowcast initial file
    if [ -f $COMOUT/$INI_FILE_NOWCAST ]
    then
      if [ $SENDDBN = YES ]; then
        if [ "${OFS,,}" == "gomofs" -o "${OFS,,}" == "creofs" -o "${OFS,,}" == "wcofs" -o "${OFS,,}" == "ciofs" ]; then
           $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF_LRG $job $COMOUT/$INI_FILE_NOWCAST
        else
           $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job $COMOUT/$INI_FILE_NOWCAST
        fi
      fi
    else  
      if [ -f $DATA/$INI_FILE_NOWCAST ]
      then
        cp -p $DATA/$INI_FILE_NOWCAST $COMOUT/$INI_FILE_NOWCAST
        if [ $SENDDBN = YES ]; then
          if [ "${OFS,,}" == "gomofs" -o "${OFS,,}" == "creofs" -o "${OFS,,}" == "wcofs" -o "${OFS,,}" == "ciofs" ]; then
             $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF_LRG $job $COMOUT/$INI_FILE_NOWCAST
          else
             $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job $COMOUT/$INI_FILE_NOWCAST
          fi
        fi
      fi
    fi
# 1.5 ROMS runtime control file for nowcast
    if [ -f $COMOUT/$RUNTIME_CTL_NOWCAST ]
    then
      if [ $SENDDBN = YES ]; then
         $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_TEXT $job $COMOUT/${RUNTIME_CTL_NOWCAST}
      fi
    else  
      if [ -f $DATA/$RUNTIME_CTL_NOWCAST ]
      then
        cp -p $DATA/$RUNTIME_CTL_NOWCAST $COMOUT/$RUNTIME_CTL_NOWCAST
        if [ $SENDDBN = YES ]; then
           $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job $COMOUT/$RUNTIME_CTL_NOWCAST
        fi
      fi
    fi

# 1.6 Met forcing file for nowcast
    if [ -f $COMOUT/$MET_NETCDF_1_NOWCAST ]
    then
      if [ $SENDDBN = YES ]; then
         $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job $COMOUT/${MET_NETCDF_1_NOWCAST}
      fi
    else  
      if [ -f $DATA/$MET_NETCDF_1_NOWCAST ]
      then
        cp -p $MET_NETCDF_1_NOWCAST $COMOUT/$MET_NETCDF_1_NOWCAST
        if [ $SENDDBN = YES ]; then
           $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job $COMOUT/${MET_NETCDF_1_NOWCAST}
        fi
      fi
    fi
# 1.7 Met heat flux forcing file for nowcast
    if [ -f $COMOUT/$MET_NETCDF_2_NOWCAST ]
    then
      if [ $SENDDBN = YES ]; then
         $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job $COMOUT/${MET_NETCDF_2_NOWCAST}
      fi
    else  
      if [ -f $DATA/$MET_NETCDF_2_NOWCAST ]
      then
        cp -p $MET_NETCDF_2_NOWCAST $COMOUT/$MET_NETCDF_2_NOWCAST
        if [ $SENDDBN = YES ]; then
         $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job $COMOUT/${MET_NETCDF_2_NOWCAST}
        fi
      fi
    fi
# 1.8 Restart file from nowcast run used by forecast cycle
#     11/20/17 - alert disabled because initial file, also alerted, is a copy of the previous cycle's restart file
    if [ -f $COMOUT/$RST_OUT_NOWCAST ]
    then
      echo "$RST_OUT_NOWCAST is saved in $COMOUT"
#      if [ $SENDDBN = YES ]; then
#        if [ "${OFS,,}" == "gomofs" -o "${OFS,,}" == "creofs" -o "${OFS,,}" == "wcofs" -o "${OFS,,}" == "ciofs" ]; then
#          $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF_LRG $job $COMOUT/${RST_OUT_NOWCAST}
#        else
#          $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job $COMOUT/${RST_OUT_NOWCAST}
#        fi
#      fi
    else  
      if [ -f $DATA/$RST_OUT_NOWCAST ]
      then
        cp $DATA/$RST_OUT_NOWCAST $COMOUT/$RST_OUT_NOWCAST
        echo "   $RST_OUT_NOWCAST saved "
#        if [ $SENDDBN = YES ]; then
#          if [ "${OFS,,}" == "gomofs" -o "${OFS,,}" == "creofs" -o "${OFS,,}" == "wcofs" -o "${OFS,,}" == "ciofs" ]; then
#            $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF_LRG $job $COMOUT/${RST_OUT_NOWCAST}
#          else
#            $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job $COMOUT/${RST_OUT_NOWCAST}
#          fi
#        fi
      fi
    fi
    echo "ARCHIVE_NOWCAST DONE 100" >> $cormslogfile
fi

# --------------------------------------------------------------------------- #
# 2  Save forecast output 
if [ $RUNTYPE == "FORECAST" -o $RUNTYPE == "forecast" ]
then

# 2.1 Forecast log 
    if [ -f $COMOUT/${MODEL_LOG_FORECAST} ]
    then
      if [ $SENDDBN = YES ]; then
         $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_TEXT $job $COMOUT/${MODEL_LOG_FORECAST}
      fi
    else  
      if [ -f ${MODEL_LOG_FORECAST} ]
      then
        cp ${MODEL_LOG_FORECAST} $COMOUT/${MODEL_LOG_FORECAST}  
        echo "  ${MODEL_LOG_FORECAST}  saved "
        if [ $SENDDBN = YES ]; then
          $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_TEXT $job $COMOUT/${MODEL_LOG_FORECAST}
        fi
      fi
    fi

  export pgm=$DATA/$HIS_OUT_FORECAST"_copy"
  . prep_step

# 2.2 HIS forecast 
#    if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
#    then
     if [ -s $DATA/nos.${OFS}.2ds.f003.$PDY.t${cyc}z.nc ]; then 
       for combinefields in `ls $COMOUT/nos.${OFS}.2ds.f*t${cyc}z.nc`
       do
         if [ -f ${combinefields} ]
         then
           if [ $SENDDBN = YES ]; then
             $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job ${combinefields}
           fi
         fi
       done
     fi
       for combinefields in `ls $COMOUT/nos.${OFS}.fields.f*t${cyc}z.nc`
       do
         if [ -f ${combinefields} ]
         then
           if [ $SENDDBN = YES ]; then
             if [ "${OFS,,}" == "gomofs" -o "${OFS,,}" == "wcofs" -o "${OFS,,}" == "ciofs" ]; then
               $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF_LRG $job ${combinefields}
             else
               $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job ${combinefields}
             fi
           fi
         fi
       done
     if [ -s $DATA/nos.${OFS}.avg.forecast.$PDY.t${cyc}z.nc ]; then
       for combinefields in `ls $COMOUT/nos.${OFS}.avg.f*.nc`
       do
         if [ -f ${combinefields} ]
         then
           if [ $SENDDBN = YES ]; then
             if [ "${OFS,,}" == "gomofs" -o "${OFS,,}" == "wcofs" -o "${OFS,,}" == "ciofs" ]; then
               $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF_LRG $job ${combinefields}
             else
               $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job ${combinefields}
             fi
           fi
         fi
       done
     fi
#    else   
       if [ -f $COMOUT/$HIS_OUT_FORECAST ]
       then
         if [ $SENDDBN = YES ]; then
           $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF_LRG $job $COMOUT/${HIS_OUT_FORECAST}
         fi
       else  
         if [ -f $DATA/$HIS_OUT_FORECAST ]
         then
           cp $DATA/$HIS_OUT_FORECAST $COMOUT/$HIS_OUT_FORECAST
           if [ $SENDDBN = YES ]; then
             $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF_LRG $job $COMOUT/${HIS_OUT_FORECAST}
           fi
           export err=$?; err_chk
           echo "   $HIS_OUT_FORECAST saved "
         fi
       fi 
#    fi

# 2.3 STA forecast
    if [ -f $COMOUT/$STA_OUT_FORECAST ]
    then
      if [ $SENDDBN = YES ]; then
        $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job $COMOUT/${STA_OUT_FORECAST}
      fi
    else  
      if [ -f $DATA/$STA_OUT_FORECAST ]
      then
        cp $DATA/$STA_OUT_FORECAST $COMOUT/$STA_OUT_FORECAST
        export err=$?; err_chk
        echo "   $STA_OUT_FORECAST saved "
        if [ $SENDDBN = YES ]; then
          $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job $COMOUT/${STA_OUT_FORECAST}
        fi
      fi	 
    fi
   
# 2.4 Model runtime control file for forecast
    if [ -f  $COMOUT/$RUNTIME_CTL_FORECAST ]
    then
      if [ $SENDDBN = YES ]; then
        $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_TEXT $job  $COMOUT/${RUNTIME_CTL_FORECAST}
      fi
    else
      if [ -f  $DATA/$RUNTIME_CTL_FORECAST ]
      then
        cp $DATA/$RUNTIME_CTL_FORECAST $COMOUT/$RUNTIME_CTL_FORECAST
        if [ $SENDDBN = YES ]; then
          $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_TEXT $job  $COMOUT/${RUNTIME_CTL_FORECAST}
        fi
      fi	
    fi
 #   if [ $SENDDBN = YES ]; then
 #       $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_TEXT $job  $cormslogfile
 #   fi
    echo "ARCHIVE_FORECAST DONE 100" >> $cormslogfile

# 2.5 Met forcing file for forecast
    if [ -f $COMOUT/$MET_NETCDF_1_FORECAST ]
    then
      if [ $SENDDBN = YES ]; then
         $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job $COMOUT/${MET_NETCDF_1_FORECAST}
      fi
    else  
      if [ -f $DATA/$MET_NETCDF_1_FORECAST ]
      then
        cp -p $MET_NETCDF_1_FORECAST $COMOUT/$MET_NETCDF_1_FORECAST
        if [ $SENDDBN = YES ]; then
           $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job $COMOUT/${MET_NETCDF_1_FORECAST}
        fi
      fi
    fi
# 2.6 Met heat flux forcing file for forecast
    if [ -f $COMOUT/$MET_NETCDF_2_FORECAST ]
    then
      if [ $SENDDBN = YES ]; then
         $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job $COMOUT/${MET_NETCDF_2_FORECAST}
      fi
    else  
      if [ -f $DATA/$MET_NETCDF_2_FORECAST ]
      then
        cp -p $MET_NETCDF_2_FORECAST $COMOUT/$MET_NETCDF_2_FORECAST
        if [ $SENDDBN = YES ]; then
         $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_NETCDF $job $COMOUT/${MET_NETCDF_2_FORECAST}
        fi
      fi
    fi

# 2.7. Save CORMSLOG file
    if [ -s $cormslogfile ]; then
      if [ $SENDDBN = YES ]; then
        $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_TEXT $job $cormslogfile
      fi
    fi
# 2.8. Save jlogjile
#    if [ -s $jlogfile ]; then
#      cp -p $DATA/$jlogfile $COMOUT/$jlogfile
#      if [ $SENDDBN = YES ]; then
#        $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_TEXT $job $jlogfile
#      fi
#    else
#      if [ -s $DATA/$jlogfile ]; then
#        cp $DATA/$jlogfile $COMOUT/$jlogfile
#      fi	
#      if [ $SENDDBN = YES ]; then
#        $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_TEXT $job $COMOUT/$jlogfile
#      fi
#    fi
    if [ -s $nosjlogfile ]; then
      if [ $SENDDBN = YES ]; then
        $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_TEXT $job $nosjlogfile
      fi
    fi

# 2.9. Save status file
    if [ -s $COMOUT/${OFS}.status ]; then
      if [ $SENDDBN = YES ]; then
        $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_TEXT $job $COMOUT/${OFS}.status
      fi
    else
      if [ -s $DATA/${OFS}.status ]; then
        cp $DATA/${OFS}.status $COMOUT/${OFS}.status
        if [ $SENDDBN = YES ]; then
          $DBNROOT/bin/dbn_alert MODEL $DBN_ALERT_TYPE_TEXT $job $COMOUT/${OFS}.status
        fi
      fi	
    fi
fi  

# --------------------------------------------------------------------------- #
# 4.  Ending output

  set $setoff
  echo ' '
  echo "Ending nos_ofs_archive.sh at : `date`"
  echo ' '
  echo '                     *** End of NOS OFS ARCHIVE SCRIPT ***'
  echo ' '
