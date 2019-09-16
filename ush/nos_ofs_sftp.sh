#!/bin/sh
#  Script Name:  exnos_ofs_ftp.sms.sh
#  Purpose:                                                                   #
#  This script is to make copy model files to corresonding directories after  #
#  successfully completing nowcast and forecast simulations by running:       #
#  exnos_ofs_nowcast_forecast.sms.prod                                        #
#                                                                             #
#  Child scripts :                                                            #
#                                                                             #
#  The utililty script used:                                                  #
#                                                                             #
# Remarks :                                                                   #
# - For non-fatal errors outecho "put  is written to the *.log file.                 #
#                                                                             #
# Language:  C shell script
# Nowcast  
# Inecho "put :
#     nos.${RUN}.river.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${RUN}.obc.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${RUN}.met.nowcast.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${RUN}.init.nowcast.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${RUN}.hflux.nowcast.$yyyy$mm$dd.t${HH}z.nc
#     ${RUN}_roms_nowcast.in
#     nos.${RUN}.roms.tides.nc
# Outecho "put :
#     nos.${RUN}.stations.nowcast.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${RUN}.fields.nowcast.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${RUN}.fields.forecast.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${RUN}.rst.nowcast.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${RUN}.roms.nowcast.$yyyy$mm$dd.t${cyc}z.log
# Forecast  
# Inecho "put :
#     nos.${RUN}.river.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${RUN}.obc.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${RUN}.met.forecast.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${RUN}.rst.nowcast.$yyyy$mm$dd.t${cyc}z.nc
#     ${RUN}_roms_forecast.in
#     nos.${RUN}.roms.tides.nc
# Outecho "put :
#     nos.${RUN}.stations.forecast.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${RUN}.fields.forecast.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${RUN}.rst.forecast.$yyyy$mm$dd.t${cyc}z.nc
#     nos.${RUN}.roms.forecast.$yyyy$mm$dd.t${cyc}z.log
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
set -xa
cd $DATA
#cd $COMOUT
echo ' '
echo '  		    ****************************************'
echo '  		    *** NOS OFS  SFTP SCRIPT  ***        '
echo '  		    ****************************************'
echo ' '
echo "Starting nos_ofs_ftp.sms.sh at : `date`"
RUNTYPE=$1
#export MP_PGMMODEL=mpmd
#export MP_CMDFILE=cmdfile
###############################################################################

if [ $RUNTYPE == "NOWCAST" -o $RUNTYPE == "nowcast" ]
then
    if [ -f ftp_nowcast.txt ]
    then
      rm -f  ftp_nowcast.txt  
    fi
    echo " binary "   > ftp_nowcast.txt
    echo " cd $FTPDIR "  >> ftp_nowcast.txt

# 1  push nowcast output to CO-OPS tidepool 
# 1.1 Nowcast log 
    if [ -f ${MODEL_LOG_NOWCAST} ]
    then
      echo " put ${MODEL_LOG_NOWCAST} " >> ftp_nowcast.txt 
    else
      if [ -f $COMOUT/${MODEL_LOG_NOWCAST} ]
      then
       cp -p $COMOUT/${MODEL_LOG_NOWCAST}  $DATA/.
      fi  
      echo " put ${MODEL_LOG_NOWCAST} " >> ftp_nowcast.txt 
    fi

# 1.2 HIS nowcast 
#    if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
#    then 
       for combinefields in `ls nos.${OFS}.fields.n*.nc`
       do
         if [ -f ${combinefields} ]
         then
           echo " put ${combinefields}" >> ftp_nowcast.txt 
         fi
       done
#    else 
       if [ -f $HIS_OUT_NOWCAST ]
       then
         echo " put $HIS_OUT_NOWCAST " >> ftp_nowcast.txt 
       else  
         if [ -f $COMOUT/$HIS_OUT_NOWCAST ]
         then
           cp -p $COMOUT/$HIS_OUT_NOWCAST  $DATA/.
         fi	
         echo " put $HIS_OUT_NOWCAST " >> ftp_nowcast.txt 
       fi
#    fi

# 1.3 STA nowcast
    if [ -f $STA_OUT_NOWCAST ]
    then
      echo " put $STA_OUT_NOWCAST " >> ftp_nowcast.txt
      echo "   $STA_OUT_NOWCAST pushed to CO-OPS "
    else
      if [ -f $COMOUT/$STA_OUT_NOWCAST ]
      then
        cp -p $COMOUT/$STA_OUT_NOWCAST  $DATA/.
      fi	
      echo " put $STA_OUT_NOWCAST " >> ftp_nowcast.txt
    fi
# 1.4 restart file for nowcast
    if [ -f $INI_FILE_NOWCAST ]
    then
      echo " put $INI_FILE_NOWCAST " >> ftp_nowcast.txt
      echo "   $INI_FILE_NOWCAST pushed to CO-OPS "
    else
      if [ -f $COMOUT/$INI_FILE_NOWCAST ]
      then
        cp -p $COMOUT/$INI_FILE_NOWCAST $DATA/.
      fi	
      echo " put $INI_FILE_NOWCAST " >> ftp_nowcast.txt
      
    fi

# 1.4 RST nowcast
 #   if [ -f $RST_OUT_NOWCAST ]
 #   then
 #     echo " put $RST_OUT_NOWCAST " >> ftp_nowcast.txt
 #     echo "   $RST_OUT_NOWCAST pushed to CO-OPS "
 #   fi
# 1.5 OBC Forcing 
    if [ -f $OBC_FORCING_FILE ]
    then
      echo " put $OBC_FORCING_FILE " >> ftp_nowcast.txt
      echo "   $OBC_FORCING_FILE pushed to CO-OPS "
    else
      if [ -f $COMOUT/$OBC_FORCING_FILE ]
      then
        cp -p $COMOUT/$OBC_FORCING_FILE $DATA/.
      fi	
      echo " put $OBC_FORCING_FILE " >> ftp_nowcast.txt
    fi
    if [ -f $OBC_FORCING_FILE_EL ]
    then
      echo " put $OBC_FORCING_FILE_EL " >> ftp_nowcast.txt
      echo "   $OBC_FORCING_FILE_EL pushed to CO-OPS "
    else
      if [ -f $COMOUT/$OBC_FORCING_FILE_EL ]
      then
        cp -p $COMOUT/$OBC_FORCING_FILE_EL $DATA/.
      fi	
      echo " put $OBC_FORCING_FILE_EL " >> ftp_nowcast.txt
    fi
    if [ -f $OBC_FORCING_FILE_TS ]
    then
      echo " put $OBC_FORCING_FILE_TS " >> ftp_nowcast.txt
      echo "   $OBC_FORCING_FILE_TS pushed to CO-OPS "
    else
      if [ -f $COMOUT/$OBC_FORCING_FILE_TS ]
      then
        cp -p $COMOUT/$OBC_FORCING_FILE_TS $DATA/.
      fi	
      echo " put $OBC_FORCING_FILE_TS " >> ftp_nowcast.txt
    fi
# 1.6 River Forcing 
    if [ -f $RIVER_FORCING_FILE ]
    then
       echo " put $RIVER_FORCING_FILE " >> ftp_nowcast.txt
       echo "   $RIVER_FORCING_FILE pushed to CO-OPS "
    else
      if [ -f $COMOUT/$RIVER_FORCING_FILE ]
      then
        cp -p $COMOUT/$RIVER_FORCING_FILE $DATA/.
      fi
       echo " put $RIVER_FORCING_FILE " >> ftp_nowcast.txt

    fi
# 1.7 Surface Forcing 
    if [ -f $MET_NETCDF_1_NOWCAST ]
    then
      echo " put $MET_NETCDF_1_NOWCAST " >> ftp_nowcast.txt
      echo "   $MET_NETCDF_1_NOWCAST pushed to CO-OPS "
    else
      if [ -f $COMOUT/$MET_NETCDF_1_NOWCAST ]
      then
        cp -p $COMOUT/$MET_NETCDF_1_NOWCAST $DATA/.
      fi
       echo " put $MET_NETCDF_1_NOWCAST " >> ftp_nowcast.txt

    fi
# 1.8 Surface Forcing 2
    if [ -f $MET_NETCDF_2_NOWCAST ]
    then
      echo " put $MET_NETCDF_2_NOWCAST " >> ftp_nowcast.txt
      echo "   $MET_NETCDF_2_NOWCAST pushed to CO-OPS "
    else
      if [ -f $COMOUT/$MET_NETCDF_2_NOWCAST ]
      then
        cp -p $COMOUT/$MET_NETCDF_2_NOWCAST $DATA/.
      fi
       echo " put $MET_NETCDF_2_NOWCAST " >> ftp_nowcast.txt

    fi
# 1.9 Model runtime control file for nowcast
    if [ -f $RUNTIME_CTL_NOWCAST ]
    then
      echo " put $RUNTIME_CTL_NOWCAST " >> ftp_nowcast.txt
    else
      if [ -f $COMOUT/$RUNTIME_CTL_NOWCAST ]; then
         cp -p $COMOUT/$RUNTIME_CTL_NOWCAST $DATA/.
      fi	 
      echo " put $RUNTIME_CTL_NOWCAST " >> ftp_forecast.txt
    fi
# 1.10 CORMS FLAG file for nowcast
#    if [ -f $CORMSLOG ]
#    then
#      echo " put $CORMSLOG " >> ftp_nowcast.txt
#    fi
    cp -p ftp_nowcast.txt ${COMOUT}/nos.${OFS}.ftp.${cyc}.ctl
    echo "bye" >> ftp_nowcast.txt
     cp -p ftp_nowcast.txt ${COMOUT}/ftp_nowcast.${cyc}
#    /usr/bin/ftp -v tidepool.nos.noaa.gov < ftp_nowcast.txt
    ftp -v tidepool.nos.noaa.gov < ftp_nowcast.txt
    export err=$?
    if [ $err -ne 0 ]
    then
      echo "ftp tidepool.nos.noaa.gov for $RUNTYPE did not complete normally"
      msg="ftp tidepool.nos.noaa.gov for $RUNTYPE did not complete normally"
      ./postmsg "$jlogfile" "$msg"
      echo "SFTP_NOWCAST DONE 0" >> $CORMSLOG
      echo "NCEP_PUSH DONE 0" >> $CORMSLOG
#      ${DATA}/err_chk
    else
      echo "ftp tidepool.nos.noaa.gov for $RUNTYPE completed normally"
      msg="ftp tidepool.nos.noaa.gov for $RUNTYPE  completed normally"
      ./postmsg "$jlogfile" "$msg"
      echo "SFTP_NOWCAST DONE 100" >> $CORMSLOG
      echo "NCEP_PUSH DONE 100" >> $CORMSLOG
    fi
    
fi
# --------------------------------------------------------------------------- #
# 2  Save forecast output 
if [ $RUNTYPE == "FORECAST" -o $RUNTYPE == "forecast" ]
then
    if [ -f ftp_forecast.txt ]
    then
      rm -f  ftp_forecast.txt  
    fi
    echo " binary "   > ftp_forecast.txt
    echo " cd $FTPDIR "  >> ftp_forecast.txt

# 2.1 forecast log 
    if [ -f ${MODEL_LOG_FORECAST} ]
    then
      echo " put ${MODEL_LOG_FORECAST} " >> ftp_forecast.txt 
    else
      if [ -f $COMOUT/${MODEL_LOG_FORECAST} ]
      then
        cp -p $COMOUT/${MODEL_LOG_FORECAST} $DATA/.
      fi	
      echo " put ${MODEL_LOG_FORECAST} " >> ftp_forecast.txt 
    fi

# 2.2 HIS FORECAST 
#    if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
#    then 
       for combinefields in `ls nos.${OFS}.fields.f*.nc`
       do
         if [ -f ${combinefields} ]
         then
           echo " put ${combinefields}" >> ftp_forecast.txt 
         fi
       done
#    else 
       if [ -f $HIS_OUT_FORECAST ]
       then
         echo " put $HIS_OUT_FORECAST" >> ftp_forecast.txt 
       else
         if [ -f $COMOUT/$HIS_OUT_FORECAST ]
         then
           cp -p $COMOUT/$HIS_OUT_FORECAST  $DATA/.
         fi	   
         echo " put $HIS_OUT_FORECAST" >> ftp_forecast.txt 
       fi
#    fi

# 2.3 STA FORECAST
    if [ -f $STA_OUT_FORECAST ]
    then
      echo " put $STA_OUT_FORECAST" >> ftp_forecast.txt
      echo "   $STA_OUT_FORECAST pushed to CO-OPS "
    else
     if [ -f $COMOUT/$STA_OUT_FORECAST ]
      then
       cp -p  $COMOUT/$STA_OUT_FORECAST $DATA/.
      fi 
      echo " put $STA_OUT_FORECAST" >> ftp_forecast.txt
    fi
# 2.4 Surface Forcing 
    if [ -f $MET_NETCDF_1_FORECAST ]
    then
      echo " put $MET_NETCDF_1_FORECAST" >> ftp_forecast.txt
      echo "   $MET_NETCDF_1_FORECAST pushed to CO-OPS "
    fi
# 2.5 Surface Forcing 2
    if [ -f $MET_NETCDF_2_FORECAST ]
    then
      echo " put $MET_NETCDF_2_FORECAST " >> ftp_forecast.txt
      echo "   $MET_NETCDF_2_FORECAST pushed to CO-OPS "
    fi
# 2.6 Mdel runtime control file for FORECAST
    if [ -f $RUNTIME_CTL_FORECAST ]
    then
      echo " put $RUNTIME_CTL_FORECAST " >> ftp_forecast.txt
    else
      if [ -f $COMOUT/$RUNTIME_CTL_FORECAST ]; then
         cp -p $COMOUT/$RUNTIME_CTL_FORECAST $DATA/.
      fi	 
      echo " put $RUNTIME_CTL_FORECAST " >> ftp_forecast.txt
    fi
# 2.7 CORMS FLAG file for forecast
#export CORMSLOG=${COMOUT}/${NET}.${RUN}.corms.${PDY}.${cycle}.log
    if [ -f $CORMSLOG ]
    then
      cp $CORMSLOG ${NET}.${RUN}.corms.${PDY}.${cycle}.log
      echo " put ${NET}.${RUN}.corms.${PDY}.${cycle}.log " >> ftp_forecast.txt
#    else
#      if [ -f $COMOUT/$CORMSLOG ]
#      then
#        cp -p $COMOUT/$CORMSLOG $DATA/.
#      fi	
#      echo " put $CORMSLOG " >> ftp_forecast.txt
        
    fi
# 2.8 jlog file for nowcast and forecast
    if [ -f $nosjlogfile ]
    then
      cp -p $nosjlogfile ${NET}.${RUN}.jlogfile.${PDY}.${cycle}.log
      echo " put ${NET}.${RUN}.jlogfile.${PDY}.${cycle}.log " >> ftp_forecast.txt
#    else
#      if [ -f $COMOUT/$jlogfile ]
#      then
#        cp -p $COMOUT/$jlogfile $DATA/.
#      fi	
#      echo " put $jlogfile " >> ftp_forecast.txt
    fi

    if [ -f $jlogfile ]
    then
      echo " put ${NET}.${RUN}.jlog.${PDY}.${cycle}.log " >> ftp_forecast.txt
#    else
#      if [ -f $COMOUT/$jlogfile ]
#      then
#        cp -p $COMOUT/$jlogfile $DATA/.
#      fi	
#      echo " put $jlogfile " >> ftp_forecast.txt
    fi
# 2.9 OFS status file for nowcast and forecast
    if [ -f ${RUN}.status ]
    then
      echo " put  ${RUN}.status  " >> ftp_forecast.txt
    else
      if [ -f $COMOUT/${RUN}.status ]
      then
        cp -p $COMOUT/${RUN}.status $DATA/.
      fi	
      echo " put  ${RUN}.status  " >> ftp_forecast.txt
    fi

    echo " bye " >> ftp_forecast.txt
    cp -p ftp_forecast.txt ${COMOUT}/ftp_forecast.${cyc}
    ftp -v tidepool.nos.noaa.gov < ftp_forecast.txt
    export err=$?
    if [ $err -ne 0 ]
    then
      echo "ftp tidepool.nos.noaa.gov for $RUNTYPE did not complete normally"
      msg="ftp tidepool.nos.noaa.gov for $RUNTYPE did not complete normally"
      ./postmsg "$jlogfile" "$msg"
#      ${DATA}/err_chk
      echo "SFTP_FORECAST DONE 0" >> $CORMSLOG
    else
      echo "ftp tidepool.nos.noaa.gov for $RUNTYPE completed normally"
      msg="ftp tidepool.nos.noaa.gov for $RUNTYPE  completed normally"
      ./postmsg "$jlogfile" "$msg"
      echo "SFTP_FORECAST DONE 100" >> $CORMSLOG
    fi
fi  
cat ${COMOUT}/nos.${OFS}.ftp.${cyc}.ctl ftp_forecast.txt > tmp.ctl
cp -p tmp.ctl ${COMOUT}/nos.${OFS}.ftp.${cyc}.ctl
# --------------------------------------------------------------------------- #
# 4.  Ending output

  echo ' '
  echo "Ending exnos_ofs_ftp.sms.sh at : `date`"
  echo ' '
  echo '                     *** End of NOS OFS SFTP SCRIPT ***'
  echo ' '

