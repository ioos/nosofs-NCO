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
#     nos.${RUN}.hflux.nowcast.$yyyy$mm$dd.t${cyc}z.nc
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
#  Control Files For Model Run
if [ -s ${FIXofs}/${NET}.${RUN}.ctl ]
then
  . ${FIXofs}/${NET}.${RUN}.ctl
else
  echo "${RUN} control file is not found"
  echo "please provide  ${RUN} control file of ${NET}.${RUN}.ctl in ${FIXofs}"
  msg="${RUN} control file is not found"
  postmsg "$jlogfile" "$msg"
  postmsg "$nosjlogfile" "$msg"
  echo "${RUN} control file is not found"  >> $cormslogfile
  err_chk
fi
set -xa
#cd $DATA
cd $COMOUT
echo ' '
echo '  		    ****************************************'
echo '  		    *** NOS OFS  SFTP SCRIPT  ***        '
echo '  		    ****************************************'
echo ' '
echo "Starting nos_ofs_ftp.sms.sh at : `date`"
#RUNTYPE=$1
#export MP_PGMMODEL=mpmd
#export MP_CMDFILE=cmdfile
###############################################################################

## -- End of prep Only --------------------------------'
export OBC_FORCING_FILE=${NET}.${RUN}.obc.$PDY.t${cyc}z.nc
export OBC_FORCING_FILE_EL=${NET}.${RUN}.obc.el.$PDY.t${cyc}z.nc
export OBC_FORCING_FILE_TS=${NET}.${RUN}.obc.ts.$PDY.t${cyc}z.nc
export RIVER_FORCING_FILE=${NET}.${RUN}.river.$PDY.t${cyc}z.nc
export INI_FILE_NOWCAST=${NET}.${RUN}.init.nowcast.$PDY.t${cyc}z.nc
export HIS_OUT_NOWCAST=${NET}.${RUN}.fields.nowcast.$PDY.t${cyc}z.nc
export AVG_OUT_NOWCAST=${NET}.${RUN}.avg.nowcast.$PDY.t${cyc}z.nc
export STA_OUT_NOWCAST=${NET}.${RUN}.stations.nowcast.$PDY.t${cyc}z.nc
export RST_OUT_NOWCAST=${NET}.${RUN}.rst.nowcast.$PDY.t${cyc}z.nc
export MET_NETCDF_1_NOWCAST=${NET}.${RUN}.met.nowcast.$PDY.t${cyc}z.nc
export MET_NETCDF_2_NOWCAST=${NET}.${RUN}.hflux.nowcast.$PDY.t${cyc}z.nc
#export OBC_TIDALFORCING_FILE==${NET}.${RUN}.roms.tides.nc
export OBC_TIDALFORCING_FILE=${NET}.${RUN}.roms.tides.$PDY.t${cyc}z.nc
export OBC_FORCING_FILE_NWGOFS_NOW=${NET}.${RUN}.nestnode.nwgofs.nowcast.$PDY.t${cyc}z.nc
export OBC_FORCING_FILE_NEGOFS_NOW=${NET}.${RUN}.nestnode.negofs.nowcast.$PDY.t${cyc}z.nc
export OBC_FORCING_FILE_NWGOFS_FOR=${NET}.${RUN}.nestnode.nwgofs.forecast.$PDY.t${cyc}z.nc
export OBC_FORCING_FILE_NEGOFS_FOR=${NET}.${RUN}.nestnode.negofs.forecast.$PDY.t${cyc}z.nc

#export INI_FILE_FORECAST=$RST_OUT_NOWCAST
export HIS_OUT_FORECAST=${NET}.${RUN}.fields.forecast.$PDY.t${cyc}z.nc
export STA_OUT_FORECAST=${NET}.${RUN}.stations.forecast.$PDY.t${cyc}z.nc
export AVG_OUT_FORECAST=${NET}.${RUN}.avg.forecast.$PDY.t${cyc}z.nc
export RST_OUT_FORECAST=${NET}.${RUN}.rst.forecast.$PDY.t${cyc}z.nc
export MET_NETCDF_1_FORECAST=${NET}.${RUN}.met.forecast.$PDY.t${cyc}z.nc
export MET_NETCDF_2_FORECAST=${NET}.${RUN}.hflux.forecast.$PDY.t${cyc}z.nc
export MODEL_LOG_NOWCAST=${NET}.${RUN}.nowcast.$PDY.t${cyc}z.log
export MODEL_LOG_FORECAST=${NET}.${RUN}.forecast.$PDY.t${cyc}z.log
export RUNTIME_CTL_NOWCAST=${NET}.${RUN}.nowcast.$PDY.t${cyc}z.in
export RUNTIME_CTL_FORECAST=${NET}.${RUN}.forecast.$PDY.t${cyc}z.in
export NUDG_FORCING_FILE=${NET}.${RUN}.clim.$PDY.t${cyc}z.nc
if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
then
  export MET_NETCDF_1_NOWCAST=${NET}.${RUN}.met.nowcast.$PDY.t${cyc}z.nc.tar
  export MET_NETCDF_1_FORECAST=${NET}.${RUN}.met.forecast.$PDY.t${cyc}z.nc.tar
  export OBC_FORCING_FILE=${NET}.${RUN}.obc.$PDY.t${cyc}z.tar
  export OBC_FORCING_FILE_EL=${NET}.${RUN}.obc.el.$PDY.t${cyc}z.tar
  export OBC_FORCING_FILE_TS=${NET}.${RUN}.obc.ts.$PDY.t${cyc}z.tar
  export RIVER_FORCING_FILE=${NET}.${RUN}.river.$PDY.t${cyc}z.th.tar
  export INI_FILE_NOWCAST=${NET}.${RUN}.init.nowcast.$PDY.t${cyc}z.bin
  export RST_OUT_NOWCAST=${NET}.${RUN}.rst.nowcast.$PDY.t${cyc}z.bin
  export RST_OUT_FORECAST=${NET}.${RUN}.rst.forecast.$PDY.t${cyc}z.bin
#  export INI_FILE_FORECAST=$RST_OUT_NOWCAST
  export RUNTIME_MET_CTL_NOWCAST=${NET}.${RUN}.met_ctl.nowcast.$PDY.t${cyc}z.in
  export RUNTIME_MET_CTL_FORECAST=${NET}.${RUN}.met_ctl.forecast.$PDY.t${cyc}z.in
  export RUNTIME_COMBINE_RST_NOWCAST=${NET}.${RUN}.combine.hotstart.nowcast.$PDY.t${cyc}z.in
  export RUNTIME_COMBINE_NETCDF_NOWCAST=${NET}.${RUN}.combine.netcdf.nowcast.$PDY.t${cyc}z.in
  export RUNTIME_COMBINE_NETCDF_FORECAST=${NET}.${RUN}.combine.netcdf.forecast.$PDY.t${cyc}z.in
  export RUNTIME_COMBINE_NETCDF_STA_NOWCAST=${NET}.${RUN}.combine.netcdf.sta.nowcast.$PDY.t${cyc}z.in
  export RUNTIME_COMBINE_NETCDF_STA_FORECAST=${NET}.${RUN}.combine.netcdf.sta.forecast.$PDY.t${cyc}z.in

elif [ ${OCEAN_MODEL} == "FVCOM" -o ${OCEAN_MODEL} == "fvcom" ]
then
  export RIVER_FORCING_FILE=${NET}.${RUN}.river.$PDY.t${cyc}z.nc.tar
fi
Len_waitingtime=7200  # Maximum waiting time in seconds
waitingtime=0

#until [  -s nos.${OFS}.fields.f047.$PDY.t${cyc}z.nc ]
until [ -s ${RUN}.status ]
do
     echo 'field forecast NetCDF file of forecast hour 48 is not available yet, waiting ...'
     echo 'script nos_ofs_sftp.sh is sleeping at time : ' ` date `
     sleep 60
     waitingtime=`expr $waitingtime + 60`
     if [  $waitingtime -ge $Len_waitingtime ]
     then
         echo "waiting time exceeds $Len_waitingtime seconds"
         echo "system exit"
         exit
     fi
done



if [ -f $DATA/ftp_nowcast.txt ]; then
      rm -f  $DATA/ftp_nowcast.txt  
fi
echo " binary "   > $DATA/ftp_nowcast.txt
echo " cd $FTPDIR "  >> $DATA/ftp_nowcast.txt

# 1  push nowcast output to CO-OPS tidepool 
# 1.1 Nowcast log 
if [ -f ${MODEL_LOG_NOWCAST} ]
then
  echo " put ${MODEL_LOG_NOWCAST} " >> $DATA/ftp_nowcast.txt 
fi
# 1.2 STA nowcast
if [ -f $STA_OUT_NOWCAST ]
then
  echo " put $STA_OUT_NOWCAST " >> $DATA/ftp_nowcast.txt
fi
# 1.3 HIS nowcast 
#    if [ ${OCEAN_MODEL} == "SELFE" -o ${OCEAN_MODEL} == "selfe" ]
#    then 
nfile_2d=`ls nos.${OFS}.2ds.n*t${cyc}z.nc |wc -l`
if [ $nfile_2d -ge 1 ]; then
  for combinefields in `ls nos.${OFS}.2ds.n*t${cyc}z.nc`
  do
    if [ -f ${combinefields} ];  then
      echo " put ${combinefields}" >> $DATA/ftp_nowcast.txt
    fi
  done
else
  for combinefields in `ls nos.${OFS}.fields.n*t${cyc}z.nc`
  do
    if [ -f ${combinefields} ];  then
      echo " put ${combinefields}" >> $DATA/ftp_nowcast.txt
    fi
  done
fi

#if [ ${OFS} != "GOMOFS" -a ${OFS} != "gomofs" ]; then

#  for combinefields in `ls nos.${OFS}.fields.n*t${cyc}z.nc`
#  do
#    if [ -f ${combinefields} ];  then
#      echo " put ${combinefields}" >> $DATA/ftp_nowcast.txt 
#    fi
#  done
#else
#    I=0
#    while (( I < 25 ))
#    do
#       fhr4=`echo $I |  awk '{printf("%04i",$1)}'`
#       fhr3=`echo $I |  awk '{printf("%03i",$1)}'`
#       fileout=nos.${RUN}.fields.n${fhr3}.$PDY.t${cyc}z.nc
#       if [ -s $fileout ]; then
#          echo " put ${fileout}" >> $DATA/ftp_nowcast.txt
#       fi
#       (( I = I + 1 ))
#    done
#fi
if [ -f $HIS_OUT_NOWCAST ]
then
  echo " put $HIS_OUT_NOWCAST " >> $DATA/ftp_nowcast.txt 
fi
if [ -f $AVG_OUT_NOWCAST ]
then
  echo " put $AVG_OUT_NOWCAST " >> $DATA/ftp_nowcast.txt
fi

# 1.4 restart file for nowcast
if [ ${OFS} != "wcofs" -a ${OFS} != "gomofs" ]; then
  if [ ${cyc} = "00" -o ${cyc} = "03" ]; then
     if [ -f $INI_FILE_NOWCAST ]; then
       echo " put $INI_FILE_NOWCAST " >> $DATA/ftp_nowcast.txt
     fi
  fi
fi

# 1.4 RST nowcast
 #   if [ -f $RST_OUT_NOWCAST ]
 #   then
 #     echo " put $RST_OUT_NOWCAST " >> $DATA/ftp_nowcast.txt
 #     echo "   $RST_OUT_NOWCAST pushed to CO-OPS "
 #   fi
# 1.5 OBC Forcing 
if [ -f $OBC_FORCING_FILE ]
then
  echo " put $OBC_FORCING_FILE " >> $DATA/ftp_nowcast.txt
fi
if [ -f $OBC_FORCING_FILE_EL ]
then
  echo " put $OBC_FORCING_FILE_EL " >> $DATA/ftp_nowcast.txt
  echo "   $OBC_FORCING_FILE_EL pushed to CO-OPS "
fi
if [ -f $OBC_FORCING_FILE_TS ]
then
  echo " put $OBC_FORCING_FILE_TS " >> $DATA/ftp_nowcast.txt
  echo "   $OBC_FORCING_FILE_TS pushed to CO-OPS "
fi
if [ -f $NUDG_FORCING_FILE ]; then
  echo " put $NUDG_FORCING_FILE " >> $DATA/ftp_nowcast.txt
fi

#if [ -f $OBC_TIDALFORCING_FILE ]
#then
#  echo " put $OBC_TIDALFORCING_FILE " >> $DATA/ftp_nowcast.txt
#  echo "   $OBC_TIDALFORCING_FILE pushed to CO-OPS "
#fi
# 1.6 River Forcing 
if [ -f $RIVER_FORCING_FILE ]
then
   echo " put $RIVER_FORCING_FILE " >> $DATA/ftp_nowcast.txt
   echo "   $RIVER_FORCING_FILE pushed to CO-OPS "
fi
# 1.7 Surface Forcing 
if [ -f $MET_NETCDF_1_NOWCAST ]
then
  echo " put $MET_NETCDF_1_NOWCAST " >> $DATA/ftp_nowcast.txt
  echo "   $MET_NETCDF_1_NOWCAST pushed to CO-OPS "
fi
# 1.8 Surface Forcing 2
if [ -f $MET_NETCDF_2_NOWCAST ]
then
  echo " put $MET_NETCDF_2_NOWCAST " >> $DATA/ftp_nowcast.txt
  echo "   $MET_NETCDF_2_NOWCAST pushed to CO-OPS "
fi
# 1.9 Model runtime control file for nowcast
if [ -f $RUNTIME_CTL_NOWCAST ]
then
  echo " put $RUNTIME_CTL_NOWCAST " >> $DATA/ftp_nowcast.txt
fi

if [ -f $OBC_FORCING_FILE_NWGOFS_NOW ]; then
   echo " put $OBC_FORCING_FILE_NWGOFS_NOW " >> $DATA/ftp_nowcast.txt
fi

if [ -f $OBC_FORCING_FILE_NEGOFS_NOW ]; then
   echo " put $OBC_FORCING_FILE_NEGOFS_NOW " >> $DATA/ftp_nowcast.txt
fi
if [ -f $OBC_FORCING_FILE_NWGOFS_FOR ]; then
   echo " put $OBC_FORCING_FILE_NWGOFS_FOR " >> $DATA/ftp_nowcast.txt
fi
if [ -f $OBC_FORCING_FILE_NEGOFS_FOR ]; then
   echo " put $OBC_FORCING_FILE_NEGOFS_FOR " >> $DATA/ftp_nowcast.txt
fi



# --------------------------------------------------------------------------- #
# 2  Save forecast output 
#if [ $RUNTYPE == "FORECAST" -o $RUNTYPE == "forecast" ]
#then
# 2.1 forecast log 
if [ -f ${MODEL_LOG_FORECAST} ]
then
  echo " put ${MODEL_LOG_FORECAST} " >> $DATA/ftp_nowcast.txt 
fi
# 2.2 STA FORECAST
if [ -f $STA_OUT_FORECAST ]
then
  echo " put $STA_OUT_FORECAST" >> $DATA/ftp_nowcast.txt
fi
# 2.3 HIS FORECAST 
#for combinefields in `ls nos.${OFS}.fields.f*t${cyc}z.nc`
#do
#  if [ -f ${combinefields} ]
#  then
#    echo " put ${combinefields}" >> $DATA/ftp_nowcast.txt
#  fi
#done

nfile=`ls nos.${OFS}.2ds.f*t${cyc}z.nc |wc -l`
if [ $nfile -ge 1 ]; then
  I=0
  while (( I < 49))
  do
    fhr4=`echo $I |  awk '{printf("%04i",$1)}'`
    fhr3=`echo $I |  awk '{printf("%03i",$1)}'`
    fileout=nos.${OFS}.2ds.f${fhr3}.$PDY.t${cyc}z.nc
    if [ -s $fileout ]; then
      echo " put ${fileout}" >> $DATA/ftp_nowcast.txt
    fi
    (( I = I + 1 ))
  done

else
  I=0
  while (( I < 49))
  do
    fhr4=`echo $I |  awk '{printf("%04i",$1)}'`
    fhr3=`echo $I |  awk '{printf("%03i",$1)}'`
    fileout=nos.${RUN}.fields.f${fhr3}.$PDY.t${cyc}z.nc
    if [ -s $fileout ]; then
      echo " put ${fileout}" >> $DATA/ftp_nowcast.txt
    fi
    (( I = I + 1 ))
  done

#  for combinefields in `ls nos.${OFS}.fields.f*t${cyc}z.nc`
#  do
#    if [ -f ${combinefields} ]; then
#      echo " put ${combinefields}" >> $DATA/ftp_nowcast.txt
#    fi
#  done
fi
# 2.10 nowcast 3-d files for archive if 2d files are used
#if [ $nfile_2d -ge 1 ]; then
#  for combinefields in `ls nos.${OFS}.fields.n*t${cyc}z.nc`
#  do
#    if [ -f ${combinefields} ];  then
#      echo " put ${combinefields}" >> $DATA/ftp_nowcast.txt
#    fi
#  done
#  for combinefields in `ls nos.${OFS}.fields.f*t${cyc}z.nc`
#  do
#    if [ -f ${combinefields} ];  then
#      echo " put ${combinefields}" >> $DATA/ftp_nowcast.txt
#    fi
#  done
#fi
#I=0
#while (( I < 73))
#do
#  fhr4=`echo $I |  awk '{printf("%04i",$1)}'`
#  fhr3=`echo $I |  awk '{printf("%03i",$1)}'`
#  fileout=nos.${RUN}.fields.f${fhr3}.$PDY.t${cyc}z.nc
#  if [ -s $fileout ]; then
#    echo " put ${fileout}" >> $DATA/ftp_nowcast.txt
#  fi
#  (( I = I + 1 ))
#done

if [ -f $HIS_OUT_FORECAST ]
then
  echo " put $HIS_OUT_FORECAST" >> $DATA/ftp_nowcast.txt 
fi
if [ -f $AVG_OUT_FORECAST ]
then
  echo " put $AVG_OUT_FORECAST" >> $DATA/ftp_nowcast.txt
fi

# 2.4 Surface Forcing 
if [ -f $MET_NETCDF_1_FORECAST ]
then
  echo " put $MET_NETCDF_1_FORECAST" >> $DATA/ftp_nowcast.txt
fi
# 2.5 Surface Forcing 2
if [ -f $MET_NETCDF_2_FORECAST ]
then
  echo " put $MET_NETCDF_2_FORECAST " >> $DATA/ftp_nowcast.txt
fi
# 2.6 Mdel runtime control file for FORECAST
if [ -f $RUNTIME_CTL_FORECAST ]
then
  echo " put $RUNTIME_CTL_FORECAST " >> $DATA/ftp_nowcast.txt
fi
# 2.7 CORMS FLAG file for forecast
if [ -f ${NET}.${RUN}.corms.${PDY}.${cycle}.log ]
then
  echo " put ${NET}.${RUN}.corms.${PDY}.${cycle}.log " >> $DATA/ftp_nowcast.txt
fi
# 2.8 jlog file for nowcast and forecast
if [ -f ${NET}.${RUN}.jlogfile.${PDY}.${cycle}.log ]
then
  echo " put ${NET}.${RUN}.jlogfile.${PDY}.${cycle}.log " >> $DATA/ftp_nowcast.txt
fi

if [ -f ${NET}.${RUN}.jlog.${PDY}.${cycle}.log ]
then
  echo " put ${NET}.${RUN}.jlog.${PDY}.${cycle}.log " >> $DATA/ftp_nowcast.txt
fi
if [ -f  ${NET}.${RUN}.avg.${PDY}.${cycle}.nc ];  then
   echo " put ${NET}.${RUN}.avg.${PDY}.${cycle}.nc " >> $DATA/ftp_nowcast.txt
fi


# 2.9 OFS status file for nowcast and forecast
if [ -f ${RUN}.status ]
then
  echo " put  ${RUN}.status  " >> $DATA/ftp_nowcast.txt
fi

### still transfer 3D field outputs after ftp ${RUN}.status
nfile=`ls nos.${OFS}.fields.f*t${cyc}z.nc |wc -l`
if [ $nfile -ge 1 ]; then
  I=0
  while (( I < 49))
  do
    fhr4=`echo $I |  awk '{printf("%04i",$1)}'`
    fhr3=`echo $I |  awk '{printf("%03i",$1)}'`
    fileout=nos.${OFS}.fields.n${fhr3}.$PDY.t${cyc}z.nc
    if [ -s $fileout ]; then
      echo " put ${fileout}" >> $DATA/ftp_nowcast.txt
    fi
    (( I = I + 1 ))
  done
  I=0
  while (( I < 24))
  do
    fhr4=`echo $I |  awk '{printf("%04i",$1)}'`
    fhr3=`echo $I |  awk '{printf("%03i",$1)}'`
    fileout=nos.${RUN}.fields.f${fhr3}.$PDY.t${cyc}z.nc
    if [ -s $fileout ]; then
      echo " put ${fileout}" >> $DATA/ftp_nowcast.txt
    fi
    (( I = I + 1 ))
  done
fi
echo " bye " >> $DATA/ftp_nowcast.txt
cp -p $DATA/ftp_nowcast.txt ${COMOUT}/nos.${OFS}.ftp.${cyc}.ctl

ftp -v tidepool.nos.noaa.gov < $DATA/ftp_nowcast.txt

export err=$?
if [ $err -ne 0 ]
then
  echo "ftp tidepool.nos.noaa.gov did not complete normally"
  msg="ftp tidepool.nos.noaa.gov did not complete normally"
 # postmsg "$jlogfile" "$msg"
  echo "SFTP_FORECAST DONE 0" >> $cormslogfile
else
  echo "ftp tidepool.nos.noaa.gov completed normally"
  msg="ftp tidepool.nos.noaa.gov completed normally"
  #postmsg "$jlogfile" "$msg"
  echo "SFTP_FORECAST DONE 100" >> $cormslogfile
fi
# --------------------------------------------------------------------------- #
# 4.  Ending output

  echo ' '
  echo "Ending exnos_ofs_ftp.sms.sh at : `date`"
  echo ' '
  echo '                     *** End of NOS OFS SFTP SCRIPT ***'
  echo ' '
