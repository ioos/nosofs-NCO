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

#export MP_PGMMODEL=mpmd
#export MP_CMDFILE=cmdfile

RUNTYPE=forecast
RUN=ngofs
NET=nos
PDY=20191212
PDY1=20191212
HH='03'

STA_OUT_FORECAST=${NET}.${RUN}.stations.forecast.$PDY1.t${HH}z.nc
###############################################################################
###############################################################################
#### FORECAST
###############################################################################
###############################################################################
 
# --------------------------------------------------------------------------- #
# FVCOM


       export $time_nowcastend = 2019121409
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
        NC_OUT_INTERVAL='3600.0'
        NCSF_OUT_INTERVAL='3600.0'

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
    if [ ${OFS} == "NGOFS" -o ${OFS} == "ngofs" ]; then
       if [ -s nos_${RUN}_nestnode_negofs.nc ]; then
          cp -p nos_${RUN}_nestnode_negofs.nc $COMOUT/nos.${RUN}.nestnode.negofs.forecast.$PDY.t${cyc}z.nc
       fi
       if [ -s nos_${RUN}_nestnode_nwgofs.nc ]; then
          cp -p nos_${RUN}_nestnode_nwgofs.nc $COMOUT/nos.${RUN}.nestnode.nwgofs.forecast.$PDY.t${cyc}z.nc
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

