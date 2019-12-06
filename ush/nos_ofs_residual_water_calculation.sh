#!/bin/sh
# Script Name:  nos_ofs_create_wl_residual.sh
set -x
echo "start nos_ofs_create_wl_residual.sh at time:" `date`
RUNTYPE=$1
echo $RUNTYPE ${DBASE}
ls -al *.${DBASE}
if [ -s PRATE.${DBASE} ]; then
   cp PRATE.${DBASE} EVP.${DBASE}
elif [ -s TMP.${DBASE} ]; then
   cp TMP.${DBASE} PRATE.${DBASE}
   cp TMP.${DBASE} EVP.${DBASE}
fi   
if [ $RUNTYPE == "NOWCAST" -o $RUNTYPE == "nowcast" ]; then
     cp -p $FIXofs/$RESIDUAL_CTL $DATA
     ncwa -x -v node -a node  $RST_FILE lake_average_over_node.nc
     model_average_zeta=$(ncdump -v zeta lake_average_over_node.nc | grep 'zeta =' | cut -f2- -d=   | awk '{print $1}')
     if [ -s nos.lmhofs.wl.calculation.ctl ]; then
        rm nos.lmhofs.wl.calculation.ctl
     fi
     echo $time_nowcastend >> nos.lmhofs.wl.calculation.ctl   ### $START_TIME
     echo $DCOMINports >> nos.lmhofs.wl.calculation.ctl   ### $NOSWLDIR
     echo $NOSBUFR  >>  nos.lmhofs.wl.calculation.ctl   ### $NOSBUFR
     echo ${COMOUTroot} >>  nos.lmhofs.wl.calculation.ctl   ### COMOUT00
     echo $RESIDUAL_CTL >>  nos.lmhofs.wl.calculation.ctl 
     echo 'nos_'$OFS'_residual.dat' >>  nos.lmhofs.wl.calculation.ctl  ###  this is the output file
     echo $model_average_zeta  >>  nos.lmhofs.wl.calculation.ctl
     $EXECnos/nos_ofs_residual_water_calculation < nos.lmhofs.wl.calculation.ctl > nos.lmhofs.wl.calculation.log
     export residual=$( < nos_lmhofs_residual.dat )
     cp nos_lmhofs_residual.dat nos_lmhofs_residual_nowcast.dat
     precip=$residual
elif [ $RUNTYPE == "FORECAST" -o $RUNTYPE == "forecast" ]; then
     echo 0.0 > nos_lmhofs_residual.dat
     cp nos_lmhofs_residual.dat nos_lmhofs_residual_forecast.dat
     precip=0.0
fi

if [ $RUNTYPE == "NOWCAST" -o $RUNTYPE == "nowcast" ]; then
     read precip < nos_lmhofs_residual.dat
 		if [ "$precip" \> 0 ]
		then

			awk -F','  -v OFS=','   '{
			if ($1 ~ /lon/)
  				{ print $0 }
			else
 			{ print $1,$2,'$precip'}
			}' PRATE.${DBASE} >  PRATE.${DBASE}.GOOD


			awk -F','  -v OFS=','   '{
			if ($1 ~ /lon/)
 			 { print $0 }
			else
			 { print $1,$2,'0'}
			}' EVP.${DBASE} >  EVP.${DBASE}.GOOD

		else
			awk -F','  -v OFS=','   '{
			if ($1 ~ /lon/)
  				{ print $0 }
			else
 			{ print $1,$2,'0'}
			}' PRATE.${DBASE} >  PRATE.${DBASE}.GOOD


			awk -F','  -v OFS=','   '{
			if ($1 ~ /lon/)
 			 { print $0 }
			else
			 { print $1,$2,'$precip'}
			}' EVP.${DBASE} >  EVP.${DBASE}.GOOD
		fi  ### if "$precip" \> 0

                cp PRATE.${DBASE}.GOOD PRATE.${DBASE}
                cp EVP.${DBASE}.GOOD EVP.${DBASE}
elif [ $RUNTYPE == "FORECAST" -o $RUNTYPE == "forecast" ]; then
		awk -F','  -v OFS=','   '{
		if ($1 ~ /lon/)
  		{ print $0 }
		else
 		{ print $1,$2,'0'}
		}' PRATE.${DBASE} >  PRATE.${DBASE}.GOOD


		awk -F','  -v OFS=','   '{
		if ($1 ~ /lon/)
  		{ print $0 }
		else
 		{ print $1,$2,'0'}
		}' EVP.${DBASE} >  EVP.${DBASE}.GOOD


        	cp PRATE.${DBASE}.GOOD PRATE.${DBASE}
        	cp EVP.${DBASE}.GOOD EVP.${DBASE}
fi  ### if nowcast or forecast

