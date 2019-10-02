#!/bin/sh
# #########################################################################
#  Script Name: exnos_ofs_prep.sh
#  Purpose:                                                                #
#  This is the main script is launch sripts to generating forcing files    #
# Location:   ~/jobs
# Technical Contact:    Aijun Zhang         Org:  NOS/CO-OPS
#                       Phone: 301-7132890 ext. 127
#                       E-Mail: aijun.zhang@noaa.gov
#
#  Usage: 
#
# Input Parameters:
#   OFS 
#
# Modification History:
#     Degui Cao     02/18/2010   
# #########################################################################

set -x
#PS4=" \${SECONDS} \${0##*/} L\${LINENO} + "

echo "Start ${RUN} Preparation " > $cormslogfile
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

export pgm="$USHnos/nos_ofs_launch.sh $OFS prep"
echo "run the launch script to set the NOS configuration"

. $USHnos/nos_ofs_launch.sh $OFS prep
export err=$?
if [ $err -ne 0 ]
then
   echo "Execution of $pgm did not complete normally, FATAL ERROR!"
   echo "Execution of $pgm did not complete normally, FATAL ERROR!" >> $cormslogfile 
   msg=" Execution of $pgm did not complete normally, FATAL ERROR!"
   postmsg "$jlogfile" "$msg"
   postmsg "$nosjlogfile" "$msg"
   err_chk
else
   echo "Execution of $pgm completed normally" >> $cormslogfile
   echo "Execution of $pgm completed normally"
   msg=" Execution of $pgm completed normally"
   postmsg "$jlogfile" "$msg"
   postmsg "$nosjlogfile" "$msg"
fi


echo "Generating the open boundary forcing"
export pgm=nos_ofs_create_forcing_obc_tides.sh
. prep_step
$USHnos/nos_ofs_create_forcing_obc_tides.sh
export err=$?
if [ $err -ne 0 ]
then
  echo "Execution of $pgm did not complete normally, FATAL ERROR!"
  echo "Execution of $pgm did not complete normally, FATAL ERROR!" >> $cormslogfile   
  msg=" Execution of $pgm did not complete normally, FATAL ERROR!"
  postmsg "$jlogfile" "$msg"
  postmsg "$nosjlogfile" "$msg"
  err_chk
else
  echo "Execution of $pgm completed normally"
  echo "Execution of $pgm completed normally" >> $cormslogfile
  msg=" Execution of $pgm completed normally"
  postmsg "$jlogfile" "$msg"
  postmsg "$nosjlogfile" "$msg"
fi

cp -p $jlogfile $COMOUT
	   echo "		  	        "
	   echo "END OF PREP TIDES SUCCESSFULLY "
	   echo "		                "

