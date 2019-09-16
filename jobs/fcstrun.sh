#!/bin/sh
set -x

########################################
# NOS_OFS_PREP 
########################################
export PS4=' $SECONDS + '
date

######################################################
# The following variables should be defined in the
# LSF/ecFlow submission script
#######################################################
#export envir=${envir:-nco}
#export DATAROOT=${DATAROOT:-/gpfs/hps/nco/ops/tmpnwprd}
#export jlogfile=${jlogfile:-/com2/logs/jlogfiles/jlogfile.$jobid}
#export SENDECF=${SENDECF:-YES}
#export SENDDBN=${SENDDBN:-YES}
#export MP_PULSE=0
#export MP_TIMEOUT=9000
#export MP_SHARED_MEMORY=yes

module load netcdf
module load mpi/intel
module load produtil

export NPP=96    # Number of processors
export KEEPDATA=YES

export envir=ec2
export SENDDBN=NO
export OFS=cbofs

export CDATE=20190906     # The hindcast date
export cyc='00'
export cycle=t${cyc}z
export nosofs_ver=v3.1.9.1
export NWROOT=/RPSDEV
export COMROOT=/save/com
#export COMIN=$COMROOT
export jobid=testfcst

###########################################
# Run setpdy and initialize PDY variables
###########################################
#sh setpdy.sh
# If CDATE is defined, use it (hindcast)
if [[ $CDATE ]] ; then
  export PDY=$CDATE
else
  setpdy.sh
  . ./PDY
fi

export DATA=/save/DATA/cbofsrun.$PDY
export jlogfile=$DATA/jlogfile.$$

###################################
# Specify NET and RUN Name and model
####################################
export OFS=${OFS:-ngofs}
export NET=${NET:-nos}
export RUN=${RUN:-$OFS}

###############################################################
# Specify DBN_ALERT_TYPE_???? for different Production envir.
###############################################################
export DBN_ALERT_TYPE_NETCDF=${DBN_ALERT_TYPE_NETCDF:-NOS_OFS_FCST_NETCDF}
export DBN_ALERT_TYPE_NETCDF_LRG=${DBN_ALERT_TYPE_NETCDF_LRG:-NOS_OFS_FCST_NETCDF_LP}
export DBN_ALERT_TYPE_TEXT=${DBN_ALERT_TYPE_TEXT:-NOS_OFS_FCST_TEXT}


########################################################
# Make working directory
########################################################
export DATA=${DATA:-${DATAROOT:?}/${jobid:?}}

if [ ! -d $DATA ]; then
  mkdir -p $DATA
  cd $DATA
else
  cd $DATA
  rm -fr $DATA/*
fi

############################################
#   Determine Job Output Name on System
############################################
export pgmout="OUTPUT.$$"

####################################
# Specify Execution Areas
####################################
export HOMEnos=${HOMEnos:-${NWROOT:?}/nosofs.${nosofs_ver:?}}
export EXECnos=${EXECnos:-${HOMEnos}/exec}
export FIXnos=${FIXnos:-${HOMEnos}/fix/shared}
export FIXofs=${FIXofs:-${HOMEnos}/fix/${OFS}}
export PARMnos=${PARMnos:-${HOMEnos}/parm}
export USHnos=${USHnos:-${HOMEnos}/ush}
export SCRIPTSnos=${SCRIPTSnos:-${HOMEnos}/scripts}


##############################################
# Define COM directories
##############################################
export COMIN=${COMIN:-${COMROOT}/${NET}/${envir}/${RUN}.${PDY}}          # input directory

export COMOUTroot=${COMOUTroot:-${COMROOT}/${NET}/${envir}}              # output directory
export COMOUT=${COMOUT:-${COMOUTroot}/${RUN}.${PDY}}                     # output directory

echo $COMIN
echo $COMOUT

mkdir -m 775 -p $COMOUT

##############################################
####  Log File To Sys Report  
##############################################
export nosjlogfile=${COMOUT}/${NET}.${RUN}.jlogfile.${PDY}.${cycle}.log 

##############################################
####  Log File To CORMS
##############################################
export cormslogfile=${COMOUT}/${NET}.${RUN}.corms.${PDY}.${cycle}.log
set +x
echo "LAUNCH ${RUN} NOWCAST/FORECAST SIMULATIONS at time: " `date ` >> $cormslogfile
echo "NOWCAST/FORECAST CYCLE IS: " $time_nowcastend >> $cormslogfile
echo "Start ${RUN} " >> $cormslogfile
set -x

env  

########################################################
# Execute the script.
########################################################
$SCRIPTSnos/exnos_ofs_nowcast_forecast.sh $OFS
########################################################

cat $pgmout

postmsg "$jlogfile" "$0 completed normally"

##############################
# Remove the Temporary working directory
##############################
if [ "${KEEPDATA^^}" != YES ]; then
  rm -rf $DATA
fi

date
