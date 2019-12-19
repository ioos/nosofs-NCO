#!/bin/bash
set -x
ulimit -s unlimited
ulimit -c unlimited

#export HYDRA_TOPO_DEBUG=1

# NGOFS 20191017 03

if [ $# -ne 2 ] ; then
  echo "Usage: $0 YYYYMMDD HH"
  exit 1
fi

export CDATE=$1
HH=$2

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

# IBM POE settings
#export MP_PULSE=0
#export MP_TIMEOUT=9000
#export MP_SHARED_MEMORY=yes

module purge
module load gcc/6.5.0
module load mpi/intel
module load hdf5-impi
module load netcdf/4.5
module load produtil

export OFS=${OFS:-ngofs}

NOWCAST=NO      # Run the nowcast?
FORECAST=YES    # Run the forecast?

export cyc=${HH}
export cycle=$cyc
export nosofs_ver=v3.2.1
export NWROOT=/save
export COMROOT=/com
export jobid=fcst.$$
export HOMEnos=$(dirname $PWD)
export LD_LIBRARY_PATH=$HOMEnos/lib:$LD_LIBRARY_PATH

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

#export DATA=/ptmp/$USER/$OFS.$PDY
export DATA=/ptmp/$OFS.$PDY

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


############################################
#   Determine Job Output Name on System
############################################
export pgmout="OUTPUT.$$"

########################################################
# Make working directory
########################################################
export DATA=${DATA:-${DATAROOT:?}/${jobid:?}}


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
export COMIN=${COMIN:-${COMROOT}/${NET}/${RUN}.${PDY}}          # input directory
export COMOUTroot=${COMOUTroot:-${COMROOT}/${NET}}              # output directory
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


  $USHnos/fix_forecast.sh $OFS


