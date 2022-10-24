#!/bin/bash
set -x
ulimit -s unlimited
ulimit -c unlimited

if [ $# -ne 2 ] ; then
  echo "Usage: $0 YYYYMMDD HH"
  exit 1
fi

export CDATE=$1
HH=$2

export HOMEnos=$(dirname $PWD)

# YES will not delete /ptmp run directory, useful when debugging
export KEEPDATA=NO
#export KEEPDATA=YES

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

. /usr/share/Modules/init/bash

module purge
export I_MPI_OFI_LIBRARY_INTERNAL=1

module use -a $HOMEnos/modulefiles/nosofs

#module load v3.2.1_gcc8_skylake_512
module load v3.2.1_intel_skylake_512

export I_MPI_DEBUG=1
#export I_MPI_FABRICS=shm    # This is needed for Intel MPI 2019+ with Docker
#export FI_PROVIDER=efa      # default if EFA is available
#export FI_PROVIDER=sockets  # fall back to TCP instead
#export FI_PROVIDER=tcp

export OFS=${OFS:-cbofs}

NOWCAST=${NOWCAST:-NO}      # Run the nowcast?
FORECAST=${FORECAST:-YES}    # Run the forecast?

export NODES=${NODES:-1}
export NPP=${NPP:-16}     # Number of processors
export PPN=${PPN:-$((NPP/NODES))}

export HOSTFILE=${HOSTFILE:-$PWD/hosts}
export SENDDBN=NO

export MPIEXEC=mpirun

if [[ "$OFS" == "ngofs" || "$OFS" == "nwgofs" ]] ; then
  export MPIOPTS=${MPIOPTS:-"-np $NPP -ppn $PPN -bind-to core"}
fi

export MPIOPTS=${MPIOPTS:-"-np $NPP -ppn $PPN"}

NOWCAST=NO      # Run the nowcast?
FORECAST=YES    # Run the forecast?

export cyc=${HH}
export cycle=$cyc
export nosofs_ver=v3.2.1
export NWROOT=/save
export COMROOT=/com
export jobid=fcst.$$

export LD_LIBRARY_PATH=$HOMEnos/lib:$LD_LIBRARY_PATH

###########################################
# Run setpdy and initialize PDY variables
###########################################
# If CDATE is defined, use it (hindcast)
if [[ $CDATE ]] ; then
  export PDY=$CDATE
else
  setpdy.sh
  . ./PDY
fi

export DATA=/ptmp/$OFS.${CDATE}${HH}

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

if [ ! -d $DATA ]; then
  mkdir -p $DATA
  cd $DATA
else
  cd $DATA
  rm -fr $DATA/*
fi

# Copy exec to run directory, sorc/build directory is not on NFS
# This does not work as shared runtime libraries are still needed
#export EXECnos=$DATA
#cp -p ${HOMEnos}/exec/*${OFS}* $EXECnos

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
export COMIN=${COMIN:-${COMROOT}/${NET}/${RUN}.${PDY}${HH}}          # input directory
export COMOUTroot=${COMOUTroot:-${COMROOT}/${NET}}              # output directory
export COMOUT=${COMOUT:-${COMOUTroot}/${RUN}.${PDY}${HH}}                     # output directory

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

result=0

########################################################
# Execute the script.
########################################################
if [[ $NOWCAST == "YES" ]] ; then

  $SCRIPTSnos/exnos_ofs_nowcast.sh $OFS
  ((result+=$?))
 
  echo "-----------------------------------------------------"
  echo "-----------------------------------------------------"
  echo "-----------------------------------------------------"
  echo "-----------------------------------------------------"
  #echo "    FINISHED NOWCAST FOR $CDATE $cycle               "
  echo "-----------------------------------------------------"
  echo "-----------------------------------------------------"
  echo "-----------------------------------------------------"
  echo "-----------------------------------------------------"
fi


if [[ $FORECAST == "YES" ]] ; then

  $SCRIPTSnos/exnos_ofs_forecast.sh $OFS
  ((result+=$?))

  echo "-----------------------------------------------------"
  echo "-----------------------------------------------------"
  echo "-----------------------------------------------------"
  echo "-----------------------------------------------------"
  echo "    FINISHED FORECAST FOR $CDATE $cycle               "
  echo "-----------------------------------------------------"
  echo "-----------------------------------------------------"
  echo "-----------------------------------------------------"
  echo "-----------------------------------------------------"
  ########################################################
fi

cat $pgmout

postmsg "$jlogfile" "$0 completed normally"

# Save the log file
cp -p $DATA/nos.*.log $COMOUT

##############################
# Remove the Temporary working directory
##############################
if [ "${KEEPDATA}" != YES ]; then
  rm -rf $DATA
fi

date
exit $result
