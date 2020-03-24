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

# IBM POE settings
#export MP_PULSE=0
#export MP_TIMEOUT=9000
#export MP_SHARED_MEMORY=yes

. /usr/share/Modules/init/bash

module purge
export I_MPI_OFI_LIBRARY_INTERNAL=${I_MPI_OFI_LIBRARY_INTERNAL:-1}

module use -a $HOMEnos/modulefiles
module load nosofs/v3.2.1_aws

#printenv
#export I_MPI_DEBUG=1
# This is needed for Intel MPI 2019+ with Docker
#export I_MPI_FABRICS=shm
#unset I_MPI_PMI_LIBRARY
#export FI_PROVIDER=efa  # default if EFA is available
#export FI_PROVIDER=sockets  # fall back to TCP instead
#source /opt/intel/compilers_and_libraries_2019.5.281/linux/mpi/intel64/bin/mpivars.sh
#export I_MPI_DEBUG=4,nobuf
#export I_MPI_HYDRA_DEBUG=1
#export I_MPI_HYDRA_ENV=all
#export I_MPI_HYDRA_IFACE=ens5
#export I_MPI_OFI_PROVIDER_DUMP=1
#export I_MPI_EXTRA_FILESYSTEM=1
#export I_MPI_FABRICS=shm:ofi
#export I_MPI_FABRICS=shm
#export I_MPI_FABRICS=efa
#export I_MPI_FABRICS=verbs
#export FI_PROVIDER=efa
#export FI_PROVIDER=sockets
#export FI_PROVIDER=tcp
#export FI_EFA_ENABLE_SHM_TRANSFER=1
#export I_MPI_WAIT_MODE=1   #default is 0

export OFS=${OFS:-ngofs}
#export NPP=4

NOWCAST=NO      # Run the nowcast?
FORECAST=YES    # Run the forecast?

export NODES=${NODES:-1}
export NPP=${NPP:-16}     # Number of processors
export PPN=${PPN:-$((NPP/NODES))}

export HOSTFILE=${HOSTFILE:-$PWD/hosts}
export SENDDBN=NO

export MPIEXEC=mpirun

if [[ "$OFS" == "ngofs" || "$OFS" == "nwgofs" ]] ; then
  #export MPIOPTS=${MPIOPTS:-"-np $NPP -bind-to numa:1 -map-by C"}
  #export MPIOPTS=${MPIOPTS:-"-np $NPP -bind-to core:2 -map-by C"}
  #export MPIOPTS=${MPIOPTS:-"-np $NPP -bind-to core:$NPP"}
  export MPIOPTS=${MPIOPTS:-"-np $NPP -ppn $PPN -bind-to core"}
  #export MPIOPTS=${MPIOPTS:-"-np $NPP -bind-to numa:1 -map-by C"}
fi

export MPIOPTS=${MPIOPTS:-"-np $NPP -ppn $PPN"}

#     mpirun -np $NPP -ppn $PPN -f $HOSTFILE 

NOWCAST=NO      # Run the nowcast?
FORECAST=YES    # Run the forecast?

export cyc=${HH}
export cycle=$cyc
export nosofs_ver=v3.2.1
export NWROOT=/save
export COMROOT=/com
#export COMIN=$COMROOT
export jobid=fcst.$$

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
#export HOMEnos=${NWROOT}/nosofs-cbofs.${nosofs_ver:?}

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

# Fetch/Copy the ICs
# ./getICs.sh


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
if [[ $NOWCAST == "YES" ]] ; then

  $SCRIPTSnos/exnos_ofs_nowcast.sh $OFS
  
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

##############################
# Remove the Temporary working directory
##############################
if [ "${KEEPDATA}" != YES ]; then
  rm -rf $DATA
fi

date
