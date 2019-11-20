#!/bin/sh
set -x

########################################
# NOS_OFS_PREP 
########################################
export PS4=' $SECONDS + '
date

#FIXed 
# ush/nos_ofs_create_forcing_obc.sh: line 178: [: =: unary operator expected 
export SENDDBN=NO

export KEEPDATA=YES

###################################
# Specify NET and RUN Name and model
####################################
export OFS=ngofs
export NET=${NET:-nos}
export RUN=${RUN:-$OFS}
export envir=''

module load gcc/6.5.0
module load netcdf
#module load mpi/intel
module load mpi/mpich
module load produtil
module load wgrib2
export NPP=8

export CDATE=20191030
export cyc='09'

#export CDATE=20190905     # The hindcast date
#export cyc='18'

export cycle=t${cyc}z
export nosofs_ver=v3.1.9.1
export NWROOT=/save/$USER
export COMROOT=/data/com
export COMIN=$COMROOT
export jobid=test

# If CDATE is defined, use it (hindcast)
if [[ $CDATE ]] ; then
  export PDY=$CDATE
else
  setpdy.sh
  . ./PDY
fi

export DATA=/data/temp/$OFS.$PDY

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

###########################################
# Run setpdy and initialize PDY variables
###########################################
#sh setpdy.sh

#echo "***************************************************************"
#echo "  I AM HERE  "
#echo "***************************************************************"
#echo "PDY is $PDY"
#pwd


##############################################
# Define COM directories
##############################################

export COMIN=${COMIN:-${COMROOT}/${NET}/${envir}/${RUN}.${PDY}}   # input directory
export COMOUTroot=${COMOUTroot:-${COMROOT}/${NET}/${envir}}       # output directory
export COMOUT=${COMOUT:-${COMOUTroot}/${RUN}.${PDY}}              # output directory

export COMINnestedparent=${COMINnestedparent:-${COMROOT}/${NET}/${envir}}

mkdir -m 775 -p $COMOUT

##############################################
### Set up input data path
##############################################

export COMINnam=$COMIN/nam/${envir}
export COMINhrrr=$COMIN/hrrr
export COMINrap=$COMIN/rap
export COMINgfs=$COMIN/gfs
export COMINrtma=$COMIN/rtma
export COMINetss=$COMIN/etss/${envir}
export COMINrtofs_2d=$COMIN/rtofs/${envir}
export COMINrtofs_3d=$COMIN/rtofs/${envir}


export COMINnam=${COMINnam:-$(compath.py nam/prod)}
export COMINhrrr=${COMINhrrr:-$(compath.py hrrr/prod)}
export COMINrap=${COMINrap:-$(compath.py rap/prod)}
export COMINgfs=${COMINgfs:-$(compath.py gfs/prod)}
export COMINrtma=${COMINrtma:-$(compath.py rtma/prod)}
export COMINetss=${COMINetss:-$(compath.py etss/prod)}
export COMINrtofs_2d=${COMINrtofs_2d:-$(compath.py rtofs/prod)}
export COMINrtofs_3d=${COMINrtofs_3d:-$(compath.py rtofs/prod)}


if [ ${RUN} == "NEGOFS" -o ${RUN} == "negofs" -o ${RUN} == "NWGOFS" -o ${RUN} == "nwgofs" ]
then
  export NESTED_PARENT_OFS=ngofs
fi

export DCOMINndfd=${DCOMINndfd:-${DCOMROOT}/us007003}
export DCOMINncom=${DCOMINncom:-${DCOMROOT}/us007003}
export DCOMINusgs=${DCOMINusgs:-${DCOMROOT}/us007003}
export DCOMINports=${DCOMINports:-${DCOMROOT}/us007003}
export NOSBUFR=xx012
export USGSBUFR=xx009

##################################################################
####  Log File To Sys Report  
##################################################################
export jlogfile=${COMOUT}/jlogfile
export nosjlogfile=${COMOUT}/${NET}.${RUN}.jlogfile.${PDY}.${cycle}.log 

##################################################################
####  Log File To CORMS
##################################################################
export cormslogfile=${COMOUT}/${NET}.${RUN}.corms.${PDY}.${cycle}.log

env  

########################################################
# Execute the script.
########################################################
$SCRIPTSnos/exnos_ofs_prep.sh $OFS
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
