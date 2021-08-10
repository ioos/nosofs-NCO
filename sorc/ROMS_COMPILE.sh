#!/bin/bash

export nosofs_ver=3.2.1
HOMEnos=$(dirname $PWD)
export HOMEnos=${HOMEnos:-${NWROOT:?}/nosofs.${nosofs_ver:?}}

module purge
module use $HOMEnos/modulefiles
module load nosofs/v3.2.1_aws

module list 2>&1

export SORCnos=$HOMEnos/sorc
export EXECnos=$HOMEnos/exec
export LIBnos=$HOMEnos/lib

if [ ! -s $EXECnos ]
then
  mkdir -p $EXECnos
fi
export LIBnos=$HOMEnos/lib

if [ ! -s $LIBnos ]
then
  mkdir -p $LIBnos
fi

cd $SORCnos

buildprep=no

models='ciofs'

for model in $models
do
  cd $SORCnos/ROMS.fd
  gmake clean
  gmake -f makefile_${model}
  if [ -s ${model}_roms_mpi ]; then
    mv ${model}_roms_mpi $EXECnos/.
  else
    echo 'roms executable for ${model} is not created'
  fi
done


if [[ $buildprep == "yes" ]] ; then

  cd $SORCnos/nos_ofs_utility.fd
  rm -f *.o *.a
  gmake -f makefile
  if [ -s $SORCnos/nos_ofs_utility.fd/libnosutil.a ]
  then
    chmod 755 $SORCnos/nos_ofs_utility.fd/libnosutil.a
    mv $SORCnos/nos_ofs_utility.fd/libnosutil.a ${LIBnos}
  fi

 
  cd $SORCnos/nos_ofs_create_forcing_met.fd
  rm -f *.o *.a
  gmake -f makefile


  cd $SORCnos/nos_ofs_create_forcing_obc_tides.fd
  rm -f *.o *.a
  gmake -f makefile
 
 
  cd $SORCnos/nos_ofs_create_forcing_obc.fd
  rm -f *.o *.a
  gmake -f makefile
 
 
  cd $SORCnos/nos_ofs_create_forcing_river.fd
  rm -f *.o *.a
  gmake -f makefile
 

  cd $SORCnos/nos_ofs_met_file_search.fd
  rm -f *.o *.a
  gmake -f makefile
 

  cd $SORCnos/nos_ofs_read_restart.fd
  rm -f *.o *.a
  gmake -f makefile
 

  cd $SORCnos/nos_ofs_reformat_ROMS_CTL.fd
  rm -f *.o *.a
  gmake -f makefile


 # Unlear which model requires this
  cd $SORCnos/nos_ofs_create_forcing_nudg.fd
  gmake clean
  gmake -f makefile


  cd $SORCnos/nos_ofs_residual_water_calculation.fd
  gmake clean
  gmake -f makefile

fi  # end if buildprep
