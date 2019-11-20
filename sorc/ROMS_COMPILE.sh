#!/bin/sh

HOMEnos=$(dirname $PWD)
export HOMEnos=${HOMEnos:-${NWROOT:?}/nosofs.${nosofs_ver:?}}

module purge
module use $HOMEnos/modulefiles
module load nosofs

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


onlymodel=yes

if [[ $onlymodel == "yes" ]] ; then
  ##  Compile ocean model of ROMS for CBOFS
  cd $SORCnos/ROMS.fd
  gmake clean
  gmake -f makefile_cbofs
  if [ -s  cbofs_roms_mpi ]; then
    mv cbofs_roms_mpi $EXECnos/.
  else
    echo 'roms executable for CBOFS is not created'
  fi

  echo "Only compiling ROMS model for cbofs"
  exit 0
fi

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


export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/RPSDEV/nceplibs/bufr/v11.0.2
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

fi  # end if buildprep


## Compile DUBAI ROMS model

#cd $SORCnos/ROMS.fd
#gmake clean
#gmake -f makefile_dubai
#if [ -s  dubai_roms_mpi ]; then
#  mv dubai_roms_mpi $EXECnos/.
#else
#  echo 'roms executable for DUBAI is not created'
#fi


##  Compile ocean model of ROMS for CBOFS
#cd $SORCnos/ROMS.fd
#gmake clean
#gmake -f makefile_cbofs
#if [ -s  cbofs_roms_mpi ]; then
#  mv cbofs_roms_mpi $EXECnos/.
#else
#  echo 'roms executable for CBOFS is not created'
#fi

##  Compile ocean model of ROMS for DBOFS
#cd $SORCnos/ROMS.fd
#gmake clean
#gmake -f makefile_dbofs
#if [ -s  dbofs_roms_mpi ]; then
#  mv dbofs_roms_mpi $EXECnos/.
#else
#  echo 'roms executable for DBOFS is not created'
#fi

##  Compile ocean model of ROMS for TBOFS
#cd $SORCnos/ROMS.fd
#gmake clean
#gmake -f makefile_tbofs
#if [ -s  tbofs_roms_mpi ]; then
#  mv tbofs_roms_mpi $EXECnos/.
#else
#  echo 'roms executable for TBOFS is not created'
#fi

##  Compile ocean model of ROMS for GoMOFS
#cd $SORCnos/ROMS.fd
#gmake clean
#gmake -f makefile_gomofs
#if [ -s  gomofs_roms_mpi ]; then
#  mv gomofs_roms_mpi $EXECnos/.
#else
#  echo 'roms executable for GoMOFS is not created'
#fi

# The makefiles for these last two are not available on noaa pmb 
# https://www.nco.ncep.noaa.gov/pmb/codes/nwprod/nosofs.v3.1.9.1/sorc/ROMS.fd/

###  Compile ocean model of ROMS for WCOFS
# West Coast - Developmental non-operational
cd $SORCnos/ROMS.fd
gmake clean
gmake -f makefile_wcofs
if [ -s  wcofs_roms_mpi ]; then
  mv wcofs_roms_mpi $EXECnos/.
else
  echo 'roms executable for WCOFS is not created'
fi

###  Compile ocean model of ROMS for CIOFS
# Cook Inlet Alaska - Is an operational product
cd $SORCnos/ROMS.fd
gmake clean
gmake -f makefile_ciofs
if [ -s  ciofs_roms_mpi ]; then
  mv ciofs_roms_mpi $EXECnos/.
else
  echo 'roms executable for CIOFS is not created'
fi

