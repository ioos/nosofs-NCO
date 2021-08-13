#!/bin/bash
set -x

nosofs_ver=v3.2.1

HOMEnos=$(dirname $PWD)
export HOMEnos=${HOMEnos:-${NWROOT:?}/nosofs.${nosofs_ver:?}}

module purge
module use $HOMEnos/modulefiles
module load nosofs/v3.2.1_aws
export PATH=$PATH:/usrx/bin

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


  cd $SORCnos/nos_ofs_create_forcing_obc_fvcom_gl.fd
  rm -f *.o *.a
  gmake -f makefile

exit

BUILDPREP=NO

if [[ $BUILDPREP == "YES" ]] ; then

  cd $SORCnos/nos_ofs_utility.fd
  rm -f *.o *.a
  gmake -f makefile
  if [ -s $SORCnos/nos_ofs_utility.fd/libnosutil.a ]
  then
    chmod 755 $SORCnos/nos_ofs_utility.fd/libnosutil.a
    mv $SORCnos/nos_ofs_utility.fd/libnosutil.a ${LIBnos}
  fi
  
  
  cd $SORCnos/nos_ofs_create_forcing_met_fvcom.fd
  rm -f *.o *.a
  gmake -f makefile
  

  cd $SORCnos/nos_ofs_create_forcing_obc_tides.fd
  rm -f *.o *.a
  gmake -f makefile
  
  
  cd $SORCnos/nos_ofs_create_forcing_obc_fvcom.fd
  rm -f *.o *.a
  gmake -f makefile
  
  
  cd $SORCnos/nos_ofs_create_forcing_obc_fvcom_gl.fd
  rm -f *.o *.a
  gmake -f makefile
  
  
  cd $SORCnos/nos_ofs_create_forcing_obc_fvcom_nest.fd
  rm -f *.o *.a
  gmake -f makefile
  
   
  export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/RPSDEV/nceplibs/bufr/v11.0.2
  cd $SORCnos/nos_ofs_create_forcing_river.fd
  rm -f *.o *.a
  gmake -f makefile
  
  cd $SORCnos/nos_ofs_met_file_search.fd
  rm -f *.o *.a
  gmake -f makefile
  
  
  cd $SORCnos/nos_ofs_read_restart_fvcom.fd
  rm -f *.o *.a
  gmake -f makefile
  
  
  cd $SORCnos/nos_ofs_create_forcing_nudg.fd
  gmake clean
  gmake -f makefile
  
  cd $SORCnos/nos_ofs_residual_water_calculation.fd
  gmake clean
  gmake -f makefile
  
fi  # IF BUILDPREP

exit
##  Compile ocean model of FVCOM for NGOFS
cd  $SORCnos/FVCOM.fd/FVCOM_source/libs/julian
gmake clean
gmake -f makefile
rm -f *.o

# Proj4 needs to be unzipped before building proj4.zip
cd $SORCnos/FVCOM.fd/FVCOM_source/libs
unzip -n proj4.zip

cd $SORCnos/FVCOM.fd/FVCOM_source/libs/proj.4-master
gmake clean
./configure  --prefix=$SORCnos/FVCOM.fd/FVCOM_source/libs/proj.4-master
gmake
gmake install

# Copy shared libraries
cp -Rp $SORCnos/FVCOM.fd/FVCOM_source/libs/proj.4-master/lib/* $LIBnos

# Note: Automake tools must be installed 
cd $SORCnos/FVCOM.fd/FVCOM_source/libs/proj4-fortran-master
gmake clean
./configure  CC=gcc FC=gfortran CFLAGS='-DGFORTRAN -g -O2' proj4=$SORCnos/FVCOM.fd/FVCOM_source/libs/proj.4-master --prefix=$SORCnos/FVCOM.fd/FVCOM_source/libs/proj4-fortran-master
gmake
gmake install


cd $SORCnos/FVCOM.fd/METIS_source
gmake clean
rm -f *.o
gmake -f makefile
rm -f *.o

cd $SORCnos/FVCOM.fd/FVCOM_source

gmake clean
gmake -f makefile_NGOFS
if [ -s  fvcom_ngofs ]; then
  mv fvcom_ngofs $EXECnos/.
else
  echo 'fvcom executable is not created'
fi  

gmake clean
gmake -f makefile_NEGOFS
if [ -s  fvcom_negofs ]; then
  mv fvcom_negofs $EXECnos/.
else
  echo 'fvcom executable is not created'
fi


gmake clean
gmake -f makefile_NWGOFS
if [ -s  fvcom_nwgofs ]; then
  mv fvcom_nwgofs $EXECnos/.
else
  echo 'fvcom executable is not created'
fi


gmake clean
gmake -f makefile_SFBOFS
if [ -s  fvcom_sfbofs ]; then
  mv fvcom_sfbofs $EXECnos/.
else
  echo 'fvcom executable is not created'
fi


gmake clean
gmake -f makefile_LEOFS
if [ -s  fvcom_leofs ]; then
  mv fvcom_leofs $EXECnos/.
else
  echo 'fvcom executable is not created'
fi

gmake clean
gmake -f makefile_LMHOFS
if [ -s  fvcom_lmhofs ]; then
  mv fvcom_lmhofs $EXECnos/.
else
  echo 'fvcom executable is not created'
fi


