#!/bin/sh
cd ..

HOMEnos=`pwd`
export HOMEnos=${HOMEnos:-${NWROOT:?}/nosofs.${nosofs_ver:?}}

module purge
printenv SHELL
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

cd $SORCnos/FVCOM.fd/FVCOM_source

gmake clean
gmake -f makefile_NGOFS
if [ -s  fvcom_ngofs ]; then
  mv fvcom_ngofs $EXECnos/.
else
  echo 'fvcom executable is not created'
fi
exit



#  Compile ocean model of FVCOM for NGOFS
cd  $SORCnos/FVCOM.fd/FVCOM_source/libs/julian
gmake clean
gmake -f makefile
rm -f *.o

cd  $SORCnos/FVCOM.fd/FVCOM_source/libs/proj.4-master
gmake clean
./configure  --prefix=$SORCnos/FVCOM.fd/FVCOM_source/libs/proj.4-master
gmake
gmake install

cd $SORCnos/FVCOM.fd/FVCOM_source/libs/proj4-fortran-master
gmake clean
./configure  CC=icc FC=ifort CFLAGS='-DIFORT -g -O2' proj4=$SORCnos/FVCOM.fd/FVCOM_source/libs/proj.4-master --prefix=$SORCnos/FVCOM.fd/FVCOM_source/libs/proj4-fortran-master
gmake
gmake install

cd $SORCnos/FVCOM.fd/METIS_source
gmake clean
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
exit
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
exit
#  Compile ocean model of SELFE.fd for CREOFS
cd $SORCnos/SELFE.fd/ParMetis-3.1-64bit
gmake -f Makefile
cd $SORCnos/SELFE.fd
gmake clean
gmake -f makefile
if [ -s  selfe_creofs ]; then
  mv selfe_creofs $EXECnos/.
else
  echo 'selfe executable is not created'
fi  

#  Compile ocean model of ROMS for WCOFS
cd $SORCnos/ROMS.fd
gmake clean
gmake -f makefile_wcofs
if [ -s  wcofs_roms_mpi ]; then
  mv wcofs_roms_mpi $EXECnos/.
else
  echo 'roms executable for WCOFS is not created'
fi

#  Compile ocean model of ROMS for CIOFS
cd $SORCnos/ROMS.fd
gmake clean
gmake -f makefile_ciofs
if [ -s  ciofs_roms_mpi ]; then
  mv ciofs_roms_mpi $EXECnos/.
else
  echo 'roms executable for CIOFS is not created'
fi

