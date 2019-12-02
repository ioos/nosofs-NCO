#!/bin/sh
cd ../..

HOMEnos=`pwd`
export HOMEnos=${HOMEnos:-${NWROOT:?}/nosofs.${nosofs_ver:?}}
echo $HOMEnos
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

##  Compile ocean model of ROMS for CBOFS
cd $SORCnos/ROMS.fd
gmake clean
gmake -f makefile_cbofs

##  Compile ocean model of ROMS for DBOFS
cd $SORCnos/ROMS.fd
gmake clean
gmake -f makefile_dbofs
##  Compile ocean model of ROMS for TBOFS
cd $SORCnos/ROMS.fd
gmake clean
gmake -f makefile_tbofs

##  Compile ocean model of ROMS for GoMOFS
cd $SORCnos/ROMS.fd
gmake clean
gmake -f makefile_gomofs

###  Compile ocean model of ROMS for WCOFS
cd $SORCnos/ROMS.fd
gmake clean
gmake -f makefile_wcofs

###  Compile ocean model of ROMS for CIOFS
cd $SORCnos/ROMS.fd
gmake clean
gmake -f makefile_ciofs

