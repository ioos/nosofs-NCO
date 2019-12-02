#!/bin/sh
cd ..

HOMEnos=`pwd`
HOMEnos=/gpfs/dell2/nos/noscrub/$LOGNAME/nwprod/nosofs.v3.2.0
export HOMEnos=${HOMEnos:-${NWROOT:?}/nosofs.${nosofs_ver:?}}

module purge
printenv SHELL
module use $HOMEnos/modulefiles
module load nosofs
module list 2>&1

export SORCnos=$HOMEnos/sorc
export EXECnos=$HOMEnos/exec
export LIBnos=$HOMEnos/lib

cd $SORCnos/nos_ofs_combine_field_netcdf_selfe.fd
rm -f *.o *.a
gmake -f makefile

cd $SORCnos/nos_ofs_combine_station_netcdf_selfe.fd
rm -f *.o *.a
gmake -f makefile

cd $SORCnos/nos_ofs_combine_hotstart_out_selfe.fd
rm -f *.o *.a
gmake -f makefile
 
cd $SORCnos/nos_ofs_read_restart_selfe.fd
rm -f *.o *.a
gmake -f makefile

cd $SORCnos/nos_ofs_create_forcing_obc_selfe.fd
rm -f *.o *.a
gmake -f makefile 

#  Compile ocean model of SELFE.fd for CREOFS
cd $SORCnos/SELFE.fd/ParMetis-3.1-64bit
gmake clean
gmake -f Makefile
cd $SORCnos/SELFE.fd
gmake clean
gmake -f makefile
if [ -s  selfe_creofs ]; then
  mv selfe_creofs $EXECnos/.
else
  echo 'selfe executable is not created'
fi

exit


