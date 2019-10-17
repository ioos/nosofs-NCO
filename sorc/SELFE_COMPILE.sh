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


#cd $SORCnos/nos_ofs_utility.fd
#rm -f *.o *.a
#gmake -f makefile
#if [ -s $SORCnos/nos_ofs_utility.fd/libnosutil.a ]
#then
#  chmod 755 $SORCnos/nos_ofs_utility.fd/libnosutil.a
#  mv $SORCnos/nos_ofs_utility.fd/libnosutil.a ${LIBnos}
#fi


#cd $SORCnos/nos_ofs_combine_field_netcdf_selfe.fd
#rm -f *.o *.a
#gmake -f makefile


#cd $SORCnos/nos_ofs_combine_station_netcdf_selfe.fd
#rm -f *.o *.a
#gmake -f makefile


#cd $SORCnos/nos_ofs_combine_hotstart_out_selfe.fd
#rm -f *.o *.a
#gmake -f makefile


#cd $SORCnos/nos_ofs_create_forcing_met.fd
#rm -f *.o *.a
#gmake -f makefile


#cd $SORCnos/nos_ofs_create_forcing_obc_tides.fd
#rm -f *.o *.a
#gmake -f makefile


#cd $SORCnos/nos_ofs_create_forcing_obc_selfe.fd
#rm -f *.o *.a
#gmake -f makefile


#export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/RPSDEV/nceplibs/bufr/v11.0.2
#cd $SORCnos/nos_ofs_create_forcing_river.fd
#rm -f *.o *.a
#gmake -f makefile

#cd $SORCnos/nos_ofs_met_file_search.fd
#rm -f *.o *.a
#gmake -f makefile

#cd $SORCnos/nos_ofs_read_restart.fd
#rm -f *.o *.a
#gmake -f makefile


#cd $SORCnos/nos_ofs_read_restart_selfe.fd
#rm -f *.o *.a
#gmake -f makefile


#cd $SORCnos/nos_creofs_wl_offset_correction.fd
#gmake clean
#gmake -f makefile


#cd $SORCnos/nos_ofs_create_forcing_nudg.fd
#gmake clean
#gmake -f makefile


##  Compile ocean model of SELFE.fd for CREOFS
#cd $SORCnos/SELFE.fd/ParMetis-3.1-64bit
#gmake clean -f Makefile
#gmake -f Makefile

cd $SORCnos/SELFE.fd
gmake clean
gmake -f makefile
if [ -s  selfe_creofs ]; then
  mv selfe_creofs $EXECnos/.
else
  echo 'selfe executable is not created'
fi

exit


