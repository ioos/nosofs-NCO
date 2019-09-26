#!/bin/sh
cd ..

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

cd $SORCnos

cd $SORCnos/nos_ofs_utility.fd
rm -f *.o *.a
gmake -f makefile
if [ -s $SORCnos/nos_ofs_utility.fd/libnosutil.a ]
then
  chmod 755 $SORCnos/nos_ofs_utility.fd/libnosutil.a
  mv $SORCnos/nos_ofs_utility.fd/libnosutil.a ${LIBnos}
fi
gmake clean

# SKIP
#cd $SORCnos/nos_ofs_combine_field_netcdf_selfe.fd
#rm -f *.o *.a
#gmake -f makefile

# SKIP
#cd $SORCnos/nos_ofs_combine_station_netcdf_selfe.fd
#rm -f *.o *.a
#gmake -f makefile

# SKIP
#cd $SORCnos/nos_ofs_combine_hotstart_out_selfe.fd
#rm -f *.o *.a
#gmake -f makefile

cd $SORCnos/nos_ofs_create_forcing_met.fd
rm -f *.o *.a
gmake -f makefile

# SKIP
#cd $SORCnos/nos_ofs_create_forcing_met_fvcom.fd
#rm -f *.o *.a
#gmake -f makefile

# OKAY
cd $SORCnos/nos_ofs_create_forcing_obc_tides.fd
rm -f *.o *.a
gmake -f makefile


# OKAY
cd $SORCnos/nos_ofs_create_forcing_obc.fd
rm -f *.o *.a
gmake -f makefile

# SKIP
#cd $SORCnos/nos_ofs_create_forcing_obc_fvcom.fd
#rm -f *.o *.a
#gmake -f makefile

# SKIP
#cd $SORCnos/nos_ofs_create_forcing_obc_fvcom_gl.fd
#rm -f *.o *.a
#gmake -f makefile

# SKIP
#cd $SORCnos/nos_ofs_create_forcing_obc_fvcom_nest.fd
#rm -f *.o *.a
#gmake -f makefile

# SKIP
#cd $SORCnos/nos_ofs_create_forcing_obc_selfe.fd
#rm -f *.o *.a
#gmake -f makefile

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


# SKIP
#cd $SORCnos/nos_ofs_read_restart_fvcom.fd
#rm -f *.o *.a
#gmake -f makefile

# SKIP
#cd $SORCnos/nos_ofs_read_restart_selfe.fd
#rm -f *.o *.a
#gmake -f makefile

cd $SORCnos/nos_ofs_reformat_ROMS_CTL.fd
rm -f *.o *.a
gmake -f makefile


# SKIP
# cd $SORCnos/nos_creofs_wl_offset_correction.fd
# gmake clean
# gmake -f makefile

# OKAY - not needed for cbofs?
cd $SORCnos/nos_ofs_create_forcing_nudg.fd
gmake clean
gmake -f makefile

## Compile DUBAI

#echo "Start: " > ~/compiletime
#echo `date` >> ~/compiletime

#cd $SORCnos/ROMS.fd
#gmake clean
#gmake -f makefile_dubai
#if [ -s  dubai_roms_mpi ]; then
#  mv dubai_roms_mpi $EXECnos/.
#else
#  echo 'roms executable for DUBAI is not created'
#fi

# Build time is 1 minute on EC2 w/ 1 CPU
#echo "Finish: " >> ~/compiletime
#echo `date` >> ~/compiletime

##  Compile ocean model of ROMS for CBOFS
cd $SORCnos/ROMS.fd
gmake clean
gmake -f makefile_cbofs
if [ -s  cbofs_roms_mpi ]; then
  mv cbofs_roms_mpi $EXECnos/.
else
  echo 'roms executable for CBOFS is not created'
fi



exit 0



##  Compile ocean model of ROMS for DBOFS
cd $SORCnos/ROMS.fd
gmake clean
gmake -f makefile_dbofs
if [ -s  dbofs_roms_mpi ]; then
  mv dbofs_roms_mpi $EXECnos/.
else
  echo 'roms executable for DBOFS is not created'
fi
##  Compile ocean model of ROMS for TBOFS
cd $SORCnos/ROMS.fd
gmake clean
gmake -f makefile_tbofs
if [ -s  tbofs_roms_mpi ]; then
  mv tbofs_roms_mpi $EXECnos/.
else
  echo 'roms executable for TBOFS is not created'
fi

##  Compile ocean model of ROMS for GoMOFS
cd $SORCnos/ROMS.fd
gmake clean
gmake -f makefile_gomofs
if [ -s  gomofs_roms_mpi ]; then
  mv gomofs_roms_mpi $EXECnos/.
else
  echo 'roms executable for GoMOFS is not created'
fi

##  Compile ocean model of FVCOM for NGOFS
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

##  Compile ocean model of SELFE.fd for CREOFS
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
exit
###  Compile ocean model of ROMS for WCOFS
cd $SORCnos/ROMS.fd
gmake clean
gmake -f makefile_wcofs
if [ -s  wcofs_roms_mpi ]; then
  mv wcofs_roms_mpi $EXECnos/.
else
  echo 'roms executable for WCOFS is not created'
fi

###  Compile ocean model of ROMS for CIOFS
cd $SORCnos/ROMS.fd
gmake clean
gmake -f makefile_ciofs
if [ -s  ciofs_roms_mpi ]; then
  mv ciofs_roms_mpi $EXECnos/.
else
  echo 'roms executable for CIOFS is not created'
fi

