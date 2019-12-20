#!/bin/bash

. /usr/share/Modules/init/sh
module load produtil
module load gcc

if [ $# -lt 2 ] ; then
  echo "Usage: $0 YYYYMMDD HH [ngofs|negofs|etc.]"
  exit 1
fi

CDATE=$1
cyc=$2
ofs=${3:-ngofs}

# Need to create a single script that will do any ofs

ICDIR=/com/nos/${ofs}.$CDATE
mkdir -p $ICDIR
cd $ICDIR

nomads=https://nomads.ncep.noaa.gov/pub/data/nccf/com/nos/prod/${ofs}.${CDATE}

pfx=nos.$ofs
sfx=${CDATE}.t${cyc}z.nc

flist="
  $pfx.met.forecast.$sfx
  $pfx.obc.$sfx
  $pfx.river.$sfx.tar
  $pfx.hflux.forecast.$sfx
  $pfx.forecast.${CDATE}.t${cyc}z.in
  $pfx.init.nowcast.$sfx
"


for file in $flist
do
  echo "wget $nomads/$file"
  wget -nc $nomads/$file
done

# Fetch the restart/init file
# The restart file is the next cycle's 'init' file

# Get $cdate$cyc +6 hours init file, rename it to $cdate$cyc restart file
NEXT=`$NDATE +6 ${CDATE}${cyc}`
NCDATE=`echo $NEXT | cut -c1-8`
ncyc=`echo $NEXT | cut -c9-10`

nsfx=${NCDATE}.t${ncyc}z.nc

# if current cycle is '21' the next cycle will be during the next day
if [ $cyc == 21 ] ; then
  nomads=https://nomads.ncep.noaa.gov/pub/data/nccf/com/nos/prod/${ofs}.$NCDATE
fi


ifile=${pfx}.init.nowcast.${nsfx}
rfile=${pfx}.rst.nowcast.${sfx}

wget -nc ${nomads}/$ifile
if [[ $? -ne 0 ]] ; then
  echo "ERROR: Unable to retrieve $ifile from $nomads"
fi

# Rename it
cp -p $ifile $rfile

