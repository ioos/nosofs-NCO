#!/bin/sh
#set -x

. /usr/share/Modules/init/sh
module load produtil
module load gcc

if [ $# -ne 3 ] ; then
  echo "Usage: $0 YYYYMMDD HH cbofs|other ROMS model"
  exit 1
fi

CDATE=$1
cyc=$2
ofs=$3

url=https://nomads.ncep.noaa.gov/pub/data/nccf/com/nos/prod/${ofs}.$CDATE

ICDIR=/com/nos/${ofs}.$CDATE
mkdir -p $ICDIR
cd $ICDIR

# nos.cbofs.met.forecast.20190906.t00z.nc
# nos.cbofs.obc.20190906.t00z.nc
# nos.cbofs.river.20190906.t00z.nc
# nos.cbofs.roms.tides.20190906.t00z.nc
# nos.cbofs.rst.nowcast.20190906.t00z.nc
# nos.cbofs.forecast.20191001.t00z.in 

pfx=nos.${ofs}
sfx=${CDATE}.t${cyc}z.nc

icfiles="
$pfx.met.forecast.$sfx
$pfx.obc.$sfx
$pfx.river.$sfx
$pfx.roms.tides.$sfx
$pfx.forecast.${CDATE}.t${cyc}z.in
"

for file in $icfiles
do
  wget -nc ${url}/$file
  if [[ $? -ne 0 ]] ; then
    echo "ERROR: Unable to retrieve $file from $url"
  fi
done

# Need to rename the tides file - roms is still using generic name
cp -pf $pfx.roms.tides.$sfx nos.${ofs}.roms.tides.nc 

# Fetch the restart/init file
# ININAME == nos.cbofs.rst.nowcast.20191001.t00z.nc

# Get $cdate$cyc +6 hours init file, rename it to $cdate$cyc restart file
NEXT=`$NDATE +6 ${CDATE}${cyc}`
NCDATE=`echo $NEXT | cut -c1-8`
ncyc=`echo $NEXT | cut -c9-10`

nsfx=${NCDATE}.t${ncyc}z.nc

if [ $cyc == 18 ] ; then
  url=https://nomads.ncep.noaa.gov/pub/data/nccf/com/nos/prod/${ofs}.$NCDATE 
fi

ifile=${pfx}.init.nowcast.${nsfx}
rfile=${pfx}.rst.nowcast.${sfx}

wget -nc ${url}/$ifile
if [[ $? -ne 0 ]] ; then
  echo "ERROR: Unable to retrieve $file from \n $url"
fi

# Rename it
cp -p $ifile $rfile




