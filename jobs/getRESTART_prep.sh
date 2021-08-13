#!/bin/sh
set -x

# This receives the forcing data required to run OFS prep step
# It needs to be modified to support any ROMS based model

#__copyright__ = "Copyright © 2020 RPS Group, Inc. All rights reserved."
#__license__ = "See LICENSE.txt"
#__email__ = "patrick.tripp@rpsgroup.com"

. /usr/share/Modules/init/sh
module load produtil
module load gcc

ofs=ciofs
NOMADS=https://nomads.ncep.noaa.gov/pub/data/nccf/com/nos/prod

# Get current cycle forcing data
# For prep step rename current desired cycle init to rst minus 6 hours
# FIX this for prep - e.g. if desired cyc is 00 need previous days
# Forecast is different

# Current forecast cycle
# PREP wants the nowcast cycle
CDATE=20210216
cyc='18'

# Get current cycle init but rename it -6 hours
#PDY=20190905

COMROT=/com/nos
COM=$COMROT/$ofs.$CDATE$cyc
mkdir -p $COM
cd $COM

# # Get $cdate$cyc +6 hours init file, rename it to $cdate$cyc restart file
PREV=`$NDATE -6 ${CDATE}${cyc}`
NCDATE=`echo $PREV | cut -c1-8`
ncyc=`echo $PREV | cut -c9-10`

pfx=nos.${ofs}
sfx=${CDATE}.t${cyc}z.nc
nsfx=${NCDATE}.t${ncyc}z.nc

url=$NOMADS/${ofs}.$CDATE

if [[ $cyc -eq 18 ]] ; then
  url=$NOMADS/${ofs}.$NCDATE
fi

ifile=${pfx}.init.nowcast.${sfx}
rfile=${pfx}.rst.nowcast.${nsfx}

wget ${url}/$ifile
if [[ $? -ne 0 ]] ; then
  echo "ERROR: Unable to retrieve $file from \n $url"
  exit -1
fi

# Rename it
cp -p $ifile $rfile

