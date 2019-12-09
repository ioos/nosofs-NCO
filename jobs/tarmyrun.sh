#!/bin/sh

if [ $# -ne 2 ] ; then
  echo "Usage: $0 YYYYMMDD HH"
  exit -1
fi

CDATE=$1
CYC=$2

COMDIR=/com/nos/cbofs.$CDATE

tarfile=AWS.cbofs.$CDATE.tgz
tmpdir=/ptmp/$USER/tmp/cbofs.$CDATE
mkdir -p $tmpdir


hhlist='01 06 12 18 24 48'

for hh in $hhlist
do
  cp -p $COMDIR/nos.cbofs.fields.f0$hh.$CDATE.t${CYC}z.nc $tmpdir
done

cd $tmpdir
cd ..
tar -czvf $tarfile cbofs.$CDATE/

rm -Rf $tmpdir

