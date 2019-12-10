#!/bin/sh

if [ $# -ne 2 ] ; then
  echo "Usage: $0 YYYYMMDD HH"
  exit -1
fi

CDATE=$1
CYC=$2

COMDIR=/com/nos/cbofs.$CDATE

tmpdir=AWS.cbofs.${CDATE}.28ppn.140cpu
tarfile=${tmpdir}.tgz
tmproot=/ptmp/$USER/tmp
mkdir -p $tmproot/$tmpdir


hhlist='01 06 12 18 24 48'

for hh in $hhlist
do
  cp -p $COMDIR/nos.cbofs.fields.f0$hh.$CDATE.t${CYC}z.nc $tmproot/$tmpdir
done

cd $tmproot
tar -czvf $tarfile $tmpdir/

rm -Rf $tmproot/$tmpdir

