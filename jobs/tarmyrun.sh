#!/bin/sh

CDATE=$1
CYC=$2

COMDIR=/noscrub/com/nos/cbofs.$CDATE

tarfile=cbofs.$CDATE.tgz
tmpdir=/ptmp/$USER/cbofs.$CDATE
mkdir -p $tmpdir


hhlist='01 06 12 18 24 48'

for hh in $hhlist
do
  cp -p $COMDIR/nos.cbofs.fields.f0$hh.$CDATE.t${CYC}z.nc $tmpdir
done

cd $tmpdir
cd ..
tar -czvf $tarfile cbofs.$CDATE/

