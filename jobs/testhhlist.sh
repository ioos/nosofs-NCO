#!/bin/sh

hhlist=`./hhlist.sh`

echo $hhlist

for hh in $hhlist
do
  echo "HH : $hh"
done

