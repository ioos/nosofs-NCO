#!/bin/sh

GRB2FILE=${1:?}
HH=${2:?}
NPP=${3:?}

TMPDIR=$(basename ${GRB2FILE//./_})
PS4=" \${SECONDS} \${TMPDIR} + "
echo $GRB2FILE $HH
set -x

# Import the VARNAME and LEV arrays
. ./var_lev_arrays

if [ -d $TMPDIR ]; then
  rm -f $TMPDIR
fi
mkdir -p $TMPDIR

TMPGRB2=$TMPDIR/${RUN:?}_${DBASE:?}.grib2
TMP1GRB2=$TMPDIR/${RUN}_${DBASE}1.grib2

${WGRIB2:?} $GRB2FILE -small_grib ${MINLON:?}:${MAXLON:?} ${MINLAT:?}:${MAXLAT:?} $TMPGRB2 
N=0
(( N = 10#$HH ))
(( N3 = $N - 3 ))
if [ $N3 -lt 0 ]; then
  N3=0
fi   
count=0
while (( count < $NPP ))
do
  if [ $N = 0 ]; then
    FPSTG="anl"
  else 
    FPSTG="$N hour fcst"
  fi   
  if [ $count = 16 ]; then
#    FPSTG="${N3}-${N} hour fcst"
    FPSTG="" 
  fi   
  if [ $DBASE == 'GFS' -o $DBASE == 'GFS25' ]; then
    if [ $count -ge 6 -a $count -le 11 ]; then
      FPSTG=""
    fi   
  fi	     
  if [ $count -ge 13 -a $count -le 16 ]; then
#  if [  $count = 13 -o $count = 14  -o $count = 15 ]; then
      FPSTG=""
  fi
  if [ $DBASE == 'RTMA' ]; then
    FPSTG="anl"
  fi   
  echo decoding ${VARNAME[count]}

  TMPSPRD=$TMPDIR/${VARNAME[count]}.$DBASE

  $WGRIB2 $TMPGRB2 -s | grep "${VARNAME[count]}${LEV[count]}$FPSTG" | sed '2,$d' | $WGRIB2 -i $TMPGRB2 -rpn "sto_1:-9999.9:rcl_1:merge:" -spread $TMPSPRD

  if [ $DBASE == 'RUC' -a \( $count = 2 -o $count = 5 \) ]; then
    $WGRIB2 $TMP1GRB2 -s | grep "${VARNAME[count]}${LEV[count]}$FPSTG" | sed '2,$d' | $WGRIB2 -i $TMP1GRB2 -rpn "sto_1:-9999.9:rcl_1:merge:" -spread $TMPSPRD
  fi

  (( count++ ))
done
