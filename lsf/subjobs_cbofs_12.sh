#!/bin/bash -l
. /gpfs/dell2/nos/noscrub/Aijun.Zhang/nwprod/versions/nosofs.ver
module load lsf/${lsf_ver:?}
export LSFDIR=/gpfs/dell2/nos/noscrub/Aijun.Zhang/nwprod/nosofs.v3.2.0/lsf 
bsub < $LSFDIR/jnos_cbofs_prep_12.lsf; bsub < $LSFDIR/jnos_cbofs_nowcst_fcst_12.lsf; bsub < $LSFDIR/jnos_cbofs_ftp_12.lsf
