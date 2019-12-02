#!/bin/bash -l
. /gpfs/dell2/nos/noscrub/Aijun.Zhang/nwprod/versions/nosofs.ver
module load lsf/${lsf_ver:?}
export LSFDIR=/gpfs/dell2/nos/noscrub/Aijun.Zhang/nwprod/nosofs.v3.2.0/lsf 
bsub < $LSFDIR/jnos_cbofs_prep_18.lsf; bsub < $LSFDIR/jnos_cbofs_nowcst_fcst_18.lsf; bsub < $LSFDIR/jnos_cbofs_ftp_18.lsf
