#!/bin/bash -l
. /gpfs/dell2/nos/noscrub/Aijun.Zhang/nwprod/versions/nosofs.ver
module load lsf/${lsf_ver:?}
export LSFDIR=/gpfs/dell2/nos/noscrub/Aijun.Zhang/nwprod/nosofs.v3.2.0/lsf 
bsub < $LSFDIR/jnos_ngofs_prep_15.lsf; bsub < $LSFDIR/jnos_ngofs_nowcst_fcst_15.lsf #; bsub < $LSFDIR/jnos_ngofs_ftp_15.lsf
