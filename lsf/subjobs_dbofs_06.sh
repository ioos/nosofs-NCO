#!/bin/bash -l
. /gpfs/dell2/nos/noscrub/Aijun.Zhang/nwprod/versions/nosofs.ver
module load lsf/${lsf_ver:?}
export LSFDIR=/gpfs/dell2/nos/noscrub/Aijun.Zhang/nwprod/nosofs.v3.2.0/lsf 
bsub < $LSFDIR/jnos_dbofs_prep_06.lsf; bsub < $LSFDIR/jnos_dbofs_nowcst_fcst_06.lsf; bsub < $LSFDIR/jnos_dbofs_ftp_06.lsf
