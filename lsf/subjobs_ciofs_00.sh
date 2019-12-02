#!/bin/bash -l
. /gpfs/dell2/nos/noscrub/Aijun.Zhang/nwprod/versions/nosofs.ver
module load lsf/${lsf_ver:?}
export LSFDIR=/gpfs/dell2/nos/noscrub/Aijun.Zhang/nwprod/nosofs.v3.2.0/lsf 
bsub < $LSFDIR/jnos_ciofs_prep_00.lsf; bsub < $LSFDIR/jnos_ciofs_nowcst_fcst_00.lsf; bsub < $LSFDIR/jnos_ciofs_ftp_00.lsf
