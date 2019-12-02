#!/bin/bash -l
. /gpfs/dell2/nos/noscrub/Aijun.Zhang/nwprod/versions/nosofs.ver
module load lsf/${lsf_ver:?}
export LSFDIR=/gpfs/dell2/nos/noscrub/Aijun.Zhang/nwprod/nosofs.v3.2.0/lsf 
bsub < $LSFDIR/jnos_negofs_prep_15.lsf; bsub < $LSFDIR/jnos_negofs_nowcst_fcst_15.lsf #; bsub < $LSFDIR/jnos_negofs_ftp_15.lsf
