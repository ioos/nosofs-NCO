#!/bin/bash -l
. /gpfs/dell2/nos/noscrub/Aijun.Zhang/nwprod/versions/nosofs.ver
module load lsf/${lsf_ver:?}
export LSFDIR=/gpfs/dell2/nos/noscrub/Aijun.Zhang/nwprod/nosofs.v3.2.0/lsf 
bsub < $LSFDIR/jnos_sfbofs_prep_03.lsf; bsub < $LSFDIR/jnos_sfbofs_nowcst_fcst_03.lsf #; bsub < $LSFDIR/jnos_sfbofs_ftp_03.lsf
