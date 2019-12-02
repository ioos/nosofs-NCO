#! /bin/sh 
. /usrx/local/Modules/3.2.9/init/sh
module use /nwprod2/modulefiles
module load lsf
export LSFDIR=/nos2/noscrub/Aijun.Zhang/nwprod2/nosofs.v3.1.3a/lsf 
bsub < $LSFDIR/jnos_wcofs_prep_06.lsf; bsub < $LSFDIR/jnos_wcofs_nowcst_fcst_06.lsf #; bsub < $LSFDIR/jnos_wcofs_ftp_06.lsf
