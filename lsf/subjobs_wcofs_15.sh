#! /bin/sh 
. /usrx/local/Modules/3.2.9/init/sh
module use /nwprod2/modulefiles
module load lsf
export LSFDIR=/nos2/noscrub/Aijun.Zhang/nwprod2/nosofs.v3.2.0/lsf 
bsub < $LSFDIR/jnos_wcofs_prep_15.lsf; bsub < $LSFDIR/jnos_wcofs_nowcst_fcst_15.lsf; bsub < $LSFDIR/jnos_wcofs_ftp_15.lsf
