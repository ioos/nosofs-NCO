#! /bin/sh 
. /usrx/local/Modules/3.2.9/init/sh
module use /nwprod2/modulefiles
module load lsf
export LSFDIR=/nos2/noscrub/Aijun.Zhang/nwprod2/nosofs.v3.2.0/lsf 
bsub < $LSFDIR/jnos_wcofs4_prep_03.lsf; bsub < $LSFDIR/jnos_wcofs4_nowcst_fcst_03.lsf #; bsub < $LSFDIR/jnos_wcofs4_ftp_03.lsf
