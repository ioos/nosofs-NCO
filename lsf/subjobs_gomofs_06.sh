#! /bin/sh 
. /usrx/local/Modules/3.2.9/init/sh
module use /nwprod2/modulefiles
module load lsf
export LSFDIR=/nos/save/Aijun.Zhang/nwprod/nosofs.v3.1.5/lsf 
bsub < $LSFDIR/jnos_gomofs_prep_06.lsf; bsub < $LSFDIR/jnos_gomofs_nowcst_fcst_06.lsf #; bsub < $LSFDIR/jnos_gomofs_ftp_06.lsf
