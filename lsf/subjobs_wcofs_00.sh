#! /bin/sh 
. /usrx/local/Modules/3.2.9/init/sh
module use /nwprod2/modulefiles
module load lsf
export LSFDIR=/nos/save/Aijun.Zhang/nwprod/nosofs.v3.1.5/lsf 
bsub < $LSFDIR/jnos_wcofs_prep_00.lsf; bsub < $LSFDIR/jnos_wcofs_nowcst_fcst_00.lsf #; bsub < $LSFDIR/jnos_wcofs_ftp_00.lsf
