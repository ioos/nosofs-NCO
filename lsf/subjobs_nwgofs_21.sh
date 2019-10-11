#! /bin/sh 
. /usrx/local/Modules/3.2.9/init/sh
module use /nwprod2/modulefiles
module load lsf
export LSFDIR=/nos/save/Aijun.Zhang/nwprod/nosofs.v3.1.5/lsf 
bsub < $LSFDIR/jnos_nwgofs_prep_21.lsf; bsub < $LSFDIR/jnos_nwgofs_nowcst_fcst_21.lsf #; bsub < $LSFDIR/jnos_nwgofs_ftp_21.lsf
