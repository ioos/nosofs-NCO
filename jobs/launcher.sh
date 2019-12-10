#!/bin/bash
set -x

# This was created to launch a job via Python
# The Python scripts create the cluster on-demand 
# and submits this job with the list of hosts available.

if [ $# -ne 6 ] ; then
  echo "Usage: $0 YYYYMMDD HH NP PPN HOSTS <cbofs|ngofs>"
  exit 1
fi

export HOMEnos=/save/nosofs-NCO
export I_MPI_OFI_LIBRARY_INTERNAL=0
export I_MPI_DEBUG=1

#export I_MPI_FABRICS='shm:ofi'
#export I_MPI_FABRICS=efa
#export FI_PROVIDER=efa
#export FI_PROVIDER=tcp
#export FI_PROVIDER=sockets
#export FI_EFA_ENABLE_SHM_TRANSFER=1

export CDATE=$1
       HH=$2
export NP=$3
export PPN=$4
HOSTS=$5
export OFS=$6

export cyc=$HH

cd $HOMEnos/jobs
export HOSTFILE=$HOMEnos/jobs/hosts.launcher
echo $HOSTS > $HOSTFILE

#host1=`awk -F, '{print $1}' hosts.launcher`
#localhost=`hostname`
#export mpiopts="-localhost $host1 -nolocal -launcher ssh -genvall "
#export mpiopts="-localhost $localhost -nolocal -path /ptmp/ngofs.20191030 -bootstrap ssh" 

#export NP=140
#export PPN=28

# The following will launch the job on a non-local cluster, e.g. from a head node
#export MPIOPTS="-np $NP -ppn $PPN -hosts $HOSTS -nolocal -bootstrap ssh -iface ens5" 
#export MPIOPTS="-verbose -np $NP -ppn $PPN -hosts $HOSTS -nolocal -launcher ssh -iface ens5" 

export MPIOPTS="-nolocal -launcher ssh -hosts $HOSTS -np $NP -ppn $PPN " 
./fcstrun.sh $CDATE $HH


#       -ofi   Use this option to select an OpenFabrics Interfaces* (OFI*) capable network fabric.  The
#              application  attempts  to use an OFI-capable network fabric. If no such fabric is avail‐
#              able, another fabric from the list tmi,dapl,tcp,ofa is used.
#
#       -OFI   Use this option to select an OFI-capable network fabric. The  application  fails  if  no
#              such fabric is found.


