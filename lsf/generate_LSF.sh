#!/bin/bash
if [ $# -ne 2 ]; then
   echo Usage: $0 Phase Queue
   echo For Example: $0 Phase1 dev for scripts to be run on Phase1 in queue dev
   exit 1
fi
##  platform = Phase1 Phase2 or Dell
##  que= dev or devhigh or devmax
platform=$1
que=$2
echo $platform $que
export $platform

if [ $platform == 'Phase1' ]; then
   VER=/nos/save/$LOGNAME/nwprod/versions/nosofs.ver
   . $VER
   version_file='\/nos\/save\/$LOGNAME\/nwprod\/versions\/nosofs\.ver'
   work=ptmpp1
   phase=1
   HOMEnos=/nos/save/$LOGNAME/nwprod/nosofs.${nosofs_ver}
   job_script_prep='\/nos\/save\/\$LOGNAME\/nwprod\/nosofs\.\${nosofs_ver}\/jobs\/JNOS_OFS_PREP\.\${envir}'
   job_script_nf='\/nos\/save\/\$LOGNAME\/nwprod\/nosofs\.\${nosofs_ver}\/jobs\/JNOS_OFS_NOWCST_FCST\.\${envir}'
   job_script_ftp='\/nos\/save\/\$LOGNAME\/nwprod\/nosofs\.\${nosofs_ver}\/jobs\/JNOS_OFS_FTP\.\${envir}'
   ptile=16
  queue=$que
elif  [ $platform == 'Phase2' ]; then
   VER=/nos2/noscrub/$LOGNAME/nwprod2/versions/nosofs.ver
   . $VER
   version_file='\/nos2\/noscrub\/\$LOGNAME\/nwprod2\/versions\/nosofs\.ver'
   work=ptmpp2
   phase=2
   HOMEnos=/nos2/noscrub/$LOGNAME/nwprod2/nosofs.${nosofs_ver}
   job_script_prep='\/nos2\/noscrub\/$LOGNAME\/nwprod2\/nosofs\.\${nosofs_ver}\/jobs\/JNOS_OFS_PREP\.\${envir}'
   job_script_nf='\/nos2\/noscrub\/$LOGNAME\/nwprod2\/nosofs\.\${nosofs_ver}\/jobs\/JNOS_OFS_NOWCST_FCST\.\${envir}'
   job_script_ftp='\/nos2\/noscrub\/$LOGNAME\/nwprod2\/nosofs\.\${nosofs_ver}\/jobs\/JNOS_OFS_FTP\.\${envir}'
   job_script_ftp_get='\/nos2\/noscrub\/$LOGNAME\/nwprod2\/nosofs\.\${nosofs_ver}\/jobs\/JNOS_OFS_FTP_GET\.\${envir}'
   ptile=24
   queue=${que}2
elif  [[ $platform == "Dell"* ]]; then
   VER=/gpfs/dell2/nos/noscrub/$LOGNAME/nwprod/versions/nosofs.ver
   . $VER
   version_file='\/gpfs\/dell2\/nos\/noscrub\/\$LOGNAME\/nwprod\/versions\/nosofs\.ver'
   work="gpfs\/${platform,,}\/ptmp"
   echo work= $work
   phase=d
   HOMEnos=/gpfs/dell2/nos/noscrub/$LOGNAME/nwprod/nosofs.${nosofs_ver}
   job_script_prep='\/gpfs\/dell2\/nos\/noscrub\/\$LOGNAME\/nwprod\/nosofs\.\${nosofs_ver}\/jobs\/JNOS_OFS_PREP\.\${envir}'
   job_script_nf='\/gpfs\/dell2\/nos\/noscrub\/\$LOGNAME\/nwprod\/nosofs\.\${nosofs_ver}\/jobs\/JNOS_OFS_NOWCST_FCST\.\${envir}'
   job_script_ftp='\/gpfs\/dell2\/nos\/noscrub\/\$LOGNAME\/nwprod\/nosofs\.\${nosofs_ver}\/jobs\/JNOS_OFS_FTP\.\${envir}'
   job_script_ftp_get='\/gpfs\/dell2\/nos\/noscrub\/\$LOGNAME\/nwprod\/nosofs\.\${nosofs_ver}\/jobs\/JNOS_OFS_FTP_GET\.\${envir}'
   ptile=28
   queue=$que
else
   echo platform needs to be either "Phase1" or "Phase2" or "Dell"
   exit
fi

echo nosofs version is set to $nosofs_ver
echo HOMEnos is set to $HOMEnos

for model in cbofs dbofs tbofs leofs ciofs gomofs wcofs ngofs negofs nwgofs sfbofs creofs lmhofs wcofs4 wcofs2 wcofs4-da
do
  CYC1=00;CYC2=06; CYC3=12; CYC4=18
  if [ $model == "ngofs" -o $model == "negofs" -o $model == "nwgofs" -o $model == "sfbofs" -o $model == "creofs"  -o -z "${model##wcofs*}" ]; then
     CYC1=03;CYC2=09; CYC3=15; CYC4=21
  fi
  total_tasks=$ptile
  for cyc in $CYC1 $CYC2 $CYC3 $CYC4
  do
   sed -e "s/MODEL/$model/g" \
       -e "s/QUEUE/$queue/g" \
       -e "s/TOTAL_TASKS/$total_tasks/g" \
       -e "s/CYC/$cyc/g" \
       -e "s/PLATFORM/$platform/g" \
       -e "s/WORK/$work/g" \
       -e "s/PHASE/$phase/g" \
       -e "s/VERSION_FILE/$version_file/g" \
       -e "s/JOB_SCRIPT_PREP/$job_script_prep/g" \
       -e "s/PTILE/$ptile/g" nos_prep.ecf.dev > ./new/jnos_${model}_prep_${cyc}.lsf
  done 
done

for model in cbofs dbofs tbofs leofs ciofs gomofs wcofs ngofs negofs nwgofs sfbofs creofs lmhofs wcofs4 wcofs2 wcofs4-da
do
   run_time=02:30
   CYC1=00;CYC2=06; CYC3=12; CYC4=18
   if [ $model == "ngofs" -o $model == "negofs" -o $model == "nwgofs" -o $model == "sfbofs" -o $model == "creofs"  -o -z "${model##wcofs*}" ]; then
     CYC1=03;CYC2=09; CYC3=15; CYC4=21
   fi
   if [ $model == "ngofs" -o $model == "negofs" -o $model == "nwgofs" -o $model == "sfbofs" -o $model == "creofs" ]; then
       total_tasks=120
   elif [ $model == "cbofs" ]; then
       total_tasks=240
   elif [ $model == "tbofs" ]; then
       total_tasks=144
   elif [ $model == "dbofs" ]; then
       total_tasks=192
  elif [ $model == "ciofs" ]; then
        total_tasks=720
  elif [ $model == "lmhofs" ]; then
        total_tasks=960
  elif [ $model == "leofs" ]; then
        total_tasks=48
  elif [ $model == "gomofs" -o $model == "wcofs2" ]; then
        total_tasks=1440
  elif [ $model == "wcofs4" -o $model == "wcofs" ]; then
        total_tasks=480
  elif [ $model == "wcofs4-da" ]; then
        total_tasks=800
        run_time=06:00
  fi
  for cyc in $CYC1 $CYC2 $CYC3 $CYC4
  do
   sed -e "s/MODEL/$model/g" \
       -e "s/QUEUE/$queue/g" \
       -e "s/TOTAL_TASKS/$total_tasks/g" \
       -e "s/RUN_TIME/$run_time/g" \
       -e "s/CYC/$cyc/g" \
       -e "s/PLATFORM/$platform/g" \
       -e "s/WORK/$work/g" \
       -e "s/PHASE/$phase/g" \
       -e "s/VERSION_FILE/$version_file/g" \
       -e "s/JOB_SCRIPT_NF/$job_script_nf/g" \
       -e "s/PTILE/$ptile/g" nos_nowcst_fcst.ecf.dev > ./new/jnos_${model}_nowcst_fcst_${cyc}.lsf
  if [ $model == 'wcofs4-da' ]; then
    sed -i "s/.*-w.*/#BSUB -w 'done (${model}_ftp_get_${phase}_$cyc)'/" ./new/jnos_${model}_nowcst_fcst_${cyc}.lsf
  fi
  done 
done

for model in cbofs dbofs tbofs leofs ciofs gomofs wcofs ngofs negofs nwgofs sfbofs creofs lmhofs wcofs4 wcofs2 wcofs4-da
do
   CYC1=00;CYC2=06; CYC3=12; CYC4=18
   if [ $model == "ngofs" -o $model == "negofs" -o $model == "nwgofs" -o $model == "sfbofs" -o $model == "creofs"  -o -z "${model##wcofs*}" ]; then
     CYC1=03;CYC2=09; CYC3=15; CYC4=21
   fi
  for cyc in $CYC1 $CYC2 $CYC3 $CYC4
  do
   sed -e "s/MODEL/$model/g" \
       -e "s/QUEUE/transfer/g" \
       -e "s/CYC/$cyc/g" \
       -e "s/PLATFORM/$platform/g" \
       -e "s/WORK/$work/g" \
       -e "s/PHASE/$phase/g" \
       -e "s/VERSION_FILE/$version_file/g" \
       -e "s/JOB_SCRIPT_FTP/$job_script_ftp/g" \
       -e "s/PTILE/$ptile/g" nos_ftp.ecf.dev > ./new/jnos_${model}_ftp_${cyc}.lsf
  done 
done

for model in cbofs dbofs tbofs leofs ciofs gomofs wcofs ngofs negofs nwgofs sfbofs creofs lmhofs wcofs4 wcofs2 wcofs4-da
do
   CYC1=00;CYC2=06; CYC3=12; CYC4=18
   if [ $model == "ngofs" -o $model == "negofs" -o $model == "nwgofs" -o $model == "sfbofs" -o $model == "creofs"  -o -z "${model##wcofs*}" ]; then
     CYC1=03;CYC2=09; CYC3=15; CYC4=21
   fi
  
  for cyc in $CYC1 $CYC2 $CYC3 $CYC4
  do
    echo "#!/bin/bash -l" > ./new/subjobs_${model}_${cyc}.sh
#    echo ". /usrx/local/Modules/3.2.9/init/sh" >> ./new/subjobs_${model}_${cyc}.sh
#    echo "module use /nwprod2/modulefiles"     >> ./new/subjobs_${model}_${cyc}.sh
    echo -e ". $VER" >> ./new/subjobs_${model}_${cyc}.sh
    echo "module load lsf/\${lsf_ver:?}" >> ./new/subjobs_${model}_${cyc}.sh
    echo "export LSFDIR=$HOMEnos/lsf " >> ./new/subjobs_${model}_${cyc}.sh
    if [ $model == "dbofs" -o $model == "tbofs" -o $model == "cbofs" -o $model == "ciofs"  -o $model == "lmhofs"  -o $model == "wcofs" ]; then
      echo "bsub < \$LSFDIR/jnos_${model}_prep_${cyc}.lsf; bsub < \$LSFDIR/jnos_${model}_nowcst_fcst_${cyc}.lsf #; bsub < \$LSFDIR/jnos_${model}_ftp_${cyc}.lsf" >> ./new/subjobs_${model}_${cyc}.sh
    elif [ $model == "wcofs4-da" ]; then
      echo "bsub < \$LSFDIR/jnos_${model}_prep_${cyc}.lsf; bsub < \$LSFDIR/jnos_${model}_ftp_get_${cyc}.lsf; bsub < \$LSFDIR/jnos_${model}_nowcst_fcst_${cyc}.lsf" >> ./new/subjobs_${model}_${cyc}.sh
    else
      echo "bsub < \$LSFDIR/jnos_${model}_prep_${cyc}.lsf; bsub < \$LSFDIR/jnos_${model}_nowcst_fcst_${cyc}.lsf #; bsub < \$LSFDIR/jnos_${model}_ftp_${cyc}.lsf" >> ./new/subjobs_${model}_${cyc}.sh  
   fi
  done 

done

model='wcofs4-da'
  CYC1=03;CYC2=09; CYC3=15; CYC4=21
  for cyc in $CYC1 $CYC2 $CYC3 $CYC4
  do
   sed -e "s/MODEL/$model/g" \
       -e "s/CYC/$cyc/g" \
       -e "s/PLATFORM/$platform/g" \
       -e "s/WORK/$work/g" \
       -e "s/PHASE/$phase/g" \
       -e "s/VERSION_FILE/$version_file/g" \
       -e "s/JOB_SCRIPT_FTP_GET/$job_script_ftp_get/g" \
           nos_ftp_get.ecf.dev > ./new/jnos_${model}_ftp_get_${cyc}.lsf
  done

chmod 755 ./new/*
