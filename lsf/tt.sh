for combinefields in `ls jnos* `
do

   sed -e "s/ptmpp2/gpfs\/hps\/ptmp/g" ${combinefields} > ./new/${combinefields}

#   N=`wc -l ${combinefields} | awk '{print $1}'`
#   echo $N
#   N=`expr $N - 7`
#   head -7 ${combinefields} > ./new/${combinefields}
#   echo "#BSUB -cwd /gpfs/hps/ptmp/Aijun.Zhang" >> ./new/${combinefields}
#   tail -$N  ${combinefields} >> ./new/${combinefields}
done

