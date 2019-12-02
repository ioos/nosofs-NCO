for combinefields in `ls sub*`
do
   sed -e "s/Aijun\.Zhang/\$LOGNAME/g" ${combinefields} > ./new/${combinefields}
done
#exit


for combinefields in `ls jnos* `
do
   sed -e "s/\#BSUB \-o \/gpfs\/hps\/ptmp\/Aijun\.Zhang\/rpt/\#BSUB \-o rpt/g" \
       -e "s/\#BSUB \-e \/gpfs\/hps\/ptmp\/Aijun\.Zhang\/rpt/\#BSUB \-e rpt/g" \
       -e "s/\#BSUB \-cwd \/gpfs\/hps\/ptmp\/Aijun.Zhang/\#BSUB \-cwd \/gpfs\/hps\/ptmp\/\%U/g" \
       -e "s/Aijun\.Zhang/\$LOGNAME/g" \
${combinefields} > ./new/${combinefields}

done

exit

for combinefields in `ls jnos* subjob*`
do
   sed -e "s/nosofs_shared/nosofs/g" \
       -e "s/\${OFS\}\.ver/nosofs\.ver/g" \
${combinefields} > ./new/${combinefields}
#    sed -e "s/\"dev\"/\"devmax\"/g" ${combinefields} > ../${combinefields}
done

