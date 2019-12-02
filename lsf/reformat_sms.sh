for combinefields in `ls jnos* subjob*`
do

    sed -e "s/v3.1.2/v3.1.3/g" ${combinefields} > ./new/${combinefields}

#   sed -e "s/v3.1.0/v3.1.1/g" \
#       -e "s/\/ptmpp2\/Aijun.Zhang\/rpt/rpt/g" \
#       -e "s/\-cwd \/ptmpp2\/Aijun.Zhang/\-cwd \/ptmpp2\/\%U/g" \
#       -e "s/Aijun\.Zhang/\$LOGNAME/g" ${combinefields} > ./new/${combinefields}
#    sed -e "s/\#BSUB\ \-n\ 16/\#BSUB\ \-n\ 24/g" ${combinefields} > ./new/${combinefields}
#    sed -e "s/ptile\=16/ptile\=24/g" ${combinefields} > ./new/${combinefields}
done

