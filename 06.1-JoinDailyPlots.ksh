MEC_dir_old=`pwd`
cd ${MEC_D_img}
echo 
echo ===========================================================================
echo JOINING DAILY IMAGES
echo ===========================================================================
MASKs=( CONT ALL )
RESs=( High ) # Low Med High
REGs=( REG NREG )
for MASK in "${MASKs[@]}"; do
  for RES in "${RESs[@]}"; do
    for REG in "${REGs[@]}"; do
       for D in $(seq 0 30); do
        for MOD in "${MEC_models[@]}"; do
          # FCT  -------------------------------------------------------------------
          # ************ FILE NAME IS ALSO DEFINED IN 04b.1-PlotPrevious.R *********
          MEC_QTD_FILES=`ls -l ${MEC_var}_${MOD}_???_D-${D}_${MASK}_${RES}_${REG}_${MEC_init_time}.jpeg 2>/dev/null | wc -l`
          if [ "${MEC_QTD_FILES}" != "0" ] ; then
            echo "${MOD} ${MEC_var} ${MASK} ${RES} ${REG} ${D} ${MOD} ($MEC_QTD_FILES)"
            MEC_FNAME=${MEC_D_deploy}/IMG/Img_FCT_${MEC_var}_${MOD}_D-${D}_${MASK}_${RES}_${REG}_${MEC_init_time}.jpeg
            touch ${MEC_FNAME}; rm -f ${MEC_FNAME}
            montage -tile 1x ${MEC_var}_${MOD}_???_D-${D}_${MASK}_${RES}_${REG}_${MEC_init_time}.jpeg -geometry +1+1 ${MEC_FNAME}

            # BIAS -------------------------------------------------------------------
            MEC_FNAME=${MEC_D_deploy}/IMG/Img_BIAS_${MEC_var}_${MOD}x${MEC_obs}_D-${D}_${MASK}_${RES}_${REG}_${MEC_init_time}.jpeg
            touch ${MEC_FNAME}; rm -f ${MEC_FNAME}
            montage -tile 1x ${MEC_var}_BIAS_${MOD}x${MEC_obs}_???_D-${D}_${MASK}_${RES}_${REG}_${MEC_init_time}.jpeg -geometry +1+1 ${MEC_FNAME}
          fi
        done
      done
    done
  done
done
cd ${MEC_dir_old}

