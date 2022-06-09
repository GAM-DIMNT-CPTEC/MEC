MEC_dir_old=`pwd`
cd ${MEC_D_img}
echo 
echo ===========================================================================
echo JOINING ${MEC_PRIOR_NAME} IMAGES
echo ===========================================================================
MASKs=( CONT ALL )
RESs=( High ) # Low Med High
REGs=( REG NREG )
for MASK in "${MASKs[@]}"; do
  for RES in "${RESs[@]}"; do
    for REG in "${REGs[@]}"; do
      for MOD in "${MEC_models[@]}"; do
        MEC_QTD_FILES=`ls -l ${MEC_var}_${MOD}_???_${MEC_PRIOR_NAME}_${MASK}_${RES}_${REG}_${MEC_init_time}.jpeg 2>/dev/null | wc -l`
        if [ "${MEC_QTD_FILES}" != "0" ] ; then
          echo "${MEC_var} ${MASK} ${RES} ${REG} ${D} ${MOD} ($MEC_QTD_FILES)"
          # FCT  -------------------------------------------------------------------
          # ************ FILE NAME IS ALSO DEFINED IN 04b.1-PlotPrevious.R *********
          MEC_FNAME=${MEC_D_deploy}/IMG/Img_FCT_${MEC_var}_${MOD}_${MEC_PRIOR_NAME}_${MASK}_${RES}_${REG}_${MEC_init_time}.jpeg
          touch ${MEC_FNAME}; rm -f ${MEC_FNAME}
          montage -tile 1x ${MEC_var}_${MOD}_???_${MEC_PRIOR_NAME}_${MASK}_${RES}_${REG}_${MEC_init_time}.jpeg -geometry +1+1 ${MEC_FNAME}

          # BIAS -------------------------------------------------------------------
          MEC_FNAME=${MEC_D_deploy}/IMG/Img_BIAS_${MEC_var}_${MOD}x${MEC_obs}_${MEC_PRIOR_NAME}_${MASK}_${RES}_${REG}_${MEC_init_time}.jpeg
          touch ${MEC_FNAME}; rm -f ${MEC_FNAME}
          montage -tile 1x ${MEC_var}_BIAS_${MOD}x${MEC_obs}_???_${MEC_PRIOR_NAME}_${MASK}_${RES}_${REG}_${MEC_init_time}.jpeg -geometry +1+1 ${MEC_FNAME}

          # SCORR -------------------------------------------------------------------
          MEC_FNAME=${MEC_D_deploy}/IMG/Img_SCORR_${MEC_var}_${MOD}x${MEC_obs}_${MEC_PRIOR_NAME}_${MASK}_${RES}_${REG}_${MEC_init_time}.jpeg
          touch ${MEC_FNAME}; rm -f ${MEC_FNAME}
          montage -tile 1x ${MEC_var}_SCORR_${MOD}x${MEC_obs}_???_${MEC_PRIOR_NAME}_${MASK}_${RES}_${REG}_${MEC_init_time}.jpeg -geometry +1+1 ${MEC_FNAME}

          # SRMSE -------------------------------------------------------------------
          MEC_FNAME=${MEC_D_deploy}/IMG/Img_SRMSE_${MEC_var}_${MOD}x${MEC_obs}_${MEC_PRIOR_NAME}_${MASK}_${RES}_${REG}_${MEC_init_time}.jpeg
          touch ${MEC_FNAME}; rm -f ${MEC_FNAME}
          montage -tile 1x ${MEC_var}_SRMSE_${MOD}x${MEC_obs}_???_${MEC_PRIOR_NAME}_${MASK}_${RES}_${REG}_${MEC_init_time}.jpeg -geometry +1+1 ${MEC_FNAME}
        fi
      done
      # ===========================
      # Joining BESTOF
      # ===========================
      MEC_QTD_FILES=`ls -l ${MEC_var}_BESTOF_SCORR_${MEC_obs}_???_${MEC_PRIOR_NAME}_${MASK}_${RES}_${REG}_${MEC_init_time}.jpeg 2>/dev/null | wc -l`
      if [ "${MEC_QTD_FILES}" != "0" ] ; then
        echo "BESTOF SCORR ${MEC_var} ${MASK} ${RES} ${REG} ($MEC_QTD_FILES)"
        MEC_FNAME=${MEC_D_deploy}/IMG/Img_BESTOF_SCORR_${MEC_var}_${MEC_obs}_${MEC_PRIOR_NAME}_${MASK}_${RES}_${REG}_${MEC_init_time}.jpeg
        touch ${MEC_FNAME}; rm -f ${MEC_FNAME}
        montage -tile 1x ${MEC_var}_BESTOF_SCORR_${MEC_obs}_???_${MEC_PRIOR_NAME}_${MASK}_${RES}_${REG}_${MEC_init_time}.jpeg -geometry +1+1 ${MEC_FNAME}
      fi
      MEC_QTD_FILES=`ls -l ${MEC_var}_BESTOF_SRMSE_${MEC_obs}_???_${MEC_PRIOR_NAME}_${MASK}_${RES}_${REG}_${MEC_init_time}.jpeg 2>/dev/null | wc -l`
      if [ "${MEC_QTD_FILES}" != "0" ] ; then
        echo "BESTOF SRMSE ${MEC_var} ${MASK} ${RES} ${REG} ($MEC_QTD_FILES)"
        MEC_FNAME=${MEC_D_deploy}/IMG/Img_BESTOF_SRMSE_${MEC_var}_${MEC_obs}_${MEC_PRIOR_NAME}_${MASK}_${RES}_${REG}_${MEC_init_time}.jpeg
        touch ${MEC_FNAME}; rm -f ${MEC_FNAME}
        montage -tile 1x ${MEC_var}_BESTOF_SRMSE_${MEC_obs}_???_${MEC_PRIOR_NAME}_${MASK}_${RES}_${REG}_${MEC_init_time}.jpeg -geometry +1+1 ${MEC_FNAME}
      fi
      MEC_QTD_FILES=`ls -l ${MEC_var}_BESTOF_SBIAS_${MEC_obs}_???_${MEC_PRIOR_NAME}_${MASK}_${RES}_${REG}_${MEC_init_time}.jpeg 2>/dev/null | wc -l`
      if [ "${MEC_QTD_FILES}" != "0" ] ; then
        echo "BESTOF SBIAS ${MEC_var} ${MASK} ${RES} ${REG} ($MEC_QTD_FILES)"
        MEC_FNAME=${MEC_D_deploy}/IMG/Img_BESTOF_SBIAS_${MEC_var}_${MEC_obs}_${MEC_PRIOR_NAME}_${MASK}_${RES}_${REG}_${MEC_init_time}.jpeg
        touch ${MEC_FNAME}; rm -f ${MEC_FNAME}
        montage -tile 1x ${MEC_var}_BESTOF_SBIAS_${MEC_obs}_???_${MEC_PRIOR_NAME}_${MASK}_${RES}_${REG}_${MEC_init_time}.jpeg -geometry +1+1 ${MEC_FNAME}
      fi
    done
  done
done
cd ${MEC_dir_old}
