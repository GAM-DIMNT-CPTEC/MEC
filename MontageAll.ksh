var=${1}
obs=${2}
cd /dados/dmdpesq/roberto.garcia/MEC_DATA/${MEC_eval_name}/IMG
fname=REG21_${var}_${obs}_AllMod.jpeg
montage -tile 5x REG21_${var}_${obs}.jpeg Img_FCT_${var}_BRAMS_08km_202101_CONT_High_NREG_00.jpeg Img_FCT_${var}_Eta_08km_202101_CONT_High_NREG_00.jpeg Img_FCT_${var}_WRFG_07km_202101_CONT_High_NREG_00.jpeg Img_FCT_${var}_WRF_07km_202101_CONT_High_NREG_00.jpeg Img_FCT_${var}_GFS_30km_202101_CONT_High_NREG_00.jpeg -geometry +1+1 -gravity North ${fname}


fname=REG21_Bias_${var}_AllMod.jpeg
montage -tile 4x Img_BIAS_${var}_BRAMS_08kmx${obs}_202101_CONT_High_NREG_00.jpeg Img_BIAS_${var}_Eta_08kmx${obs}_202101_CONT_High_NREG_00.jpeg Img_BIAS_${var}_WRF_07kmx${obs}_202101_CONT_High_NREG_00.jpeg Img_BIAS_${var}_GFS_30kmx${obs}_202101_CONT_High_NREG_00.jpeg -geometry +1+1 -gravity North ${fname}

cd /home/roberto.garcia/MEC-DEV
