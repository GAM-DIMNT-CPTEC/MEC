mod=$1
var=$2
obs=$3

echo 
echo Gennerating OBS image ...
./MontageObs.ksh ${var} ${obs}

echo 
echo Gennerating OBS x FCT image ...
cd /dados/dmdpesq/roberto.garcia/MEC_DATA/${MEC_eval_name}/IMG
fname=REG21_${var}_${obs}_${mod}+GFS.jpeg
montage -tile 3x REG21_${var}_${obs}.jpeg Img_FCT_${var}_${mod}_202101_CONT_High_NREG_00.jpeg Img_FCT_${var}_GFS_30km_202101_CONT_High_NREG_00.jpeg -geometry +1+1 -gravity North ${fname}

echo 
echo Gennerating BIAS image ...
fname=REG21_Bias_${var}_${mod}+GFS.jpeg;
montage -tile 2x Img_BIAS_${var}_${mod}x${obs}_202101_CONT_High_NREG_00.jpeg Img_BIAS_${var}_GFS_30kmx${obs}_202101_CONT_High_NREG_00.jpeg -geometry +1+1 -gravity North ${fname}
cd /home/roberto.garcia/MEC-DEV
