cd /dados/dmdpesq/roberto.garcia/MEC_DATA/${MEC_eval_name}/IMG

var=$1
obs=$2
imgs=`for i in {1..9}; do echo -n "${var}_${obs}_202101_CONT_High_NREG_00.jpeg "; done`
montage -tile 1x ${imgs} -geometry +1+1 -gravity North REG21_${var}_${obs}.jpeg

cd /home/roberto.garcia/MEC-DEV
