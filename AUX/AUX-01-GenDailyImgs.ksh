cd /dados/dmdpesq/roberto.garcia/MEC_DATA/REG21/IMG

rm -f REG21-??-???.jpeg

d=0
d2=30
while (( ${d} <= ${d2} )); do
  d_str=`printf "%02d" ${d}`
  h=36
  h2=252
  while (( ${h} <= ${h2} )); do
    h_str=`printf "%03d" ${h}`
    echo Gennerating Day: $d_str   HS: $h_str
    montage -tile 5x PREC_MERGE_D-${d}_ALL_High_REG_00.jpeg \
                     PREC_BRAMS_08km_${h_str}_D-${d}_ALL_High_REG_00.jpeg \
                     PREC_Eta_08km_${h_str}_D-${d}_ALL_High_REG_00.jpeg \
                     PREC_WRF_07km_${h_str}_D-${d}_ALL_High_REG_00.jpeg \
                     PREC_GFS_30km_${h_str}_D-${d}_ALL_High_REG_00.jpeg \
            -geometry 350x450+1+1 -gravity North REG21-${d_str}-${h_str}.jpeg
    h=$(( ${h} + 24 ))
  done
  d=$(( ${d} + 1 ))
done

