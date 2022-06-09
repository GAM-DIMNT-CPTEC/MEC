cd /dados/dmdpesq/roberto.garcia/MEC_DATA/REG21/IMG

rm -f REG21-??.jpeg

d=0
d2=30
while (( ${d} <= ${d2} )); do
  d_str=`printf "%02d" ${d}`
  echo Joining Day: $d_str

  montage -tile x30 REG21-${d_str}-???.jpeg -geometry +1+1 REG21-${d_str}.jpeg

  d=$(( ${d} + 1 ))
done


