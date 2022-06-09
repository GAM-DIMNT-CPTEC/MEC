cd /dados/dmdpesq/roberto.garcia/MEC_DATA/REG21/IMG

d=0
d2=30
while (( ${d} <= ${d2} )); do
  d_str=`printf "%02d" ${d}`
  h=36
  h2=252
  while (( ${h} <= ${h2} )); do
    echo Gen: Day: $d   HS: $h
    h_str=`printf "%03d" ${h}`
    display -delay 5 REG21-??-???.jpeg
    h=$(( ${h} + 24 ))
  done
  d=$(( ${d} + 1 ))
done
