valid_date=20210101
m1=${valid_date:4:2}
echo $m1
dt2=$(dt1 -d "+1 day" +%m)
dt3=$(date +%m)

echo $valid_date $dt1 $dt2 $dt3
#if [[ $(date -d "+1 day" +%m) != $(date +%m) ]]; then
#    echo "Today is the last day of the month"
#else
#    echo "Today is NOT the last day of the month"
#fi
