#!/bin/ksh
for dd in $(seq 1 31); do
  d_str=`printf "%02d" ${dd}`
  nf=`ls -l /oper/ioper/tempo/GFS/0p25/brutos/2020/12/${d_str}/00/*.grib2 | wc -l`
  if [[ ${nf} -lt 84 ]]; then
     echo GFS Dia ${d_str}/12/2020 ${nf} gribs
  fi
done
for dd in $(seq 1 31); do
  d_str=`printf "%02d" ${dd}`
  nf=`ls -l /oper/ioper/tempo/GFS/0p25/brutos/2021/01/${d_str}/00/*.grib2 | wc -l`
  if [[ ${nf} -lt 84 ]]; then
     echo GFS Dia ${d_str}/01/2021 ${nf} gribs
  fi
done

