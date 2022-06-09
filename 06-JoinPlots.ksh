#!/bin/ksh
# ==============================================================================
# JRM Garcia (roberto.garcia@inpe.br)
# DIMNT / CGCT / INPE / MCTI
# ------------------------------------------------------------------------------
# Plot fields: lasta 5 days, last month, last seson and last rainy season
# ==============================================================================

cd /home/roberto.garcia/MEC-DEV
MEC_debbug=false

if [ ${#} -ne 3 ]; then
  echo
  echo "Use:"
  echo
  echo "./06-JoinPlots.ksh <EVAL_NAME> <VAR> <INIT_TIME>"
  echo
  echo "<EVAL_NAME>   A name for the evaluation, root directory of the main output directory"
  echo "<VAR>         VARIABLE NAME"
  echo "<INIT_TIME>   00 | 12"
  echo
  exit 1
fi
export MEC_eval_name=${1}   # REG_2021 | Eta | ...
source ${MEC_eval_name}/DefEvalCfg.ksh
MEC_var=${2}
MEC_init_time=${3}   # 00 | 12

source ${MEC_eval_name}/EVAL_SET.ksh
source ${MEC_D_aux}/LateConfig.ksh    # Dependent configuation

# Força as datas para este caso
MEC_valid_date1="20210201"
MEC_valid_date2="20210201"

echo "================================================================================"
echo "MEC - Model Evaluation Comparator - Starting joining plots at `date \"+%Y-%m-%d %H:%M:%S\"`"
cat $MEC_D_aux/Signature.txt
echo "--------------------------------------------------------------------------------"
echo "FROM/TO..: ${MEC_valid_date1}...${MEC_valid_date2} ${MEC_init_time}h - STARTING"
echo "EVAL NAME: ${MEC_eval_name}   VARIABLE: ${MEC_var}    REGRID MODE: ${MEC_grid_res}"
echo "SP DOMAIN: ${MEC_sp_domain}   OBS: ${MEC_obs}"
echo "================================================================================"

####### LOOP ALONG VALID DATES #################################################
MEC_valid_date=${MEC_valid_date1}
while (( ${MEC_valid_date} <= ${MEC_valid_date2} )); do
  echo
  echo "-------------------------------------------------------------------------"
  echo "MEC - JOINING PLOTS - VALID DATE ${MEC_valid_date}"
  echo "-------------------------------------------------------------------------"

  # Check if current valid date is at least 5 days before
  #MEC_d1=$(date -d "${MEC_valid_date2}" +%s)
  #MEC_d2=$(date -d "${MEC_valid_date}" +%s)
  #MEC_days_diff=`echo $(( (MEC_d1 - MEC_d2) / 86400 ))`
  # Check if current valid date is the last eval valid date
  #if [[ ${MEC_valid_date} = ${MEC_valid_date2} ]]; then 
  #  source ./06.1-JoinDailyPlots.ksh
  #fi 

  if [[ "${MEC_valid_date:6:2}" = "01" ]]; then
    MEC_PRIOR_NAME=$(date -d "${MEC_valid_date:0:8} 12 - 1 day" "+%Y%m")
    source ./06.2-JoinPriorPlots.ksh

    # Verifica a estação do ano
    if [[ "${MEC_valid_date:4:2}" = "06" ]]; then # se 01/junho faz outono que acabou de acabar (MAM)
       MEC_PRIOR_NAME=${MEC_valid_date:0:4}.1-MAM
       source ./06.2-JoinPriorPlots.ksh
    fi
    if [[ "${MEC_valid_date:4:2}" = "09" ]]; then # se 01/setembro, faz inverno que acabou de acabar (JJA)
       MEC_PRIOR_NAME=${MEC_valid_date:0:4}.2-JJA
       source ./06.2-JoinPriorPlots.ksh
    fi
    if [[ "${MEC_valid_date:4:2}" = "12" ]]; then # se 01/dezembro, faz primavera que acabou de acabar (SON)
       MEC_PRIOR_NAME=${MEC_valid_date:0:4}.3-SON
       source ./06.2-JoinPriorPlots.ksh
    fi
    if [[ "${MEC_valid_date:4:2}" = "03" ]]; then # se 01/marco, faz verao que acabou de acabar (DJF)
       MEC_PRIOR_NAME=$(( ${MEC_valid_date:0:4} - 1 )).4-DJF
       source ./06.2-JoinPriorPlots.ksh
    fi
    if [[ "${MEC_valid_date:4:2}" = "04" ]]; then # se 01/abril, faz estacao chuvosa (NDJFM)
       MEC_PRIOR_NAME=$(( ${dt:0:4} - 1 )).5-NDJFM
       source ./06.2-JoinPriorPlots.ksh
    fi
  fi

  MEC_valid_date=$(date -d "${MEC_valid_date} + 1 day" "+%Y%m%d")
done # WHILE valid_date

echo "================================================================================"
echo "MEC - Model Evaluation Comparator - Finished joining plots at `date \"+%Y-%m-%d %H:%M:%S\"`"
cat $MEC_D_aux/Signature.txt
echo "--------------------------------------------------------------------------------"
echo "FROM/TO..: ${MEC_valid_date1}...${MEC_valid_date2} ${MEC_init_time}h - STARTING"
echo "EVAL NAME: ${MEC_eval_name}   VARIABLE: ${MEC_var}    REGRID MODE: ${MEC_grid_res}"
echo "SP DOMAIN: ${MEC_sp_domain}   OBS: ${MEC_obs}"
echo "================================================================================"


