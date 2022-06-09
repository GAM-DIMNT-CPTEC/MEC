#!/bin/ksh
# ==============================================================================
# JRM Garcia (roberto.garcia@inpe.br)
# DIMNT / CGCT / INPE / MCTI
# ------------------------------------------------------------------------------
# Plot best of consolidated fields
# ==============================================================================

cd /home/roberto.garcia/MEC-DEV
MEC_debbug=false

if [ ${#} -ne 5 ]; then
  echo
  echo "Use:"
  echo
  echo "./05c-PlotSpBestOf.ksh <EVAL_NAME> <VAR> <INIT_TIME> <LAST_MONTH> <LAST_SEASON>"
  echo
  echo "<EVAL_NAME>      A name for the evaluation, root directory of the main output directory"
  echo "<VAR>            VARIABLE NAME"
  echo "<INIT_TIME>      00 | 12"
  echo "<LAST_MONTH>     Reference year + month of the summarized fields"
  echo "<LAST_SEASON>    Reference ID of the last season to of the summarized fields"
  echo
  exit 1
fi

export MEC_eval_name=${1}   # REG_2021 | Eta | ...
source ${MEC_eval_name}/DefEvalCfg.ksh
MEC_var=${2}
MEC_init_time=${3}   # 00 | 12
MEC_last_month=${4}
MEC_last_season=${5}

source ${MEC_eval_name}/EVAL_SET.ksh
source ${MEC_D_aux}/LateConfig.ksh    # Dependent configuration

# Controls consolidate plots
echo
echo "================================================================================"
echo "MEC - BestOf ploting in ${MEC_last_month}"
echo "================================================================================"

####### LOOP ALONG MODELS #####################################################
echo -n "* Getting max lead time: "
MEC_MAX_LTIME=0
for MEC_model in "${MEC_models_cmp[@]}"; do
  # Unset previous lead time variables
  MEC_val_days_fct=`set | grep MEC_${MEC_var}_ | wc -l`
  if [[ ${MEC_val_days_fct} != "0" ]]; then
    unset `set | grep "^MEC_${MEC_var}_" | sed 's;=.*;;' | sort`
  fi

  # Load model's particular configuration
  source ${MEC_D_cfg}/${MEC_model}.ksh

  # Amount of lead time (3 days, 11, days, ...)
  MEC_val_days_fct=`set | grep MEC_${MEC_var}_ | wc -l` 
  if [[ ${MEC_val_days_fct} > ${MEC_MAX_LTIME} ]]; then
    MEC_MAX_LTIME=${MEC_val_days_fct}
  fi
done
echo "${MEC_MAX_LTIME}, Ok!"

MEC_models_cmp=`echo ${MEC_models_cmp[@]}`

echo "* Processing all forecasting horizon"
#    ####### LOOP ALONG FORECASTING HORIZON ##############################
MEC_hs_fct=${MEC_hs_fct_beg}
MEC_hs_fct_end=$(( ${MEC_MAX_LTIME} * 24 ))
while (( ${MEC_hs_fct} <= ${MEC_hs_fct_end} )); do
  MEC_hs_fct_str=`printf "%03d" ${MEC_hs_fct}`
  echo "  ${MEC_hs_fct_str}h forecast"

  # Build a R script with the last iteration values
  # Automatically captures when running in the interactive mode (R prompt)
  FNAME_DEF_CFG="${MEC_eval_name}/LastEvalCfg.R"
  printf '%s\n'                                          \
         "CFG\$eval_name='"${MEC_eval_name}"'"           \
         "CFG\$model='"${MEC_model}"'"                   \
         "CFG\$var='"${MEC_var}"'"                       \
         "CFG\$ltime='"`printf "%03d" ${MEC_hs_fct}`"'"  \
         "CFG\$dt='"${MEC_valid_date2}"'"                \
         "CFG\$init_time='"${MEC_init_time}"'"           \
         "CFG\$obs='"${MEC_obs}"'"                       \
         "CFG\$oper='"EVAL"'"                            \
         "CFG\$eval_mode='"${MEC_eval_mode}"'" > ${FNAME_DEF_CFG}

  Rscript 05c-PlotSpBestOf.R ${MEC_eval_name} ${MEC_var} ${MEC_hs_fct_str} \
         "${MEC_models_cmp}" ${MEC_init_time} ${MEC_last_month} ${MEC_last_season}

  MEC_hs_fct=$(( ${MEC_hs_fct} + 24 ))
done


