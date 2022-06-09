#!/bin/ksh
# ==============================================================================
# JRM Garcia (roberto.garcia@inpe.br)
# DIMNT / CGCT / INPE / MCTI
# ------------------------------------------------------------------------------
# Plot consolidated fields: last month, last season, last rainy season, custom period
# ==============================================================================

cd /home/roberto.garcia/MEC-DEV
MEC_debbug=false

if [ ${#} -ne 4 ]; then
  echo
  echo "Use:"
  echo
  echo "./05-Plot.ksh <EVAL_NAME> <VAR> <INIT_TIME> <EVAL_MODE>"
  echo
  echo "<EVAL_NAME>   A name for the evaluation, root directory of the main output directory"
  echo "<VAR>         VARIABLE NAME"
  echo "<INIT_TIME>   00 | 12"
  echo "<EVAL_MODE>   1=eval only, 2=eval & CI, 3=CI only"
  echo
  exit 1
fi

export MEC_eval_name=${1}   # REG_2021 | Eta | ...
source ${MEC_eval_name}/DefEvalCfg.ksh
MEC_var=${2}
MEC_init_time=${3}   # 00 | 12
MEC_eval_mode=${4}   # 1 | 2 | 3

source ${MEC_eval_name}/EVAL_SET.ksh
source ${MEC_D_aux}/LateConfig.ksh    # Dependent configuration

echo "================================================================================"
echo "MEC - Model Evaluation Comparator - Starting Ploting at `date \"+%Y-%m-%d %H:%M:%S\"`"
cat $MEC_D_aux/Signature.txt
echo "--------------------------------------------------------------------------------"
echo "FROM/TO..: ${MEC_valid_date1}...${MEC_valid_date2} ${MEC_init_time}h - STARTING"
echo "EVAL NAME: ${MEC_eval_name}   VARIABLE: ${MEC_var}    REGRID MODE: ${MEC_grid_res}"
echo "SP DOMAIN: ${MEC_sp_domain}   OBS: ${MEC_obs}"
echo "================================================================================"

####### LOOP ALONG MODELS #####################################################
for MEC_model in "${MEC_models[@]}"; do
  echo
  echo "-------------------------------------------------------------------------"
  echo "MEC - PLOTING PERIOD: ${MEC_valid_date1} to ${MEC_valid_date2} - MODEL: ${MEC_model}"
  echo "-------------------------------------------------------------------------"
  # Formato de algumas entradas: GRIB ou NCDF
  if [[ "${MEC_model}" == "WRFG_07km" || "${MEC_model}" == "WRF_07km" ]]; then
    MEC_model_data_format="NCDF"
  else
    MEC_model_data_format="GRIB"
  fi

  # Unset previous lead time variables
  MEC_val_days_fct=`set | grep MEC_${MEC_var}_ | wc -l`
  if [[ ${MEC_val_days_fct} != "0" ]]; then
    unset `set | grep "^MEC_${MEC_var}_" | sed 's;=.*;;' | sort`
  fi

  # Load model's particular configuration
  source ${MEC_D_cfg}/${MEC_model}.ksh

  # Amount of lead time (3 days, 11, days, ...)
  # if NCDF it must be set in model's config file
  if [[ "${MEC_model_data_format}" != "NCDF" ]]; then
    MEC_val_days_fct=`set | grep MEC_${MEC_var}_ | wc -l`
  fi
  # Ending lead time
  MEC_hs_fct_end=$(( ${MEC_hs_fct_beg} + (24 * (${MEC_val_days_fct}-1)) ))

  ####### LOOP ALONG MODEL`S FORECASTING HORIZON ##############################
  MEC_hs_fct=${MEC_hs_fct_beg}
  while (( ${MEC_hs_fct} <= ${MEC_hs_fct_end} )); do
    # Build a R script with the last iteration values
    # Automatically captures when running in the interactive mode (R prompt)
    FNAME_DEF_CFG="${MEC_eval_name}/LastEvalCfg.R"
    printf '%s\n'                                          \
           "CFG\$eval_name='"${MEC_eval_name}"'"           \
           "CFG\$model='"${MEC_model}"'"                   \
           "CFG\$var='"${MEC_var}"'"                       \
           "CFG\$ltime='"`printf "%03d" ${MEC_hs_fct}`"'"  \
           "CFG\$val_dt1='"${MEC_valid_date1}"'"           \
           "CFG\$val_dt2='"${MEC_valid_date2}"'"           \
           "CFG\$init_time='"${MEC_init_time}"'"           \
           "CFG\$obs='"${MEC_obs}"'"                       \
           "CFG\$oper='"EVAL"'"                            \
           "CFG\$eval_mode='"${MEC_eval_mode}"'" > ${FNAME_DEF_CFG}

    Rscript 05b-PlotPrevious.R ${MEC_eval_name} ${MEC_model} ${MEC_var} `printf "%03d" ${MEC_hs_fct}` \
                                  ${MEC_valid_date1} ${MEC_valid_date2} ${MEC_init_time} ${MEC_obs} ${MEC_eval_mode}
    MEC_hs_fct=$(( ${MEC_hs_fct} + 24 ))
  done # WHILE hs_fct
done # FOR model

#Rscript 05d-JoinSummaryDSs.R ${MEC_eval_name} ${MEC_var} ${MEC_init_time}

echo "================================================================================"
echo "MEC - Model Evaluation Comparator - Finished ploting at `date \"+%Y-%m-%d %H:%M:%S\"`"
cat $MEC_D_aux/Signature.txt
echo "--------------------------------------------------------------------------------"
echo "FROM/TO..: ${MEC_valid_date1}...${MEC_valid_date2} ${MEC_init_time}h - STARTING"
echo "EVAL NAME: ${MEC_eval_name}   VARIABLE: ${MEC_var}    REGRID MODE: ${MEC_grid_res}"
echo "SP DOMAIN: ${MEC_sp_domain}   OBS: ${MEC_obs}"
echo "================================================================================"

#./06-JoinPlots.ksh ${MEC_eval_name} ${MEC_var} ${MEC_init_time}
