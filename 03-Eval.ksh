#!/bin/ksh
# ==============================================================================
# JRM Garcia (roberto.garcia@inpe.br)
# DIMNT / CGCT / INPE / MCTI
# ------------------------------------------------------------------------------
# Script that starts the evaluation of NWPSs
# ==============================================================================
cd /home/roberto.garcia/MEC-DEV
MEC_debbug=false

if [ ${#} -ne 4 ]; then
  echo
  echo "Use:"
  echo
  echo "./03-Eval.ksh <EVAL_NAME> <VAR> <INIT_TIME> <EVAL_MODE>"
  echo
  echo "<EVAL_NAME>   A name of the evaluation, e.g. REG21 | Eta | ... "
  echo "<VAR>         Variable name, e.g. PREC | TMAX | TMIN | ..."
  echo "<INIT_TIME>   Model's initialization time, e.g. 00 | 12"
  echo "<EVAL_MODE>   1=eval only, 2=eval & CI, 3=CI only"
  echo
  exit 1
fi

export MEC_eval_name=${1}   # REG21 | REG21T | Eta | ...
source ${MEC_eval_name}/DefEvalCfg.ksh
MEC_var=${2}
MEC_init_time=${3}   # 00 | 12
MEC_eval_mode=${4}   # 1 | 2 | 3

source ${MEC_eval_name}/EVAL_SET.ksh
source ${MEC_D_aux}/LateConfig.ksh    # Dependent configuation

# Cria o DS de eventos dos modelos para mostrar na interface
Rscript 03a-Events.R

echo "================================================================================"
echo "MEC - Model Evaluation Comparator - Starting Evaluation at `date \"+%Y-%m-%d %H:%M:%S\"`"
cat $MEC_D_aux/Signature.txt
echo "--------------------------------------------------------------------------------"
echo "FROM/TO..: ${MEC_valid_date1}...${MEC_valid_date2} ${MEC_init_time}h - STARTING"
echo "EVAL NAME: ${MEC_eval_name}   VARIABLE: ${MEC_var}    REGRID MODE: ${MEC_grid_res}"
echo "SP DOMAIN: ${MEC_sp_domain}   OBS: ${MEC_obs}"
echo "================================================================================"

####### LOOP ALONG VALID DATES #################################################
MEC_valid_date=${MEC_valid_date1}
while (( ${MEC_valid_date} <= ${MEC_valid_date2} )); do
  MEC_val_yea=${MEC_valid_date:0:4}
  MEC_val_mon=${MEC_valid_date:4:2}
  MEC_val_day=${MEC_valid_date:6:2}

  # Assuring that the evaluation's output directories exist
  source ${MEC_D_aux}/CheckDirs.ksh

  echo
  echo "-------------------------------------------------------------------------"
  echo "MEC - EVALUATION - VALID DATE ${MEC_valid_date}"
  echo "-------------------------------------------------------------------------"

  ####### LOOP ALONG MODELS #####################################################
  for MEC_model in "${MEC_models[@]}"; do
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
             "CFG\$dt='"${MEC_valid_date}"'"                 \
             "CFG\$init_time='"${MEC_init_time}"'"           \
             "CFG\$obs='"${MEC_obs}"'"                       \
             "CFG\$oper='"EVAL"'"                            \
             "CFG\$eval_mode='"${MEC_eval_mode}"'" > ${FNAME_DEF_CFG}

      Rscript 03b-Eval.R ${MEC_eval_name} ${MEC_model} ${MEC_var} `printf "%03d" ${MEC_hs_fct}` \
                         ${MEC_valid_date}${MEC_init_time} ${MEC_obs} ${MEC_eval_mode}

      MEC_hs_fct=$(( ${MEC_hs_fct} + 24 ))
    done
  done

  # If it is the last day of the month, process period evaluation (MONTH, 3 MONTHS, SEASON, RAINY)
  m1=${MEC_valid_date:4:2}
  m2=$(date -d "${MEC_valid_date} + 1 day" "+%m")

  MEC_valid_date=$(date -d "${MEC_valid_date} + 1 day" "+%Y%m%d")
done

echo
echo "================================================================================"
echo "MEC - Model Evaluation Comparator - Finished evaluation at `date \"+%Y-%m-%d %H:%M:%S\"`"
cat $MEC_D_aux/Signature.txt
echo "--------------------------------------------------------------------------------"
echo "FROM/TO..: ${MEC_valid_date1}...${MEC_valid_date2} ${MEC_init_time}h - THE END!"
echo "EVAL NAME: ${MEC_eval_name}   VARIABLE: ${MEC_var}    REGRID MODE: ${MEC_grid_res}"
echo "SP DOMAIN: ${MEC_sp_domain}   OBS: ${MEC_obs}"
echo "================================================================================"

Rscript 04-JoinDatasets.R ${MEC_eval_name} ${MEC_var} ${MEC_init_time}

# Deploy the application at https://garcia-cptec.shinyapps.io/CPTEC_DAILY/
#./08-Deploy.ksh

# Send email that the process has finished to whom it concerns
#./10-SendEmail.ksh

