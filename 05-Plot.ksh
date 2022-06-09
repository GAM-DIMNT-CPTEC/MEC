#!/bin/ksh
# ==============================================================================
# JRM Garcia (roberto.garcia@inpe.br)
# DIMNT / CGCT / INPE / MCTI
# ------------------------------------------------------------------------------
# Plot fields: lasta 5 days, last month, last seson and last rainy season
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

####### LOOP ALONG VALID DATES #################################################
MEC_valid_date=${MEC_valid_date1}
while (( ${MEC_valid_date} <= ${MEC_valid_date2} )); do
  MEC_val_yea=${MEC_valid_date:0:4}
  MEC_val_mon=${MEC_valid_date:4:2}
  MEC_val_day=${MEC_valid_date:6:2}

  # Assuring that the evaluation's output directories exist
  source ${MEC_D_aux}/CheckDirs.ksh

  # Check if it is day "01" in order to plot previous month or season
  MEC_PRIOR_MONTH=NA
  MEC_PRIOR_NAME=NA
  if [[ "${MEC_valid_date:6:2}" = "01" ]]; then
     MEC_PRIOR_MONTH=$(date -d "${MEC_valid_date} - 1 day" "+%Y%m")

     # Check if it is also previous season to plot
     cur_mon=${MEC_valid_date:4:2}
     if [[ "${cur_mon}" =~ ^(06|09|12|03)$ ]]; then
        MEC_PRIOR_NAME=SAZONAL
     fi
     if [[ "${cur_mon}" = "04" ]]; then 
        MEC_PRIOR_NAME=RAINY
     fi
  fi

  ####### LOOP ALONG MODELS #####################################################
  for MEC_model in "${MEC_models[@]}"; do
    echo
    echo "-------------------------------------------------------------------------"
    echo "MEC - PLOT - VALID DATE ${MEC_valid_date} - MODEL: ${MEC_model}"
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

      #Rscript 05a-PlotDaily.R ${MEC_eval_name} ${MEC_model} ${MEC_var} `printf "%03d" ${MEC_hs_fct}` \
      #                        ${MEC_valid_date}${MEC_init_time} ${MEC_obs} ${MEC_eval_mode}

#      if [[ "${MEC_PRIOR_MONTH}" != "NA" ]]; then
#         Rscript 05b-PlotPrevious.R ${MEC_eval_name} ${MEC_model} ${MEC_var} `printf "%03d" ${MEC_hs_fct}` \
#                                    ${MEC_valid_date1} ${MEC_valid_date2} ${MEC_init_time} ${MEC_obs} ${MEC_eval_mode} ${MEC_PRIOR_MONTH}
#      fi
      if [[ "${MEC_PRIOR_NAME}" != "NA" ]]; then
         Rscript 05b-PlotPrevious.R ${MEC_eval_name} ${MEC_model} ${MEC_var} `printf "%03d" ${MEC_hs_fct}` \
                                  ${MEC_valid_date1} ${MEC_valid_date2} ${MEC_init_time} ${MEC_obs} ${MEC_eval_mode} ${MEC_PRIOR_NAME}
      fi
      MEC_hs_fct=$(( ${MEC_hs_fct} + 24 ))
    done # WHILE hs_fct
  done # FOR model

  # Best of consolidated fields here ????
  #if [[ "${MEC_PRIOR_MONTH}" != "NA" ]]; then
  #  ./05c-PlotSpBestOf.ksh ${MEC_eval_name} ${MEC_var} ${MEC_init_time} ${MEC_PRIOR_MONTH} ${MEC_PRIOR_NAME}
  #fi

  MEC_valid_date=$(date -d "${MEC_valid_date} + 1 day" "+%Y%m%d")
done # WHILE valid_date

Rscript 05d-JoinSummaryDSs.R ${MEC_eval_name} ${MEC_var} ${MEC_init_time}

echo "================================================================================"
echo "MEC - Model Evaluation Comparator - Finished ploting at `date \"+%Y-%m-%d %H:%M:%S\"`"
cat $MEC_D_aux/Signature.txt
echo "--------------------------------------------------------------------------------"
echo "FROM/TO..: ${MEC_valid_date1}...${MEC_valid_date2} ${MEC_init_time}h - STARTING"
echo "EVAL NAME: ${MEC_eval_name}   VARIABLE: ${MEC_var}    REGRID MODE: ${MEC_grid_res}"
echo "SP DOMAIN: ${MEC_sp_domain}   OBS: ${MEC_obs}"
echo "================================================================================"

./06-JoinPlots.ksh ${MEC_eval_name} ${MEC_var} ${MEC_init_time}
