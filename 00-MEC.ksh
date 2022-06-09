#!/bin/ksh
# ==============================================================================
# JRM Garcia (roberto.garcia@inpe.br)
# DIMNT / CGCT / INPE / MCTI
# ------------------------------------------------------------------------------
# Script that starts the evaluation of NWPSs
# ==============================================================================
cd /home/roberto.garcia/MEC-DEV
MEC_debbug=false

if [[ ${#} -ne 1 ]] && [[ ${#} -ne 7 ]]; then
  cat AUX/Sintaxe.txt
  exit 1
else
  export MEC_eval_name=${1}   # REG21 | Eta | ...
  #if [ ${#} -ne 7 ]; then
  source ${MEC_eval_name}/DefEvalCfg.ksh
  #else 
  MEC_var=${2}
  MEC_init_time=${3}
  MEC_valid_date1=${4}
  MEC_valid_date2=${5}
  MEC_grid_res=${6}
  MEC_sp_domain=${7}
  #fi
fi
source ${MEC_eval_name}/EVAL_SET.ksh
source ${MEC_D_aux}/LateConfig.ksh    # Dependent configuation

#set | grep "^MEC_" > LAST_SET_${MEC_eval_name}.ksh; cat LAST_SET_${MEC_eval_name}.ksh

echo "================================================================================"
echo "MEC - Model Evaluation Comparator - Starting PREPROC at `date \"+%Y-%m-%d %H:%M:%S\"`"
cat $MEC_D_aux/Signature.txt
echo "--------------------------------------------------------------------------------"
echo "FROM/TO..: ${MEC_valid_date1}...${MEC_valid_date2} ${MEC_init_time}h - STARTING"
echo "EVAL NAME: ${MEC_eval_name}   VARIABLE: ${MEC_var}    REGRID MODE: ${MEC_grid_res}"
echo "SP DOMAIN: ${MEC_sp_domain}   OBS: ${MEC_obs}"
echo "================================================================================"

MEC_valid_date=${MEC_valid_date1}
while (( ${MEC_valid_date} <= ${MEC_valid_date2} )); do
  MEC_val_yea=${MEC_valid_date:0:4}
  MEC_val_mon=${MEC_valid_date:4:2}
  MEC_val_day=${MEC_valid_date:6:2}

  # Assuring that the evaluation's output directories exist
  source ${MEC_D_aux}/CheckDirs.ksh

  # =================================================
  # Definition of MCD (Maximum Common Domain)
  # This values matches XY grid points in both observed sources
  # See README_MCD.txt
  # =================================================
  MEC_MCD_lon="277.05 326.95"  # LON IN 0:360
  MEC_MCD_lon2="-82.95 -33.05" # LON IN -180:180
  MEC_MCD_lat="-47.05 12.95"

  echo
  echo "-------------------------------------------------------------------------"
  echo "MEC - PREPROCESSING - VALID DATE ${MEC_valid_date}"
  echo "-------------------------------------------------------------------------"

  set | grep "^MEC_" > ${MEC_eval_name}/LAST_SET.ksh
  # Preprocess observed
  if [[ ${MEC_var} = "PREC" ]]; then
    source ./01.1-PreprocMERGE.ksh  # Must have a return value (0=ok)
  fi
  if [[ ${MEC_var} = "TMAX" || ${MEC_var} = "TMIN" ]]; then
    source ./01.2-PreprocSAMeT.ksh  # Must have a return value (0=ok)
  fi
  if [[ ${MEC_var} = "U10M" || ${MEC_var} = "V10M" || ${MEC_var} = "VEL10M" || ${MEC_var} = "MSLP" ]]; then
    source ./01.3-PreprocAnlGFS.ksh  # Must have a return value (0=ok)
  fi
  if [[ ${MEC_st_get_obs} != 0 ]]; then
    MEC_valid_date=$(date -d "${MEC_valid_date} + 1 day" "+%Y%m%d")   # Increment the day
    continue
  fi

  for MEC_model in "${MEC_models[@]}"; do
    source ./02-PreprocModels.ksh
  done

  cd ${MEC_D_mec}
  set | grep "^MEC_" > ${MEC_eval_name}/LAST_SET.ksh
  echo ${MEC_valid_date} > ${MEC_eval_name}/LAST_EVAL_DT.txt

  MEC_valid_date=$(date -d "${MEC_valid_date} + 1 day" "+%Y%m%d")
done

if [[ "${MEC_var}" = "PREC" ]]; then
  # Moving observed to output directory and deleting the remaining files
  mv -f ${MEC_D_temp}/${MEC_obs}_${MEC_var}_*.grib2 ${MEC_D_dat_obs_grib}/
  rm -f ${MEC_D_temp}/${MEC_obs}_${MEC_var}_*.ctl
  rm -f ${MEC_D_temp}/${MEC_obs}_${MEC_var}_*.idx
  rm -f ${MEC_D_temp}/${MEC_obs}_${MEC_var}_*.template
fi

echo
echo "================================================================================"
echo "MEC - Model Evaluation Comparator - Finished preproc at `date \"+%Y-%m-%d %H:%M:%S\"`"
cat $MEC_D_aux/Signature.txt
echo "--------------------------------------------------------------------------------"
echo "FROM/TO..: ${MEC_valid_date1}...${MEC_valid_date2} ${MEC_init_time}h - THE END!"
echo "EVAL NAME: ${MEC_eval_name}   VARIABLE: ${MEC_var}    REGRID MODE: ${MEC_grid_res}"
echo "SP DOMAIN: ${MEC_sp_domain}   OBS: ${MEC_obs}"
echo "================================================================================"
echo

#./03-Eval.ksh ${MEC_eval_name} ${MEC_eval_mode}

# Deploy the application at https://garcia-cptec.shinyapps.io/CPTEC_DAILY/
#./08-Deploy.ksh

# Send email that the process has finished to whom it concerns
#./10-SendEmail.ksh

