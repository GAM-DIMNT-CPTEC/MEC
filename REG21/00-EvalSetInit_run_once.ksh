##!/bin/ksh
# ==============================================================================
# JRM Garcia (roberto.garcia@inpe.br)
# DIMNT / CGCT / INPE / MCTI
# ------------------------------------------------------------------------------
# Run this script once only, it creates the pertinent directories and builds 
# configuration scripts for this for both environments, shell and R.
# ==============================================================================

# The name of the evaluation, an ID (will be a subdir of the directory below)
MEC_eval_name="REG21"

# Wwhere all files wrt this evaluation are stored
MEC_D_data=/dados/dmdpesq/roberto.garcia/MEC_DATA

# Make it
MEC_D_root=${MEC_D_data}/${MEC_eval_name} # Root eval's directory
if [[ ! -d ${MEC_D_root} ]]; then
  echo "Root evaluation directory ${MEC_D_root} does not exist!"
  echo "Do you wish to create new evaluation root?"
  select NEW_EVAL in "Yes" "No"; do
    case ${NEW_EVAL} in
     Yes ) break;;
      No ) return 1;;
    esac
  done
fi


MEC_D_mec=/home/roberto.garcia/MEC-DEV   # Where MEC app runs
MEC_D_aux=${MEC_D_mec}/AUX/
MEC_D_cfg=${MEC_D_mec}/CONFIG/
MEC_D_grads=${MEC_D_mec}/GRADS/
MEC_D_common=/dados/dmdpesq/roberto.garcia/CommonData/
MEC_D_shp=${MEC_D_common}/shapefiles/

# Error filename
MEC_fname_err=ERRORS.txt

# Observation source, for each variable
MEC_obs_PREC=MERGE
MEC_obs_TMAX=SAMeT
MEC_obs_TMIN=SAMeT
MEC_obs_XXX=ANALYSIS

# Evaluation specific variables
MEC_D_eval=${MEC_D_root}/EVAL             # For evaluation RData files
MEC_D_img=${MEC_D_root}/IMG               # For image files
MEC_D_masks=${MEC_D_root}/MASKS           # For RData mask files
MEC_D_deploy=${MEC_D_root}/DEPLOY         # For aggregated RData evaluation files (ready to deploy)
MEC_D_log=${MEC_D_root}/LOG               # For logging
MEC_D_temp=${MEC_D_root}/TEMP             # Temporary
MEC_D_templ=${MEC_D_root}/TEMPLATE        # To keep templates
mkdir -p ${MEC_D_eval} ${MEC_D_img} ${MEC_D_masks} ${MEC_D_deploy} ${MEC_D_log} ${MEC_D_temp} ${MEC_D_templ}

# Copying preprocessing needed files
cp ${MEC_D_grads}/GS_g2grb_prec.gs ${MEC_D_temp}
cp ${MEC_D_grads}/g2ctl            ${MEC_D_temp}
cp ${MEC_D_grads}/template.grb2    ${MEC_D_temp}
cp ${MEC_D_mec}/TEMPLATE/*         ${MEC_D_templ}
cp ${MEC_D_mec}/MASKS/*            ${MEC_D_masks}
cp ${MEC_D_grads}/wgrib2           ${MEC_D_temp}

set | grep "^MEC_" > EVAL_SET.ksh

# =====================================================
# Creating a R scrip with the exactly definitions above
# =====================================================
fname_r=EVAL_SET.R
echo "# ======================================================="    >  ${fname_r}
echo "# R script created by `basename "$0"`"                        >> ${fname_r}
echo "# This means that these directories are already created. "    >> ${fname_r}
echo "# and replicates the definitions in EVAL_SET.ksh         "    >> ${fname_r}
echo "# ======================================================="    >> ${fname_r}
for i in `set | grep "^MEC_"`; do
 echo "$i\"" | sed 's/=/=\"/'    >> ${fname_r}
done
