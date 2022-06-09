# ==============================================================================
# JRM Garcia (roberto.garcia@inpe.br)
# DIMNT / CGCT / INPE / MCTI
# ------------------------------------------------------------------------------
# Preprocess the GFS analisys 
# ==============================================================================
MEC_st_get_obs=0
# Load model's particular configuration
source ${MEC_D_cfg}/GFS_30km.ksh

echo -n "* OBS: ${MEC_obs} (${MEC_var} analisys) preprocessing, "

MEC_anl_dt=$(date -d "${MEC_valid_date} ${MEC_init_time} + 1 day" "+%Y%m%d%H")
MEC_anl_yea=${MEC_anl_dt:0:4}
MEC_anl_mon=${MEC_anl_dt:4:2}
MEC_anl_day=${MEC_anl_dt:6:2}

# Changing date template by actual init date
MEC_ctl_model=`echo ${MEC_ctl_full_fname_model}                                   | \
               sed "s:YYYY:${MEC_anl_yea}:g" | sed "s:MM:${MEC_anl_mon}:g"        | \
               sed "s:DD:${MEC_anl_day}:g"   | sed "s:HH:${MEC_init_time}:g"`

if [[ ! -s ${MEC_ctl_model} ]] ; then
  echo "  *** PROBLEMAS NO DOWLOAD DO CTL DO ${MEC_obs} ***  "
  MEC_st_get_obs=1
  return ${MEC_st_get_obs}
fi

# ===========================================================================
# Verifica se é para mudar o range de lon de -180:180 para 0:360
# Procura por xdef no ctl original e se houver for < 0
# 1) Copia o CTL para o diretorio local, 
# 2) muda o inicio da longitude para 360+(xdef) [xdef é < 0]
# 3) substitui "^" pelo diretório do modelo pois vai ser usado localmente
# 4) muda o nome do MEC_ctl_model para o CTL local
# ===========================================================================
MEC_lon_ini_ori=`cat ${MEC_ctl_model} | grep -i xdef | tr -s ' ' | cut -d ' ' -f 4`
MEC_IS_LON_NEG=`echo "${MEC_lon_ini_ori} < 0" | bc`
if [[ 1 -eq ${MEC_IS_LON_NEG} ]]; then
  MEC_lon_ini_360=`echo 360 + ${MEC_lon_ini_ori} | bc`
  MEC_ctl_model_path=`readlink -f ${MEC_ctl_model} | xargs dirname`
  cat $MEC_ctl_model | sed "s:${MEC_lon_ini_ori}:${MEC_lon_ini_360}:g" | \
      sed "s:\^:${MEC_ctl_model_path}/:g" > ${MEC_D_temp}/GFS_30km_${MEC_init_date}.ctl
  MEC_ctl_model=${MEC_D_temp}/GFS_30km_${MEC_init_date}.ctl
fi

# If regrid mode is LOWEST, get observed and model resolutions
if [[ ${MEC_grid_res} = "LOWEST" ]]; then
   MEC_res_model=`cat ${MEC_ctl_model}  | grep xdef | tr -s ' ' | cut -d' ' -f5`
   MEC_res_obs=`cat ${MEC_ctl_obs}  | grep xdef | tr -s ' ' | cut -d' ' -f5`
    
   # Defining regriding direction
   if [[ ${MEC_res_model} -lt ${MEC_res_obs} ]]; then
     MEC_ctl_from=${MEC_ctl_model}
     MEC_ctl_to=${MEC_ctl_obs}
   else
     MEC_ctl_from=${MEC_ctl_obs}
     MEC_ctl_to=${MEC_ctl_model}
   fi
else
  if [[ "${MEC_grid_res}" =~ "ctl" ]]; then
     MEC_ctl_from=${MEC_ctl_model}
     MEC_ctl_to=${MEC_D_templ}/${MEC_grid_res}
  fi
fi
# Create an script to acummulate and regrid based on a template
MEC_model_out=${MEC_obs}_${MEC_var}_${MEC_valid_date}${MEC_init_time}
MEC_acc=MEC_${MEC_var}
eval MEC_acc=\$$MEC_acc   # Captura o valor da variável dada pela variavel

echo -n "reggriding, "
sed -e "s:CTL_MODEL:${MEC_ctl_from}:g" -e "s:CTL_OBS:${MEC_ctl_to}:g"                \
    -e "s:ACC_COMMAND:${MEC_acc}:g"    -e "s:DT:${MEC_valid_date}${MEC_init_time}:g" \
    -e "s:DIR_OUT:.:g"                 -e "s:MODELO:${MEC_model_out}:g"              \
    -e "s:LONGITUDE:${MEC_MCD_lon}:g"  -e "s:LATITUDE:${MEC_MCD_lat}:g"              \
    -e "s:OPEN_COMM:open:g"                                                          \
    ${MEC_D_grads}/GS_AccumModelAndRegrid_template.gs > ${MEC_D_temp}/GS_AccumModelAndRegrid.gs

# Accumulate the model in its original spatial domain and regrid the output to
# the MCD (Maximum Common Domain) among the involved models based on MERGE's 
# resolution
set | grep "^MEC_" > ${MEC_eval_name}/LAST_SET.ksh

cd ${MEC_D_temp}
grads -blc "run GS_AccumModelAndRegrid.gs" > ${MEC_D_log}/grads_GS_AccumModelAndRegrid_${MEC_obs}.log

if [[ ! -s ${MEC_model_out}.grib2 ]] ; then
  echo "ACCUMULATION OR REGRID PROBLEM"
  MEC_st_get_obs=2
  return ${MEC_st_get_obs}
else
  ./g2ctl ${MEC_model_out}.grib2 > ${MEC_model_out}.ctl
  gribmap -i ${MEC_model_out}.ctl > ${MEC_D_log}/gribmap_${MEC_obs}.log

  echo -n "converting to NetCDF, "
  # Convert to NetCDF
  cdo -s -f nc -selvar,param8.1.0 ${MEC_model_out}.grib2 TEMP_${MEC_obs}.nc

  # Delete length 1 dimension by averaging it (height)
  ncwa -O -a height TEMP_${MEC_obs}.nc TEMP2_${MEC_obs}.nc

  # Remove the variable "height"
  ncks -C -O -x -v height TEMP2_${MEC_obs}.nc TEMP3_${MEC_obs}.nc
  ncrename -O -v param8.1.0,${MEC_var} TEMP3_${MEC_obs}.nc ${MEC_D_dat_obs_ncdf}/${MEC_model_out}.nc > ${MEC_D_log}/ncrename_${MEC_obs}.log

  # Move GRIB2 or GRB to the output diretory ---------------------------------
  mv ${MEC_model_out}.grib2 ${MEC_D_dat_model_grib}/
  rm -f ${MEC_obs}_*
  echo "ok!"
fi
cd ${MEC_D_mec}

