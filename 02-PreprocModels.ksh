# ==============================================================================
# JRM Garcia (roberto.garcia@inpe.br)
# DIMNT / CGCT / INPE / MCTI
# ------------------------------------------------------------------------------
# Preprocess the model
# ==============================================================================
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
echo
echo "MODEL: ${MEC_model} (${MEC_val_days_fct} dias)  at  ${MEC_ctl_full_path_model} **********"

# Loop on the model's forecasting horizon
MEC_hs_fct=${MEC_hs_fct_beg}
while (( ${MEC_hs_fct} <= ${MEC_hs_fct_end} )); do
  MEC_ndays_back=$((MEC_hs_fct / 24))
  MEC_init_date=$(date -d "${MEC_valid_date} ${MEC_init_time} - ${MEC_ndays_back} day" "+%Y%m%d%H")
  MEC_ini_yea=${MEC_init_date:0:4}
  MEC_ini_mon=${MEC_init_date:4:2}
  MEC_ini_day=${MEC_init_date:6:2}

  MEC_hs_fct2=`printf "%03d" ${MEC_hs_fct}`
  MEC_err=""
  MEC_line_out="${MEC_ndays_back}d (${MEC_hs_fct2}h):"

  # Changing date template by actual init date
  if [[ "${MEC_model_data_format}" != "NCDF" ]]; then
     VALID_TIME=${MEC_init_time}
  else
     if [[ "${MEC_var}" == "PREC" ]]; then
        VALID_TIME=12
     else
        VALID_TIME=${MEC_init_time}
     fi
  fi
      
  # Works for CTL/GRIB and NETCDF
  MEC_ctl_model=`echo ${MEC_ctl_full_fname_model}                                | \
                 sed "s:YYYY:${MEC_ini_yea}:g" | sed "s:MM:${MEC_ini_mon}:g"     | \
                 sed "s:DD:${MEC_ini_day}:g"   | sed "s:HH:${VALID_TIME}:g"      | \
                 sed "s:FCT_LT:${MEC_hs_fct2}:g"`
  MEC_line_out="${MEC_line_out} InitDt=${MEC_init_date} "
  if [[ ! -s ${MEC_ctl_model} ]] ; then
    MEC_line_out="${MEC_line_out} MODEL CTL or NCDF NOT FOUND!"
    echo ${MEC_ctl_model}
    MEC_err="CTL or NCDF MODEL NOT FOUND"
  fi

  if [[ -z ${MEC_err} ]]; then
    # ===========================================================================
    # Verifica se é para mudar o range de lon de -180:180 para 0:360
    # Procura por xdef no ctl original e se houver for < 0
    # 1) Copia o CTL para o diretorio local, 
    # 2) muda o inicio da longitude para 360+(xdef) [xdef é < 0]
    # 3) substitui "^" pelo diretório do modelo pois vai ser usado localmente
    # 4) muda o nome do MEC_ctl_model para o CTL local
    # ===========================================================================
    if [[ "${MEC_model_data_format}" != "NCDF" ]]; then
      MEC_lon_ini_ori=`cat ${MEC_ctl_model} | grep -i xdef | tr -s ' ' | cut -d ' ' -f 4`
      MEC_IS_LON_NEG=`echo "${MEC_lon_ini_ori} < 0" | bc`
      if [[ 1 -eq ${MEC_IS_LON_NEG}  ]]; then
        MEC_lon_ini_360=`echo 360 + ${MEC_lon_ini_ori} | bc`
        MEC_ctl_model_path=`readlink -f ${MEC_ctl_model} | xargs dirname`
        cat $MEC_ctl_model | sed "s:${MEC_lon_ini_ori}:${MEC_lon_ini_360}:g" | \
            sed "s:\^:${MEC_ctl_model_path}/:g" > ${MEC_D_temp}/${MEC_model}_${MEC_init_date}.ctl
        MEC_ctl_model=${MEC_D_temp}/${MEC_model}_${MEC_init_date}.ctl
        MEC_line_out="${MEC_line_out} IniLonAdj=${MEC_lon_ini_360}"
      fi
    else
      # NC's lon is -180:180
      MEC_MCD_lon=${MEC_MCD_lon2}
    fi

    # If regrid mode is LOWEST, get observed and model resolutions
    if [[ ${MEC_grid_res} = "LOWEST" ]]; then
       MEC_res_model=`cat ${MEC_ctl_model}  | grep xdef | tr -s ' ' | cut -d' ' -f5`
       MEC_res_obs=`cat ${MEC_ctl_obs}  | grep xdef | tr -s ' ' | cut -d' ' -f5`
        
       # Defining regriding direction
       if [[ ${MEC_res_model} -lt ${MEC_res_obs} ]]; then
         MEC_ctl_from=${MEC_ctl_model}
         MEC_ctl_to=${MEC_ctl_obs}
         MEC_line_out="${MEC_line_out} ${MEC_model} to ${MEC_obs}"
       else
         MEC_ctl_from=${MEC_ctl_obs}
         MEC_ctl_to=${MEC_ctl_model}
         MEC_line_out="${MEC_line_out} ${MEC_obs} to ${MEC_model}"
       fi
    else
      if [[ "${MEC_grid_res}" =~ "ctl" ]]; then
         MEC_ctl_from=${MEC_ctl_model}
         MEC_ctl_to=${MEC_D_templ}/${MEC_grid_res}
         MEC_line_out="${MEC_line_out} Regrid=`basename ${MEC_grid_res}`"
      fi
    fi
    # Create an script to acummulate and regrid based on a template
    MEC_model_out=${MEC_model}_${MEC_var}_${MEC_hs_fct2}_${MEC_valid_date}${MEC_init_time}
    if [[ "${MEC_model_data_format}" == "NCDF" ]]; then
       MEC_acc=MEC_${MEC_var}
       MEC_dt_grads=${VALID_TIME}Z$(date -d "${MEC_valid_date}" "+%d%^b%Y")
       eval MEC_acc=\$$MEC_acc   # Captura o valor da variável dada pela variavel
       MEC_acc=`echo "${MEC_acc/VALID_DATE/${MEC_dt_grads}}"` # substitui data valida
       MEC_acc=`echo "${MEC_acc/VALID_DATE/${MEC_dt_grads}}"` # pode haver duas - TODO
       MEC_open_comm=sdfopen
    else
       MEC_acc=MEC_${MEC_var}_${MEC_hs_fct2}
       eval MEC_acc=\$$MEC_acc   # Captura o valor da variável dada pela variavel
       MEC_open_comm=open
    fi
    MEC_line_out="${MEC_line_out} ${MEC_acc}"

    sed -e "s:CTL_MODEL:${MEC_ctl_from}:g"  -e "s:CTL_OBS:${MEC_ctl_to}:g"                \
        -e "s:HS:${MEC_hs_fct2}:g"          -e "s:DT:${MEC_valid_date}${MEC_init_time}:g" \
        -e "s:DIR_OUT:.:g"                  -e "s:MODELO:${MEC_model_out}:g"              \
        -e "s:VAR:${MEC_var}:g"             -e "s:ACC_COMMAND:${MEC_acc}:g"               \
        -e "s:LONGITUDE:${MEC_MCD_lon}:g"   -e "s:LATITUDE:${MEC_MCD_lat}:g"              \
        -e "s:OBS_NEW:${MEC_fname_obs}:g"   -e "s:dt_obs:${MEC_valid_date}12:g"           \
        -e "s:OPEN_COMM:${MEC_open_comm}:g" -e "s:FCTNEW:V${MEC_var}:g"                   \
        ${MEC_D_grads}/GS_AccumModelAndRegrid_template.gs > ${MEC_D_temp}/GS_AccumModelAndRegrid.gs

    # Accumulate the model in its original spatial domain and regrid the output to
    # the MCD (Maximum Common Domain) among the involved models based on MERGE's 
    # resolution
    set | grep "^MEC_" > ${MEC_eval_name}/LAST_SET.ksh

    cd ${MEC_D_temp}
    grads -blc "run GS_AccumModelAndRegrid.gs" > ${MEC_D_log}/grads_GS_AccumModelAndRegrid_${MEC_model}_${MEC_hs_fct2}.log

    if [[ ! -s ${MEC_model_out}.grib2 ]] ; then
      MEC_err="ACCUMULATION OR REGRID PROBLEM"
    else
      ./g2ctl ${MEC_model_out}.grib2 > ${MEC_model_out}.ctl
      gribmap -i ${MEC_model_out}.ctl > ${MEC_D_log}/gribmap_${MEC_model}_${MEC_hs_fct2}.log

      # Convert to NetCDF
      cdo -s -f nc -selvar,param8.1.0 ${MEC_model_out}.grib2 TEMP_${MEC_model}.nc

      # Delete length 1 dimension by averaging it (height)
      ncwa -O -a height TEMP_${MEC_model}.nc TEMP2_${MEC_model}.nc

      # Remove the variable "height"
      ncks -C -O -x -v height TEMP2_${MEC_model}.nc TEMP3_${MEC_model}.nc
      ncrename -O -v param8.1.0,${MEC_var} TEMP3_${MEC_model}.nc ${MEC_D_dat_model_ncdf}/${MEC_model_out}.nc > ${MEC_D_log}/ncrename_${MEC_model}_${MEC_hs_fct2}.log

      # Move GRIB2 or GRB to the output diretory ---------------------------------
      mv ${MEC_model_out}.grib2 ${MEC_D_dat_model_grib}/
      rm -f ${MEC_model}_*
    fi
    cd ${MEC_D_mec}

    if [[ -z ${MEC_err} ]]; then
      MEC_line_out="${MEC_line_out} - Ok!"
    else
      MEC_line_out="${MEC_line_out} - ${MEC_err}"
    fi
 fi
 echo "  ${MEC_line_out}"
 MEC_hs_fct=$(( ${MEC_hs_fct} + 24 ))
done
