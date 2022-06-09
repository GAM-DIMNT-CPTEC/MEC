if [[ ${MEC_model} == "WRFG_07km" ]]; then
   MOD_VERSION=GFDL
else 
   MOD_VERSION=RRTMG
fi

if [[ ${MEC_var} == "PREC" ]]; then
  MOD_FNAME=rain
elif [[ ${MEC_var} == "TMIN" ]]; then
  MOD_FNAME=tmin
elif [[ ${MEC_var} == "TMAX" ]]; then
  MOD_FNAME=tmax
elif [[ ${MEC_var} == "U10M" || ${MEC_var} == "V10M" || ${MEC_var} == "VEL10M" ]]; then
  MOD_FNAME=wind
fi


MEC_ctl_full_path_model=/dados/dmdpesq/roberto.garcia/MEC_DATA/REG21_WRF/${MOD_VERSION}
MEC_ctl_full_fname_model=${MEC_ctl_full_path_model}/WRF_${MOD_FNAME}_JAN21.FFCT_LT.HHZ.nc 
# EXEMPLO: /dados/dmdpesq/roberto.garcia/MEC_DATA/REG21_WRF/GFDL/WRF_wind_JAN21.F024.00Z.nc

MEC_val_days_fct=`ls -1 ${MEC_ctl_full_path_model}/WRF_${MOD_FNAME}_JAN21.F* | wc -l`

# 24h-accumulated precipitation
MEC_PREC='FCT=prec(time=VALID_DATE)'

# Temp max
MEC_TMAX='FCT=tmax(time=VALID_DATE)-273.16'

# Temp min
MEC_TMIN='FCT=tmin(time=VALID_DATE)-273.16'

# Componente zonal do vento a 10m (U) # m/s
MEC_U10M='FCT=u10m(time=VALID_DATE)'

# Componente meridional do vento a 10m (V) # m/s
MEC_V10M='FCT=v10m(time=VALID_DATE)'

# Velocidade do vento a 10m (magnitude, velocidade) # m/s
MEC_VEL10M='FCT=sqrt(pow(u10m(time=VALID_DATE),2)+pow(v10m(time=VALID_DATE),2))'

# MSLP (Pa) / 100 = hPa
MEC_MSLP='FCT=slp(time=VALID_DATE)/100'

