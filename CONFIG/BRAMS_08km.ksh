#MEC_ctl_full_path_model=/share/bramsrd/dist/avaliacao_BRAMS_5.6/grads_precip_temp/YYYYMMDD21/
MEC_ctl_full_path_model=/dados/dmdpesq/roberto.garcia/MEC_DATA/REG21_BRAMS/YYYYMMDD21/
MEC_ctl_full_fname_model=${MEC_ctl_full_path_model}/template.ctl
# EXEMPLO: /dados/dmdpesq/roberto.garcia/MEC_DATA/REG21_BRAMS/2021010121/template.ctl

# 00hr model run  ================================
# Use R00 to set accum formulas for 00hr model run
# And TXXX for each lead time
# ================================================
if [ "$MEC_init_time" == "00" ]; then
  MEC_PREC_036='FCT=precip(t=13)-precip(t=5)'    # em mm ou Kg m-2
  MEC_PREC_060='FCT=precip(t=21)-precip(t=13)'
  MEC_PREC_084='FCT=precip(t=29)-precip(t=21)'
  MEC_PREC_108='FCT=precip(t=37)-precip(t=29)'
  MEC_PREC_132='FCT=precip(t=45)-precip(t=37)'
  MEC_PREC_156='FCT=precip(t=53)-precip(t=45)'

  MEC_TMAX_024='FCT=t2mj_max(t=9)'    # em Celsius
  MEC_TMAX_048='FCT=t2mj_max(t=17)'
  MEC_TMAX_072='FCT=t2mj_max(t=25)'
  MEC_TMAX_096='FCT=t2mj_max(t=33)'
  MEC_TMAX_120='FCT=t2mj_max(t=41)'
  MEC_TMAX_144='FCT=t2mj_max(t=49)'
  MEC_TMAX_168='FCT=t2mj_max(t=57)'

  MEC_TMIN_024='FCT=t2mj_min(t=9)'    # em Celsius
  MEC_TMIN_048='FCT=t2mj_min(t=17)'
  MEC_TMIN_072='FCT=t2mj_min(t=25)'
  MEC_TMIN_096='FCT=t2mj_min(t=33)'
  MEC_TMIN_120='FCT=t2mj_min(t=41)'
  MEC_TMIN_144='FCT=t2mj_min(t=49)'
  MEC_TMIN_168='FCT=t2mj_min(t=57)'

  # Componente zonal do vento (U), m/s
  # Instantânea, sempre às 00h
  MEC_U10M_024='FCT=u10mj(t=9)'    
  MEC_U10M_048='FCT=u10mj(t=17)'
  MEC_U10M_072='FCT=u10mj(t=25)'
  MEC_U10M_096='FCT=u10mj(t=33)'
  MEC_U10M_120='FCT=u10mj(t=41)'
  MEC_U10M_144='FCT=u10mj(t=49)'
  MEC_U10M_168='FCT=u10mj(t=57)'

  # Componente meridional do vento (V), m/s
  # Instantânea, sempre às 00h
  MEC_V10M_024='FCT=v10mj(t=9)'    
  MEC_V10M_048='FCT=v10mj(t=17)'
  MEC_V10M_072='FCT=v10mj(t=25)'
  MEC_V10M_096='FCT=v10mj(t=33)'
  MEC_V10M_120='FCT=v10mj(t=41)'
  MEC_V10M_144='FCT=v10mj(t=49)'
  MEC_V10M_168='FCT=v10mj(t=57)'

  # Velocidade do vento (W), m/s
  # Instantânea, sempre às 00h
  MEC_VEL10M_024='FCT=sqrt(pow(u10mj(t=9),2)+pow(v10mj(t=9),2))'
  MEC_VEL10M_048='FCT=sqrt(pow(u10mj(t=17),2)+pow(v10mj(t=17),2))'
  MEC_VEL10M_072='FCT=sqrt(pow(u10mj(t=25),2)+pow(v10mj(t=25),2))'
  MEC_VEL10M_096='FCT=sqrt(pow(u10mj(t=33),2)+pow(v10mj(t=33),2))'
  MEC_VEL10M_120='FCT=sqrt(pow(u10mj(t=41),2)+pow(v10mj(t=41),2))'
  MEC_VEL10M_144='FCT=sqrt(pow(u10mj(t=49),2)+pow(v10mj(t=49),2))'
  MEC_VEL10M_168='FCT=sqrt(pow(u10mj(t=57),2)+pow(v10mj(t=57),2))'

  # MSLP mb = hPa
  # Instantânea, sempre às 00h
  MEC_MSLP_024='FCT=SEA_PRESS(t=9)'    
  MEC_MSLP_048='FCT=SEA_PRESS(t=17)'
  MEC_MSLP_072='FCT=SEA_PRESS(t=25)'
  MEC_MSLP_096='FCT=SEA_PRESS(t=33)'
  MEC_MSLP_120='FCT=SEA_PRESS(t=41)'
  MEC_MSLP_144='FCT=SEA_PRESS(t=49)'
  MEC_MSLP_168='FCT=SEA_PRESS(t=57)'
fi

# 12hr model run  ================================
# Use R12 to set accum formulas for 12hr model run
# And TXXX for each lead time
# ================================================
if [ "$MEC_init_time" == "12" ]; then
  MEC_PREC_024='FCT=precip(t=9)'
  MEC_PREC_048='FCT=precip(t=17)-precip(t=9)'
  MEC_PREC_072='FCT=precip(t=25)-precip(t=17)'
  MEC_PREC_096='FCT=precip(t=33)-precip(t=25)'
  MEC_PREC_120='FCT=precip(t=41)-precip(t=33)'
  MEC_PREC_144='FCT=precip(t=49)-precip(t=41)'
fi
