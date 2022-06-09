#
#### Do not use inline comments! ###
#
#MEC_ctl_full_path_model=/oper/ioper/tempo/GFS/0p25/brutos/YYYY/MM/DD/HH/
#MEC_ctl_full_fname_model=${MEC_ctl_full_path_model}/gfs025gr.pgrb2.YYYYMMDDHH.ctl
# EXEMPLO: /oper/ioper/tempo/GFS/0p25/brutos/2021/03/01/00/gfs025gr.pgrb2.2021030100.ctl
MEC_ctl_full_path_model=/dados/dmdpesq/roberto.garcia/MEC_DATA/REG21_GFS/YYYY/MM/DD/HH
MEC_ctl_full_fname_model=${MEC_ctl_full_path_model}/gfs025gr.pgrb2.YYYYMMDDHH.ctl
# EXEMPLO: /dados/dmdpesq/roberto.garcia/MEC_DATA/REG21_GFS/2021/01/01/00/gfs025gr.pgrb2.2021010100.ctl

# 00hr model run  ================================
# Use R00 to set accum formulas for 00hr model run
# And TXXX for each lead time
# ================================================
if [ "$MEC_init_time" == "00" ]; then
  MEC_PREC_036='FCT=prec(t=13)-prec(t=5)'
  MEC_PREC_060='FCT=prec(t=21)-prec(t=13)'
  MEC_PREC_084='FCT=prec(t=29)-prec(t=21)'
  MEC_PREC_108='FCT=prec(t=37)-prec(t=29)'
  MEC_PREC_132='FCT=prec(t=45)-prec(t=37)'
  MEC_PREC_156='FCT=prec(t=53)-prec(t=45)'
  MEC_PREC_180='FCT=prec(t=61)-prec(t=53)'
  MEC_PREC_204='FCT=prec(t=69)-prec(t=61)'
  MEC_PREC_228='FCT=prec(t=77)-prec(t=69)'  # após isso fica de 12h em 12h
  #MEC_PREC_228='FCT=prec(t=81)-prec(t=77)'

  MEC_TMAX_024='FCT=max(mxtp,t=2,t=9)-273.16'    # em Celsius
  MEC_TMAX_048='FCT=max(mxtp,t=10,t=17)-273.16'
  MEC_TMAX_072='FCT=max(mxtp,t=18,t=25)-273.16'
  MEC_TMAX_096='FCT=max(mxtp,t=26,t=33)-273.16'
  MEC_TMAX_120='FCT=max(mxtp,t=34,t=41)-273.16'
  MEC_TMAX_144='FCT=max(mxtp,t=42,t=49)-273.16'
  MEC_TMAX_168='FCT=max(mxtp,t=50,t=57)-273.16'
  MEC_TMAX_192='FCT=max(mxtp,t=58,t=65)-273.16'
  MEC_TMAX_216='FCT=max(mxtp,t=66,t=73)-273.16'
  MEC_TMAX_240='FCT=max(mxtp,t=74,t=81)-273.16'

  MEC_TMIN_024='FCT=min(mntp,t=2,t=9)-273.16'    # em Celsius
  MEC_TMIN_048='FCT=min(mntp,t=10,t=17)-273.16'
  MEC_TMIN_072='FCT=min(mntp,t=18,t=25)-273.16'
  MEC_TMIN_096='FCT=min(mntp,t=26,t=33)-273.16'
  MEC_TMIN_120='FCT=min(mntp,t=34,t=41)-273.16'
  MEC_TMIN_144='FCT=min(mntp,t=42,t=49)-273.16'
  MEC_TMIN_168='FCT=min(mntp,t=50,t=57)-273.16'
  MEC_TMIN_192='FCT=min(mntp,t=58,t=65)-273.16'
  MEC_TMIN_216='FCT=min(mntp,t=66,t=73)-273.16'
  MEC_TMIN_240='FCT=min(mntp,t=74,t=81)-273.16'

  MEC_U10M_024='FCT=u10m(t=9)'    # em m/s
  MEC_U10M_048='FCT=u10m(t=17)'
  MEC_U10M_072='FCT=u10m(t=25)'
  MEC_U10M_096='FCT=u10m(t=33)'
  MEC_U10M_120='FCT=u10m(t=41)'
  MEC_U10M_144='FCT=u10m(t=49)'
  MEC_U10M_168='FCT=u10m(t=57)'
  MEC_U10M_192='FCT=u10m(t=65)'
  MEC_U10M_216='FCT=u10m(t=73)'
  MEC_U10M_240='FCT=u10m(t=81)'

  MEC_V10M_024='FCT=v10m(t=9)'    # em m/s
  MEC_V10M_048='FCT=v10m(t=17)'
  MEC_V10M_072='FCT=v10m(t=25)'
  MEC_V10M_096='FCT=v10m(t=33)'
  MEC_V10M_120='FCT=v10m(t=41)'
  MEC_V10M_144='FCT=v10m(t=49)'
  MEC_V10M_168='FCT=v10m(t=57)'
  MEC_V10M_192='FCT=v10m(t=65)'
  MEC_V10M_216='FCT=v10m(t=73)'
  MEC_V10M_240='FCT=v10m(t=81)'

  MEC_VEL10M_024='FCT=sqrt(pow(u10m(t=9),2)+pow(v10m(t=9),2))'    # em m/s
  MEC_VEL10M_048='FCT=sqrt(pow(u10m(t=17),2)+pow(v10m(t=17),2))'
  MEC_VEL10M_072='FCT=sqrt(pow(u10m(t=25),2)+pow(v10m(t=25),2))'
  MEC_VEL10M_096='FCT=sqrt(pow(u10m(t=33),2)+pow(v10m(t=33),2))'
  MEC_VEL10M_120='FCT=sqrt(pow(u10m(t=41),2)+pow(v10m(t=41),2))'
  MEC_VEL10M_144='FCT=sqrt(pow(u10m(t=49),2)+pow(v10m(t=49),2))'
  MEC_VEL10M_168='FCT=sqrt(pow(u10m(t=57),2)+pow(v10m(t=57),2))'
  MEC_VEL10M_192='FCT=sqrt(pow(u10m(t=65),2)+pow(v10m(t=65),2))'
  MEC_VEL10M_216='FCT=sqrt(pow(u10m(t=73),2)+pow(v10m(t=73),2))'
  MEC_VEL10M_240='FCT=sqrt(pow(u10m(t=81),2)+pow(v10m(t=81),2))'

  # MSLP 
  MEC_MSLP_024='FCT=pslm(t=9)/100'    # em Pa / 100 = hPa
  MEC_MSLP_048='FCT=pslm(t=17)/100'
  MEC_MSLP_072='FCT=pslm(t=25)/100'
  MEC_MSLP_096='FCT=pslm(t=33)/100'
  MEC_MSLP_120='FCT=pslm(t=41)/100'
  MEC_MSLP_144='FCT=pslm(t=49)/100'
  MEC_MSLP_168='FCT=pslm(t=57)/100'
  MEC_MSLP_192='FCT=pslm(t=65)/100'
  MEC_MSLP_216='FCT=pslm(t=73)/100'
  MEC_MSLP_240='FCT=pslm(t=81)/100'

  # U AND V COMPONENT, ANALISYS
  MEC_U10M='FCT=u10m'    # m/s-1
  MEC_V10M='FCT=v10m'    # 
  MEC_VEL10M='FCT=sqrt(pow(u10m,2)+pow(v10m,2))'

  # mean sea level Pressure Reduced to MSL [Pa]
  MEC_MSLP='FCT=pslm/100'    # hPa
fi

# 12hr model run  ================================
# Use R12 to set accum formulas for 12hr model run
# And TXXX for each lead time
# ================================================
if [ "$MEC_init_time" == "12" ]; then
  MEC_R12T024='FCT=prec(t=9)'
  MEC_R12T048='FCT=prec(t=17)-prec(t=9)'
  MEC_R12T072='FCT=prec(t=25)-prec(t=17)'
  MEC_R12T096='FCT=prec(t=33)-prec(t=25)'
  MEC_R12T120='FCT=prec(t=41)-prec(t=33)'
  MEC_R12T144='FCT=prec(t=49)-prec(t=41)'
fi
