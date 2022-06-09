# Do not add inline comments!
MEC_ctl_full_path_model=/dados/grpeta/dsk003/dados/Eta8km_tempo/binctl/YYYYMMDDHH/2D/
MEC_ctl_full_fname_model=${MEC_ctl_full_path_model}/Eta_8km_BMJ_FER_YYYYMMDDHH_2D.ctl
# EXEMPLO: /dados/grpeta/dsk003/dados/Eta8km_tempo/binctl/2020122500/2D/Eta_8km_BMJ_FER_2020122500_2D.ctl

# 00hr model run  ================================
# Use R00 to set accum formulas for 00hr model run
# And TXXX for each lead time
# ================================================
if [ "$MEC_init_time" == "00" ]; then
  MEC_PREC_036='FCT=1000*sum(prec,t=14,t=37)'
  MEC_PREC_060='FCT=1000*sum(prec,t=38,t=61)'
  MEC_PREC_084='FCT=1000*sum(prec,t=62,t=85)'
  MEC_PREC_108='FCT=1000*sum(prec,t=86,t=109)'
  MEC_PREC_132='FCT=1000*sum(prec,t=110,t=133)'
  MEC_PREC_156='FCT=1000*sum(prec,t=134,t=157)'
  MEC_PREC_180='FCT=1000*sum(prec,t=158,t=181)'
  MEC_PREC_204='FCT=1000*sum(prec,t=182,t=205)'
  MEC_PREC_228='FCT=1000*sum(prec,t=206,t=229)'

  MEC_TMAX_024='FCT=max(mxtp,t=2,t=25)-273.16'    # em Celsius
  MEC_TMAX_048='FCT=max(mxtp,t=26,t=49)-273.16'
  MEC_TMAX_072='FCT=max(mxtp,t=50,t=73)-273.16'
  MEC_TMAX_096='FCT=max(mxtp,t=74,t=97)-273.16'
  MEC_TMAX_120='FCT=max(mxtp,t=98,t=121)-273.16'
  MEC_TMAX_144='FCT=max(mxtp,t=122,t=145)-273.16'
  MEC_TMAX_168='FCT=max(mxtp,t=146,t=169)-273.16'
  MEC_TMAX_192='FCT=max(mxtp,t=170,t=193)-273.16'
  MEC_TMAX_216='FCT=max(mxtp,t=194,t=217)-273.16'

  MEC_TMIN_024='FCT=min(mntp,t=2,t=25)-273.16'
  MEC_TMIN_048='FCT=min(mntp,t=26,t=49)-273.16'
  MEC_TMIN_072='FCT=min(mntp,t=50,t=73)-273.16'
  MEC_TMIN_096='FCT=min(mntp,t=74,t=97)-273.16'
  MEC_TMIN_120='FCT=min(mntp,t=98,t=121)-273.16'
  MEC_TMIN_144='FCT=min(mntp,t=122,t=145)-273.16'
  MEC_TMIN_168='FCT=min(mntp,t=146,t=169)-273.16'
  MEC_TMIN_192='FCT=min(mntp,t=170,t=193)-273.16'
  MEC_TMIN_216='FCT=min(mntp,t=194,t=217)-273.16'

  # Componente zonal do vento (U) m*s^-1
  MEC_U10M_024='FCT=u10m(t=25)'
  MEC_U10M_048='FCT=u10m(t=49)'
  MEC_U10M_072='FCT=u10m(t=73)'
  MEC_U10M_096='FCT=u10m(t=97)'
  MEC_U10M_120='FCT=u10m(t=121)'
  MEC_U10M_144='FCT=u10m(t=145)'
  MEC_U10M_168='FCT=u10m(t=169)'
  MEC_U10M_192='FCT=u10m(t=193)'
  MEC_U10M_216='FCT=u10m(t=217)'

  # Componente meridional do vento (V) m*s^-1
  MEC_V10M_024='FCT=v10m(t=25)'
  MEC_V10M_048='FCT=v10m(t=49)'
  MEC_V10M_072='FCT=v10m(t=73)'
  MEC_V10M_096='FCT=v10m(t=97)'
  MEC_V10M_120='FCT=v10m(t=121)'
  MEC_V10M_144='FCT=v10m(t=145)'
  MEC_V10M_168='FCT=v10m(t=169)'
  MEC_V10M_192='FCT=v10m(t=193)'
  MEC_V10M_216='FCT=v10m(t=217)'

  # Velocidade do vento (WS) m*s^-1
  MEC_VEL10M_024='FCT=sqrt(pow(u10m(t=25),2)+pow(v10m(t=25),2))'
  MEC_VEL10M_048='FCT=sqrt(pow(u10m(t=49),2)+pow(v10m(t=49),2))'
  MEC_VEL10M_072='FCT=sqrt(pow(u10m(t=73),2)+pow(v10m(t=73),2))'
  MEC_VEL10M_096='FCT=sqrt(pow(u10m(t=97),2)+pow(v10m(t=97),2))'
  MEC_VEL10M_120='FCT=sqrt(pow(u10m(t=121),2)+pow(v10m(t=121),2))'
  MEC_VEL10M_144='FCT=sqrt(pow(u10m(t=145),2)+pow(v10m(t=145),2))'
  MEC_VEL10M_168='FCT=sqrt(pow(u10m(t=169),2)+pow(v10m(t=169),2))'
  MEC_VEL10M_192='FCT=sqrt(pow(u10m(t=193),2)+pow(v10m(t=193),2))'
  MEC_VEL10M_216='FCT=sqrt(pow(u10m(t=217),2)+pow(v10m(t=217),2))'

  # MSLP (hPa)
  MEC_MSLP_024='FCT=pslm(t=25)'
  MEC_MSLP_048='FCT=pslm(t=49)'
  MEC_MSLP_072='FCT=pslm(t=73)'
  MEC_MSLP_096='FCT=pslm(t=97)'
  MEC_MSLP_120='FCT=pslm(t=121)'
  MEC_MSLP_144='FCT=pslm(t=145)'
  MEC_MSLP_168='FCT=pslm(t=169)'
  MEC_MSLP_192='FCT=pslm(t=193)'
  MEC_MSLP_216='FCT=pslm(t=217)'
fi

# 12hr model run  ================================
# Use R12 to set accum formulas for 12hr model run
# And TXXX for each lead time
# ================================================
if [ "$MEC_init_time" == "12" ]; then
  MEC_R12T024='FCT=1000*sum(prec,t=2,t=25)'
  MEC_R12T048='FCT=1000*sum(prec,t=26,t=49)'
  MEC_R12T072='FCT=1000*sum(prec,t=50,t=73)'
  MEC_R12T096='FCT=1000*sum(prec,t=74,t=97)'
  MEC_R12T120='FCT=1000*sum(prec,t=98,t=121)'
  MEC_R12T144='FCT=1000*sum(prec,t=122,t=145)'
  MEC_R12T168='FCT=1000*sum(prec,t=146,t=169)'
  MEC_R12T192='FCT=1000*sum(prec,t=170,t=193)'
  MEC_R12T216='FCT=1000*sum(prec,t=194,t=217)'
  MEC_R12T240='FCT=1000*sum(prec,t=218,t=241)'
  MEC_R12T264='FCT=1000*sum(prec,t=242,t=265)'
fi

