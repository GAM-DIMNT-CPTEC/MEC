# Do not add inline comments!
MEC_ctl_full_path_model=/oper/share/ioper/tempo/WRF/ams_05km/brutos/YYYY/MM/DD/HH/
MEC_ctl_full_fname_model=${MEC_ctl_full_path_model}/WRF_cpt_05KM_YYYYMMDDHH.ctl
# EXEMPLO: /oper/share/ioper/tempo/WRF/ams_05km/brutos/2021/04/01/00/WRF_cpt_05KM_2021040100.ctl

# 00hr model run  ================================
# Use R00 to set accum formulas for 00hr model run
# And TXXX for each lead time
# ================================================
if [ "$MEC_init_time" == "00" ]; then
  # Frq 1h
  MEC_R00T036='FCT=APCPsfc(t=37)-APCPsfc(t=13)'
  MEC_R00T060='FCT=APCPsfc(t=61)-APCPsfc(t=37)'
  MEC_R00T084='FCT=APCPsfc(t=85)-APCPsfc(t=61)'
fi

# 12hr model run  ================================
# Use R12 to set accum formulas for 12hr model run
# And TXXX for each lead time
# ================================================
if [ "$MEC_init_time" == "12" ]; then
  MEC_R12T024='FCT=APCPsfc(t=25)'
  MEC_R12T048='FCT=APCPsfc(t=49)-APCPsfc(t=25)'
  MEC_R12T072='FCT=APCPsfc(t=73)-APCPsfc(t=49)'
fi
