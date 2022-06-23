dir_model=/oper/share/ioper/tempo/BAM/TQ0666L064/brutos
ctl_prefix=GPOSNMC                       # texto antes de ${dt} no nome do CTL
ctl_suffix=P.fct.TQ0666L064.ctl          # texto apos ${dt} no nome do CTL

# 00hr model run  ================================
# Use R00 to set accum formulas for 00hr model run
# And TXXX for each lead time
# ================================================
MEC_ctl_full_path_model=/dados/dmdpesq/roberto.garcia/MEC_DATA/REG21_BAM/YYYYMMDD00/
MEC_ctl_full_fname_model=${MEC_ctl_full_path_model}/GPOSNMCYYYYMMDD00P.fct.TQ0666L064.ctl
# EXEMPLO: /dados/dmdpesq/roberto.garcia/MEC_DATA/REG21_BAM/2021010100/GPOSNMC2021010100P.fct.TQ0666L064.ctl
# q ctlinfo  ==> tdef 41 linear 00Z01JAN2021 360mn
# set t 41   ==> Time values set: 2021:1:11:0 2021:1:11:0  *****   DIA 11 ÀS 00h  ****** 10 dias!!!!

# 00hr model run  ================================
# Use R00 to set accum formulas for 00hr model run
# And TXXX for each lead time
# ================================================
if [ "$MEC_init_time" == "00" ]; then
  MEC_PREC_036='FCT=sum(PREC,t=4,t=7)*24'    # em mm/dia ou Kg m-2/dia ... está em mm/h 
  MEC_PREC_060='FCT=sum(PREC,t=8,t=11)*24'
  MEC_PREC_084='FCT=sum(PREC,t=12,t=15)*24'
  MEC_PREC_108='FCT=sum(PREC,t=16,t=19)*24'
  MEC_PREC_132='FCT=sum(PREC,t=20,t=23)*24'
  MEC_PREC_156='FCT=sum(PREC,t=24,t=27)*24'
  MEC_PREC_180='FCT=sum(PREC,t=28,t=31)*24'
  MEC_PREC_204='FCT=sum(PREC,t=32,t=35)*24'
  MEC_PREC_228='FCT=sum(PREC,t=36,t=39)*24'

  MEC_TMAX_024='FCT=max(TMAX,t=2,t=5)-273.16'    # em C
  MEC_TMAX_048='FCT=max(TMAX,t=6,t=9)-273.16'
  MEC_TMAX_072='FCT=max(TMAX,t=10,t=13)-273.16'
  MEC_TMAX_096='FCT=max(TMAX,t=14,t=17)-273.16'
  MEC_TMAX_120='FCT=max(TMAX,t=18,t=21)-273.16'
  MEC_TMAX_144='FCT=max(TMAX,t=22,t=25)-273.16'
  MEC_TMAX_168='FCT=max(TMAX,t=26,t=29)-273.16'
  MEC_TMAX_192='FCT=max(TMAX,t=30,t=33)-273.16'
  MEC_TMAX_216='FCT=max(TMAX,t=34,t=37)-273.16'
  MEC_TMAX_240='FCT=max(TMAX,t=38,t=41)-273.16'

  MEC_TMIN_024='FCT=min(TMIN,t=2,t=5)-273.16'    # em C
  MEC_TMIN_048='FCT=min(TMIN,t=6,t=9)-273.16'
  MEC_TMIN_072='FCT=min(TMIN,t=10,t=13)-273.16'
  MEC_TMIN_096='FCT=min(TMIN,t=14,t=17)-273.16'
  MEC_TMIN_120='FCT=min(TMIN,t=18,t=21)-273.16'
  MEC_TMIN_144='FCT=min(TMIN,t=22,t=25)-273.16'
  MEC_TMIN_168='FCT=min(TMIN,t=26,t=29)-273.16'
  MEC_TMIN_192='FCT=min(TMIN,t=30,t=33)-273.16'
  MEC_TMIN_216='FCT=min(TMIN,t=34,t=37)-273.16'
  MEC_TMIN_240='FCT=min(TMIN,t=38,t=41)-273.16'

  # Componente zonal do vento (U), m/s
  # Instantânea, sempre às 00h
  MEC_U10M_024='FCT=U10M(t=5)'    # em m/s
  MEC_U10M_048='FCT=U10M(t=9)'
  MEC_U10M_072='FCT=U10M(t=13)'
  MEC_U10M_096='FCT=U10M(t=17)'
  MEC_U10M_120='FCT=U10M(t=21)'
  MEC_U10M_144='FCT=U10M(t=25)'
  MEC_U10M_168='FCT=U10M(t=29)'
  MEC_U10M_192='FCT=U10M(t=33)'
  MEC_U10M_216='FCT=U10M(t=37)'
  MEC_U10M_240='FCT=U10M(t=41)'

  # Componente meridional do vento (V), m/s
  # Instantânea, sempre às 90h
  MEC_V10M_024='FCT=V10M(t=5)'    # em m/s
  MEC_V10M_048='FCT=V10M(t=9)'
  MEC_V10M_072='FCT=V10M(t=13)'
  MEC_V10M_096='FCT=V10M(t=17)'
  MEC_V10M_120='FCT=V10M(t=21)'
  MEC_V10M_144='FCT=V10M(t=25)'
  MEC_V10M_168='FCT=V10M(t=29)'
  MEC_V10M_192='FCT=V10M(t=33)'
  MEC_V10M_216='FCT=V10M(t=37)'
  MEC_V10M_240='FCT=V10M(t=41)'

  # Velocidade do vento (W), m/s
  # Instantânea, sempre às 00h
  MEC_VEL10M_024='FCT=sqrt(pow(U10M(t=5),2)+pow(V10M(t=5),2))'    # em m/s
  MEC_VEL10M_048='FCT=sqrt(pow(U10M(t=9),2)+pow(V10M(t=9),2))'
  MEC_VEL10M_072='FCT=sqrt(pow(U10M(t=13),2)+pow(V10M(t=13),2))'
  MEC_VEL10M_096='FCT=sqrt(pow(U10M(t=17),2)+pow(V10M(t=17),2))'
  MEC_VEL10M_120='FCT=sqrt(pow(U10M(t=21),2)+pow(V10M(t=21),2))'
  MEC_VEL10M_144='FCT=sqrt(pow(U10M(t=25),2)+pow(V10M(t=25),2))'
  MEC_VEL10M_168='FCT=sqrt(pow(U10M(t=29),2)+pow(V10M(t=29),2))'
  MEC_VEL10M_192='FCT=sqrt(pow(U10M(t=33),2)+pow(V10M(t=33),2))'
  MEC_VEL10M_216='FCT=sqrt(pow(U10M(t=37),2)+pow(V10M(t=37),2))'
  MEC_VEL10M_240='FCT=sqrt(pow(U10M(t=41),2)+pow(V10M(t=41),2))'
fi

