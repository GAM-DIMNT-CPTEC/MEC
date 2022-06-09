#
# Dependent configuration
#

if [[ ${MEC_init_time} = "12" || ${MEC_var} != "PREC" ]]; then
  # Se for avaliação das 12 inicia em 24hs
  MEC_hs_fct_beg=24
else
  # Se for avaliação das 00 inicia em 36hs
  MEC_hs_fct_beg=36
fi

# Chooses observation source wrt 'MEC_var'
MEC_obs=MEC_obs_${MEC_var}
eval MEC_obs=\$$MEC_obs

