# ------------------------------------------------------------------------------
# CI for continuous evaluation by bootstraping
# ------------------------------------------------------------------------------
# Processing the CI level stablished for the system
cont_booted <- boot(data.frame(OBS=vOBS, FCT=vFCT), cont_booter_cons, R=CFG$ci.bs.n)

# Saving the results in the result object
EV_CI_CONT = list()
st_names = names(cont_booted$t0)
idx_st=1
# Índices resultantes são os mesmos do retorno da função de avaliação
for (idx_st in 1:length(st_names)) {
  sink("boot.ci.output")
  CI = try(boot.ci(cont_booted, conf=CFG$ci.level/100, type="perc", index=idx_st), silent=T)
  sink()
  # Caso vOBS ou vFCT seja todo zerado pode dar problema
  # Registra LCI e UCI
  if (!class(CI)  %in% c("try-error", "NULL")) {
    EV_CI_CONT[[st_names[idx_st]]] = c(CI$percent[4], CI$percent[5])
  } else {
    EV_CI_CONT[[st_names[idx_st]]] = c(cont_booted$t0[idx_st], cont_booted$t0[idx_st]) # the value itself
  }
}

# Persist CI results for the statistics in the DS
SBIAS_CI[d1, d2, ] = unlist(EV_CI_CONT["SBIAS"])
SCORR_CI[d1, d2, ] = unlist(EV_CI_CONT["SCORR"])
SRMSE_CI[d1, d2, ] = unlist(EV_CI_CONT["SRMSE"])
