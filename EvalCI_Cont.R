# ------------------------------------------------------------------------------
# CI for continuous evaluation by bootstraping
# ------------------------------------------------------------------------------
# Processing the CI level stablished for the system
f.Cat("\n  CI Cont: ")
cont_booted <- boot(data.frame(OBS=vOBS, FCT=vFCT), cont_booter, R=CFG$ci.bs.n)
# Saving the results in the result object
EV_CI_CONT = list()
st_names = names(cont_booted$t0)
idx_st=1
# Índices resultantes são os mesmos do retorno da função de avaliação
for (idx_st in 1:length(st_names)) {
  f.Cat(st_names[idx_st], ",")
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
f.Cat(".")

# Examples using t-test
#ci_fct  = as.numeric(t.test(vFCT, conf.level=CFG$ci.level/100)$conf.int)
#ci_obs  = as.numeric(t.test(vOBS, conf.level=CFG$ci.level/100)$conf.int)
#ci_bias = as.numeric(t.test(vFCT-vOBS, conf.level=CFG$ci.level/100)$conf.int)
# scores OBS = "OBS_AVG", "OBS_SD", "OBS_VAR"
# scores FCT = "FCT_AVG", "FCT_BIAS", "FCT_MAE", "FCT_MSE", "FCT_RMSE", "FCT_SD", "FCT_URMSE", "FCT_VAR"

# Persist CI results for the statistics in the DS
ci_eval=st_names[1]
for (ci_eval in st_names) {
  keys = unlist(strsplit(ci_eval, "_"))
  st_src=keys[1]
  st_name=keys[2]
  if (st_src == "OBS") {
    f.PutCI_Obs(hh.targ, st_name, EV_CI_CONT[[ci_eval]]) # Manda c(LCI, UCI)
  } else {
    f.PutCI_Cont(st_name, EV_CI_CONT[[ci_eval]])         # Manda c(LCI, UCI)
  }
}

