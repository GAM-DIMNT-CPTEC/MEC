# ------------------------------------------------------------------------------
# CI for categorical evaluation by bootstraping
# ------------------------------------------------------------------------------
# Processing the CI level stablished for de system
f.Cat("\n  CI Cat.: ")
# Processing each threshold
thres=thresholds[1]
for (thres in thresholds) { # thresholds já foi definido (retira boordas)
  f.Cat(thres, ", ")
  cat_booted <- boot(data.frame(OBS=vOBS, FCT=vFCT), cat_booter, threshold=thres, R=CFG$ci.bs.n) 
  # Saving the results in the result object
  EV_CI_CAT = list()
  st_names = names(cat_booted$t0)
  idx_st=1

  # Índices resultantes são os mesmos do retorno da função de avaliação
  for (idx_st in 1:length(st_names)) {
    #f.Cat(st_names[idx_st], ",")
    sink("boot.ci.output")
    CI = try(boot.ci(cat_booted, conf=CFG$ci.level/100, type="perc", index=idx_st), silent=T)
    sink()
    # Para categorias muito altas às vezes não os elementos da TC são 0
    if (!class(CI)  %in% c("try-error", "NULL")) {
      EV_CI_CAT[[st_names[idx_st]]] = c(CI$percent[4], CI$percent[5])
    } else {
      EV_CI_CAT[[st_names[idx_st]]] = c(cat_booted$t0[idx_st], cat_booted$t0[idx_st]) # the value itself
    }
  }

  # Persist CI results for the statistics in the DS
  st_name=st_names[1]
  for (st_name in st_names) {
    f.PutCI_Cat(st_name, thres, EV_CI_CAT[[st_name]]) # Manda c(LCI, UCI)
  }
}
f.Cat(". Ok!")

