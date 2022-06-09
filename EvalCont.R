# ------------------------------------------------------------------------------
# CONTINUOS EVALUATION (COR, RMSE and URMSE* are performed in the App, on-the-fly)
# ------------------------------------------------------------------------------
EV_CONT = f.EvalCont(vOBS, vFCT)
      
# Persisting the statistics of the QPF evaluation
st_names = names(EV_CONT)
idx_st=1
for (idx_st in 1:length(st_names)) {
  keys = unlist(strsplit(st_names[idx_st], "_"))
  st_src=keys[1]
  st_name=keys[2]
  if (st_src == "OBS") {
    f.PutObs(hh.targ, st_name, EV_CONT[[idx_st]])
  } else {
    f.PutCont(st_name, EV_CONT[[idx_st]])
  }
}
