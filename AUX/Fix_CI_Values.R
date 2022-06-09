library(data.table)
models = c("BRAMS_08km", "Eta_08km", "GFS_30km", "WRF_07km")
path = "/dados/dmdpesq/roberto.garcia/MEC_DATA/REG21T/EVAL/"

model = models[1]
for (model in models) {
  fnames = dir(path=path, pattern=paste0("EVAL_CONT_", model, "_.*.RData"), full.names=T)
  
  fname = fnames[1]
  for (fname in fnames) {
    print(fname, flush=T)
    load(fname)
    EVAL.CONT$LCI95 = NA_real_
    EVAL.CONT$UCI95 = NA_real_

    save(EVAL.CONT, file=fname)
  }  
}

model = models[1]
for (model in models) {
  fnames = dir(path=path, pattern=paste0("EVAL_CONT_PER_", model, "_.*.RData"), full.names=T)
  
  fname = fnames[1]
  for (fname in fnames) {
    print(fname, flush=T)
    load(fname)
    EVAL.CONT$LCI95 = NA_real_
    EVAL.CONT$UCI95 = NA_real_

    key(EVAL.CONT.PER)
    save(EVAL.CONT.PER, file=fname)
  }  
}

# OBS
fnames = dir(path=path, pattern=paste0("^OBS_.*.RData"), full.names=T)
fname = fnames[1]
for (fname in fnames) {
  print(fname, flush=T)
  load(fname)
  
  OBS$LCI95 = NA_real_
  OBS$UCI95 = NA_real_
  save(OBS, file=fname)
}  

