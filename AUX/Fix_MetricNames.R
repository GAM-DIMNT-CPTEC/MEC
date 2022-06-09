library(data.table)
models = c("BRAMS_08km", "Eta_08km", "GFS_30km", "WRF_07km")
path = "/dados/dmdpesq/roberto.garcia/MEC_DATA/REG21/EVAL/"

model = models[1]
for (model in models) {
  fnames = dir(path=path, pattern=paste0("EVAL_CONT_", model, "_.*.RData"), full.names=T)
  
  fname = fnames[1]
  for (fname in fnames) {
    print(fname, flush=T)
    load(fname)

    M1 = EVAL.CONT[TYPE=="ME", mean(VALUE)]
    M2 = EVAL.CONT[TYPE=="S", mean(VALUE)]
    M3 = EVAL.CONT[TYPE=="S2", mean(VALUE)]
    
    EVAL.CONT[TYPE=="ME"]$TYPE = "BIAS"
    EVAL.CONT[TYPE=="S"]$TYPE = "SD"
    EVAL.CONT[TYPE=="S2"]$TYPE = "VAR"
    EVAL.CONT = droplevels(EVAL.CONT)
    EVAL.CONT$TYPE = factor(EVAL.CONT$TYPE, levels=sort(unique(as.character(EVAL.CONT$TYPE))))

    M4 = EVAL.CONT[TYPE=="BIAS", mean(VALUE)]
    M5 = EVAL.CONT[TYPE=="SD", mean(VALUE)]
    M6 = EVAL.CONT[TYPE=="VAR", mean(VALUE)]
    setkey(EVAL.CONT, REG, MASK, DT, HH, TYPE)

    if (M1 != M4) {
      print(M1, M4)
      stop("M1 != M4")
    }
    if (M2 != M5) {
      print(M2, M5)
      stop("M2 != M5")
    }
    if (M3 != M6) {
      print(M3, M6)
      stop("M3 != M6")
    }
    save(EVAL.CONT, file=fname)
  }  
}
