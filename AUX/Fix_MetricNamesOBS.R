library(data.table)
path = "/dados/dmdpesq/roberto.garcia/MEC_DATA/REG21T/EVAL/"

fnames = dir(path=path, pattern=paste0("^OBS_.*.RData"), full.names=T)
fname = fnames[1]
for (fname in fnames) {
  print(fname, flush=T)
  load(fname)

  M2 = OBS[TYPE=="S", mean(VALUE)]
  M3 = OBS[TYPE=="S2", mean(VALUE)]
  
  OBS[TYPE=="S"]$TYPE = "SD"
  OBS[TYPE=="S2"]$TYPE = "VAR"
  OBS = droplevels(OBS)
  OBS$TYPE = factor(OBS$TYPE, levels=sort(unique(as.character(OBS$TYPE))))

  M5 = OBS[TYPE=="SD", mean(VALUE)]
  M6 = OBS[TYPE=="VAR", mean(VALUE)]
  setkey(OBS, REG, MASK, DT, HH, TYPE)

  if (M2 != M5) {
    print(M2, M5)
    stop("M2 != M5")
  }
  if (M3 != M6) {
    print(M3, M6)
    stop("M3 != M6")
  }
  save(OBS, file=fname)
}  

