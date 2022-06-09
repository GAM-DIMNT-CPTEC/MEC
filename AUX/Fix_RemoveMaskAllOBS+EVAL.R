library(data.table)
path = "/dados/dmdpesq/roberto.garcia/MEC_DATA/REG21T/EVAL/"

fnames = dir(path=path, pattern=paste0("^OBS_.*.RData"), full.names=T)
fname = fnames[1]
for (fname in fnames) {
  print(fname, flush=T)
  load(fname)

  OBS = OBS[MASK=="CONT"]
  setkey(OBS, REG, MASK, DT, HH, TYPE)
  save(OBS, file=fname)
}  
rm(OBS)

fnames = dir(path=path, pattern=paste0("^EVAL_CONT_.*.RData"), full.names=T)
fnames = fnames[-grep("_PER_", fnames)] # Remove os arquivos consolidados
fname = fnames[1]
for (fname in fnames) {
  print(fname, flush=T)
  load(fname)

  EVAL.CONT = EVAL.CONT[MASK=="CONT"]
  setkey(EVAL.CONT, REG, MASK, DT, HH, TYPE)
  save(EVAL.CONT, file=fname)
}  

