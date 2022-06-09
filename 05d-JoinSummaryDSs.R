# ==============================================================================
# JRM Garcia (roberto.garcia@inpe.br)
# DIMNT / CGCT / INPE / MCTI
# ------------------------------------------------------------------------------
# Joins consolidated evaluated results by period (month, season, rainy) EVAL.CONT.PER*
# ==============================================================================
args <- commandArgs(trailingOnly=T)
args2 <- commandArgs(trailingOnly=F)  # Includes some unnecessary stuff for normal use
if (interactive()) {
  MEC_eval_name="REG21"
  MEC_var="PREC"
  MEC_init_time="00"
} else {
  # Current R script name
  script.name <- sub("--file=", "", args2[grep("--file=", args2)])
  if (length(args) != 3) {
    cat("\nMEC -", script.name, "\n\n")
    cat("Rscript", script.name, " <EVAL_NAME> <VAR> <INIT_TIME>\n")
    cat("<EVAL_NAME>      A name of the evaluation, e.g. REG21 | Eta | ...\n")
    cat("<VAR>            Variable name, e.g. PREC | TMAX | TMIN | ...\n")
    cat("<INIT_TIME>      Model's initialization time, e.g. 00 | 12\n")
    stop("Wrong number of arguments!\n")
  }
  MEC_eval_name=args[1]
  MEC_dont_check_args=T; source('PrepCommonEnv.R')
  MEC_var=args[2]
  MEC_init_time=args[3]
}

MEC_dont_check_args=T; source('PrepCommonEnv.R')

if (MEC_init_time == "00" && MEC_var == "PREC") {
  shift <- 12
} else {
  shift <- 0
}

method <- 'UPDATE' # LOOP | REDUCE | UPDATE
timing <- list()
my_metrics = c("SBIAS", "SCORR", "SRMSE")

cat('\n===========================================================================\n')
cat('Joining consolidated evaluation datasets via', method, 'method:\n')
cat('===========================================================================\n')
if (MEC_var == "PREC") {
  ltime2 = 9
} else {
  ltime2 = 10
}

# ==============================================================================
cat('** EVAL_CONT_PER:\n')
# ==============================================================================
ltime.num=1
for (ltime.num in 1:ltime2) {
  ltime <-sprintf("%03d", ltime.num*24+shift)
  cat('   - ', ltime, 'h: ', sep='')
  patt <- paste0("EVAL_CONT_PER_.*_", ltime,".*_", MEC_init_time, ".RData$")
  files <- dir(DIR$eval, patt)
  cat(length(files), 'files, ')
  f=files[1]
  num.lin <- 0L
  for (f in files) {
    load(paste0(DIR$eval, f))
    num.lin <- num.lin + nrow(EVAL.CONT.PER)
  }
  cat(num.lin, 'lines, ')
  CONT.UPDATE <- data.table(EV_SET=character(num.lin), MOD=character(num.lin), LTIME=ltime.num, EVAL.CONT.PER[0][, list(REG,MASK,PER,HH,SOBS,SFCT,SBIAS,SCORR,SRMSE,LCI95_SOBS,UCI95_SOBS,LCI95_SFCT,UCI95_SFCT,LCI95_SBIAS,UCI95_SBIAS,LCI95_SCORR,UCI95_SCORR,LCI95_SRMSE,UCI95_SRMSE)]) 
  #CONT.UPDATE$REG <- droplevels(CONT.UPDATE$REG)
  CONT.UPDATE <- droplevels(CONT.UPDATE)
  num.lin <- 0
  f=files[1]
  cat('updating, ')
  for (f in files) {
    cfg <- unlist(strsplit(x=gsub("\\.RData", '', f), split='_'))
    MOD=paste(cfg[4],'_', cfg[5], sep='')
    VAR=cfg[6]
    EV.SET=paste0(VAR, '-', cfg[8])
    load(paste0(DIR$eval, f))
    set(CONT.UPDATE, (num.lin+1):(num.lin+nrow(EVAL.CONT.PER)), 1:ncol(CONT.UPDATE), 
        cbind.data.frame(EV.SET, MOD, ltime.num, EVAL.CONT.PER[, list(REG,MASK,PER,HH,SOBS,SFCT,SBIAS,SCORR,SRMSE,LCI95_SOBS,UCI95_SOBS,LCI95_SFCT,UCI95_SFCT,LCI95_SBIAS,UCI95_SBIAS,LCI95_SCORR,UCI95_SCORR,LCI95_SRMSE,UCI95_SRMSE)]))
    num.lin <- num.lin + nrow(EVAL.CONT.PER)
  }
  cat('erasing VALUE=NA (')
  # Mantem somente as linhas que possuem valor nao nulo e data menor ou igual a atual 
  CONT.UPDATE <- CONT.UPDATE[!is.na(CONT.UPDATE[[my_metrics[1]]])]
  cat(nrow(CONT.UPDATE), 'lines now), ')
  cat('factoring cols: 1'); CONT.UPDATE$EV_SET <- factor(CONT.UPDATE$EV_SET)
  cat(',2'); CONT.UPDATE$MOD   <- factor(CONT.UPDATE$MOD)
  cat(',3'); CONT.UPDATE$REG   <- droplevels(CONT.UPDATE$REG)
  cat(',4'); CONT.UPDATE$MASK  <- droplevels(CONT.UPDATE$MASK)
  cat(',5'); CONT.UPDATE$PER   <- droplevels(CONT.UPDATE$PER)
  cat(',6, '); CONT.UPDATE$HH    <- droplevels(CONT.UPDATE$HH)
  setkey(CONT.UPDATE)

  cat('saving ... ')
  EVAL.CONT.PER <- CONT.UPDATE
  setkey(EVAL.CONT.PER)
  save(EVAL.CONT.PER, file=paste0(DIR$deploy, '/EVAL_CONT_PER_', ltime.num, "_", MEC_init_time, '.RData'))
  cat('ok!\n')
}

