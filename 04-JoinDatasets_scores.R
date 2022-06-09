# ==============================================================================
# JRM Garcia (roberto.garcia@inpe.br)
# DIMNT / CGCT / INPE / MCTI
# ------------------------------------------------------------------------------
# Join daily evaluated results (EVAL_CONTs e EVAL_CATs)
# ==============================================================================
# Current R script name
args <- commandArgs(trailingOnly=F)
script.name <- sub("--file=", "", args[grep("--file=", args)])

args <- commandArgs(trailingOnly=T)
if (!length(args) %in% c(3,4)) {
  cat("\nMEC -", script.name, "\n\n")
  cat("Rscript", script.name, " <EVAL_NAME> <VAR> <INIT_TIME> or\n")
  cat("Rscript", script.name, " <EVAL_NAME> <VAR> <INIT_TIME> <LAST_EVAL_DATE>\n\n")
  cat("<EVAL_NAME>      A name of the evaluation, e.g. REG21 | Eta | ...\n")
  cat("<VAR>            Variable name, e.g. PREC | TMAX | TMIN | ...\n")
  cat("<INIT_TIME>      Model's initialization time, e.g. 00 | 12\n")
  cat("<LAST_EVAL_DATE> Last evaluation date in YYYYMMDD, records after this date will be excluded\n")
  stop("Wrong number of arguments!\n")
}

MEC_eval_name=args[1]
MEC_dont_check_args=T; source('PrepCommonEnv.R')
MEC_var=args[2]
MEC_init_time=args[3]

thresholds = THRES[[CFG$var]][2:(length(THRES[[CFG$var]])-1)]

# Captura a última data de processamento para poder apagar os registros depois desta data
# pois o sistema automaticamente cria o DS com todos os dias do mês para poder ser mais rápido
# e só atualiza a linha ao invés de criá-las
if (length(args) == 3) {
  MEC_last_dt = Sys.Date()
} else {
  MEC_last_dt = as.Date(args[4], format="%Y%M%d%H") #  DEFAULT
}

if (MEC_init_time == "00" && MEC_var == "PREC") {
  hs2shift <- 12
} else {
  hs2shift <- 0
}

method <- 'UPDATE' # LOOP | REDUCE | UPDATE
cat('===========================================================================\n')
cat('Joining evaluation datasets via', method, 'method:\n')
cat('===========================================================================\n')

# ==============================================================================
cat('** OBS: ')
# ==============================================================================
files <- dir(DIR$eval, paste0("^OBS_", CFG$cfg.set, "_.*\\.RData$"))
cat(length(files), 'files, ')
f=files[1]

num.lin <- 0L
for (f in files) {
  load(paste0(DIR$eval, f))
  num.lin <- num.lin + nrow(OBS)
}
cat(num.lin, 'lines, ')
OBS2 <- data.table(REG=factor(character(num.lin)), MASK=factor(character(num.lin)), TYPE=factor(character(num.lin)),
        DT=rep(as.Date(character()), length.out=num.lin), HH=factor(character(num.lin)), VALUE=numeric(num.lin),
        LCI95=numeric(num.lin), UCI95=numeric(num.lin))
OBS2 <- droplevels(OBS2)
num.lin <- 0
f=files[2]
cat('updating, ')
for (f in files) {
  load(paste0(DIR$eval, f))
  set(OBS2, (num.lin+1):(num.lin+nrow(OBS)), 1:ncol(OBS2), OBS[, list(REG, MASK, TYPE, DT, HH, VALUE, LCI95, UCI95)])
  num.lin <- num.lin + nrow(OBS)
}
cat('erasing VALUE=NA (')
OBS <- OBS2[!is.na(VALUE) & DT <= MEC_last_dt]  # Elimina as linhas que possuem valor nulo
cat(nrow(OBS), 'lines now), ')
cat('factoring ... ')
OBS <- droplevels(OBS)
setkey(OBS)
save(OBS, file=paste0(DIR$deploy, '/OBS_', CFG$cfg.set, '.RData'))
cat('ok!\n')
if (MEC_var == "PREC") {
  ltime2 = 9
} else {
  ltime2 = 10
}

# ==============================================================================
cat('** EVAL_CONT:\n')
# ==============================================================================
ltime.num=1
for (ltime.num in 1:ltime2) {
  ltime <-sprintf("%03d", ltime.num*24+hs2shift)
  cat('   - ', ltime, 'h: ', sep='')
  patt <- paste0("EVAL_CONT_.*_", ltime,".*_", MEC_init_time, ".RData$")
  files <- dir(DIR$eval, patt)
  files <- files[!grepl("_PER_", files)] # Removes consolidated evaluation DSs
  cat(length(files), 'files, ')
  f=files[1]
  num.lin <- 0L
  for (f in files) {
    load(paste0(DIR$eval, f))
    num.lin <- num.lin + nrow(EVAL.CONT)
  }
  cat(num.lin, 'lines, ')
  CONT.UPDATE <- data.table(EV_SET=character(num.lin), MOD=character(num.lin), LTIME=ltime.num, REG=factor(character(num.lin)), 
                            MASK=factor(character(num.lin)), TYPE=factor(character(num.lin)), DT=rep(as.Date(character()), length.out=num.lin), HH=factor(character(num.lin)), VALUE=numeric(num.lin), LCI95=numeric(num.lin), UCI95=numeric(num.lin))
  CONT.UPDATE$REG <- droplevels(CONT.UPDATE$REG)
  num.lin <- 0
  f=files[2]
  cat('updating, ')
  for (f in files) {
    cfg <- unlist(strsplit(x=gsub("\\.RData", '', f), split='_'))
    MOD=paste(cfg[3],'_', cfg[4], sep=''); VAR=cfg[5]
    EV.SET=paste0(VAR, '-', cfg[7])
    load(paste0(DIR$eval, f))
    set(CONT.UPDATE, (num.lin+1):(num.lin+nrow(EVAL.CONT)), 1:ncol(CONT.UPDATE), 
        cbind.data.frame(EV.SET, MOD, ltime.num, EVAL.CONT[, list(REG,MASK,TYPE,DT,HH,VALUE,LCI95,UCI95)]))
    num.lin <- num.lin + nrow(EVAL.CONT)
  }
  cat('erasing VALUE=NA (')
  # Mantem somente as linhas que possuem valor nao nulo e data menor ou igual a atual 
  CONT.UPDATE <- CONT.UPDATE[!is.na(VALUE) & DT <= MEC_last_dt]
  cat(nrow(CONT.UPDATE), 'lines now), ')
  cat('factoring cols: 1'); CONT.UPDATE$EV_SET <- factor(CONT.UPDATE$EV_SET)
  cat(',2'); CONT.UPDATE$MOD <- factor(CONT.UPDATE$MOD)
  cat(',3'); CONT.UPDATE$REG <- droplevels(CONT.UPDATE$REG)
  cat(',4'); CONT.UPDATE$MASK <- droplevels(CONT.UPDATE$MASK)
  cat(',5'); CONT.UPDATE$HH <- droplevels(CONT.UPDATE$HH)
  cat(',6, '); CONT.UPDATE$TYPE <- droplevels(CONT.UPDATE$TYPE)
  setkey(CONT.UPDATE)

  cat('saving ... ')
  EVAL.CONT <- CONT.UPDATE
  setkey(EVAL.CONT)
  save(EVAL.CONT, file=paste0(DIR$deploy, '/EVAL_CONT_', ltime.num, "_", MEC_init_time, '.RData'))
  cat('ok!\n')
}

if (CFG.SET$eval.cat) {
  # ==============================================================================
  cat('** EVAL_CAT:\n')
  # ==============================================================================
  ltime.num=1
  for (ltime.num in 1:ltime2) {
    ltime <-sprintf("%03d", ltime.num*24+hs2shift)
    cat('   - ', ltime, 'h: ', sep='')
    patt <- paste0("EVAL_CAT_.*_", ltime,".*_", MEC_init_time, ".RData$")
    files <- dir(DIR$eval, patt)
    files <- files[!grepl("_PER_", files)] # Removes consolidated evaluation DSs
    if (length(files)>0) {
      cat(length(files), 'files, ')
      f=files[1]
      num.lin <- 0L
      for (f in files) {
        load(paste0(DIR$eval, f))
        num.lin <- num.lin + nrow(EVAL.CAT)
      }
      cat(num.lin, 'lines, ')
      # old
      #CAT.UPDATE <- data.table(EV_SET=character(num.lin), MOD=character(num.lin), LTIME=ltime.num, REG=factor(character(num.lin)), 
      #                         MASK=factor(character(num.lin)), DT=rep(as.Date(character()), length.out=num.lin), HH=factor(character(num.lin)), 
      #                         TYPE=factor(character(num.lin)), THRES=numeric(num.lin), VALUE=numeric(num.lin), LCI95=numeric(num.lin), UCI95=numeric(num.lin))

      comm = paste0("CAT.UPDATE <- data.table(EV_SET=character(num.lin), MOD=character(num.lin), LTIME=ltime.num, 
                     REG=CFG$area1.names[1], MASK='ALL', DT=CFG$dates, HH=CFG$hh, THRES=thresholds, ", 
                     paste0(c(METRICS$CAT, paste0(c("LCI95","UCI95"), "_", rep(METRICS$CAT, each=2))), "=NA_real_", collapse=", "), ")")
      eval(parse(text=comm))

      CAT.UPDATE <- droplevels(CAT.UPDATE)
      num.lin <- 0
      f=files[1]
      cat('updating, ')
      for (f in files) {
        cfg <- unlist(strsplit(x=gsub("\\.RData", '', f), split='_'))
        MOD=paste(cfg[3],'_', cfg[4], sep=''); VAR=cfg[5]
        EV.SET=paste0(VAR, '-', cfg[7])
        load(paste0(DIR$eval, f))
        set(CAT.UPDATE, (num.lin+1):(num.lin+nrow(EVAL.CAT)), 1:ncol(CAT.UPDATE), 
            cbind.data.frame(EV.SET, MOD, ltime.num, EVAL.CAT)) #[, list(REG,MASK,DT,HH,TYPE,THRES,VALUE,LCI95,UCI95)]))
        num.lin <- num.lin + nrow(EVAL.CAT)
      }
      cat('erasing VALUE=NA (')
      # Mantem somente as linhas que possuem valor nao nulo e data menor ou igual a atual 
      CAT.UPDATE <- CAT.UPDATE[!(is.na(ETS) | is.na(FAR) | is.na(FBIAS) | is.na(PC) | is.na(POD) | is.na(SR)) & DT <= MEC_last_dt]
      cat(nrow(CAT.UPDATE), 'lines now), ')
      cat('factoring cols: 1'); CAT.UPDATE$EV_SET <- factor(CAT.UPDATE$EV_SET)
      CAT.UPDATE <- droplevels(CAT.UPDATE)
      cat(',2'); CAT.UPDATE$MOD    <- factor(CAT.UPDATE$MOD)
      cat(',3'); CAT.UPDATE$REG    <- factor(CAT.UPDATE$REG)
      cat(',4');  CAT.UPDATE$MASK  <- factor(CAT.UPDATE$MASK)
      cat(',5'); CAT.UPDATE$HH     <- factor(CAT.UPDATE$HH)
      cat(',6 ');  CAT.UPDATE$TYPE <- factor(CAT.UPDATE$TYPE)
      setkey(CAT.UPDATE)
    
      cat('saving ... ')
      EVAL.CAT <- CAT.UPDATE
      setkey(EVAL.CAT)
      save(EVAL.CAT, file=paste0(DIR$deploy, '/EVAL_CAT_', ltime.num, "_", MEC_init_time, '.RData'))
      cat('ok!\n')
    } else {
      cat("No files found!\n")
    }
  }
}

