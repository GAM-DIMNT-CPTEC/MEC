stopifnot("MEC_eval_name" %in% ls())
t1 <- Sys.time()

f.LoadCfgSet <- function() {
  fname <- paste0(DIR$eval, '/CFG_', CFG$cfg.set, '.RData')
  stopifnot(file.exists(fname))
  load(fname)
}
f.SaveCfgSet <- function() {
  save(CFG, file=paste0(DIR$deploy, '/CFG.RData'))
  save(CFG.SET, file=paste0(DIR$eval, '/CFG_', CFG$cfg.set, '.RData'))
}

f.Cat <- function(text, ...) {
  cat(text, ..., sep='')
}

# Show the time spent for the script since t1, which is assigned in PrepCommonEnv.R
f.Time <- function() {
  f.Cat('\n=== ', round(Sys.time()-t1, 0), ' secs\n')
}

# Returns the period stored in CFG
f.GetPerDates <- function() {
  CFG[[paste0('PER_', CFG$dt.str)]]
}

# Show the "Ok!" followed by the time since t2, which must be assigned previously somewhere
f.OkAndTime <- function() {
  f.Cat('Ok! ', round(Sys.time()-t2, 0), ' secs')
}

f.FmtNum <- function(num, dig=9, bigmark=',') {
  format(num, digits=dig, big.mark=bigmark)
}

f.CatTitle <- function(title, ch='=') {
  n.char <- nchar(title)
  cat('\n', paste0(rep(ch, n.char), collapse=''), '\n', sep='')
  cat(title, '\n', sep='')
  cat(paste0(rep(ch, n.char), collapse=''), '\n', sep='')
}

f.CatStep <- function(title, ch='-') {
  f.CatTitle(title, ch)
}

f.CatMark <- function(title, ch='-', n.extrap=10) {
  f.Cat(paste(rep(ch, n.extrap), collapse=''), title, paste0(rep(ch, n.extrap), collapse=''), '\n')
}

# Captura a media de cada dia
# Retorna os dias em que a média é NA, ou seja, os dias em que todos os valores do campo são NA
# E retorna tb os dias em que a média menor que um valor grande negativo, 
# . que significa que Verifica se a soma dos numeros nao NA de cada dia é igual a zero,
# se for é pq todos os valores são NA
# Verifica tb se 
f.CheckBadDays <- function(M) {
  res <- c(
    which(apply(M, 3, f.CheckAllDataUndef)),  # check if all data == undef
    which(apply(M, 3, f.CheckAllDataNA)))     # check if all data == NA
  sort(unique(res))
}

# Checks if the time dimension of observation and model match
f.TimesMatch <- function() {
  length(CFG.SET$time.obs) == length(CFG.SET$time.fct)
}

f.CheckAllDataUndef <- function(x) {
  length(which(x < -10000)) == length(x)
}
f.CheckAllDataNA <- function(x) {
  length(which(is.na(x))) == length(x)
}




# ==============================================================================
# Pausa até ENTER, intertivo ou nao
# ==============================================================================
f.Pause <- function() {
  if (interactive()) {
    invisible(readline(prompt = "Press <Enter> to continue..."))
  } else {
    cat("Press <Enter> to continue...")
    invisible(readLines(file("stdin"), 1))
  }
}

# ==============================================================================
# PADR 
# ==============================================================================
f.PadR <- function(str, len, pch=' ') {
  paste0(str, paste0(rep(pch, times=len-nchar(str)), collapse = ''), collapse = '')
}

# ==============================================================================
# Funcao que persiste estatísticas sumarizadas por período
# ==============================================================================
#my.metric="SCORR"; my.value=.3
f.PutPer <- function(my.metric, my.value) {
  stopifnot(length(my.value)==1)
  # If ! found, adds otherwise, update
  if (is.na(EVAL.CONT.PER[.(reg, mask, season, CFG$hh, my.metric), VALUE])) {
     EVAL.CONT.PER = rbind.data.frame(EVAL.CONT.PER,
        data.table(REG=reg, MASK=mask, PER=season, HH=CFG$hh, TYPE=my.metric, VALUE= my.value))
     setkey(EVAL.CONT.PER, REG, MASK, PER, HH, TYPE)
  } else {
     EVAL.CONT.PER[.(reg, mask, season, CFG$hh, my.metric), VALUE := my.value]
  }
  if (is.na(EVAL.CONT.PER[.(reg, mask, season, CFG$hh, my.metric), VALUE]) ||
      is.infinite(EVAL.CONT.PER[.(reg, mask, season, CFG$hh, my.metric), VALUE])) {
    cat("\n* ERR PutPer(): ", my.metric, my.value, season)
  }
}

# ==============================================================================
# Funcao que persiste estatísticas contínuas da previsão
# ==============================================================================
#my.metric=score; my.value=EVCONT[[score]]
f.PutCont <- function(my.metric, my.value) {
  stopifnot(length(my.value)==1)
  EVAL.CONT[.(reg, mask, CFG$dt, CFG$hh, my.metric), VALUE := my.value]
  if (is.na(EVAL.CONT[.(reg, mask, CFG$dt, CFG$hh, my.metric), VALUE]) ||
      is.infinite(EVAL.CONT[.(reg, mask, CFG$dt, CFG$hh, my.metric), VALUE])) {
    cat("\n* ERR PutCont(): ", my.metric, my.value, "OBS=", mean(vOBS), " FCT=", mean(vFCT))
  }
}

# ==============================================================================
# Funcao que persiste os CIs das estatísticas contínuas da previsão
# ==============================================================================
f.PutCI_Cont <- function(my.metric, ci.lims) {
  stopifnot(length(ci.lims)==2)
  EVAL.CONT[.(reg, mask, CFG$dt, CFG$hh, my.metric), c("LCI95", "UCI95") := list(ci.lims[1], ci.lims[2])]
  if (is.na(EVAL.CONT[.(reg, mask, CFG$dt, CFG$hh, my.metric ), LCI95]) ||
      is.infinite(EVAL.CONT[.(reg, mask, CFG$dt, CFG$hh, my.metric), LCI95])) {
    stop("\n* ERR PutCI_Cont(): ", my.metric, paste0(ci.lims))
  }
}

# ==============================================================================
# Funcao que persiste os CIs das estatísticas categóricas da previsão
# ==============================================================================
f.PutCI_Cat <- function(my.metric, thres, ci.lims) {
  stopifnot(length(ci.lims)==2)
  EVAL.CAT[.(reg, mask, CFG$dt, CFG$hh, thres), paste0(c("LCI95", "UCI95"), "_", st_name) := list(ci.lims[1], ci.lims[2])]
  if (is.na(EVAL.CAT[.(reg, mask, CFG$dt, CFG$hh, thres)][[paste0("LCI95_", st_name)]]) ||
      is.infinite(EVAL.CAT[.(reg, mask, CFG$dt, CFG$hh, thres)][[paste0("LCI95_", st_name)]])) {
    stop("\n* ERR PutCI_Cat(): ", my.metric, thres, paste0(ci.lims))
  }
}

# ==============================================================================
# Funcao que persiste estatisticas da observacao
# ==============================================================================
#my.metric=score;  my.value=EVOBS[[score]];  hh=hh.targ
f.PutObs <- function(hh, my.metric, my.value) {
  stopifnot(length(my.value)==1)
  OBS[.(reg, mask, CFG$dt, hh, my.metric), VALUE := my.value]
  if (is.na(OBS[.(reg, mask, CFG$dt, hh, my.metric), VALUE]) ||
      is.infinite(OBS[.(reg, mask, CFG$dt, hh, my.metric), VALUE])) {
    cat("\n* ERR PutObs(): ", my.metric, my.value, "OBS=", mean(vOBS))
  }
}

# ==============================================================================
# Função que persiste os CIs das estatisticas da observacao
# ==============================================================================
#hh=hh.targ; my.metric=st_name; ci.lims=EV_CI_CONT[[ci_eval]]
f.PutCI_Obs <- function(hh, my.metric, ci.lims) {
  stopifnot(length(ci.lims)==2)
  OBS[.(reg, mask, CFG$dt, hh, my.metric), c("LCI95", "UCI95") := list(ci.lims[1], ci.lims[2])]
  if (is.na(OBS[.(reg, mask, CFG$dt, hh, my.metric), LCI95]) ||
      is.infinite(OBS[.(reg, mask, CFG$dt, hh, my.metric), LCI95])) {
    cat("\n* ERR PutCI_Obs():", my.metric, paste0(ci.lims))
#stop()
  }
}

# ==============================================================================
# Retorna a string contendo o nome do arquivo referente ao objeto FLD_
# que armazena as medias dos arquivos NetCDF processados
# Medias de: FCT, OBS, Mean Error (ME=BIAS) e Root Mean Squared Error (RMSE)
# ==============================================================================
f.GetFldFName <- function(tipo) {
  if (tipo == 'OBS') {
    paste0(DIR$eval.tmp, 'FLD_', tipo, '_', toupper(CFG$var), '_', CFG$obs, '.RData') 
  } else {
    if (tipo == 'FCT') {
      paste0(DIR$eval.tmp, 'FLD_', tipo, '_' , CFG$model, '_', toupper(CFG$var), '_', CFG$ltime, '_', CFG$dt.str, '.RData')
    } else { # Campos de métricas
      paste0(DIR$eval.tmp, 'FLD_', tipo, '_' , CFG$model, 'x', CFG$obs, '_', CFG$ltime, '_', CFG$dt.str, '.RData')
    }
  }
}

