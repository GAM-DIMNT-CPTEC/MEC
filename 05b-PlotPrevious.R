# ==============================================================================
# ZR Garcia (roberto.garcia@inpe.br)
# DIMNT / CGCT / INPE / MCTI
# ------------------------------------------------------------------------------
# Dispose several fields of a period together, evaluate in an element-wise way 
# and plot a consolidades field. Do it every each day 01MMYYY or after a custom 
# period 
# ==============================================================================
rm(list=ls())
# Current R script name
args <- commandArgs(trailingOnly=F)
script_name <- sub("--file=", "", args[grep("--file=", args)])
args <- commandArgs(trailingOnly=T)

if (interactive()) {
  MEC_eval_name=Sys.getenv("MEC_eval_name")         # "export MEC_eval_name=REG21" before running this script
} else { 
  MEC_eval_name = args[1]                           # read the 1st arg
}

source(paste0(MEC_eval_name, "/EVAL_SET.R"))        # Env vars
source('LoadCommonDSs.R')	                    

if (interactive()) {
  source(paste0(MEC_eval_name, '/LastEvalCfg.R'))   # Needs CFG, load last run setup
} else {
  CFG$eval_name = args[1]	                    
  CFG$model     = args[2]
  CFG$var       = args[3]
  CFG$ltime     = args[4]
  CFG$val_dt1   = args[5]
  CFG$val_dt2   = args[6]
  CFG$init_time = args[7]
  CFG$obs       = args[8]
  CFG$eval_mode = args[9]
}

source('PrepCommonEnv_new.R')
source('FLD-PlotConfig.R')
source("AUX-EvalFunctions.R")

CFG$sp_metrics = c("SOBS", "SFCT", "SBIAS", "SCORR", "SRMSE")
dt_beg = as.Date(CFG$val_dt1, format="%Y%m%d")
dt_end = as.Date(CFG$val_dt2, format="%Y%m%d")
nDays  = as.integer(dt_end-dt_beg+1) # the number of days that will be joined
CFG$dt = dt_beg

# Establish the season wrt val_dt1 and val_dt2
mmdd1 = substring(CFG$val_dt1, 5)
mmdd2 = substring(CFG$val_dt2, 5)
is_lastday = format(dt_beg + 1, format="%d") == "01"
if (mmdd1 == "0301" && mmdd2 == "0531") {
  season = paste0(year(dt_beg), '.1-MAM')
  fig.type = "SAZONAL"
} else if (mmdd1 == "0601" && mmdd2 == "0831") {
  season = paste0(year(dt_beg), '.2-JJA')
  fig.type = "SAZONAL"
} else if (mmdd1 == "0901" && mmdd2 == "1130") {
  season = paste0(year(dt_beg), '.3-SON')
  fig.type = "SAZONAL"
} else if (mmdd1 == "1201" && mmdd2 %in% c("0228", "0229")) {
  season = paste0(year(dt_beg), '.4-DJF')
  fig.type = "SAZONAL"
} else if (mmdd1 == "1101" && mmdd2 == "0331") {
  season = paste0(year(dt_beg), '.5-NDJFM')
  fig.type = "RAINY"
} else if (substring(mmdd1, 3) == "01" && substring(mmdd1, 1, 2) == substring(mmdd2, 1, 2) &&
       is_lastday) {
  season = format(dt_beg, "%Y%m")
  fig.type = "MENSAL"
} else {
  season = paste0(CFG$val_dt1, "-", CFG$val_dt2)
  fig.type = "CUSTOM"
}

# Isto precisa ficar aqui pois há vários if que alteram o fluxo
# Thresholds must cut off one element for both sides because they were included 
# for the intervals for histograms and are huge
thresholds = THRES[[CFG$var]][2:(length(THRES[[CFG$var]])-1)]
file_end = paste0("_", CFG$eval.key, "_", season, "_", CFG$hh, '.RData')

# ------------------------------------------------------------------------------
# EVAL CONT PER - Cria arquivos separados pOra guardar a avaliação contínua por 
# MOD+VAR+LTIME+OBS+PER+ITIME mas devem ser unidos em apenas 1 posteriormente.
# ------------------------------------------------------------------------------
fname_eval_cont <- paste0(DIR$eval, 'EVAL_CONT_PER', file_end)
if (!file.exists(fname_eval_cont)) {
  # Só a região maior (a primeira) é sem máscara, caso seja para avaliar sem máscara
  if ("ALL" %in% CFG.SET$masks) {
    comm = paste0("EVAL.CONT.PER <- data.table(expand.grid(REG=CFG$area1.names[1], MASK='ALL', ", paste0(CFG$sp_metrics, "=NA_real_", collapse=", "), "))")
      eval(parse(text=comm))
  } else {
    EVAL.CONT.PER = data.table()
  }
  # Acrescenta todas as regiões somente com a máscara CONT
  comm = paste0("EVAL.CONT.PER <- rbind.data.frame(EVAL.CONT.PER, data.table(expand.grid(REG=c(CFG$area1.names, REGIONS$NAME), MASK='CONT', ", paste0(CFG$sp_metrics, "=NA_real_", collapse=", "), ")))")
  eval(parse(text=comm))
  setkey(EVAL.CONT.PER, REG, MASK)
} else {
  load(fname_eval_cont)
}

# ------------------------------------------------------------------------------
# EVAL CAT PER - Cria arquivos separados para guardar a avaliação categórica por 
# MOD+VAR+LTIME+OBS+PER+ITIME mas devem ser unidos em apenas 1 posteriormente.
# ------------------------------------------------------------------------------
if (CFG.SET$eval.cat) {
  fname_eval_cat <- paste0(DIR$eval, 'EVAL_CAT_PER', file_end)
  if (!file.exists(fname_eval_cat)) {
    # Só a região maior (a primeira) é sem máscara, caso seja para avaliar sem máscara
    if ("ALL" %in% CFG.SET$masks) {
      comm = paste0("EVAL.CAT.PER <- data.table(expand.grid(REG=CFG$area1.names[1], MASK='ALL', THRES=thresholds, ", paste0(METRICS$CT, "=NA_integer_", collapse=", "), "))")
        eval(parse(text=comm))
    } else {
      EVAL.CAT.PER = data.table()
    }
    # Acrescenta todas as regiões somente com a máscara CONTinente
    comm = paste0("EVAL.CAT.PER <- rbind.data.frame(EVAL.CAT.PER, data.table(expand.grid(REG=c(CFG$area1.names, REGIONS$NAME), MASK='CONT', THRES=thresholds, ", paste0(METRICS$CT, "=NA_integer_", collapse=", "), ")))")
    eval(parse(text=comm))
    setkey(EVAL.CAT.PER, REG, MASK, THRES)
  } else {
    load(fname_eval_cat)
  }
}
# Filenames of the consolidated 2D matrix of each model in order to extract the best model in each grid point
# at each metric (bias, scor, rmse, etc)
fname_fld_obs  = paste0(DIR$eval, "FLD_", CFG$obs, '_', CFG$var, '_', season, '.RData')
fname_fld_fct  = paste0(DIR$eval, "FLD", file_end)
fname_fld_scor = paste0(DIR$eval, "FLD_SCORR", file_end)
fname_fld_bias = paste0(DIR$eval, "FLD_SBIAS", file_end)
fname_fld_rmse = paste0(DIR$eval, "FLD_SRMSE", file_end)

do_eval=T
do_plot=T
# Loads MASK.CONT for plotting or for evaluating
load(paste0(DIR$masks, CFG$mask.cont.prefix, CFG$domain, '_', CFG$area1.names[1], '_', CFG$mask.cont.suffix, '.RData'))

# ==============================================================================
f.Cat(CFG$ltime, ' ', fig.type, "=", season, ' spatial FCT and OBS mean: ')
# ==============================================================================
if (do_eval) {
  f.Cat('joining: ', nDays, " days, ")
  # Create an 3D array, lon x lat x days (every day in the period above)
  FCT <- array(NA_real_, c(length(CFG$lon), length(CFG$lat), nDays))
  OBS <- array(NA_real_, c(length(CFG$lon), length(CFG$lat), nDays))
  dia <- 1
  while (CFG$dt <= dt_end) {
    CFG$dt.str <- format(CFG$dt, "%Y%m%d")
    source('ReadNcOBS.R'); source('ReadNcFCT.R')
    if (file.exists(fname.fct.full)) {
      #cat(CFG$dt, sum(NC.FCT), '\n')
      FCT[,,dia] <- NC.FCT
      OBS[,,dia] <- NC.OBS
     } 
     CFG$dt <- CFG$dt + 1
     dia <- dia + 1
  }
  
  f.Cat("checking bad-days: ")
  bad_days <- f.CheckBadDays(FCT)  # Checking for bad_days
  if (dim(FCT)[3] - length(bad_days) > 0) {
    f.Cat(bad_days)
    SFCT  <- apply(FCT, c(1,2), mean, na.rm=T)  # Retira os bad_days se houverem com o na.rm=T
    SOBS  <- apply(OBS, c(1,2), mean, na.rm=T)
    save(SFCT, file=fname_fld_fct)              # Persiste os campos médios do período acima (AVGs)
    save(SOBS, file=fname_fld_obs)
  }
  if (length(bad_days) > 1) { 
    f.Cat(length(bad_days))
  } else {
    f.Cat("none")
  }
  f.Cat('. Ok!\n')

  f.Cat('\r    Spatial BIAS, CORR and RMSE: ')
  SBIAS    = array(NA, c(length(CFG$lon), length(CFG$lat)))
  SCORR    = array(NA, c(length(CFG$lon), length(CFG$lat)))
  SRMSE    = array(NA, c(length(CFG$lon), length(CFG$lat)))
  d1=1; d2=1
 
  for (d1 in 1:(dim(FCT)[1])) {
    f.Cat("\r    Spatial BIAS, CORR and RMSE: ", d1, "/", dim(FCT)[1], " (", round(d1/dim(FCT)[1] * 100, 2), "%) ")
    for (d2 in 1:(dim(FCT)[2])) {
      vOBS = OBS[d1, d2, ]
      vFCT = FCT[d1, d2, ]
      res = f.EvalContCons(vOBS, vFCT)
      SBIAS[d1, d2] = res$SBIAS
      SCORR[d1, d2] = res$SCORR
      SRMSE[d1, d2] = res$SRMSE
      vOBS = vOBS[!is.na(vOBS)]
      vFCT = vFCT[!is.na(vFCT)]
    }
  }
  # persisting the 2D consolidated field
  FLD=SBIAS; save(FLD, file=fname_fld_bias)
  FLD=SRMSE; save(FLD, file=fname_fld_rmse)
  FLD=SCORR; save(FLD, file=fname_fld_scor)
  f.Cat('Ok!\n')

  f.Cat('    Evaluating by region: ')
  GUIDE_DS = unique(EVAL.CONT.PER[, list(REG, MASK)])
  MASK.ALL <- matrix(0, nrow=length(CFG$lon), ncol=length(CFG$lat))  # Template

  idx_g=1
  for (idx_g in 1:nrow(GUIDE_DS)) {
    reg=as.character(GUIDE_DS[idx_g, REG])
    mask=as.character(GUIDE_DS[idx_g, MASK])
 
    f.Cat(".")
    #f.Cat(" (", reg, ", ", mask, ", ")
  
    if (reg %in% REGIONS$NAME) {
      idx.reg = which(REGIONS$NAME == reg)
      idx.lon = which(CFG.SET$lon >= REGIONS$LON1[idx.reg] & CFG.SET$lon <= REGIONS$LON2[idx.reg])
      idx.lat = which(CFG.SET$lat >= REGIONS$LAT1[idx.reg] & CFG.SET$lat <= REGIONS$LAT2[idx.reg])
    } else {
      idx.lon = 1:length(CFG.SET$lon)   # Captura toda regiao se reg não for quadrante 
      idx.lat = 1:length(CFG.SET$lat)   # que estão delimitados em REGIONS
    }
  
    # Todas as máscaras vêm com o nome de MASK.CONT
    if (mask == 'ALL') {
      # simula máscara "tudo" (todos os valores = 0, pois valor+0=valor)
      MASK.CONT <- MASK.ALL
    } else {
      if (!reg %in% REGIONS$NAME) {
        # Carrega máscara somente continente (cujo nome é dado pelo 1o. item de CFG$area1.names)
        # Somente os pontos de dentro da região = 0, o testante=NA, pois valor+NA=NA
        load(paste0(DIR$masks, CFG$mask.cont.prefix, CFG$domain, '_', reg, '_', CFG$mask.cont.suffix, '.RData'))
      } else {
        # Carrega a máscara do contorno da AS, para aplicar nos quadrantes
        load(paste0(DIR$masks, CFG$mask.cont.prefix, CFG$domain, '_', CFG$area1.names[1], '_', CFG$mask.cont.suffix, '.RData'))
      }
    }
    
    # Separa a área da região
    SOBS_REG  = SOBS[idx.lon, idx.lat]
    SFCT_REG  = SFCT[idx.lon, idx.lat]
    SBIAS_REG = SBIAS[idx.lon, idx.lat]
    SCORR_REG = SCORR[idx.lon, idx.lat]
    SRMSE_REG = SRMSE[idx.lon, idx.lat]
    MASK.REG  = MASK.CONT[idx.lon,idx.lat]
 
    # Aplica a mascara do conjunto de dados (CONT=0, OCEANO ou outra parte=NA), basta somar pois 0 nao influencia e 
    # qq soma com NA se torna NA. E transforma em vetor.
    vOBS  = c(SOBS_REG  + MASK.REG)
    vFCT  = c(SFCT_REG  + MASK.REG)
    vSBIAS = c(SBIAS_REG + MASK.REG)
    vSCORR = c(SCORR_REG + MASK.REG)
    vSRMSE = c(SRMSE_REG + MASK.REG)

    # ------------------------------------------------------------------------------
    # Eliminating all possible NAs from the evaluating vectors
    # Here that MASKS, in fact, take effect because every NA value is deleted from both vectors
    # ------------------------------------------------------------------------------
    bad = which(is.na(vSBIAS))
    if (length(bad) > 0) {
      vOBS   = vOBS[-bad]
      vFCT   = vFCT[-bad]
      vSBIAS = vSBIAS[-bad]
      vSCORR = vSCORR[-bad]
      vSRMSE = vSRMSE[-bad]
    }

    sp_metric=CFG$sp_metrics[1]
    for (sp_metric in CFG$sp_metrics) {
      my_val = switch(sp_metric,
        'SOBS' ={mean(vOBS  , na.rm=T)},
        'SFCT' ={mean(vFCT  , na.rm=T)},
        'SBIAS'={mean(vSBIAS, na.rm=T)},
        'SCORR'={mean(vSCORR, na.rm=T)},
        'SRMSE'={mean(vSRMSE, na.rm=T)}
      )
      # Persisting EVAL CONT
      EVAL.CONT.PER[.(reg, mask)][[sp_metric]] = my_val
      # SCORR can be negative in very small regions
      if (sp_metric != "SCORR") {
        if (is.na(EVAL.CONT.PER[.(reg, mask)][[sp_metric]])) {
          stop(paste("* ERR:", script_name, reg, mask, my_val))
        }
      }
    }

    # EVAL CAT
    if (CFG.SET$eval.cat) {
       source("EvalCatPer_new.R")
       #source("EvalHist.R")
    }
  } # for GUIDE
  f.Cat(' Ok!\n')
 
  f.Cat('    Persisting summarized metrics ... ')
  EVAL.CONT.PER = droplevels(EVAL.CONT.PER) 
  setkey(EVAL.CONT.PER, REG, MASK)
  save(EVAL.CONT.PER, file=fname_eval_cont)
  if (CFG.SET$eval.cat) {
    EVAL.CAT.PER = droplevels(EVAL.CAT.PER) 
    setkey(EVAL.CAT.PER, REG, MASK, THRES)
    save(EVAL.CAT.PER, file=fname_eval_cat)
  }
} else {
  f.Cat("loading consolidated fields ... ")
  load(fname_fld_fct)
  load(fname_fld_obs)
  SCORR = get(load(fname_fld_scor))
  SBIAS = get(load(fname_fld_bias))
  SRMSE = get(load(fname_fld_rmse))
}
f.Cat('Ok!\n')

# ==============================================================================
if (do_plot) {
# ==============================================================================
  f.Cat('    Plotting FCT, OBS, SBIAS, ')
  mask <- 'ALL'
  FLD=SOBS; src='OBS'; source('FLD-Plot.R')
  FLD=SFCT; src='FCT'; source('FLD-Plot.R')
  FLD=SBIAS; src='BIAS'; source('FLD-Plot.R')
  
  mask <- 'CONT'
  FLD=SOBS+MASK.CONT; src='OBS'; source('FLD-Plot.R')
  FLD=SFCT+MASK.CONT; src='FCT'; source('FLD-Plot.R')
  FLD=SBIAS+MASK.CONT; src='BIAS'; source('FLD-Plot.R')
  
  f.Cat('SRMSE, ')
  mask <- 'ALL'
  FLD=SRMSE; src='SRMSE'; source('FLD-Plot.R')
  mask <- 'CONT'
  FLD=SRMSE+MASK.CONT; src='SRMSE'; source('FLD-Plot.R')
  
  f.Cat('SCORR. ')
  mask <- 'ALL'
  FLD=SCORR; src='SCORR'; source('FLD-Plot.R')
  mask <- 'CONT'
  FLD=SCORR+MASK.CONT; src='SCORR'; source('FLD-Plot.R')
  f.Cat('Ok!\n')
}    
