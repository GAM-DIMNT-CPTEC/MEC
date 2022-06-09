# ==============================================================================
# JRM Garcia (roberto.garcia@inpe.br)
# DIMNT / CGCT / INPE / MCTI
# ==============================================================================

# Current R script name
#args <- commandArgs(trailingOnly=F)
#script.name <- sub("--file=", "", args[grep("--file=", args)])

source('PrepCommonEnv.R')
if (!interactive()) {
  CFG$eval_mode = args[7]  # extra arguments
#} else {
#  CFG$eval_name="REG21"
#  CFG$model="BRAMS_08km"
#  CFG$var="PREC"
#  CFG$ltime="036"
#  CFG$dt=as.Date("2021-01-01")
#  CFG$dt.str="20210101"
#  CFG$init_time="00"
#  CFG$obs="MERGE"
#  CFG$oper="EVAL"
#  CFG$eval_mode=3
}

# TODO
doEvCont=T

source("AUX-EvalFunctions.R")

# Isto precisa ficar aqui pois há vários if que alteram o fluxo
# Thresholds must cut off one element for both sides because they were included 
# for the intervals for histograms and are huge
thresholds = THRES[[CFG$var]][2:(length(THRES[[CFG$var]])-1)]

# Creating files
# ------------------------------------------------------------------------------
# OBS - DEVE CONTER TODOS OS HORÁRIOS DE $HH
# ------------------------------------------------------------------------------
fname.eval.obs <- paste0(DIR$eval, 'OBS_', CFG$var, '_', CFG$obs, '_', substr(CFG$dt.str, 1, 6), '.RData')
if (!file.exists(fname.eval.obs)) {
  # Só a região maior (a primeira) é sem máscara, caso seja para avaliar sem máscara
  if ("ALL" %in% CFG.SET$masks) {
    OBS <- data.table(expand.grid(
      REG=CFG$area1.names[1], MASK='ALL', DT=CFG$dates, HH=hh.targ, TYPE=METRICS$OBS, 
      VALUE=NA_real_, LCI95=NA_real_, UCI95=NA_real_))
  } else {
    OBS = data.table()
  }
  # Acrescenta todas as regiões somente com a máscara CONT
  OBS <- rbind.data.frame(OBS, data.table(expand.grid(
    REG=c(CFG$area1.names, REGIONS$NAME), MASK="CONT", DT=CFG$dates, HH=hh.targ, TYPE=METRICS$OBS, 
    VALUE=NA_real_, LCI95=NA_real_, UCI95=NA_real_)))
  setkey(OBS, REG, MASK, DT, HH, TYPE)
} else {
  load(fname.eval.obs)
}

# ------------------------------------------------------------------------------
# EVAL CONT - Cria arquivos separados por $HH mas devem ser unidos em apenas 1 posteriormente
# ------------------------------------------------------------------------------
fname.eval.cont <- paste0(DIR$eval, 'EVAL_CONT_', CFG$eval.key, '_', substr(CFG$dt.str, 1, 6), '_', CFG$hh, '.RData')
if (!file.exists(fname.eval.cont)) {
  # Só a região maior (a primeira) é sem máscara, caso seja para avaliar sem máscara
  if ("ALL" %in% CFG.SET$masks) {
    EVAL.CONT <- data.table(expand.grid(
      REG=CFG$area1.names[1], MASK='ALL', DT=CFG$dates, HH=CFG$hh, TYPE=METRICS$CONT, 
      VALUE=NA_real_, LCI95=NA_real_, UCI95=NA_real_))
  } else {
    EVAL.CONT = data.table()
  }
  # Acrescenta todas as regiões somente com a máscara CONT
  EVAL.CONT <- rbind.data.frame(EVAL.CONT, data.table(expand.grid(
    REG=c(CFG$area1.names, REGIONS$NAME), MASK="CONT", DT=CFG$dates, HH=CFG$hh, TYPE=METRICS$CONT, 
    VALUE=NA_real_, LCI95=NA_real_, UCI95=NA_real_)))
  setkey(EVAL.CONT, REG, MASK, DT, HH, TYPE)
} else {
  load(fname.eval.cont)
}

METRICS$CAT = c("HITS","FAL_AL","MISS","COR_NEG")
# ------------------------------------------------------------------------------
# EVAL CAT - Cria arquivos separados por $HH mas devem ser unidos em apenas 1 posteriormente
# ------------------------------------------------------------------------------
if (CFG.SET$eval.cat) {
  fname.eval.cat <- paste0(DIR$eval, 'EVAL_CAT_', CFG$eval.key, '_', substr(CFG$dt.str, 1, 6), '_', CFG$hh, '.RData')

  if (!file.exists(fname.eval.cat)) {
    # Só a região maior (a primeira) é sem máscara, caso seja para avaliar sem máscara
    if ("ALL" %in% CFG.SET$masks) {
      comm = paste0("EVAL.CAT <- data.table(expand.grid(REG=CFG$area1.names[1], MASK='ALL', DT=CFG$dates, HH=CFG$hh, THRES=thresholds, ", paste0(METRICS$CAT, "=NA_integer_", collapse=", "), "))")
      eval(parse(text=comm))
    } else {
      EVAL.CAT = data.table()
    }
    # Acrescenta todas as regiões somente com a máscara CONT
    comm = paste0("EVAL.CAT <- rbind.data.frame(EVAL.CAT, data.table(expand.grid(REG=c(CFG$area1.names, REGIONS$NAME), MASK='CONT', DT=CFG$dates, HH=CFG$hh, THRES=thresholds, ", paste0(METRICS$CAT, "=NA_integer_", collapse=", "), ")))")
    eval(parse(text=comm))
    setkey(EVAL.CAT, REG, MASK, DT, HH, THRES)
  } else {
    load(fname.eval.cat)
  }

  # ------------------------------------------------------------------------------
  # Histogram
  # ------------------------------------------------------------------------------
  fname.hist = paste0(DIR$eval, 'HIST_', CFG$eval.key, '_', substr(CFG$dt.str, 1, 6), '_', CFG$hh, '.RData')
  my.breaks = THRES[[CFG$var]]      # breaks with "Inf" values in both sides
  # In order to be stored, there are lenght(my.breaks)-1 bins
  my.bins = my.breaks[1:length(my.breaks)-1]

  if (!file.exists(fname.hist)) {
    # Só a região maior (a primeira) é sem máscara, caso seja para avaliar sem máscara
    if ("ALL" %in% CFG.SET$masks) {
      HIST <- data.table(expand.grid(FCT=c(CFG$model, 'OBS'), REG=CFG$area1.names[1], MASK='ALL', 
                                     THRES=my.bins, FREQ=NA_integer_, PERC=NA_real_))
    } else {
      HIST = data.table()
    }
    # Acrescenta todas as regiões somente com a máscara CONT
    HIST <- rbind.data.frame(HIST,
            data.table(expand.grid(FCT=c(CFG$model, 'OBS'), REG=c(CFG$area1.names, REGIONS$NAME), MASK="CONT", 
                                   THRES=my.bins, FREQ=NA_integer_, PERC=NA_real_)))
    setkey(HIST, FCT, REG, MASK, THRES)
  } else {
    load(fname.hist)
  }
}

source('ReadNcOBS.R'); source('ReadNcFCT.R')
f.Cat('*** ', CFG$dt.str, " *** ", CFG$eval.key, ': ')
MASK.ALL <- matrix(0, nrow=length(CFG$lon), ncol=length(CFG$lat))  # Template

plot.debug <- F
if (plot.debug) {
  cont=1
  map(xlim=range(CFG$lon), ylim=range(CFG$lat))
  box()
}

# DS de guia para fazer as avaliações, contem as combinações de região e máscara criados
GUIDE_DS = unique(OBS[,list(REG, MASK)])
idx_g=1
for (idx_g in 1:nrow(GUIDE_DS)) {
  reg=as.character(GUIDE_DS[idx_g, REG])
  mask=as.character(GUIDE_DS[idx_g, MASK])

  #f.Cat(".")
  #f.Cat("\n(", reg, ", ", mask, ", ")
  if (CFG$eval_mode != 1) {
    f.Cat("\n", CFG$dt.str, " (", reg, ",")
  } else {
    f.Cat(".")
  }

  if (reg %in% REGIONS$NAME) {
    idx.reg <- which(REGIONS$NAME == reg)
    idx.lon  <- which(CFG.SET$lon >= REGIONS$LON1[idx.reg] & CFG.SET$lon <= REGIONS$LON2[idx.reg])
    idx.lat  <- which(CFG.SET$lat >= REGIONS$LAT1[idx.reg] & CFG.SET$lat <= REGIONS$LAT2[idx.reg])
  } else {
    idx.lon  <- 1:length(CFG.SET$lon)   # Captura toda regiao se reg não for quadrante 
    idx.lat  <- 1:length(CFG.SET$lat)   # que estão delimitados em REGIONS
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

  FCT.REG  <- NC.FCT[idx.lon,idx.lat]
  OBS.REG  <- NC.OBS[idx.lon,idx.lat]
  MASK.REG <- MASK.CONT[idx.lon,idx.lat]
  
  # plot reg by reg, for debugging
  if (plot.debug) {
    image(x=CFG.SET$lon[idx.lon], y=CFG.SET$lat[idx.lat], z=MASK.REG, add=T, col=cont, useRaster=T)
    legend(-60, -45, paste0(reg, paste0(rep(' ', times=10), collapse='')), cex=1, bg='white')
    cont=cont+1
  } 

  # Aplica a mascara do conjunto de dados (CONT=0, OCEANO ou outra parte=NA), basta somar pois 0 nao influencia e 
  # qq soma com NA se torna NA. E transforma em vetor.
  vFCT <- c(FCT.REG + MASK.REG)
  vOBS <- c(OBS.REG + MASK.REG)

  # ------------------------------------------------------------------------------
  # Eliminating all possible NAs from the FCT and OBS fields (which are a vectors now) 
  # Here that MASKS, in fact, take effect because every NA value is deleted from both vectors
  # ------------------------------------------------------------------------------
  bad <- unique(c(which(is.na(vFCT)), which(is.na(vOBS))))
  if (length(bad) > 0) {
    vFCT <- vFCT[-bad]
    vOBS <- vOBS[-bad]
  }
  if (CFG$eval_mode != 1) {
    f.Cat(length(vOBS), '):')
  }

  # CFG$eval_mode ==> 1=CONT+CAT | 2=IDEM + CI | 3=CI ONLY (CONT AND CAT)
  # ------------------------------------------------------------------------------
  # CONTINUOS EVALUATION (COR IS ON-THE-FLY)
  # ------------------------------------------------------------------------------
  if (CFG$eval_mode %in% c(1,2) && doEvCont) {
    if (CFG$eval_mode != 1) {
      f.Cat("\nCont,")
    }
    source("EvalCont.R")
  } 

  # ------------------------------------------------------------------------------
  # CI FOR CONTINUOUS EVALUATION (by bootstraping)
  # ------------------------------------------------------------------------------
  if (CFG$eval_mode %in% c(2,3) && CFG.SET$eval.ci.cont && doEvCont) {
    #source("EvalCI_Cont.R")
  }

  # ------------------------------------------------------------------------------
  # CATEGORICAL EVALUATION uses raw FCT/OBS values and not the mean
  # ------------------------------------------------------------------------------
  if (CFG.SET$eval.cat) {
    if (CFG$eval_mode %in% c(1,2)) {
      if (CFG$eval_mode != 1) {
        f.Cat("\nCat,")
      }
      #source("EvalCat.R")

      #if (CFG$eval_mode != 1) {
      #  f.Cat(" Hist,")
      #}
      #source("EvalHist.R")
    }

    # ------------------------------------------------------------------------------
    # CI FOR CATEGORICAL EVALUATION (by bootstraping)
    # ------------------------------------------------------------------------------
    if (CFG$eval_mode %in% c(2,3) && CFG.SET$eval.ci.cat) {
      #source("EvalCI_Cat.R")
    }
  }
  #f.Cat(".")
} # for DS-GUIDE

#f.Cat("\nSaving ")

# EVAL CONT
setkey(EVAL.CONT, REG, MASK, DT, HH, TYPE)
save(EVAL.CONT, file=fname.eval.cont)

# OBS
setkey(OBS, REG, MASK, DT, HH, TYPE)
save(OBS, file=fname.eval.obs)

# EVAL CAT
if (CFG.SET$eval.cat) {
  setkey(EVAL.CAT, REG, MASK, DT, HH, THRES)
  save(EVAL.CAT, file=fname.eval.cat)

  setkey(HIST, FCT, REG, MASK, THRES)
  save(HIST, file=fname.hist)
}
f.Cat(' Ok!\n')

