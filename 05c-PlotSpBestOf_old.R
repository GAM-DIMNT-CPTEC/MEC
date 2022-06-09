# ==============================================================================
# ZR Garcia (roberto.garcia@inpe.br)
# DIMNT / CGCT / INPE / MCTI
# ------------------------------------------------------------------------------
# Plot best of consolidated fields (each day 01MMYYY) 
# ==============================================================================

if (interactive()) {
  MEC_eval_name="REG21UVP"
  MEC_var="VEL10M"
  MEC_ltime="024"
  MEC_models_cmp='BRAMS_08km Eta_08km WRF_07km'
  MEC_init_time="00"
  MEC_last_month="202101"
  MEC_last_season=NA
} else {
  # Current R script name
  args <- commandArgs(trailingOnly=F)
  script.name <- sub("--file=", "", args[grep("--file=", args)])
  
  args <- commandArgs(trailingOnly=T)
  if (!length(args) %in% c(7)) {
    cat("\nMEC -", script.name, "\n\n")
    cat("Rscript", script.name, " <EVAL_NAME> <VAR> <INIT_TIME> <LAST_EVAL_DATE>\n\n")
    cat("<EVAL_NAME>      A name of the evaluation, e.g. REG21 | Eta | ...\n")
    cat("<VAR>            Variable name, e.g. PREC | TMAX | TMIN | ...\n")
    cat("<LTIME>          Lead time of the evaluation, e.g. 036 | 060 | 084 | ...\n")
    cat("<MODELS>         Model names to involve, e.g. 'BRAMS_08km Eta_08km WRF_07km'\n")
    cat("<INIT_TIME>      Model's initialization time, e.g. 00 | 12\n")
    cat("<LAST_MONTH>     Last evaluated month in YYYYMM (this script always runs at day 01)")
    cat("<LAST_SEASON>    Last evaluated season, <season_id> | NA (not every day 01 is the beginning od a season)")
    stop("Wrong number of arguments!\n")
  }
  MEC_eval_name=args[1]
  MEC_var=args[2]
  MEC_ltime=args[3]
  MEC_models_cmp=args[4]
  MEC_init_time=args[5]
  MEC_last_month=args[6]
  MEC_last_season=args[7]
}

MEC_dont_check_args=T; source('PrepCommonEnv.R')
source('FLD-PlotConfig.R')
MEC_models_cmp=unlist(strsplit(MEC_models_cmp, split=" "))

# Loads MASK.CONT
load(paste0(DIR$masks, CFG$mask.cont.prefix, CFG$domain, '_', CFG$area1.names[1], '_', CFG$mask.cont.suffix, '.RData'))

MEC_metrics = c("SCORR", "SRMSE", "SBIAS")
MEC_percs   = c("SBIAS_RATE", "SCORR_RATE", "SRMSE_RATE") # bestof rate for each model

# ==============================================================================
# Month comparison
# ==============================================================================
season=MEC_last_month
fig.type="MENSAL"
my_pal = c("purple3", "gold", "dodgerblue2")
masks = c("ALL", "CONT")
src="BESTOF"

# Load eval.cont.per como guia
fname.eval.per <- paste0(DIR$eval, 'EVAL_CONT_PER_', CFG$eval.key, '_', season, "_", CFG$hh, '.RData')
load(fname.eval.per)

MEC_metric=MEC_metrics[1]; idx_mod=1
for (MEC_metric in MEC_metrics) {
  f.Cat("    - Performing best of wrt ", MEC_metric, ": ")

  # Get all FLD files available
  avail_mod = vector()
  idx_mod=1
  for (idx_mod in 1:length(MEC_models_cmp)) {
     MEC_model=MEC_models_cmp[idx_mod]
     fname_fld = paste0(DIR$eval, "FLD_", MEC_metric, "_", MEC_model, "_", MEC_var, '_', MEC_ltime, "_", season, '.RData')
     if (file.exists(fname_fld)) {
        avail_mod = c(avail_mod, idx_mod)
     }
  }

  if (length(avail_mod) > 0) {
    f.Cat(length(avail_mod), " models found: ", paste0(MEC_models_cmp[avail_mod], collapse="+"), " ")
    # 3D array with every consolidated fields
    FLDS <- array(NA, c(length(CFG$lon), length(CFG$lat), length(avail_mod)))
    idx_fld=1
    for (idx_fld in 1:length(avail_mod)) {
      MEC_model = MEC_models_cmp[avail_mod[idx_fld]]
      # Filenames of the consolidated 2D matrix of each model
      fname_met = paste0(DIR$eval, "FLD_", MEC_metric,"_", MEC_model, '_', MEC_var, '_', MEC_ltime, "_", season, '.RData')
      #print(fname_met)
      load(fname_met)  # Loads as "FLD", already checked if exists
      FLDS[,,idx_fld] = copy(FLD)
    }

    # 2D array to store the best of wrt the metric
    fld_dim = dim(FLDS)
    if (MEC_metric == "SCORR") {            # Higher corr is better
      BESTOF = matrix(as.integer(apply(FLDS, c(1,2), which.max)), nrow=fld_dim[1])
    } else {
      BESTOF = matrix(as.integer(apply(abs(FLDS), c(1,2), which.min)), nrow=fld_dim[1])
    }
    my.colors = my_pal[avail_mod]

    #dev.new()
    #par(mar=c(1,2,0,5))
    #image(x=round(CFG$lon,2), y=round(CFG$lat,2), z=FLD, useRaster=T, col=my.colors, xlab='', ylab='', pty='m', axes=F)
    #axis(1); axis(2, las=2)
    #perc = paste0(round(prop.table(table(c(FLD)))*100, 1), "%")
    #if (MEC_metric == "SCORR") {
    #  my.leg = paste0(c("Sem Cor", MEC_models_cmp), "\n", perc, c("", paste0(" (",  metr, ")")))
    #} else {
    #  my.leg = paste0(MEC_models_cmp, "\n", perc, paste0(" (",  metr, ")"))
    #} 
    #legend(x=max(CFG$lon), y=min(CFG$lat), legend=my.leg, fill=my.colors, xpd=T, pt.cex=2, horiz=F, xjust=1, yjust=0,
    #         bg="white", y.intersp=2)
    #plot(SHP_BR_REG, add=T)
    #map(add=T, col = 'black', lwd=2, xlim = range(CFG$lon), ylim = range(CFG$lat))
  
    for (mask in masks) {
      FLD = copy(BESTOF)
      metr = round(apply(FLDS, 3, mean, na.rm=T), 2)
      if (mask == "CONT") {
        FLD = FLD + MASK.CONT
        metr = round(apply(FLDS+rep(MASK.CONT,fld_dim[3]), 3, mean, na.rm=T), 2)
      }
      # Considering only non -1 grid points (-1 is no data)
      perc = paste0(round(prop.table(table(FLD, useNA="no"))*100, 1), "%")
      my.leg = paste0(MEC_models_cmp[avail_mod], "\n", perc, paste0(" (",  metr, ")"))
      source('FLD-Plot.R')
    }
    f.Cat('Ok!\n')

    f.Cat("    - Evaluating bestof rate by region: ")
    GUIDE_DS = unique(EVAL.CONT.PER[, list(REG, MASK)])
    MASK.ALL <- matrix(0, nrow=length(CFG$lon), ncol=length(CFG$lat))  # Template
    idx_g=1
    for (idx_g in 1:nrow(GUIDE_DS)) {
      reg=as.character(GUIDE_DS[idx_g, REG])
      mask=as.character(GUIDE_DS[idx_g, MASK])
  
      f.Cat(".")
      #f.Cat(" (", reg, ", ", mask, ", ")
  
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
  
      BESTOF_REG <- BESTOF[idx.lon, idx.lat]
      MASK.REG   <- MASK.CONT[idx.lon,idx.lat]
  
      # Aplica a mascara do conjunto de dados (CONT=0, OCEANO ou outra parte=NA), basta somar pois 0 nao influencia e 
      # qq soma com NA se torna NA. E transforma em vetor.
      vBESTOF <- c(BESTOF_REG + MASK.REG)
  
      # ------------------------------------------------------------------------------
      # Eliminating all possible NAs from the evaluating vectors
      # Here that MASKS, in fact, take effect because every NA value is deleted from both vectors
      # ------------------------------------------------------------------------------
      bad <- which(is.na(vBESTOF))
      if (length(bad) > 0) {
        vBESTOF <- vBESTOF[-bad]
      }
      my_val = prop.table(table(vBESTOF, useNA="no"))*100

      idx_mod=avail_mod[1]
      for (idx_mod in avail_mod) {
         MEC_model=MEC_models_cmp[idx_mod]
         # EVAL CONT PER of each model in the result, must be created by 05b-PlotPrevious.R
         fname.eval.per <- paste0(DIR$eval, 'EVAL_CONT_PER_', CFG$eval.key , '_', season, "_", CFG$hh, '.RData')
         load(fname.eval.per)
         # Persisting
         EVAL.CONT.PER[.(reg, mask, season, CFG$hh)][[paste0(MEC_metric, "_RATE")]] = my_val[idx_mod]
         if (is.na(EVAL.CONT.PER[.(reg, mask, season, CFG$hh)][[paste0(MEC_metric, "_RATE")]])) {
           stop(paste("* ERR:", script_name, reg, mask, season, CFG$hh, my_val[idx_mod]))
         }
         setkey(EVAL.CONT.PER, REG, MASK, PER, HH)
         save(EVAL.CONT.PER, file=fname.eval.per)
      }
    } # for GUIDE
  } else {
     f.Cat("No comparable models available!\n")
  }
}

