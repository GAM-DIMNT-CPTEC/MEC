# ==============================================================================
# ZR Garcia (roberto.garcia@inpe.br)
# DIMNT / CGCT / INPE / MCTI
# ------------------------------------------------------------------------------
# Plot the monthly average (each day 01MMYYY) 
# ==============================================================================
rm(list=ls())
source('PrepCommonEnv.R')

# Loading file from the last month
# OBS 
fname.eval.obs <- paste0(DIR$eval, 'OBS_', CFG$var, '_', CFG$obs, '_', substr(CFG$dt.str, 1, 6), '.RData')
if (file.exists(fname.eval.obs)) {
  load(fname.eval.obs)
  
  # EVAL CONT 
  # Verifica qual EVAL CONT mais atual para ver o ultimo dia que houve avaliacao
  # para poder ver se vai plotar ou não
  patt <- paste0("EVAL_CONT_", CFG$eval.key, "_(.*)_", CFG$hh, ".RData$")
  fnames <- sort(dir(path=DIR$eval, pattern=patt), decreasing=T)
  fname.eval.cont <- paste0(DIR$eval, fnames[1])
  load(fname.eval.cont)
  # necessita ter feito a avaliacao primeiro para saber quando foi o ultimo dia
  last.day <- max(EVAL.CONT[!is.na(VALUE)]$DT)
  MASK.ALL <- matrix(0, nrow=length(CFG$lon), ncol=length(CFG$lat))  # Template
  
  nDias <- difftime(last.day, CFG$dt)
  if (nDias <= 500) {
    # Abre o EVAL CONT, do mês que está sendo processado
    fname.eval.cont <- paste0(DIR$eval, 'EVAL_CONT_', CFG$eval.key, '_', substr(CFG$dt.str, 1, 6), "_", CFG$hh, '.RData')
    load(fname.eval.cont)
  
    source('FLD-PlotConfig.R')
    source('ReadNcOBS.R')
    source('ReadNcFCT.R')
  
    f.Cat('  ', CFG$ltime, ': Daily plot, last eval date: ', as.character(last.day), ' Plot D-', nDias, ': ')

    #MASK.ALL <- matrix(0, nrow=length(CFG$lon), ncol=length(CFG$lat))  # Template
    load(paste0(DIR$masks, CFG$mask.cont.prefix, CFG$domain, '_', CFG$area1.names[1], '_', CFG$mask.cont.suffix, '.RData'))
  
    reg <- CFG$area1.names[1]
    fig.type='DIÁRIA'
    masks <- CFG.SET$masks
    mask=masks[1]
    for (mask in masks) { # ALL, CONT 
      f.Cat(mask, '...')
   
      # MASKS OBJECTS: MASK | MASK.NOREG
      # VARIATIONS: ALL | ALL-REG | CONT | CONT-REG
      if (grepl('ALL', mask)) {
        MASK <- MASK.ALL
      } else {
        MASK <- MASK.CONT
      }
     
      # OBS
      if (mask == 'CONT') {
         FLD <- pmax(NC.OBS, MASK)
      } else {
         FLD <- NC.OBS
      }
      src='OBS' ; source('FLD-Plot.R')
  
      # FCT
      if (mask == 'CONT') {
         FLD <- pmax(NC.FCT, MASK)
      } else {
         FLD <- NC.FCT
      }
      src='FCT' ; source('FLD-Plot.R')
  
      # BIAS
      if (mask == 'CONT') {
         FLD <- NC.FCT-NC.OBS+MASK
      } else {
         FLD <- NC.FCT-NC.OBS
      }
      src='BIAS'; source('FLD-Plot.R')
  
    } # for MASKS
  } else {
    f.Cat('    ** Last eval date: ', as.character(last.day), ' Skipping daily plot D-', nDias, ' ... ')
  }
   
  f.Cat('Ok!\n')
} else {
  f.Cat('  ', CFG$ltime, ': Daily plot, file does not exist: ', fname.eval.obs, '\n')
}
