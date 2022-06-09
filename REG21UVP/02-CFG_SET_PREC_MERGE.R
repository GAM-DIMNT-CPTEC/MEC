# ==============================================================================
# Particular configuration creation for each EVALUATION + VARIABLE + OBS tuple
# JRM Garcia (roberto.garcia@inpe.br)
# DIMNT / CGCT / INPE / MCTI
# ==============================================================================
rm(list=ls())
cat("\n========== 02-CFG_SET_PREC_MERGE.R ==========")
source("EVAL_SET.R")
source(paste0(MEC_D_mec, "/LoadCommonDSs.R"))
source(paste0(MEC_D_mec, "/LoadCommonFunctions.R"))
CFG$var <- 'PREC'
CFG$obs <- 'MERGE'
CFG$cfg.set <- paste0(CFG$var, '_', CFG$obs)

# ------------------------------------------------------------------------------
f.Cat('\n* Creating configuration for ', CFG$cfg.set, ' ... ')
# ------------------------------------------------------------------------------
CFG.SET <- list()
CFG.SET$obs1 <- 'MERGE'            # Name that will appear in the field plots
CFG.SET$obs2 <- 'GPM'              # String that will appear below the CFG.SET$obs1 (version of the obs?)
CFG.SET$masks <- c('ALL', 'CONT')  # Used to gennerate images and evaluation
CFG.SET$mask.obs <- F              # T=OBS is a mask (for station data) | F=Don't apply observation as a mask
CFG.SET$eval.ci.cont <- T          # Do or do not confidence interval calculations for continuous? 
CFG.SET$eval.ci.cat  <- T          # Idem for categorical
CFG.SET$eval.cat  <- T             # Do categorical evaluation or not?

f.SaveCfgSet()
f.Cat('Ok!\n')
