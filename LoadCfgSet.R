CFG$eval.key <- paste0(c(CFG$model,CFG$var,CFG$ltime,CFG$obs), collapse="_")
CFG$cfg.set <- paste0(CFG$var, '_', CFG$obs)
if (!file.exists(paste0(DIR$eval, '/CFG_', CFG$cfg.set, '.RData'))) {
  stop('Files does not exist:\n', paste0(DIR$eval, '/CFG_', CFG$cfg.set, '.RData'))
}
load(paste0(DIR$eval, '/CFG_', CFG$cfg.set, '.RData'))
# Carrega mascara do continente
reg="1-South America"
fname.mask <- paste0(DIR$masks, CFG$mask.cont.prefix, CFG$domain, '_', reg, '_', CFG$mask.cont.suffix, '.RData')
if (!file.exists(fname.mask)) {
  stop(paste0("Mask file not found: ", fname.mask))
} else {
  load(fname.mask)
}
