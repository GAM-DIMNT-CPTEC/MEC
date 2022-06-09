# ==============================================================================
# JRM Garcia (roberto.garcia@inpe.br)
# DIMNT / CGCT / INPE / MCTI
# ------------------------------------------------------------------------------
# Cria o DS de eventos (mudanças) dos modelos para mostrar na interface
# ==============================================================================
rm(list=ls())
MEC_eval_name=Sys.getenv("MEC_eval_name")  # needs export MEC_eval_name=xxx before
source(paste0(MEC_eval_name, "/EVAL_SET.R"))

fname.txt <- paste0(MEC_eval_name, "/EVENTS.TXT")
if (file.exists(fname.txt)) {
   EVENTS <- read.csv(fname.txt, sep=';', header=T)
   EVENTS$DT <- as.Date(as.character(EVENTS$DT), format='%Y%m%d')
   EVENTS$HH <- as.character(EVENTS$HH)
   EVENTS$EVENT <- as.character(EVENTS$EVENT)
   fname.rdata <- paste0(MEC_D_deploy, '/EVENTS.RData')
   save(EVENTS, file=fname.rdata)
}
