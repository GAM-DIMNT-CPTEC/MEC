# ==============================================================================
# Common configuration creation for the evaluation suite
# JRM Garcia (roberto.garcia@inpe.br)
# DIMNT / CGCT / INPE / MCTI 
# ==============================================================================
rm(list=ls())
cat("\n========== 01-CFG_Main.R ==========")
source("EVAL_SET.R")

# Get directory of this script name, which is also the name of the evaluation set
# That is a directory in /dados/dmdpesq/roberto.garcia/MEC_DATA
#args <- commandArgs(trailingOnly=F)
#file.arg.name <- "--file="
#script.name <- sub(file.arg.name, "", args[grep(file.arg.name, args)])
#MEC_eval_name <- dirname(script.name)

CFG <- list()
CFG$ename <- MEC_eval_name
CFG$etype <- 'daily'  # Ou outra coisa qq, ajusta datain para pegar dir chamado YYYYMM
CFG$domain <- 'AS'
CFG$verb <- T
CFG$mask.cont.prefix <- 'MASK_CONT_'  # Name should be in the format <PREFIX>_<DOMAIN>_<PATTERN>_<SUFFIX>.RData | NA
CFG$mask.cont.suffix <- '0.1'
CFG$ci.level = 95  # 95% of the confidence interval (CI)
CFG$ci.bs.n  = 500 # Fiz estudos, a variação é a partir da 2a. casa decimal, achei custo-benefício bom baixar de 
                   # 1000 para 500 iterações no cálculo do IC via bootstrap. Abaixo disso (300) aparece a msg 
                   # Some intervals may be unstable: Such intervals should be considered very unstable and not 
                   # relied upon for inferences. Even if the extreme values are not used, it is possible that the 
                   # intervals are unstable if they used quantiles close to the extreme values. The function alerts 
                   # the user to intervals which use the upper or lower 10 order statistics with the message

# ------------------------------------------------------------------------------
cat('\n- Directories ... ')
# Todos os arquivos NetCDF (prontos) devem estar aqui, não procura em subdiretorios
# Cria os outputs no online9 pois dá para fazer FTP para o desktop 
# (a eslogin15 não permite acesso senão das eslogins) 
# ------------------------------------------------------------------------------
DIR <- list()
# These dirs below must exist and contain files
DIR$mod  <- paste0(MEC_D_data, "/", MEC_eval_name, "/DATA_MODEL/NCDF/")
DIR$obs  <- paste0(MEC_D_data, "/", MEC_eval_name, "/DATA_OBS/NCDF/")
# The remaining below must be created
DIR$eval <- paste0(MEC_D_data, "/", MEC_eval_name, "/EVAL/")
DIR$img <- paste0(MEC_D_data, "/", MEC_eval_name, "/IMG/")
DIR$masks <- paste0(MEC_D_data, "/", MEC_eval_name, "/MASKS/")
DIR$deploy <- paste0(MEC_D_data, "/", MEC_eval_name, "/DEPLOY/")
DIR$shp <- "/dados/dmdpesq/roberto.garcia/CommonData/shapefiles/"
save(DIR, file=paste0(DIR$deploy, "DIR.RData"))
cat('Ok!')

# Copia as mascaras para o dir de saida (que vai para a interface)
mask.files <- dir(path=DIR$masks, pattern=paste0(CFG$mask.cont.prefix, CFG$domain, '_.*_', CFG$mask.cont.suffix, '.RData'))
#file.copy(paste0('../CommonData/masks/', mask.files), paste0(DIR$data, 'masks'), overwrite=T)
CFG$area1.names <- substring(mask.files, nchar(paste0(CFG$mask.cont.prefix, CFG$domain, '_'))+1, nchar(mask.files)-nchar(paste0('_', CFG$mask.cont.suffix, '.RData')))
save(CFG, file=paste0(DIR$deploy, "/CFG.RData"))

# ==============================================================================
cat('\n- Metrics:\n')
# ==============================================================================
METRICS <- list()
# Nao tem "correlação" e na avaliação diária, é preciso juntar mais: mês, trimestre, etc
# Tratar no app
# Colocar as métricas em ordem alfabética mas 
METRICS$CONT = c("AVG", "BIAS", "MAE", "MSE", 'RMSE', "SD", "URMSE", "VAR")
#METRICS$CAT  = c("ETS", "FAR", "FBIAS", "HK", "HSS", "OR", "ORSS", "PC", "POD", "POFD", "SR", "TS")
METRICS$CAT  = c("ETS", "FAR", "FBIAS", "PC", "POD", "SR")
METRICS$OBS  = c("AVG", "SD", "VAR")
# Fica muito HD consumer caso haja todas as métricas categóricas (10)
# Tentando fazer os cálculos on-the-fly
save(METRICS, file=paste0(DIR$deploy, 'METRICS.RData'))
cat('  . CONT: ', paste0(METRICS$CONT, collapse=','), '\n')
cat('  . CAT.: ', paste0(METRICS$CAT , collapse=','), '\n')
cat('  . OBS.: ', paste0(METRICS$OBS , collapse=','))

# ==============================================================================
cat('\n- Squared regions: ')
# ==============================================================================
REGIONS <- read.csv("Regions.csv", header=T, sep=';', stringsAsFactors=F)
save(REGIONS, file=paste0(DIR$deploy, 'REGIONS.RData'))
cat(paste0(REGIONS$NAME, collapse=','), ' - Ok!')

# ==============================================================================
cat('\n- Categorical thresholds ... ')
# Rainfall
# Observed rainfall intensity threshold >= (1, 2, 5, 10, 20, 50 mm d-1). -- RECOMN
#                                 idem plus, 100, 250, 200 - Plot from recomm
#CFG$thres.prec.bin <- c(.254, 2.54, 6.35, 12.7, 19.05, 25.4, 38.1, 50.8, 76.2)
# ==============================================================================
THRES = list()
THRES[["PREC"]] = c(0, 1, 2, 5, 10, 20, 30, 50, 1000)
THRES[["TMIN"]] = c(-100, seq(-6, 42, by=3), 100)
THRES[["TMAX"]] = c(-100, seq(-6, 42, by=3), 100)
THRES[["V10M"]] = c(seq(0, 8, by=.5), 100)    # ???
save(THRES, file=paste0(DIR$deploy, 'THRESHOLDS.RData'))
cat('Ok!\n')

