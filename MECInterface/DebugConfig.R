rm(list=ls())
setwd('~/ProjDocs/Science/Projects/interative/MEC-DEV/')
source('global.R')
#source('Functions.R')
config=list();config$max.ets=0.5;config$text.size=7; 
config$allInOne=F
input=list(); 
input$maskId='CONT'; input$regioesId=F; input$modId=CFG$models
input$evsetId='PREC-MERGE'; 
#input$evsetId='TMAX-SAMeT'; 
#input$evsetId='VEL10M-GFSANL'; 
input$runTimeId='00'; input$anoMesBeg="2021"; input$anoMesEnd="2021"
#input$runTimeId='12'; input$anoMesBeg="2018"; input$anoMesEnd="2021"
input$ltimes=c(1,1)
input$ltimes2=1
input$area0Id <- "12"
input$regId <- 'AS'; input$facetReg=F
input$mainRegId <- '1'
input$subRegId <- GetSubRegs(input)[1]
input$regBrId = "AS"
input$appendYear=T; 
input$ViewMode='Cont'
input$langId = "en"

# Metrics CONT
input$allInOne=F; 
input$contId='BIAS'  # BIAS | PREV x OBS
input$ncolsContId=0; input$ticksMult=10
input$showVal=F
input$showCI=F
input$groupTsId='Dia'  # Dia | Semana | Mês | Estação
group='Data'
input$modRefId="GFS_30km"
input$doSkillScore=T

# Análise espacial
input$contSpId='SBIAS'  # SBIAS | SCORR | SRMSE
input$aeTempoId = "M"         # "D", 'M','S',"R"
input$aeTypeId = "BESTOF"     # 'PREVxOBS','BIAS',"SRMSE","SCORR","BESTOF"
input$perId="202101"
input$coloredFLD=F
input$doFilterScorr=T
input$scorrThres=0.349
input$numIsoLevels=1
input$isolineLabels = NULL
input$isolineColor = "Scale"

# Metrics CAT
input$catId="ETS"

# histogramas
#input$catNumBreaksId=6
input$thresId=THRES[["PREC"]]

# ScoreCard
input$cont2Id='BIAS'; input$colScaleId='GRAD' # GRAD | FIXED
input$areaId='12'

# Diagrama de performance
input$typeLegPerfDiag='Círculos'

#CoresMod <- function() {
#cores.mod[which(CFG$models %in% input$modId)]
#}
GetCorr    <- function(data, v1='OBS', v2='FCT') {}
GetEvalSet <- function() { sub('-', '_', input$evsetId) }
GetVar     <- function() { unlist(strsplit(input$evsetId, '-'))[1] }
GetObs     <- function() { unlist(strsplit(input$evsetId, '-'))[2] }


DSCat <- function() {
  RES <- f.FilterDS('EVAL_CAT', input)[, list(HITS=sum(HITS), FAL_AL=sum(FAL_AL), 
                                              MISS=sum(MISS), COR_NEG=sum(COR_NEG)), 
                                       by=list(MOD, REG, LTIME, THRES)]
  RES$LTIME = as.factor(RES$LTIME)
  setkey(RES, MOD, REG, LTIME, THRES)
  RES
}
