#install.packages("magick")
if (grepl('DEV', getwd())) {
  #devtools::install_github("rstudio/profvis")
  #library(profvis)
  #library(pryr)
  #object.size(EVAL.CONT)
  #object_size(EVAL.CONT)
}

#profvis({
suppressMessages({library(data.table)})         # Data table 
suppressMessages({library(ggplot2)})            # Gráficos
suppressMessages({library(maps)})               # Mapa de contorno da A.S.
suppressMessages({library(plotrix)})            # Taylor(), boxed.labels()
suppressMessages({library(sf)})                 # st_read(shp) - The best option for Maptools, Simple Feature
suppressMessages({library(shiny)})              # This interactive framework
suppressMessages({library(shinyWidgets)})       # sliderTextInput() e outros widgets incrementados
suppressMessages({library(shinydashboard)})     # dashboard
#suppressMessages({library(shinydashboardPlus)}) # dashboard+ (sobre-escreve) *** unsupported *** non official
suppressMessages({library(shinythemes)}) 
suppressMessages({library(dashboardthemes)})    # Themes for Shinydashboard 
suppressMessages({library(RColorBrewer)})       # Themes for Shinydashboard 
suppressMessages({library(viridis)})            # Color palettes
suppressMessages({library(pryr)})               # mem_used()

#suppressMessages({library(verification)})   # Avaliação categórica
#suppressMessages({library(maptools)})       # readShape...() - DEPRECATED by "sf"
#suppressMessages({library(scales)})        # pretty_breaks()
#suppressMessages({library(plotly)})        # Gráficos interativos
#suppressMessages({library(DT)})            # output$TableSumm <- DT::renderDataTable({
#suppressMessages({library(gridExtra)})     # arrangeGrob() 
#suppressMessages({library(shinyjs)})       # enable/disable components
#suppressMessages({library(raster)})        # An option for maptools 

#?`maps-deprecated`
#install.packages('mapdata')
#library(mapdata)

EVAL.CUR <- readLines('AUX-CURRENT_EVAL.txt')
load(paste0(EVAL.CUR, '/CFG.RData'))
load(paste0(EVAL.CUR, '/EVAL_CONT_1_00.RData'))
load(paste0(EVAL.CUR, "/OBS_", gsub("-", "_", EVAL.CONT$EV_SET[1]), ".RData"))
if (file.exists(paste0(EVAL.CUR, '/EVAL_CAT_1_00.RData'))) {
  load(paste0(EVAL.CUR, '/EVAL_CAT_1_00.RData'))
}
load(paste0(EVAL.CUR, '/METRICS.RData'))
load(paste0(EVAL.CUR, '/REGIONS.RData'))
load(paste0(EVAL.CUR, '/THRESHOLDS.RData'))

if (file.exists(paste0(EVAL.CUR, '/EVAL_CONT_PER_00.RData'))) {
  load(paste0(EVAL.CUR, '/EVAL_CONT_PER_00.RData'))
}
CFG$pers = sort(unique(as.character(EVAL.CONT.PER$PER)))

fname.events <- paste0(EVAL.CUR, '/EVENTS.RData')
if (file.exists(fname.events)) {
  load(fname.events)
}
#CFG$models = sort(as.character(unique(EVAL.CONT[MOD!="BAM_20km"]$MOD)))
CFG$models = sort(as.character(unique(EVAL.CONT$MOD)))

models.inputs = list()
models.inputs = lapply(CFG$models, function(x) models.inputs[[x]] = prettyToggle(
  inputId = x,
  label_on = x, 
  icon_on = icon("check"),
  status_on = "info",
  status_off = "warning", 
  label_off = x,
  icon_off = icon("remove")
))
names(models.inputs) <- CFG$models

CFG$lang = "pt" #substring(Sys.getlocale("LC_TIME"), 1, 2)

source('Functions.R')
source('FunColors.R')
source('FunCI.R')
source('PerformDiag.R')
source('TaylorDiag.R')
source('FilterDS.R')

TL = data.table(read.csv("Translations.csv", sep=";", header=T))
TL$pt = trimws(TL$pt)
TL$en = trimws(TL$en)
setkey(TL)

THRES$PREC <-  THRES$PREC[2:(length(THRES$PREC)-1)] # EXTREMOS ABERTOS
THRES$TMAX <-  THRES$TMAX[2:(length(THRES$TMAX)-1)]
THRES$TMIN <-  THRES$TMIN[2:(length(THRES$TMIN)-1)]
THRES$V10M <-  THRES$V10M[2:(length(THRES$V10M)-1)]

# Atualizar pacote em desktop sf ou rgdal
shp.br.uf <- read_sf(paste0(EVAL.CUR, "/SHAPEFILES/BRA/estados/estados_2010.shp"))
shp.br.reg <- read_sf(paste0(EVAL.CUR, "/SHAPEFILES/BRA/regioes/regioes_2010.shp"))

CFG$ev.sets <- unique(EVAL.CONT$EV_SET)
CFG$ltimes <- 1:max(EVAL.CONT.PER$LTIME) #ifelse(EVAL.CUR=="CPTEC_DAILY", 11, 9)
CFG$anomes.beg <- format(min(EVAL.CONT$DT), "%Y%m")
CFG$anomes.end <- format(max(EVAL.CONT$DT), "%Y%m")

CFG$RegUf <- list() #names(CFG$RegUf)
CFG$RegUf[['CO']] <- c('DF','GO','MS','MT')
CFG$RegUf[['N']]  <- c('AM','AC','AP','PA','RO','RR','TO')
CFG$RegUf[['NE']] <- c('AL','BA','CE','MA','PB','PE','PI','RN','SE')
CFG$RegUf[['S']]  <- c('PR','RS','SC')
CFG$RegUf[['SE']] <- c('ES','MG','RJ','SP')

CFG$area0.names <- c(tl('Am.Sul e Brasil'),tl('Estados Brasil'), tl('Regiões Brasil'), tl('Países Am.Sul'), tl('Quadrantes'))
CFG$area0.ids <- c(c('12'), '3', '4', '5', '9')
CFG$area1.names <- c(CFG$area1.names, REGIONS$NAME)
#CFG$area1.names <- CFG$area1.names[!CFG$area1.names %in% c('5-Falkland Islands', '5-French Guiana', '5-Guyana', '5-Suriname', '5-Trinidad and Tobago')]

#metrics.cont <- c('BIAS', "ME", "MAE", "MSE", 'RMSE', 'URMSE*', 'FCT x OBS')           # line chart 
#metrics.cont <- c("ME", "MAE", "RMSE", "S", "S2", "FCT x OBS")                         # new line chart
metrics.sp <- c("SBIAS", "SCORR", "SRMSE")                                              # Spatial analisys metrics
metrics.cont <- c("BIAS", "MAE", "RMSE", "SD", "VAR", "FCT x OBS")                      # new line chart
metrics.cont2 <- c('AVG', "BIAS", 'COR', "MAE", "MSE", 'RMSE', "SD", "URMSE", "VAR")    # score card
#metrics.cont3 <- c('BIAS', 'COR', 'RMSE', 'FCT')                               # texto nos quadro da serie temporal
metrics.cat   <- c("PC", "ETS", "FAR", "FBIAS", "HK", "HSS", "POD", "POFD", "SR", "TS")

par(mar=c(0,0,0,0), oma=c(0,0,0,0), mfrow=c(1,1))
#cores.mod <- c(f.GetHexColor('red'), f.GetHexColor('darkgreen'), f.GetHexColor('blue'), f.GetHexColor('purple'))
cores.mod <- c(f.GetHexColor('lightsalmon4'), f.GetHexColor('red'), f.GetHexColor('darkgreen'), 
               f.GetHexColor('blue'), f.GetHexColor('purple'), f.GetHexColor('orange'))
UNID <- list()
UNID$PREC <- paste0('(mm/', tl('dia'),')')

# File with metrics definition and source
HELP_METRICS=readLines("div/HelpMetrics.txt")
# Named vector with the perfect scores of each metric used to calculate skill scores
perfect_scores = c(BIAS=0, RMSE=0, MSE=0, COR=1)  

PERF_SC = data.table(METRIC=c("BIAS", "RMSE", "MSE", "COR", "SBIAS", "SCORR", "SRMSE"), VALUE=c(0, 0, 0, 1, 0, 1, 0))
setkey(PERF_SC)

#cores.mod2 <- c('grey45', "magenta", 'aquamarine3', 'hotpink','orange',
#                'purple','limegreen', 'sandybrown','saddlebrown', 'cyan', 'grey','yellow', 
#                'mediumpurple4', 'steelblue1','greenyellow', 'gold1', 'lightyellow3')
#stopifnot(length(CFG$models)<=length(cores.mod))
# my.colors <- cores.mod
# my.cex <- 1
# pie(rep(1,length(my.colors)), col = my.colors, labels = my.colors, cex=my.cex)
# palettes <- c('red', 'green', 'blue', 'purple', 'yellow', 'violet', 'turquoise','orange',
#               'gold', 'rose', 'maroon', 'magenta', 'salmon', 'pink', 'grey', 'cyan', 'lemon', 'coral', 'chocolate', 'brown')
# 
# for (my.cor in palettes) {
#   my.colors <- colors()[grep(my.cor, colors())]
#   pie(rep(1,length(my.colors)), col = my.colors, labels = my.colors, cex=my.cex)
# }
ltypes.mod <- c(1,1,1)
#cores.mod <- c(rgb(255,0,0,alpha=0.5, maxColorValue = 255), 
#               rgb(0,255,0,alpha=0.3, maxColorValue = 255),
#               rgb(0,0,255,alpha=0.5, maxColorValue = 255))
#symb.mod <- c(1,2,3)
cor.mark <- 'purple'
#bias.x <- -.25
bias.y <- -.25; axes.cex=1.1
bias.col <- 1 #black'colors()[36]

#majors.ets = seq(0, .5, by = .1)

# ==============================================================================
# Definição das escalas de cores  
# ==============================================================================
#f.ColNeg <- colorRampPalette(c("red", "white"))
#f.ColPos <- colorRampPalette(c("white", 'blue'))
f.ColNeg <- colorRampPalette(c("red", "orange", 'yellow', "white"))
f.ColPos <- colorRampPalette(c("white", 'cornflowerblue', 'blueviolet', 'blue4'))

breaks.prec.bias <- c(-1000, c(seq(-20, -10, by=5), seq(-5, -1)), 0, c(seq(1, 5), seq(10, 20, by=5)), 1000)
colors.prec.bias <- c(f.ColNeg(((length(breaks.prec.bias)-1)/2)), f.ColPos(((length(breaks.prec.bias)-1)/2)))
#pie(rep(1, length(colors.prec.bias)), col=colors.prec.bias)
#pie(rep(1, 100), col=colors()[1:100], cex=.4)
#colors()[62]

#source('ColorCard.R')
#df = as.data.frame(cbind(matrix(round(rnorm(50), 3), 10), sample(0:1, 10, TRUE)))
#brks <- quantile(df, probs = seq(.05, .95, .05), na.rm = TRUE)
#clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
#{paste0("rgb(255,", ., ",", ., ")")}

# TS = Threat Score
# KSS = Kuniper Skill Score
# HSS = Heidke Skill Score
# PSS = Pierce Skill Score

#library(verification)
#obs<- round(runif(100))
#pred<- round(runif(100))

#obs<- c(28, 72, 23, 2680) # 
#A<- verify(obs, pred = NULL, frcst.type = "binary", obs.type = "binary")
#summary(A)

#})
