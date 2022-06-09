# ==============================================================================
# ZR Garcia (roberto.garcia@inpe.br)
# DIMNT / CGCT / INPE / MCTI
# ------------------------------------------------------------------------------
# Environment preparation for fields plotting
# ==============================================================================
suppressMessages(library(fields))         # for image.plot()
suppressMessages(library(maptools))       # for readShapeSpatial()
suppressMessages(library(maps))           # for map()
suppressMessages(library(RColorBrewer))
suppressMessages(library(viridis))
suppressWarnings(SHP_BR_REG <- readShapeSpatial(paste0(DIR$shp, '/BR/regioes/regioes_2010.shp')))

source("FunColors.R")

# Cria valores de tick marks legais para os eixos
xs <- pretty(CFG.SET$lon.range); xs <- xs[xs>=CFG.SET$lon.range[1] & xs<=CFG.SET$lon.range[2]]
ys <- pretty(CFG.SET$lat.range); ys <- ys[ys>=CFG.SET$lat.range[1] & ys<=CFG.SET$lat.range[2]]

f.BoxedText <- function(x, y, text, col.bg='white', col.fg='black', border=T, cex=1, font=1, family='', yjust=1) {
  text2 <- gsub('-', '_', text)
  sw <- strwidth(text2, cex=cex, font=font)
  sh <- strheight(text2, cex=cex, font=font)
  frsz <- 0.05
#cat('\ntext:', text, '  text2:', text2)
#cat('\nsw:', sw, 'sh:', sh)
#cat('\nx:', x, '  y:', y)
#cat('\nusr:', par('usr'),'\n')
#lines(x=c(x,x), y=c(-10, y), col='red', xpd=T)
#lines(x=c(usr[2],usr[2]), y=c(-20, -10), col='blue', xpd=T)
#lines(x=c(CFG.SET$lon.range[2],CFG.SET$lon.range[2]), y=c(-20, -10), col='magenta', xpd=T)
  
# rect(xleft=x-sw-frsz, ybottom=y-(sh/2)+frsz, xright=x+frsz, ytop=y+(sh/2)+(frsz*10), col=fill, border=border)
# text(x=x-frsz, y=y, labels=text, pos=pos, cex=cex, font=font, family=family, col=col)
#  legend(x=x, y=y, legend=text, fill=fill, border=border, xjust=1, 
#    x.intersp = 0, y.intersp = 0, pt.cex=0)

  legend(x-frsz, y, text,
    xjust = 1,       # 0=left, 0.5=center, 1=right adjusted
    yjust = yjust,   # 0=bottom, 0.5=center, 1=top adjusted
    x.intersp = -.5, # adjust character interspacing as you like to effect box width
    y.intersp = 0,   # adjust character interspacing to effect box height
    bg=col.bg, text.col=col.fg, text.font=font, cex=cex, xpd=T, border=border,
    plot=text != '') # mostra somente se houver texto
}


# Cria e grava DS RESOL
RESOL <- data.frame(NAME=c('Low', 'Med', 'High'),
   WIDTH=c(240,320,470), #x240,x320,x480 # default resolutions
   HEIGHT=c(240,300,480), #x240,x320,x480 # default resolutions
   LIN_MAIN=c(-3.5,-1,0),
   CEX1=c(1.3, 1.3, 1.5),
   CEX2=c(1.0, 1.3, 1.3), 
   CEX3=c(0.5, 1.0, 1.1), 
   y.tit1=c(8,8,10),
   y.tit2=c(5,5,5.2),
   y.tit3=c(2,2,2.2),
   y.foot1=c(-35.5,-35.5,-45.0),
   y.foot2=c(-38.5,-38.5,-38.7),
   y.foot3=c(-41.5,-41.5,-41.9), stringsAsFactors=F)
## Adapta as resoluções para o aspect ratio da imagem
#asp.ratio <- (diff(CFG.SET$lon.range)+1)/(diff(CFG.SET$lat.range)+1)
#RESOL$WIDTH <- RESOL$HEIGHT #*asp.ratio
save(RESOL, file=paste0(DIR$eval, 'RESOL.RData'))

MARGINS <- list()
MARGINS[['Low-FCT']]  <- c(1  , 1.8, .5, 1)
MARGINS[['Low-OBS']]  <- c(1  , 1.8, .5, 1)
MARGINS[['Med-FCT']]  <- c(1  , 2  ,  0, 1)
MARGINS[['Med-OBS']]  <- c(1  , 2  ,  0, 5)
MARGINS[['High-FCT']] <- c(1.5, 1.8, .5, 1)
MARGINS[['High-OBS']] <- c(1  , 2  ,  0, 5)

# Plots the segments of the regions without repeating to avoid bolding
f.PlotSeg <- function(seg) {
  lines(x=seg$x, y=seg$y, lwd=1)#, lty=3) #, col='red')
}

# ==============================================================================
# Funções para criar gradiente entre duas cores
# ==============================================================================
#f.ColNeg <- colorRampPalette(c("red", "white"))
#f.ColPos <- colorRampPalette(c("white", 'blue'))
f.ColNeg <- colorRampPalette(c("red", "orange", 'yellow', "white"))
f.ColPos <- colorRampPalette(c("white", 'cornflowerblue', 'blueviolet', 'blue4'))
f.ColFld <- colorRampPalette(c('white', 'blue', 'red'))
f.ColHeat <- colorRampPalette(c('white', 'yellow', 'red'))
f.ColHeat2<- colorRampPalette(c('white', 'yellow', 'red4'))
f.ColHeat3<- c('white', 'khaki1', 'yellow', 'orange', 'red', 'red4', 'violet', rgb(160,  0,200, maxColorValue = 255), 'black')

# Definição da escala de cores para o bar plot dentro do plot do bias
#num.class.prec <- length(THRESHOLDS[[CFG$var]]) - 1 # diferença max entre classes prev x obs
#col.barplot.bias <- c(f.ColNeg(num.class.prec+1), f.ColPos(num.class.prec+1)[2:length(f.ColPos(num.class.prec+1))])

# From package dichromat
col.BluetoOrange.8 <- c("#0080FF","#4CC4FF","#99EEFF","#CCFFFF","#FFFFCC","#FFEE99","#FFC44C","#FF8000")
col.BluetoOrange.10 <- c("#0055FF","#3399FF","#66CCFF","#99EEFF","#CCFFFF","#FFFFCC","#FFEE99","#FFCC66","#FF9933","#FF5500") 
col.BluetoOrange.12 <- c("#002BFF","#1A66FF","#3399FF","#66CCFF","#99EEFF","#CCFFFF","#FFFFCC","#FFEE99","#FFCC66","#FF9933","#FF661A","#FF2B00")
col.BlueWhiteRed.10 <- c('#FF0000','#FF8000','#FFC44C','#FFEE99','#FFFFFF', '#FFFFFF', '#CCFFFF','#99EEFF','#4CC4FF','#0080FF')
col.WhiteBlue.5 <- c('#FFFFFF', '#99EEFF','#4CC4FF', rgb(  0,160,255, maxColorValue = 255), rgb(30,60,255, maxColorValue = 255))
col.WhiteGreen.5 <- c('#FFFFFF', "#C7EAE5", "#80CDC1", "#35978F", "#003C30")
f.ColTempCor <- colorRampPalette(c('white', '#003C30'))
f.ColTempRmse <- colorRampPalette(c('white', 'red', rgb(160,  0,200, maxColorValue = 255)))

# NEW - Palette de cores para precipitação diária
colors.prec.day <- c("#FFFFFF",                 # white
   "#B0E2FF", "#7FAAFF", "#4E73FF", "#1E3CFF",  # spectral blue
   "#E6F598", "#99C465", "#4C9432", "#006400",  # spectral green
   "#EEC900", "#CD6600", "#CD0000", "#8B2500",  # spectral red    
   "#EE82EE", "#BE40F5", "#8F00FF", "#8B008B")  # spectral pink 

# NEW - Palette de cores para precipitação média mês, estação, ...
colors.prec.med <- c("#FFFFFF", "#E5E5E5",      # white & grey90
   "#B0E2FF", "#7FAAFF", "#4E73FF", "#1E3CFF",  # spectral blue
   "#E6F598", "#73AC4C", "#006400",             # spectral green
   "#EEC900", "#CD6600", "#8B2500",             # spectral red    
   "#EE82EE", "#BE40F5", "#8F00FF", "#8B008B")  # spectral pink 

# OLD2 - Palette de cores qualificativas (melhorado do grads e aumentada)
old_colors.prec2 <- c("#FFFFFF",                     # white
   "#B0E2FF", "#7FAAFF", "#4E73FF", "#1E3CFF",  # spectral blue
   "#E6F598", "#AAD0A4", "#6EACB0", "#3288BD",  # spectral green
   "#EE82EE", "#BE40F5", "#8F00FF",             # spectral magenta
   "#FFFFBF", "#FDAE61", "#FF0000", "#9E0142")  # spectral yellow-red puro
   #"#FFFFBF", "#FDAE61", "#D53E4F", "#9E0142")  # spectral yellow-red

# OLD - Palette de cores qualificativas (melhorado do grads)
old_colors.prec <- c(
  'white',
  rgb( 30, 60,255, maxColorValue = 255),  # azul escuro (4 do grads)
  rgb(  0,160,255, maxColorValue = 255),  # azul médio (11 do grads)
  'lightskyblue1',                      
  rgb(160,230, 50, maxColorValue = 255),  # verde médio
  'palegreen1',                           # verde claro
  'green3',                               # verde escuro
  'yellow',              
  'orange', #rgb(230,175, 45, maxColorValue = 255),  # laranja claro (12 do grads)
  'red', #rgb(240,130, 40, maxColorValue = 255),  # laranga (8 do grads)
  'violet',
  rgb(160,  0,200, maxColorValue = 255))  # dark purple (14 do grads)

# Palette de cores qualificativas ==> 17 cores, 18 breaks
colors.v10m <- c('white',
  'blue','dodgerblue2',"steelblue1", "lightskyblue1",  # blue
  'green4','limegreen',"palegreen3", "darkseagreen1",  # green
  'yellow','orange',"darkorange3", "red",              # orange
  'violet',"mediumvioletred", 'deeppink4', "purple4")  # violet

# temp.set1 (like grads) ==> 14 breaks, 13 cores
#colors.temp <- c(
#  rgb(160,   0, 200, maxColorValue = 255),
#  rgb(130,   0, 220, maxColorValue = 255),
#  rgb( 30,  60, 255, maxColorValue = 255),
#  rgb(  0, 160, 255, maxColorValue = 255),
#  rgb(  0, 200, 200, maxColorValue = 255),
#  rgb(  0, 210, 140, maxColorValue = 255),
#  rgb(  0, 220,   0, maxColorValue = 255),
#  rgb(160, 230,  50, maxColorValue = 255),
#  rgb(230, 220,  50, maxColorValue = 255),
#  rgb(230, 175,  45, maxColorValue = 255),
#  rgb(240, 130,  40, maxColorValue = 255),
#  rgb(250,  60,  60, maxColorValue = 255), 
#  rgb(240,   0, 130, maxColorValue = 255)  
#)

# temp.set2 (like prec) ==> 16 breaks, 15 cores
#colors.temp <- c("grey34", "grey61", "grey88", colors.prec.med)
#colors.temp <- rev(c("#800026", "#A80026", "#C90822", "#E31A1C", "#F33C25", "#FC6330", "#FD8D3C", "#FDA546", 
#                 "#FEBF5A", "#FED976", "#FEE692", "#FFF3AE", "#FFFFCC", "#FFFFFF", "#F7FBFF", "#E4EFF9", 
#                 "#D3E3F3", "#BED7EC", "#A1CBE2", "#7DB8DA", "#5CA3D0", "#3F8FC4", "#2776B8", "#135FA7",
#                 "#08478E", "#08306B"))
#
#colors.temp <- c("#08306b", "#08519c", "#2171b5", "#4292c6", "#6baed6", "#9ecae1", "#c6dbef", "#deebf7", 
#                 "#f7fbff", "#FFFFFF", "#ffffe5", "#fff7bc", "#fee391", "#fec44f", "#fe9929", "#ec7014", 
#                 "#cc4c02", "#993404", "#662506")


# Verde e roxo
#colors.temp = c("#1D4F60", "#236169", "#2B7472", "#36877A", "#459980", "#57AA88", "#6DBC90", "#88CA9D", "#A5D8AE",
#                "#C4E6C3", "#FFFFFF", "#E7D4E8", "#D6BFDC", "#C6AAD1", "#B493C3", "#A27BB3", "#9160A2", "#814190",
#                "#70257C", "#581263", "#40004B")

# TMIN e TMAX em FLD-PlotDef.R

# Function to add num.pixels in user coordinates values for the X axis
# Returns the user coordinate value correspondent
f.UsrPxUsr <- function(val.user, num.pixels, axis) {
  if (axis == 'X') {
    v1 <- grconvertX(val.user, "user", 'device')+num.pixels # in pixels
    res <- grconvertX(v1, 'device', 'user')
  } else {
    v1 <- grconvertY(val.user, "user", 'device')+num.pixels # in pixels
    res <- grconvertY(v1, 'device', 'user')
  }
  res
}

# Function to be submitted to lapply
# Plot the horizontal and vertical grid lines
f.GridX <- function(x) {
  lines(x=c(x,x), y=c(f.UsrPxUsr(CFG.SET$lat.range[1],-5,'X'), CFG.SET$lat.range[2]), col=1, lty=3, lwd=.8, xpd=T)
}
f.GridY <- function(y) {
  lines(x=c(f.UsrPxUsr(CFG.SET$lon.range[1],5,'Y'), CFG.SET$lon.range[2]), y=c(y,y), col=1, lty=3, lwd=.8, xpd=T)
}

# ==============================================================================
# Cria legenda com limites fechados ou abertos (para cima, para baixo ou ambos)
# ------------------------------------------------------------------------------
# qtd de nums = qtd de cols + 1
# Passar todos os numeros da escala, que devem ser mostradas ao lado de cada limite veritical dos polígonos
# A função verifica se vai mostrar ou não.
# Se houver seta para cima não mostra o último num, pois não há limite
# Se houver seta para baixo não mostra o primeiro num, pois não há limite
# ==============================================================================

f.ArrowedColorBar <- function(direction,ylow,yhigh,x1,x2,x3,nums,...,cols=rainbow(length(nums))) {
#direction=my.direction; ylow=CFG.SET$lat.range[1]+6; yhigh=CFG.SET$lat.range[2]-6
#x1=f.UsrPxUsr(CFG.SET$lon.range[2], 20, 'X'); x2=f.UsrPxUsr(CFG.SET$lon.range[2],35,'X')
#x3=f.UsrPxUsr(CFG.SET$lon.range[2],31,'X'); nums=my.breaks; cols=my.colors; xpd=T

  ys <- seq(ylow,yhigh,len=length(nums))
  stopifnot((length(nums)-1)==length(cols))  # num de cores deve ser igual ao num de breaks - 1

  # Ys inicial e final dos retangulos (de baixo para cima)
  idx.rect.beg <- 1
  idx.rect.end <- length(ys)

  # Seta de baixo
  if (grepl('Down', direction)) {
    y1 <- ys[1L]
    y2 <- ys[2L]
    polygon(c(x1,(x1+x2)/2,x2),c(y2,y1,y2),col=cols[1L], xpd=T)
    idx.rect.beg <- 2
  }

  # Seta para cima
  if (grepl('Up', direction)) {
    y1 <- ys[length(ys)-1L]
    y2 <- ys[length(ys)]
    polygon(c(x1,(x1+x2)/2,x2),c(y1,y2,y1),col=cols[length(cols)], xpd=T)
    idx.rect.end <- length(ys)-1L
  }

  # Retangulos e numeros da escala
  i=2
  num.wid <- max(nchar(nums[idx.rect.beg:idx.rect.end]))+2 # + ".?"
  for (i in idx.rect.beg:(idx.rect.end-1)) {
    y1 <- ys[i];
    y2 <- ys[i+1L];
    rect(x1,y1,x2,y2,col=cols[i], xpd=T);
    #text(x2,y1,sprintf("%  .1f", nums[i]),pos=4L,family='mono', cex=RESOL$CEX_REST[idx.res], ...)
    text(x3,y1,format(nums[i], nsmall=1, width=num.wid), pos=4L, family='mono', cex=.8, ...);
  }
  #text(x2,ys[i+1],sprintf("%  .1f", nums[i+1]),pos=4L,family='mono', cex=RESOL$CEX_REST[idx.res], ...);
  text(x3,ys[i+1],format(nums[i+1], nsmall=1, width=num.wid), pos=4L, family='mono', cex=.8, ...);
};

