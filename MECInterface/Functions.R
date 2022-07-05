# ==============================================================================
# Returns the bounding box (rectangle limits) of the region in order to zoom in
# ==============================================================================
#reg.type="5"; reg.name="5-Argentina"; ext=2
f.GetMapLim <- function(reg.type="1", reg.name=CFG$area1.names[1], ext=2) {
  switch(
    reg.type,
    '1'={ lim <-c(range(CFG$lon), range(CFG$lat)) },
    '2'={ lim = st_bbox(shp.br.uf)},
    '3'={ lim2 = st_bbox(shp.br.uf[shp.br.uf$sigla %in% substring(reg.name, 3),]); 
    lim=as.vector(t(lim2)); lim=lim[c(1,3,2,4)] },
    '4'={ lim2 = st_bbox(shp.br.reg[shp.br.reg$sigla %in% substring(reg.name, 7),]) ; 
    lim=as.vector(t(lim2)); lim=lim[c(1,3,2,4)] },
    '5'={ lim = map("world", xlim=range(CFG$lon), ylim=range(CFG$lat),
                    regions=substring(reg.name, 3), plot=F)$range },
    '9'={ idx.reg <- which(REGIONS$NAME %in% reg.name);
    lim <- c(min(REGIONS[idx.reg,'LON1'])-1, max(REGIONS[idx.reg,'LON2'])+1, 
             min(REGIONS[idx.reg,'LAT1'])-1, max(REGIONS[idx.reg,'LAT2'])+1)}
  )
  # lim deve ser: X1, X2, Y1, Y2, bbox() retorna (X1, Y1, X2, Y2)
  # extensão dos limites do plot
  lim[1]<-lim[1]-ext; 
  lim[2]<-lim[2]+ext
  lim[3]<-lim[3]-ext; 
  lim[4]<-lim[4]+ext
  lim
}

f.BoxedText <- function(x, y, text, col.bg='white', col.fg='black', border=T, cex=1, font=1, family='', xjust=1, yjust=1) {
  text2 <- gsub('-', '_', text)
  sw <- strwidth(text2, cex=cex, font=font)
  sh <- strheight(text2, cex=cex, font=font)
  frsz <- 0.05
  legend(x-frsz, y, text,
         xjust = xjust,   # 0=left, 0.5=center, 1=right adjusted
         yjust = yjust,   # 0=bottom, 0.5=center, 1=top adjusted
         x.intersp = -.5, # adjust character interspacing as you like to effect box width
         y.intersp = 0,   # adjust character interspacing to effect box height
         bg=col.bg, text.col=col.fg, text.font=font, cex=cex, xpd=T, border=border,
         plot=text != '') # mostra somente se houver texto
}



# ==============================================================================
# Translator function
# ==============================================================================
# my.str="Avaliação de modelos"
tl = function(my.str, lang=CFG$lang) {
  res = my.str
  if (!is.null(lang) && lang != colnames(TL)[1]) {
    res = TL[.(my.str)][[lang]]
    if (is.na(res) || is.null(res)) {
      res = my.str
    }
  } 
  res
}

# ==============================================================================
# Applies style to html control titles
# Pode causar comportamentos estranhos em alguns componentes, sedeparar com
# algum experimente tirar esta função.
# ==============================================================================
titSty = function(x, extraSty="", extraClass="") {
  if (nchar(extraClass)>0) {
    tags$h5(x, class=extraClass, style=paste0("color:orange;", extraSty, collapse=""))
  } else {
    tags$h5(x, style=paste0("color:orange;", extraSty, collapse=""))
  }
}

CFG$debug <- T
Cat <- function(..., debug=T) {
  if (CFG$debug) cat(...)
}
#Cat("xxx")

f.Models <- function() {
  lapply(CFG$models, 
         function(x) tags$span(x, 
                               style=paste0('color:', cores.mod[which(CFG$models==x)],
                                            '; font-weight:bold; width:100%')))
}

# medida= HIT | FA | MISS | CORRREJ
GetContTable <- function(fct, obs, thres, medida) {
  fct.cat <- fct > thres
  obs.cat <- obs > thres
  switch(medida,
         "HIT"=sum(fct.cat & obs.cat),       # a
         "FAR"=sum(fct.cat & !obs.cat),      # b
         "MISS"=sum(!fct.cat & obs.cat),     # c
         "CORRREJ"=sum(!fct.cat & !obs.cat)  # d
  )
}

# ============================================================================
# Filtro das REGIOES  
# Quando a região principal são os estados, filtrar pela região senão ficam 
# muitos para serem mostrados
# ============================================================================
f.GetRegions <- function(input) {
  if (input$area0Id == 3) {
    regions <- f.GetAreas1(input) #input$area1Id
  } else {
    regions <- CFG$area1.names[unlist(lapply(substring(CFG$area1.names,1,1), grepl, input$area0Id))]
  }
}

# ==============================================================================
# Adapta o nome da região para ser mostrado
# Retira os números e "REG" dos nomes
# ==============================================================================
f.GetRegName <- function(input, reg) {
  unlist(lapply(reg, function(x) substring(x, ifelse(input$mainRegId==4, 7, 3))))
}

# ==============================================================================
# Captura as sub regiões de acordo com a região principal selecionada
# Caso seja a região principal seja os estados, retorna os estados da região do BR
# ==============================================================================
GetSubRegs <- function(input) {
  if (input$mainRegId == 3) {
    # Macro região de estados do BR selecionada, retorna todos os estados da
    # primeira região mostrada no componente da UI
    sub.regions <- paste0(input$mainRegId, '-', CFG$RegUf[[input$regBrId]])
  } else {
    # Para as outras macro regiões retorna todas as sub-regiões cadastradas
    # para a macro região selecionada no componente da UI
    sub.regions <- CFG$area1.names[unlist(lapply(substring(CFG$area1.names,1,1), grepl, input$mainRegId))]
  }
  # FALTA ISSO???
  # Se tela==ST, Taylor, Hist --> captura somente as sub-regiões selecionadas
  # Se tela for SC --> captura todas, pois serão divididas nos quadros do SC
  # if (input$ViewMode == 'SC') {
  #   regions <- sub.regs
  # } else {
  #   regions <- input$subRegId
  # }
  #regions <- ifelse(input$ViewMode == 'SC', sub.regs, input$subRegId)
  sub.regions
}

# ==============================================================================
# Captura as macro regiões para filtrar o DS
# Quando a região principal são os estados, filtrar estados pela região selecionada 
# no componente senão ficam muitos estados para serem mostrados
# ==============================================================================
# GetMainRegsFilterDS <- function(input) {
#   Cat("Functions.R: GetMainRegsFilterDS():", input$ViewMode, "BEGIN ========================================================================\n")
#   Cat("Functions.R: GetMainRegsFilterDS(): input$mainRegId:", input$mainRegId, '\n')
#   if (input$mainRegId == 3) { 
#     # Se estados do BR retorna 
#     regions <- GetMainRegs(input)
#   } else {
#     regions <- CFG$area1.names[unlist(lapply(substring(CFG$area1.names,1,1), grepl, input$mainRegId))]
#   }
#   Cat("Functions.R: GetMainRegsFilterDS(): captured regions:", regions, '\n')
#   Cat("Functions.R: GetMainRegsFilterDS(): input$mainRegId:", input$mainRegId, '\n')
#   Cat("Functions.R: GetMainRegsFilterDS():", input$ViewMode, "END  ========================================================================\n")
#   regions
# }

# Returns the bg color according to bg color
#bg.col='black';bg.col='white'
f.GetFgColor <- function(bg.col, r=.299, g=.587, b=.114) {
  # Example from http://www.mobilemotion.eu/?p=1098
  #col.thres = (3*255)/2
  #col.sum <- as.numeric(col2rgb(bg.col)['red',]+col2rgb(bg.col)['green',]+col2rgb(bg.col)['blue',])
  #ifelse(col.sum > col.thres,'black','white')
  
  # Example from  https://stackoverflow.com/questions/1855884/determine-font-color-based-on-background-color  
  col.sum = 1 - (( r * as.numeric(col2rgb(bg.col)['red',]) + 
                     g * as.numeric(col2rgb(bg.col)['green',]) + 
                     b * as.numeric(col2rgb(bg.col)['blue',]))/255)
  
  ifelse(col.sum < .7, 'black', 'white')
}

# ==============================================================================
# Converte os nomes das cores para HEXA pois o HTML não entende certas cores
# ==============================================================================
# color_name="gold1"
#f.GetHexColor("grey30")
f.GetHexColor <- function(color_name) {
  rgb_color = col2rgb(color_name)
  rgb(rgb_color["red", 1], rgb_color["green", 1], rgb_color["blue", 1], maxColorValue=255)
}


#ColCard.palette.bg[15]
#pie(rep(1, length(ColCard.palette.bg)), col = ColCard.palette.bg)

# Another example would be:
# // Counting the perceptive luminance - human eye favors green color... 
# double a = 1 - ( 0.299 * color.R + 0.587 * color.G + 0.114 * color.B)/255;
# if (a < 0.5) black font
# else         white font

f.FormatLabel <- function(l, sc='M', dig=2) {
  res = round(l, dig)
  if (sc=='K') {
    res <- paste0(round(l/1000,dig), sc)
  }
  if (sc=='M') {
    res <- paste0(round(l/1000000,dig), sc)
  }
  if (sc=='G') {
    res <- paste0(round(l/1000000000,dig), sc)
  }
  res
}

# Returns the index of the model in the main models' vector
f.GetMainIdxMod <- function(mod) {
  unlist(lapply(mod, grep, CFG$models))
}

# Returns the index of the model in the input models' vector
#f.GetModIdxInput <- function(mod) {
#  unlist(lapply(mod, grep, input$modId))
#}

# Returns the index of the model in the main models' vector
f.GetModIdxMain <- function(mod) {
  which(CFG$models==mod)
}

# Scale dist between sc1 .. sc2
f.Scale <- function(n, dist.values, sc1=0, sc2=2) {
  dist.min <- min(dist.values, na.rm=T)
  dist.max <- max(dist.values, na.rm=T)
  sc1 + (n-dist.min)*(sc2-sc1) / (dist.max-dist.min)
}

# ==============================================================================
# Cria barra de cores com limites fechados ou abertos (para cima, para baixo ou ambos)
# Imagine a barra de cores na orientação vertical:
# ------------------------------------------------------------------------------
# orientation  = Vertical | Horizontal 
# pointers     = string com "Down", "Up" (em qq ordem) ou vazia
# y1, y2       = posição inicial e final, no eixo Y, da barra de cores (do poiter ou do quadradinho). Início em baixo.
# x1, x2       = posição inicial e final da largura dos quadradinhos
# breaks       = valores dos tick marks que serão mostrados
# ...          = outros argumentos (???) - TODO: verificar se está funcionando
# colors       = cores que os quadradinhos serão pintados. 
#                Para barra de cores sem pointers (nem Down nem Up)        ==> length(colors)+1 = length(breaks)
#                Para barra de cores com apenas um pointer (ou Down ou Up) ==> length(colors)   = length(breaks)
#                Para barra de cores com os dois pointers (Down e Up)      ==> length(colors)   = length(breaks)+1
# add          = T = vai adicionar ao gráfico existente (tem que obedecer o domínio da escala do plot já plotado)
#              = F = vai criar um plot novo, com as definições de y guiadas pelos breaks e x default
# Passar todos os numeros da escala, que devem ser mostradas ao lado de cada limite veritical dos polígonos
# A função verifica se vai mostrar ou não.
# Se houver seta para cima não mostra o último num, pois não há limite
# Se houver seta para baixo não mostra o primeiro num, pois não há limite
# ==============================================================================
# par(mar=c(0,0,4,5))
# image(x=round(CFG$lon,2), y=round(CFG$lat,2), z=FLD, useRaster=T, col=my_colors, xlab='', ylab='', pty='m', axes=F, breaks = my_breaks)
# plot(SHP_BR_REG, add=T)
# map(add=T, col = 'black', lwd=2, xlim = range(CFG$lon), ylim = range(CFG$lat))
# 
# pointers="UpDown" 
# y1=f.UsrPxUsr(range(CFG$lat)[1],-50, 'Y'); y2=f.UsrPxUsr(range(CFG$lat)[2],+50, 'Y');
# x1=f.UsrPxUsr(range(CFG$lon)[2], 10, 'X'); x2=f.UsrPxUsr(range(CFG$lon)[2], 25,'X'); 
# breaks=my_breaks; colors=my_colors; xpd=T; add=T

f.ArrowedColorBar <- function(orientation='V', pointers="", y1, y2, x1, x2, breaks,..., 
                              colors=rainbow(length(breaks)-1), add=T) {
  if (!add) {
    ylim = range(0:(length(breaks)+2)) # +2 para deixar um espaçamento superior e inferior do mesmo tamanho dos quadradinhos
    xlim = c(0, .8)
    x1=xlim[1]; x2=xlim[2]/2
    ys <- seq(ylim[1]+1, ylim[2]-1, len=length(breaks))
    
    #par(mfrow=c(1,1), mar=c(2,2,2,2))
    par(mar=c(0,1,0,2))
    plot(NA, xlim=xlim, ylim=ylim, type='n', axes=F, xaxs="i", yaxs="i")
    #graphics::box(); axis(1); axis(2)
  } else {
    ylim = c(y1, y2)   # pre-defined
    xlim = c(x1, x2)
    ys <- seq(ylim[1], ylim[2], len=length(breaks))
  }
  
  if (grepl('Down', pointers)) {
    idx_tick1 = 2
  } else {
    idx_tick1 = 1
  }
  
  if (grepl('Up', pointers)) {
    idx_tick2 = length(breaks)-1
  } else {
    idx_tick2 = length(breaks)
  }
  idx_ticks = idx_tick1:idx_tick2
  
  # Qtd máx de char dos números da barra de cores que irão aparecer (espaço entre barra e tick marks em pixels fixos)
  num_wid <- max(nchar(breaks[idx_tick1:idx_tick2]))
  
  # Retangulos e numeros da escala
  #plot(NA, xlim=xlim, ylim=ylim, type='n', axes=F, xaxs="i", yaxs="i")
  #graphics::box(); axis(1, line=-2, at=-80:-20, labels=-80:-20, xpd=T); axis(2, at=0:ylim[2], labels=0:ylim[2])
  rect(xleft=x1, ybottom=ys[idx_ticks][1:(length(idx_ticks)-1)], xright=x2, ytop=ys[idx_ticks][-1], col=colors[idx_ticks], xpd=T)
  text(x2, ys[idx_ticks], format(breaks[idx_ticks], nsmall=0, width=num_wid), pos=4L, family='mono', cex=ifelse(add, 1, 1.8), xpd=T)
  #text(x2, ys[idx_ticks], format(breaks[idx_ticks], nsmall=0, width=num_wid), pos=4L, family='mono', xpd=T, ...)
  # Seta para baixo
  if (grepl('Down', pointers)) {
    polygon(x=c(x1,(x1+x2)/2,x2), y=c(ys[2],ys[1],ys[2]), col=colors[1], xpd=T)
  }
  # Seta para cima
  if (grepl('Up', pointers)) {
    polygon(x=c(x1,(x1+x2)/2,x2), y=c(ys[length(ys)-1],ys[length(ys)],ys[length(ys)-1]), col=colors[length(colors)], xpd=T)
  }
}
#f.ArrowedColorBar(orientation='V', pointers='UpDown', y1=.1, y2=.9, x1=.1, x2=.2, x3=.2, breaks = seq(0, 1, by=.1), add=T)
#points(.1, .1)
#rect(0,0,1,1)
#box()

# Convert x pixels to plot scale
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
# Color Scale  - Melhor ... pior
# ==============================================================================
f.ColScaleBestWorst.OLD <- function() {
  ylow <- .1
  yhigh <- .2
  nums <- c(0.1, 0.2, 0.25, 0.35, 0.4, 0.5)
  labels <- c('Melhor', '', 'Médio', '', 'Pior')
  xlim <- range(nums)
  ylim <- c(ylow-.1, yhigh+.1)
  #ylim <- c(ylow-.15, yhigh-.15)
  cols <- c('darkgreen', 'white', 'orange', 'white', 'red')
  
  # Xs inicial e final dos retangulos (da esquerda para direita)
  idx.rect.beg <- 1
  idx.rect.end <- length(nums)
  
  par(mfrow=c(1,1), mar=c(0,0,0,0))
  plot(NA, xlim=xlim, ylim=ylim, type='n', axes=F)
  
  # Retangulos e numeros da escala
  i=1
  for (i in idx.rect.beg:(idx.rect.end)) {
    x1 <- nums[i];
    x2 <- nums[i+1L];
    rect(x1,ylow,x2,yhigh, col=cols[i], xpd=T)
    text((x1+x2)/2, ylow, labels=labels[i], pos=1L, adj=0, family='mono', cex=1.2, xpd=T)
  }
}

# grad=F;cols=c('red', 'orange', 'darkgreen') 
f.ColScaleBestWorst <- function(grad=F, cols) {
  par(mfrow=c(1,1), mar=c(0,0,0,0), oma=c(0,0,0,0))
  plot(c(0,1), c(0,1.5), type='n', axes=F, xlab='', ylab='')
  #cols <- c('red', 'orange', 'darkgreen')
  if (!grad) {
    rect(c(0,0,0), c(0,.5,1), c(1,1,1), c(.5,1,1.5), col = cols)
  } else {
    colfunc <- colorRampPalette(cols)
    img <- as.raster(matrix(colfunc(100), ncol=1))
    #text(x=1.5, y = seq(0,1,l=5), labels = seq(0,1,l=5))
    rasterImage(rev(img), 0, 1.5, 1, 0)
  }
}

#f.ArrowedColorBar(orientation='H', pointers='UpDown', ylow=.1, yhigh=.9, x1=.1, x2=.2, x3=.2, nums = seq(0, 1, by=.1), add=F)
#points(.1, .1)
#rect(0,0,1,1)
#box()

# ==============================================================================
# Função que retorna a cor de fundo das células do score card
# ==============================================================================
f.GetCelColor <- function(value, rang, input) {
  bg.cor <- 'lightgrey'
  if (!is.na(value)) {
    # Quando calcular do BIAS
    if (input$cont2Id=='BIAS') {
      if (input$colScaleId == 'GRAD') {
        bg.cor <- colors.prec.bias[as.integer(cut(value, breaks.prec.bias))]
      } else { # Green-Orange-Red
        if (value == rang[1]) {
          bg.cor <- 'green'
        } else {
          if (value == rang[2]) {
            bg.cor <- 'red'
          } else {
            bg.cor <- 'orange'
          }
        }
      }
    }
    
    # Quando calcular do RMSE, quanto menor, melhor
    if (grepl('RMSE', input$cont2Id)) {
      if (value == rang[1]) {
        bg.cor <- 'green'
      } else {
        if (value == rang[2]) {
          bg.cor <- 'red'
        } else {
          bg.cor <- 'orange'
        }
      }
    }
  }
  bg.cor
}

f.ChangeReg <- function(string) {
  substring(string, 3)
}

# Extrai o help de 
#metric="SS"; f.GetHelp("SS")
f.GetHelp = function(metric) {
  res = list()
  res[[metric]] = list()
  help_key = "Names:"
  help_str = paste0(metric, "_", help_key)
  help_text = trimws(paste0(gsub(paste0(help_str), "", trimws(HELP_METRICS[grep(paste0("^",help_str), HELP_METRICS)]))))
  res[[metric]]$Names = help_text
  
  help_key = "Answers:"
  help_str = paste0(metric, "_", help_key)
  help_text = trimws(paste0(gsub(paste0(help_str), "", trimws(HELP_METRICS[grep(paste0("^",help_str), HELP_METRICS)]))))
  res[[metric]]$Answers = help_text
  
  help_key = "Range:"
  help_str = paste0(metric, "_", help_key)
  help_text = trimws(paste0(gsub(paste0(help_str), "", trimws(HELP_METRICS[grep(paste0("^",help_str), HELP_METRICS)]))))
  res[[metric]]$Range = help_text
  
  help_key = "Characteristics:"
  help_str = paste0(metric, "_", help_key)
  help_text = trimws(paste0(gsub(paste0(help_str), "", trimws(HELP_METRICS[grep(paste0("^",help_str), HELP_METRICS)]))))
  res[[metric]]$Characteristics = help_text
  
  res[[metric]]$Img = paste0(metric, ".png")
  res
}


# ==============================================================================
# Plota as regioes de acordo com os limites passados
# ==============================================================================
f.PlotMap <- function(reg.type, reg.name, lim, mask) {
  map("world", xlim=c(lim[1], lim[2]), ylim=c(lim[3], lim[4]), plot=F); 
  # Inclui Suriname, Guiana e Guiana Francesa só para plotar o mapa mas não há avaliação específica pra eles
  regs <- substring(c(CFG$area1.names, '5-French Guiana', '5-Guyana', '5-Suriname'),3)[which(substring(c(CFG$area1.names, '5-French Guiana', '5-Guyana', '5-Suriname'),1,1) %in% c('2','5'))]
  map("world", region=regs, xlim=c(lim[1], lim[2]), ylim=c(lim[3], lim[4]), lwd=1.5, mar=c(0,0,0,2))
  
  #cat(reg.type, reg.name, lim, mask, "\n")
  
  if (!'9' %in% reg.type) {
    r=reg.name[1]
    for (r in reg.name) {
      load(paste0(EVAL.CUR, '/MASKS/MASK_CONT_AS_', r, '_0.1.RData'))
      image(x=CFG$lon, y=CFG$lat, z=MASK.CONT, add=T, useRaster=T)
      map("world", xlim=c(lim[1], lim[2]), ylim=c(lim[3], lim[4]), lforce='e', add=T)
    }
  } else {
    idx.reg <- which(REGIONS$NAME %in% reg.name)
    if (mask=='ALL') {
      for (i in idx.reg) {
        rect(REGIONS$LON1[i], REGIONS$LAT1[i], REGIONS$LON2[i], REGIONS$LAT2[i], 
             border=F, col='orange')
      }
    }
    for (i in idx.reg) {
      map("world", 
          xlim=range(REGIONS$LON1[i], REGIONS$LON2[i]), 
          ylim=range(REGIONS$LAT1[i], REGIONS$LAT2[i]), 
          lforce='e', add=T, fill=T, col='orange', lwd=1)
    }
    # Delimita box de todas as areas com retangulos
    idx.reg=2
    for (idx.reg in 2:nrow(REGIONS)) {
      reg <- REGIONS[idx.reg, ]
      rect(reg$LON1, reg$LAT1, reg$LON2, reg$LAT2, lwd=.5)
    }
  }
  if ('3' %in% reg.type || '9' %in% reg.type) {  # ESTADOS E QUADRANTES
    plot(shp.br.uf$geometry, add=T, lwd=.5)
  } 
  if ('4' %in% reg.type) {  # Regiões BR
    plot(shp.br.reg$geometry, add=T, lwd=.5)
  }
  
  axis(1, cex.axis=.7, padj=-2, lwd=0)
  axis(4, las=2, cex.axis=.7, lwd=0, hadj=1)
  grid(col='grey')
}

# ==============================================================================
# Descobre o agrupamento no ano de uma data: DJF, MAM, JJA, SON
# ==============================================================================
f.Date2Season <- function(my.date, app.year=T) {
  m <- month(my.date)
  y1 <- year(my.date)
  y2 <- year(my.date)
  if (m %in% c(12, 1, 2)) {
    s <- 'd-DJF'
    if (m==12) {
      y1 <- year(my.date)
      y2 <- year(my.date)+1
    } else {
      y1 <- year(my.date)-1
      y2 <- year(my.date)
    }
  } else if (m %in% c(3,4,5)) {
    s <- 'a-MAM'
  } else if (m %in% c(6,7,8)) {
    s <- 'b-JJA'
  } else {
    s <- 'c-SON'
  }
  if (app.year) {
    if (y1 == y2) {
      paste0(y1, s, collapse='')
    } else {
      paste0(y1, s, '-', y2, collapse='')
    }
  } else {
    s
  }
}
#f.Date2Season(as.Date('2019-06-22'), F)

# ==============================================================================
# Descobre a estação do ano de uma data
# ==============================================================================
f.Date2SeasonReal <- function(my.date, app.year=T) {
  m <- as.POSIXlt(my.date)$mon + 1        # correct for 0:11 range
  d <- as.POSIXlt(my.date)$mday           # correct for 0:11 range
  if ((m == 9 & d >= 23) | (m == 10) | (m == 11) | (m == 12 & d < 22)) {
    s <- 'd-Prim'
  } else if ((m == 12 & d >= 22) | (m == 1) | (m == 2) | (m == 3 & d < 20)) {
    s <- 'a-Ver'
  } else if ((m == 3 & d >= 20) | (m == 4) | (m == 5) | (m == 6 & d < 21)) {
    s <- 'b-Out'
  } else {
    s <- 'c-Inv'
  }
  if (app.year) {
    paste0(year(my.date),s, collapse='')
  } else {
    s
  }
}



