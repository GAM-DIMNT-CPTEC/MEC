# ============================================================================
# Script que plota o diagrama de Taylor
# Fonte: pacote Plotrix
# Modificado por: JRMGarcia: garcia.cptec@gmail.com (CPTEC / INPE / MCTI)
# ============================================================================

# ref = vOBS; model = vFCT; mod=input$modId[1]; mod.idx.input=1; asp=1
# axis.pt=2; axes.cex=1.1; pch='1'
# gamma.col = 'cyan';           # arcos da RMSE a partir da referência
# sd.method = 'estimated';      # estimated | sample
# normalize=T;                  # normalize the models? T=reference lays on sd=1
# main='Taylor Diagram'; add=F; xlab = "Standard Deviation"; ylab = xlab
# tit1='TITULO'; tit2='SUB-TITULO'; bias.scale=biases
# models <- input$models
TaylorDiag <- function (
  ref, model, add=F, mod, mod.idx.input, pch=19,
  xlab = "Standard deviation", ylab = "Standard deviation", main = "Taylor Diagram",
  gamma.col='cyan',
  sd.method="estimated", # estimated | sample
  axis.pt=1.5, axes.cex=1.3, normalize=T, 
  bias.scale=NA, tit1='', tit2='', models, ...) {

  mod.idx.main <- f.GetModIdxMain(mod)
  mod.col <- cores.mod[mod.idx.main]; 
  
  # Variables for BIAS scaling
  # min.scale; max.scale; min.distrib; max.distrib
  bias.sc1=0; bias.sc2=2; bias.dist1=min(bias.scale, na.rm=T); bias.dist2=max(bias.scale, na.rm=T)
  #bias.col <- 1 #colors()[36]
  bias.value <- mean(model-ref, na.rm=T)  # Bias do model

  R <- cor(ref, model, use = "pairwise")  # Correlação entre obs e model
  
  grad.corr.lines=c(0.2,0.4,0.6,0.8,0.9);  # Linhas radiais do início até COR
  tit.x <- 1
  
  # OBS e MODEL pode ser uma lista
  if (is.list(ref))   ref <- unlist(ref)
  if (is.list(model)) ref <- unlist(model)
  
  SD <- function(x, subn) {
    meanx <- mean(x, na.rm = T)
    devx <- x - meanx
    ssd <- sqrt(sum(devx * devx, na.rm = TRUE)/(length(x[!is.na(x)]) - subn))
    return(ssd)
  }
  
  subn <- sd.method != "sample"
  sd.r <- SD(ref, subn)
  sd.f <- SD(model, subn)
  
  # Normalizar faz com que a sd da ref seja 1
  if (normalize) {
    sd.f <- sd.f/sd.r
    sd.r <- 1
  }
  maxsd = 2# * max(sd.f, sd.r)
  #print(maxsd)
  #if (maxsd > 2) {
  #  maxsd = 2
  #}
  
  oldpar <- par("mar", "xpd", "pty")
  #my.mar <- length(models)*.4+5
  #my.mar <- length(models)+5
  #par(pty='s', xpd=T, mar=c(my.mar,4,2,1), oma=c(0,0,0,0))

  # Se for o primeiro numero a ser plotado
  if (!add) {
    # BLANK PLOT, BUT WITH LIMITS
    plot(0, xlim=c(0, maxsd), ylim=c(0, maxsd),  axes=F, type="n", 
         xaxs="i", yaxs="i", ann=F)
    text(x=tit.x, y=2  , labels = main, pos=4, font=2, cex=1.5, xpd=T) # TITULO 1
    text(x=tit.x, y=1.9, labels = tit1, pos=4, font=2, cex=1.2, xpd=T) # TITULO 2
    
    # Arcos da URMSE* de cor 'cyan' (default)
    gamma <- seq(0, maxsd, by=.2) # pretty(c(0, maxsd), n = ngamma)[-1]
    if (gamma[length(gamma)] > maxsd)  {
      gamma <- gamma[-length(gamma)]
    }
    labelpos <- seq(40, 65, length.out = length(gamma))
    gindex <- 1
    # pi representa 180 graus, cos=projeção em X, sin=projeção em Y
    # A quantidade de pontos da curva depende do seu raio, quanto maior, mais pontos
    for (gindex in 1:(length(gamma))) {
      #             curva_de_raio_1        * percent_do_raio + cruzamento_no_X=sd.r
      xcurve <- cos(seq(0, pi, by=0.03)) * gamma[gindex] + sd.r
      #xcurve <- cos(seq(0, pi, length.out=1000*gamma[gindex])) * gamma[gindex] + sd.r
      
      # Limitadores, para não ultrapassar o 0 no X
      # Assume limite sendo o de um indice antes de ultrapassar o 0 em X
      endcurve <- which(xcurve < 0)
      endcurve <- ifelse(length(endcurve), min(endcurve) - 1, length(xcurve))
      
      #         curva_de_raio_1            * percent_do_raio + cruzamento_no_X=0
      ycurve <- sin(seq(0, pi, by=0.03)) * gamma[gindex] 
      
      # Verifica se a curva ultrapassa o limite da circunferência (raio=maxsd)
      # h^2 = a^2 + b^2
      maxcurve <- xcurve * xcurve + ycurve * ycurve # HIPOTENUSA^2
      startcurve <- which(maxcurve > maxsd * maxsd) # quais ultrapassam?
      startcurve <- ifelse(length(startcurve), max(startcurve) + 1, 1) # , 0)
      
      # Plota as curvas
      lines(xcurve[startcurve:endcurve], ycurve[startcurve:endcurve], col='grey')
      
      # Plota os labels dos arcos (em forma de caixas, para criar espaços entre as margens do texto)
      if (xcurve[labelpos[gindex]] > 0) {
        boxed.labels(xcurve[labelpos[gindex]], ycurve[labelpos[gindex]], 
                     gamma[gindex], border = FALSE, cex = .7)
      }
    }
    
    # Linhas dos eixos X e Y
    axis.ticks <- pretty(c(0, maxsd))
    axis.ticks <- axis.ticks[axis.ticks <= maxsd]
    axis(1, at = axis.ticks, labels = rep('', length(axis.ticks))) # Line and X tick marks
    axis(2, at = axis.ticks, labels = rep('', length(axis.ticks))) # Line and Y tick marks 
    # Labels Y tick marks, não imprime o primeiro pois será impresso depois, um 0.0 centralizado para ambos os eixos
    text(x=-0.02, y=axis.ticks[2:length(axis.ticks)], labels = sprintf("%.1f", axis.ticks[2:length(axis.ticks)]), cex=axes.cex, pos=2)
    # Labels X tick marks, não imprime o primeiro pois será impresso depois, um 0.0 centralizado para ambos os eixos
    text(x=c(.5, 1, 1.5, 2), y=0, labels = sprintf("%.1f", c(.5, 1, 1.5, 2)), cex=axes.cex, pos=1)
    # Label 0.0
    text(x=0.02, y=-.05, labels = sprintf("%.1f", axis.ticks[1]), cex=axes.cex, pos=2, xpd=T) 
    lines(x=c(0,2), y=c(0,0))

    #title(xlab=xlab, cex.lab=axes.cex+.2, line=.5)
    title(ylab=ylab, cex.lab=axes.cex+.2, line=2.5)
    #axis(2, at = axis.ticks, cex.axis = axes.cex) # Eixo Y: labels e tick marks
    
    # Arcos de sd, a partir de [0,0]
    sd.arcs <- axis.ticks
    for (sdarc in sd.arcs) {
      xcurve <- cos(seq(0, pi/2, by = 0.01)) * sdarc
      ycurve <- sin(seq(0, pi/2, by = 0.01)) * sdarc
      if (sdarc == sd.r) {
        lines(xcurve, ycurve, lty=3, lwd=2, col = "blue") # Arco da ref (no 1.0)
      } else {
        if (sdarc != maxsd) {
          lines(xcurve, ycurve, col = "blue", lty = 3) # Demais arcos
        } else {
          lines(xcurve, ycurve)   # Arco maior do stdev (no 2.0)
        }
      }
    }
    
    # Segmentos da correlação, de 0,0 até o limite do 1 quarto de círculo
    for (gcl in grad.corr.lines) 
      lines(c(0, maxsd * gcl), c(0, maxsd * sqrt(1 - gcl^2)), lty = 3, col='grey')
    
    # Correlation tick marks - bigger
    bigtickangles <- acos(seq(0.1, 0.9, by = 0.1))
    segments(cos(bigtickangles) * maxsd, sin(bigtickangles) * 
               maxsd, cos(bigtickangles) * 0.97 * maxsd, sin(bigtickangles) * 
               0.97 * maxsd)
    # Correlation tick marks - shorter
    medtickangles <- acos(seq(0.05, 0.95, by = 0.1))
    segments(cos(medtickangles) * maxsd, sin(medtickangles) * 
               maxsd, cos(medtickangles) * 0.98 * maxsd, sin(medtickangles) * 
               0.98 * maxsd)
    # Correlation tick marks - shortest (from 0.9 to 0.99)
    smltickangles <- acos(seq(0.91, 0.99, by = 0.01))
    segments(cos(smltickangles) * maxsd, sin(smltickangles) * 
               maxsd, cos(smltickangles) * 0.99 * maxsd, sin(smltickangles) * 
               0.99 * maxsd)
    
    # Correlation scale
    text(cos(c(bigtickangles, acos(c(0.95, 0.99)))) * 
           1.03 * maxsd, sin(c(bigtickangles, acos(c(0.95, 0.99)))) * 
           1.03 * maxsd, c(gsub('0\\.','\\.',sprintf("%.1f", seq(0.1, 0.9, by = 0.1))), '.95', '.99'), 
         cex=axes.cex)
    
    text(maxsd * 0.775, maxsd * 0.775, "Correlation", srt = 315, cex=axes.cex+.2) # Correlation title
    
    # BIAS - Régua
    if (!is.na(bias.scale[1])) {
      text(x=-0.02, y=bias.y, labels='Bias', cex=axes.cex+.2, col=bias.col, xpd=T, pos=2, srt=90)
      lines(x=c(0,2), y=c(bias.y    , bias.y)    , lty=1, lwd=1, col=bias.col, xpd=T) # linha vertical
      lines(x=c(0,0), y=c(bias.y-.04, bias.y+.04), lwd=1, xpd=T, col=bias.col, xpd=T) # linha do limite inferior
      lines(x=c(2,2), y=c(bias.y-.04, bias.y+.04), lwd=1, xpd=T, col=bias.col, xpd=T) # linha do limite superior
      # Normalize bias.scale between 0 and 2 (screen scale) and plot
      n=bias.scale[1]
      for (n in bias.scale) {
        #n.scaled <- bias.sc1 + (n-bias.dis)*(b-a) / (B-A) # ORIGINAL
        #n.scaled <- bias.sc1 + (n-bias.dist1)*(bias.sc2-bias.sc1) / (bias.dist2-bias.dist1)
        n.scaled <- f.Scale(n, bias.scale, bias.sc1, bias.sc2)
        
        #text(x=bias.x-.03, y=n.scaled, labels = sprintf("%0.1f", n), col=bias.col, pos=2, cex=axes.cex)
        text(x=n.scaled, y=bias.y+.02, labels = sprintf("%0.1f", n), col=bias.col, pos=3, cex=axes.cex)
        # ??
        if (n != bias.scale[1] && n != bias.scale[length(bias.scale)]) {
          #lines(x=c(bias.x-.04,bias.x), y=c(n.scaled,n.scaled), lwd=.5, xpd=T, col=bias.col)
          lines(x=c(n.scaled,n.scaled), y=c(bias.y, bias.y+.03), lwd=.5, xpd=T, col=bias.col)
        }
      }
    }
  } # if ADD
  
  # Ponto no gráfico
  midx <- sd.f * R
  midy <- sd.f * sin(acos(R))
  points(midx, midy, pch = pch, col = mod.col, cex = axis.pt)
  
  # Segmento da correlação do ponto na borda do gráfico
  #lines(c(0, maxsd * R), c(0, maxsd * sqrt(1 - R^2)), lty = 3, col=col)
  #acos.R <- acos(R)
  #segments(x0=cos(acos.R)*maxsd*.98     , y0=sin(acos.R)*maxsd*.98, 
  #         x1=cos(acos.R)*1.02*maxsd, y1=sin(acos.R)*1.02*maxsd, 
  #         col=col, lwd=1.5)
  
  # Pontos do BIAS =============================================================
  
  # Linhas verticais dos pontos (sutil), plota antes do primeiro ltime
  pos.y <- bias.y-.02-(mod.idx.input*.04)
  #if (pch=='1') {
    lines(x=c(0,2), y=c(pos.y    , pos.y)   , lwd=.3, xpd=T, col=mod.col, cex=axis.pt, lty=2) # linhas vertical
    lines(x=c(0,0), y=c(pos.y-.04,pos.y+.04), lwd=.3, xpd=T, col=mod.col) # linha do limite inferior
    lines(x=c(2,2), y=c(pos.y-.04,pos.y+.04), lwd=.3, xpd=T, col=mod.col) # linha do limite superior
  #}

  # Adapta na escala 0..2
  #bias.value.scaled <- bias.sc1 + (bias.value-bias.dist1)*(bias.sc2-bias.sc1) / (bias.dist2-bias.dist1)
  bias.value.scaled <- f.Scale(bias.value, bias.scale, bias.sc1, bias.sc2)

  # Mostra o valor um pouco deslocado para cada modelo
  text(x=bias.value.scaled, y=pos.y, labels=pch, col=mod.col, cex=axis.pt, xpd=T)
  # Mostra todos na mesma linha vertical, ou seja, posição x
  #text(x=bias.x, y=bias.value.scaled, labels=pch, col=mod.col, pos=4, cex=axis.pt, xpd=T)
  
  invisible(oldpar) # returns the old par configuration
  #cat('bias:', bias.value, '  Corr:', R, '  Sd:', sd.f, '\n')
}
