PerfDiag <- function(my.title='', show.legend=T, maxAxesPerf) {
  lims <- c(0, maxAxesPerf)
  # far <- seq(lims[2], lims[1], length = rr)
  # h <- seq(lims[1], lims[2], length = rr)
  # f <- function(far, h) {
  #   (lims[2] - far) * h/(lims[2] - far * (lims[2] - h))
  # }
  # g <- function(far, h) {
  #   h/(lims[2] - far)
  # }
  rr <- 501
  far <- seq(1, 0, length = rr)
  h <- seq(0, 1, length = rr)
  f <- function(far, h) {
    (1 - far) * h/(1 - far * (1 - h))
  }
  g <- function(far, h) {
    h/(1 - far)
  }
  hh <- function(h, b) {
    h/b
  }
  TS <- B <- matrix(NA, nrow = rr, ncol = rr)
  for (i in 1:rr) {
    for (j in 1:rr) {
      TS[i, j] <- f(far[i], h[j])
    }
  }
  
  bias.col <- 'brown'
  col.csi <- 'blue'
  if (show.legend) {
    par(mar=c(4,0,3,2), pty='s') # com legenda
  } else {
    par(mar=c(4,0,3,0), pty='s') # sem legenda
  }
  
  TS[which(TS > lims[2])] <- NA
  #plot.window(xlim=lims, ylim=lims)
  contour(t(TS), xlim=lims, ylim=lims, xlab = "", cex.main=1.5, line=1.5, 
          ylab = "", col=col.csi, asp=1, labcex=.8, axes=F, yaxs="i", xaxs="i", xpd=T) # (x|y)axs=1 ==> start in 0,0
  text(x=.9, y=0.12, labels = 'CSI (ou TS)', cex = .9, col=col.csi, pos = 2, xpd=T, font=2)
  box(); axis(1); axis(2, las=2)
  BB <- c(0.3, 0.5, 0.8, 1, 1.3, 1.5, 2, 3, 5, 10)
  x0 <- 0
  y0 <- 0
  x1 <- 1
  y1 <- hh(1, 1/BB)
  
  # FREQUENCY BIAS
  segments(x0, y0, x1, y1, lty = 2, xpd=F, col=bias.col, lwd=c(1,1,1,2.5,1,1,1,1,1,1))
  id <- y1 < 1
  text(x=1.005, y=y1[id], labels = y1[id], cex = 1, pos=4, xpd=T, col=bias.col)
  text(x=1, y=1.05, labels='Freq.', cex = 1, pos=4, font=2, col=bias.col, xpd=T)
  text(x=1, y=1.02, labels='Bias' , cex = 1, pos=4, font=2, col=bias.col, xpd=T)
  id <- y1 > 1
  mtext(side = 3, text = y1[id], at = 1/y1[id], line = 0, cex = 1, col=bias.col)
  
  #points(1,1, pch=24, cex=4, bg='yellow', xpd=T, lwd=1, col='black')
  #points(1,1, pch=25, cex=4, bg='yellow', xpd=T, lwd=1, col='black')
  text(x=1, y=1, labels = '1', cex = 1.2, xpd=T, col=bias.col)
  
  title(main=my.title, cex.main=1.5, line=1.5)
  title(xlab = paste(tl("Probab. de sucesso"), "(1-FAR)"), line=3, cex.lab=1.2)
  title(ylab = paste(tl("Probab. de detecção"), "(POD", tl("ou"), "Hit Rate)"), line=3, cex.lab=1.2)
  
  rect(xleft=0.01, ybottom=0.93, xright = .55, ytop = .98, col = 'white', lwd=0)
  # TODO: Boxed.labels from plotrix
  text(x=0, y=0.95, labels = tl('SUPERESTIMOU MAIS VEZES'), cex = 1, col=2, font=2, pos=4)
  text(x=1, y=0.02, labels = tl('SUBESTIMOU MAIS VEZES'), cex = 1, col=2, font=2, pos=2)
  
  #text(x=c(-0.22, -0.15, -0.12), y=-.180, labels = c('POD','=','% de acertos ao prever e o evento realmente ocorrer'), pos=4, cex = .9, xpd=T)
  #text(x=c(-0.22, -0.15, -0.12), y=-.215, labels = c('FAR','=','% de erros ao prever mas o evento não ocorrer'), pos=4, cex = .9, xpd=T)
  #text(x=c(-0.22, -0.15, -0.12), y=-.250, labels = c('CSI','=','Ind. de Sucesso Crítico [% de acerto (não considerando a não ocorrência corretamente prevista)]'), pos=4, cex = .9, xpd=T, col=col.csi)
}
#PerfDiag()

# POD = a/(a+c) ==> Prob of detection: Percentual de acertos ao prever e o evento realmente ocorrer.
# FAR = b/(a+b) ==> False Alarme Ratio: Percentual de erros ao prever e o evento não ocorrer.
# CSI = a/(a+b+c) ==> Indice de Sucesso Critico: Percentual de acerto nas estimativas (exceto a não ocorrência corretamente prevista)
