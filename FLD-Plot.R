source('FLD-PlotDef.R')   # Calcula cores e breaks das legends e campos
  
idx.res=1
for (idx.res in 3) { #:nrow(RESOL)) {
  resol <- RESOL[idx.res, ]
  plotreg="REG"
  for (plotreg in c('REG', 'NREG')) { # With/without regions in red
    #try(dev.off())
    #dev.new()

    fname.img2 <- paste0(DIR$img, fname.img, '_', mask, '_', resol$NAME, '_', plotreg, "_", CFG$hh, '.jpeg')  
    #fname.img2 <- paste0('SP_DOMAIN.jpeg')
    if (src %in% c('FCT', "BESTOF")) {
      # FCT nao tem escala de cores para economizar espaço, ve na observação
      # BESTOF tb não
      jpeg(file=fname.img2, width=resol$WIDTH*.9, height=resol$HEIGHT) # diminui 10% a largura
      my.mar <- MARGINS[[paste0(resol$NAME,'-FCT')]] 
    } else {
      jpeg(file=fname.img2, width=resol$WIDTH, height=resol$HEIGHT)
      my.mar <- MARGINS[[paste0(resol$NAME,'-OBS')]] 
    }

    footer1 <- ''
    if (fig.type == 'DIÁRIA') {
      if (src == 'OBS') {
        footer2 <- paste0(CFG$var, ' AVG=', round(OBS[.(reg, mask, CFG$dt, hh.targ, "AVG"), VALUE],2))
        stopifnot(!is.na(OBS[.(CFG$area1.names[1], mask, CFG$dt, hh.targ, "AVG"), VALUE]))
      } else {
        if (src == 'BIAS') {
           footer1 <- paste0(CFG$ltime, 'h (', as.integer(CFG$ltime)/24, 'd)')
           footer2 <- paste0(CFG$var, ' BIAS=', round(EVAL.CONT[.(CFG$area1.names[1], mask, CFG$dt, CFG$hh, "BIAS"), VALUE],2))
        } else {
          footer2 <- paste0(CFG$var, ' AVG=', round(EVAL.CONT[.(CFG$area1.names[1], mask, CFG$dt, CFG$hh, "AVG"), VALUE],2))
        }
        stopifnot(!is.na(EVAL.CONT[.(CFG$area1.names[1], mask, CFG$dt, CFG$hh, "AVG"), VALUE]))
      }
    } else { # MENSAL | SAZONAL | RAINY
      if (src == 'BIAS') {
         footer1 <- paste0(CFG$ltime, 'h (', as.integer(CFG$ltime)/24, 'd)')
         footer2 <- paste0(CFG$var, ' BIAS=', round(mean(FLD, na.rm=T),2), 
           ifelse('bad.days' %in% ls() && length(bad.days) > 0,  paste0(' (',dim(FCT)[3]-length(bad.days), ' dias)'), ''))
      } else {
        if (src %in% c('SCORR', "SRMSE")) {
           footer1 <- paste0(CFG$ltime, 'h (', as.integer(CFG$ltime)/24, 'd)')
           footer2 <- paste0(CFG$var, " ", src, "=", round(mean(FLD, na.rm=T), 2), 
              ifelse('bad.days' %in% ls() && length(bad.days) > 0,  paste0(' (',dim(FCT)[3]-length(bad.days), ' dias)'), ''))
        } else {
          if (src != "BESTOF") {
            footer2 <- paste0(CFG$var, ' AVG=', round(mean(FLD, na.rm=T),2), 
              ifelse('bad.days' %in% ls() && length(bad.days) > 0,  paste0(' (',dim(FCT)[3]-length(bad.days), ' dias)'), ''))
          } else {
            footer2 = paste0(CFG$ltime, 'h (', as.integer(CFG$ltime)/24, 'd)')
          }
        }
      }
    }
    footer3 <- per.text
    par(mar=my.mar, oma=c(0,0,0,0))
    # round( lon e lat, 2) because rounding makes grid to be non regular
    #cat("\n", length(CFG$lon), "x", length(CFG$lat), "=", dim(FLD), "\n")
    if (mask=='CONT') {
      FLD = FLD + MASK.CONT
    }

    if (src != "BESTOF") {
      image(x=round(CFG$lon,2), y=round(CFG$lat,2), z=FLD, useRaster=T, col=my.colors, xlab='', ylab='', pty='m', asp=1, axes=F, breaks = my.breaks)
    } else {
      image(x=round(CFG$lon,2), y=round(CFG$lat,2), z=FLD, useRaster=T, col=my.colors, xlab='', ylab='', pty='m', asp=1, axes=F)
    }

    # Workaround to prevent ploting the same segment twice because they can be
    # repeated among the areas
    if (plotreg == 'REG') {
      segs <- list()
      for (iReg in 1:nrow(REGIONS)) {
        # each box has 4 sides (segments)
        reg2 <- REGIONS[iReg, ]
        seg <- list(x=c(reg2$LON1, reg2$LON2), y=c(reg2$LAT1, reg2$LAT1))
        seg.key <- paste0(unlist(seg), collapse=':') 
        if (is.null(segs[[seg.key]])) {
          f.PlotSeg(seg)
          segs[[seg.key]] <- seg.key
        }
        seg <- list(x=c(reg2$LON1, reg2$LON1), y=c(reg2$LAT1, reg2$LAT2))
        seg.key <- paste0(unlist(seg), collapse=':') 
        if (is.null(segs[[seg.key]])) {
          f.PlotSeg(seg)
          segs[[seg.key]] <- seg.key
        }
        seg <- list(x=c(reg2$LON2, reg2$LON2), y=c(reg2$LAT1, reg2$LAT2))
        seg.key <- paste0(unlist(seg), collapse=':') 
        if (is.null(segs[[seg.key]])) {
          f.PlotSeg(seg)
          segs[[seg.key]] <- seg.key
        }
        seg <- list(x=c(reg2$LON1, reg2$LON2), y=c(reg2$LAT2, reg2$LAT2))
        seg.key <- paste0(unlist(seg), collapse=':') 
        if (is.null(segs[[seg.key]])) {
          f.PlotSeg(seg)
          segs[[seg.key]] <- seg.key
        }
      }
    }
    plot(SHP_BR_REG, add=T)
    map(add=T, col = 'black', lwd=2, xlim = range(CFG.SET$lon), ylim = range(CFG.SET$lat))
    # Truque para limpar o excesso de linhas do mapa
    usr <- par('usr')
    x1 <- min(range(CFG.SET$lon), usr[1])-1
    x2 <- max(range(CFG.SET$lon), usr[2])+1
    y1 <- min(range(CFG.SET$lat), usr[3])-1
    y2 <- max(range(CFG.SET$lat), usr[4])+1
    rect(xleft=x1              , xright=x2              , ybottom=y1              , ytop=range(CFG.SET$lat)[1], col='white', border='white', xpd=T) # bottom
    rect(xleft=x1              , xright=x2              , ybottom=range(CFG.SET$lat)[2], ytop=y2              , col='white', border='white', xpd=T) # top
    rect(xleft=x1              , xright=range(CFG.SET$lon)[1], ybottom=y1              , ytop=y2              , col='white', border='white', xpd=T) # left
    rect(xleft=range(CFG.SET$lon)[2], xright=x2              , ybottom=y1              , ytop=y2              , col='white', border='white', xpd=T) # right

    #title(main=my.title, cex.main=2, line=-0.7)
    # Grid lines
    invisible(lapply(xs, f.GridX))
    invisible(lapply(ys, f.GridY))

    # Textos em cima (de cima pra baixo)
    leg <- f.BoxedText(max(CFG$lon), y=max(CFG$lat)           , text=tit1, col.fg='blue', border=F, cex=resol$CEX1)
    leg <- f.BoxedText(max(CFG$lon), y=leg$rect$top-leg$rect$h, text=tit2, col.fg='blue', border=F, cex=resol$CEX2)
    leg <- f.BoxedText(max(CFG$lon), y=leg$rect$top-leg$rect$h, text=tit3, col.fg='blue', border=F, cex=resol$CEX3)

    #leg <- f.BoxedText(usr[2], y=max(CFG$lat)           , text=tit1, col.fg='blue', border=F, cex=resol$CEX1)
    #leg <- f.BoxedText(usr[2], y=leg$rect$top-leg$rect$h, text=tit2, col.fg='blue', border=F, cex=resol$CEX2)
    #leg <- f.BoxedText(usr[2], y=leg$rect$top-leg$rect$h, text=tit3, col.fg='blue', border=F, cex=resol$CEX3)

    if (src == "BESTOF") {
      legend(x=max(CFG$lon), y=min(CFG$lat), legend=my.leg, fill=my.colors, xpd=T, pt.cex=2, horiz=F, xjust=1, yjust=0, 
             bg="white", y.intersp=1.5)
    } else {
      # Textos em baixo (de baixo para cima)
      leg <- f.BoxedText(max(CFG$lon), y=min(CFG$lat), text=footer3, col.fg='blue', border=F, cex=resol$CEX3, yjust=0)
      #leg <- f.BoxedText(usr[2], y=min(CFG$lat), text=footer3, col.fg='blue', border=F, cex=resol$CEX3, yjust=0)
      if (footer2 != '') {
        leg <- f.BoxedText(max(CFG$lon), y=leg$rect$top, text=footer2, col.fg='blue', border=F, cex=resol$CEX2, yjust=0)
        #leg <- f.BoxedText(usr[2], y=leg$rect$top, text=footer2, col.fg='blue', border=F, cex=resol$CEX2, yjust=0)
      }
      if (footer1 != '') {
        leg <- f.BoxedText(max(CFG$lon), y=leg$rect$top, text=footer1, col.fg='blue', border=F, cex=resol$CEX1, yjust=0)
        #leg <- f.BoxedText(usr[2], y=leg$rect$top, text=footer1, col.fg='blue', border=F, cex=resol$CEX1, yjust=0)
      }
    }

    # Escala de cores 
    if (!src %in% c("FCT", "BESTOF")) { # FCT nao tem escala de cores para economizar espaço, ve na observação
      f.ArrowedColorBar(direction=my.direction, ylow=f.UsrPxUsr(CFG.SET$lat.range[1],-50, 'Y'), yhigh=f.UsrPxUsr(CFG.SET$lat.range[2],+50, 'Y'), 
      #f.ArrowedColorBar(direction=my.direction, ylow=CFG.SET$lat.range[1]-6, yhigh=CFG.SET$lat.range[2]+6, 
      x1=f.UsrPxUsr(CFG.SET$lon.range[2], 20, 'X'), x2=f.UsrPxUsr(CFG.SET$lon.range[2],35,'X'), x3=f.UsrPxUsr(CFG.SET$lon.range[2],31,'X'),
      nums=my.breaks, cols = my.colors, xpd=T)
    }
    
    # Eixos e box
    rect(xleft=CFG.SET$lon.range[1], xright=CFG.SET$lon.range[2], ybottom=CFG.SET$lat.range[1], ytop=CFG.SET$lat.range[2], lwd=2)
    text(x=xs, y=f.UsrPxUsr(CFG.SET$lat.range[1],15,'Y'), labels=xs, xpd=T)
    text(x=f.UsrPxUsr(CFG.SET$lon.range[1],0,'X'), y=ys, labels=ys, las=2, xpd=T, pos=2)
    dev.off()
  }
}


