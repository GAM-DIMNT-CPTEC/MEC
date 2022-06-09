# Define texto para o periodo da figura, # DIÁRIA | MENSAL | SAZONAL
#if (fig.type == 'DIÁRIA') {
#   per.text <- paste0(format(CFG$dt, format='%Y-%m-%d '), CFG$hh,'h')
#} else {
#  if (fig.type == 'MENSAL') {
#    per.text <- paste0(month.abb[as.numeric(substring(CFG$dt.str, 5,6))], '/', substring(CFG$dt.str, 1, 4))
#  } else {
#    per.text <- season
#  }
#}

per.text <- switch(fig.type,
  'DIÁRIA'=  paste0(format(CFG$dt, format='%Y-%m-%d '), hh.targ,'h'),
  'MENSAL'=  paste0(month.abb[as.numeric(substring(season,5,6))], '/', substring(season, 1, 4), " ", hh.targ,'h'),
  'SAZONAL'= paste0(substring(season,1,4), substring(season,7), " ", hh.targ,'h'),
  'RAINY'= paste0(substring(season,1,4), substring(season,7), " ", hh.targ,'h')
)

# Acrescenta a quantidade de dias envolvidos na media dos dias
#if ('bad.days' %in% ls() && length(bad.days) > 0) {
#  per.text <- paste0(per.text, '(', dim(FCT)[3]-length(bad.days), ' dias)')
#}

# ==============================================================================
if (src %in% c('FCT', "OBS")) {
# ==============================================================================
  if (CFG$var == "PREC") {
    #my.breaks <- c(0:6,8,10,12,15,18,1000)  # OLD
    my.breaks <- c(0:6,8,10,12,15,20,40,60,90,120,1000)  # NEW

    if (fig.type == 'DIÁRIA') {
      my.breaks = c(0, 1, 2.5, 5, 10, 15, 20, 30, 40, 50, 60, 80, 100, 120, 150, 180, 210, 1000)
      my.colors <- colors.prec.day
    } else { 
      my.breaks = c(0, 1, 2, 3, 4, 5, 6, 8, 10, 12, 15, 18, 21, 24, 27, 30, 1000)
      my.colors <- colors.prec.med
    }
    my.direction <- "Up"
  } else {
    if (CFG$var %in% c('TMAX', 'TMIN')) {
       my.direction <- "UpDown"
       vMax = 1000
       # Uma só paleta e breaks para tmax e tmin (para serem comparáveis)
       my_breaks = c(-vMax, seq(2, 40, by=2), vMax) 
       my_colors = c("#0C46A0","#1465BF","#1976D2","#1E87E5","#2096F2","#41A5F4","#64B4F6","#90CAF8","#BADEFA","#E3F2FD","#FFFFFF","#F6E8C3","#EBD7A3","#E1C684","#D4AC62","#C68F3E","#B37625","#9D6115","#854D09","#6C3E07","#543005")

       # Padrão Saulo Freitas
       #if (CFG$var == 'TMIN') {
       #  seq_neg = c(-vMax, seq(10, 19.5, by=.5))
       #  seq_pos = c(seq(20.5, 30, by=.5), vMax) 
       #  my.breaks = c(seq_neg, 20, seq_pos)
       #  my.colors = c(colorRampPalette(c("#1D4F60","#C4E6C3"))(length(seq_neg)-1), rep("#FFFFFF", 2), 
       #                colorRampPalette(c("#E7D4E8","#40004B"))(length(seq_pos)-1))
       #} else {
       #  seq_neg = c(-vMax, seq(20, 29.5, by=.5))      
       #  seq_pos = c(seq(30.5, 40, by=.5), vMax)
       #  my.breaks = c(seq_neg, 30, seq_pos)
       #  my.colors = c(colorRampPalette(c("#1D4F60","#C4E6C3"))(length(seq_neg)-1), rep("#FFFFFF", 2),      
       #                colorRampPalette(c("#E7D4E8","#40004B"))(length(seq_pos)-1))
       #}
    } else {
      if (CFG$var == 'V10M') {
         my.breaks <- c(seq(0,8, .5), 1000)
         my.colors <- colors.v10m
         my.direction <- "Up"
      }
    }
  }
  if (src == 'OBS') {
    fname.img <- switch(fig.type,
      'DIÁRIA'=  paste0(CFG$var, '_', CFG$obs, '_D-', nDias),
      'MENSAL'=  paste0(CFG$var, '_', CFG$obs, '_', season), #substring(CFG$dt.str,1,6)),
      'SAZONAL'= paste0(CFG$var, '_', CFG$obs, '_', season),
      'RAINY'= paste0(CFG$var, '_', CFG$obs, '_', season)
    )

    tit1 <- paste(CFG$var, src)
    tit2 <- paste0(CFG.SET$obs1, '-', CFG.SET$obs2)
    tit3 <- ''
  } else {
    fname.img <- switch(fig.type,
      'DIÁRIA'=  paste0(CFG$var, '_', CFG$model, '_', CFG$ltime, '_D-', nDias),
      'MENSAL'=  paste0(CFG$var, '_', CFG$model, '_', CFG$ltime, '_', season), #substring(CFG$dt.str,1,6)),
      'SAZONAL'= paste0(CFG$var, '_', CFG$model, '_', CFG$ltime, '_', season),
      'RAINY'= paste0(CFG$var, '_', CFG$model, '_', CFG$ltime, '_', season)
    )
    tit1 <- paste(CFG$var, CFG$model)
    tit2 <- paste0(CFG$ltime, 'h (', as.integer(CFG$ltime)/24, 'd)')
    tit3 <- ''     
  }

} else {
  tit.cex <- 1.8
  tit1 <- paste(toupper(CFG$var), src)
  tit2 <- paste(CFG$model, 'vs.')
  tit3 <- paste0(CFG.SET$obs1, '-', CFG.SET$obs2)
}

# ==============================================================================
if (src == 'BIAS') {
# ==============================================================================
  vMax <- 10000
  if (CFG$var == "PREC") {
    #seq.neg <- c(seq(-20, -4, by=4), -2)
    #seq.pos <- c(2, seq(4, 20, by=4))
    seq.neg <- c(seq(-20, -10, by=5), seq(-5, -1))
    seq.pos <- c(seq(1, 5), seq(10, 20, by=5))
    col.neg <- f.ColNeg(length(seq.neg)+1)
    col.pos <- f.ColPos(length(seq.pos)+1)
    my.colors <- c(col.neg, col.pos)
    my.breaks <- c(-vMax, seq.neg, 0, seq.pos, vMax)
  } else {
    if (CFG$var %in% c('TMAX', 'TMIN')) {
      my.breaks = c(-vMax, seq(-8, -1, by=1), 0, seq(1, 8, by=1), vMax)
      if (CFG$var == 'TMIN') {
        my.colors = c("#7C1D6F","#B02378","#D23377","#E04572","#E85E6E","#F28170","#FAAC7B","#FCDE9C","#FFFFFF","#FFFFFF","#B6EFB6","#A3D4AF","#90BBAA","#7CA1A4","#647F9D","#4A5D95","#293578","#080E5B")
      } else {
        my.colors = c("#7F3B08","#9C4B06","#B95D08","#D37510","#E8912A","#F8B057","#FDC986","#FEE0B6","#FFFFFF","#FFFFFF","#ECE6F6","#C8B8E4","#A085D2","#815BC2","#6438B6","#5830AC","#4827A1","#311A92")
      }
    } else {
      if (CFG$var == 'V10M') {
        seq.neg <- seq(-3, -.5, by=.5)
        seq.pos <- seq(.5, 3, by=.5)
      }
    }
  }

  my.direction <- "UpDown"
  fname.img <- switch(fig.type,
      'DIÁRIA'=  paste0(CFG$var, '_', src, '_', CFG$model, 'x', CFG$obs, '_', CFG$ltime, '_D-', nDias),
      'MENSAL'=  paste0(CFG$var, '_', src, '_', CFG$model, 'x', CFG$obs, '_', CFG$ltime, '_', substring(CFG$dt.str,1,6)),
      'SAZONAL'= paste0(CFG$var, '_', src, '_', CFG$model, 'x', CFG$obs, '_', CFG$ltime, '_', season),
      'RAINY'= paste0(CFG$var, '_', src, '_', CFG$model, 'x', CFG$obs, '_', CFG$ltime, '_', season)
  )
} 

# ==============================================================================
if (src == 'SCORR') {
# ==============================================================================
  my.breaks = seq(-1, 1, by=.2)
  pneg = c("#7C1D6F","#CA2F78","#E34F6F","#F58C72","#FCDE9C")

  if (CFG$var == "PREC") {
    ppos = c("#FFFFD9","#C7E9B4","#41B6C4","#225EA8","#081D58")
  }
  if (CFG$var == "TMIN") {
    ppos = c("#D7F8D0","#A5D4BD","#74B0AA","#428C97","#005873")
  }
  if (CFG$var == "TMAX") {
    ppos = c("#FFFFE5","#D9F0A3","#78C679","#238443","#004529")
  }
  my.colors = c(pneg, ppos)

  my.direction <- NA
  fname.img <- switch(fig.type,
      'DIÁRIA'=  paste0(CFG$var, '_', src, '_', CFG$model, 'x', CFG$obs, '_', CFG$ltime, '_D-', nDias),
      'MENSAL'=  paste0(CFG$var, '_', src, '_', CFG$model, 'x', CFG$obs, '_', CFG$ltime, '_', substring(CFG$dt.str,1,6)),
      'SAZONAL'= paste0(CFG$var, '_', src, '_', CFG$model, 'x', CFG$obs, '_', CFG$ltime, '_', season),
      'RAINY'= paste0(CFG$var, '_', src, '_', CFG$model, 'x', CFG$obs, '_', CFG$ltime, '_', season)
  )
} 

# ==============================================================================
if (src == 'SRMSE') {
# ==============================================================================
  if (CFG$var == "PREC") {
     my.breaks = c(0:1,seq(2, 10, by=2),15,20,30,40,50,1000)
     my.colors = c("#FFFFFF","#D1E5F0","#9ECBE1","#62A7CD","#3580B9","#1B5B9D","#FDE1BA","#FDC38C","#FC8D59","#E7533A","#BF100A","#7F0000")
  }
  if (CFG$var == "TMIN") {
     my.breaks = c(seq(0,5, by=.5), 10,1000)
     my.colors = c("#FFFFFF","#FCFCD3","#D2D199","#A8A86F","#808055","#59593D","#E2C825","#D2930F","#BB6404","#A03303","#760B0B","#400505")
  }
  if (CFG$var == "TMAX") {
     my.breaks = c(seq(0,5, by=.5), 10,1000)
     my.colors = c("#FFFFFF","#EBEEF1","#BBC7CD","#8A9FA9","#64808C","#4C646E","#FEF0AD","#FECE65","#FE9929","#E1640E","#AA3C03","#662506")
  }

  my.direction <- "Up"
  fname.img <- switch(fig.type,
      'DIÁRIA'=  paste0(CFG$var, '_', src, '_', CFG$model, 'x', CFG$obs, '_', CFG$ltime, '_D-', nDias),
      'MENSAL'=  paste0(CFG$var, '_', src, '_', CFG$model, 'x', CFG$obs, '_', CFG$ltime, '_', substring(CFG$dt.str,1,6)),
      'SAZONAL'= paste0(CFG$var, '_', src, '_', CFG$model, 'x', CFG$obs, '_', CFG$ltime, '_', season),
      'RAINY'= paste0(CFG$var, '_', src, '_', CFG$model, 'x', CFG$obs, '_', CFG$ltime, '_', season)
  )
} 
# ==============================================================================
if (src == 'BESTOF') {
# ==============================================================================
  # my.colors = c("white", "purple3", "gold", "dodgerblue2")
  my.direction <- NA
  fname.img <- switch(fig.type,
      'DIÁRIA' = paste0(MEC_var, '_', src, '_', MEC_metric, "_", CFG$obs, '_', MEC_ltime, '_D-', nDias),
      'MENSAL' = paste0(MEC_var, '_', src, '_', MEC_metric, "_", CFG$obs, '_', MEC_ltime, '_', season),
      'SAZONAL'= paste0(MEC_var, '_', src, '_', MEC_metric, "_", CFG$obs, '_', MEC_ltime, '_', season),
      'RAINY'  = paste0(MEC_var, '_', src, '_', MEC_metric, "_", CFG$obs, '_', MEC_ltime, '_', season)
  )
  tit.cex <- 1.8
  tit1 = paste(toupper(CFG$var), src)
  tit2 = paste0(MEC_metric, " ", MEC_ltime, 'h (', as.integer(MEC_ltime)/24, 'd)')
  tit3 = per.text
  #tit2 <- MEC_metric
  #tit3 <- ""
} 
