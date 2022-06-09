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
    res = f.GetColorsAndBreaks(CFG$var, "AVG")
    my.breaks = res$breaks
    my.colors = res$colors
    my.direction = res$pointers
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
    res = f.GetColorsAndBreaks(CFG$var, "BIAS")
    my.breaks = res$breaks
    my.colors = res$colors
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
  res = f.GetColorsAndBreaks(CFG$var, "SCORR")
  my.breaks = res$breaks
  my.colors = res$colors
  my.direction = res$pointers
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
  res = f.GetColorsAndBreaks(CFG$var, "SRMSE")
  my.breaks = res$breaks
  my.colors = res$colors
  my.direction = res$pointers
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
