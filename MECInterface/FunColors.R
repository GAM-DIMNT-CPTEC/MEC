# ==============================================================================
# Functions related to colors, breaks, etc
# ==============================================================================
library(colorspace)

# ==============================================================================
# Retorns colors anda breaks according to the variable and type of map
# my_var = PREC | TMAX | SBIAS | BIAS | SCORR | SRMSE | ...
# metric = "D" daily (must expect more variability) | "AC" accumulated (lower variability)
# ==============================================================================
f.GetColorsAndBreaks = function(my_var, metric="AVG") {
  # =======================================================
  # SCORR
  # =======================================================
  if (metric == "SCORR") {
    my_pointers = ""
    my_breaks <- seq(-1, 1, by=.1)
    npos=length(which(my_breaks>=0))-1; nneg=length(which(my_breaks<=0))-1
    #pn = "CARTO-SunsetDark"
    pneg = colorRampPalette(rev(c("#fcde9c","#faa476","#f0746e","#e34f6f","#dc3977","#b9257a","#7c1d6f")))(nneg)  # 1 paleta negativa só para todos
    #pneg = c("#7C1D6F","#CA2F78","#E34F6F","#F58C72","#FCDE9C")
    if (my_var == "PREC") {
      #pp = "BRW-YlGnBu"
      #ppos = colorRampPalette(pal_pos[[pp]])(npos)
      ppos = c("#FFFFD9","#C7E9B4","#41B6C4","#225EA8","#081D58")
    }
    if (my_var == "TMIN") {
      #pp = "Custom"
      ppos = c("#D7F8D0","#A5D4BD","#74B0AA","#428C97","#005873")
    }
    if (my_var == "TMAX") {
      #pp = "BRW-YlGn"
      #ppos = colorRampPalette(pal_pos[[pp]])(npos)
      ppos = c("#FFFFE5","#D9F0A3","#78C679","#238443","#004529")
    }
    if (my_var == 'U10M') {
      ppos = brewer.pal(9, "GnBu")
      pneg = rev(colorRampPalette(brewer.pal(9, "OrRd"))(nneg))
    }
    if (my_var == 'V10M') {
      ppos = brewer.pal(9, "BuGn")
      pneg = rev(colorRampPalette(brewer.pal(9, "OrRd"))(nneg))
    }
    if (my_var == 'VEL10M') {
      ppos = brewer.pal(9, "BuPu")
      pneg = rev(colorRampPalette(brewer.pal(9, "OrRd"))(nneg))
    }
    ppos = colorRampPalette(ppos)(npos)
    my_colors = c(pneg, ppos)
  } 
  # =======================================================
  # SRMSE
  # =======================================================
  if (metric == "SRMSE") {
    my_pointers = "Up"
    if (my_var == "PREC") {
      my_breaks = c(0:1,seq(2, 10, by=2),15,20,30,40,50,1000)
      #npos = floor((length(my_breaks)-2)/2); nneg=length(my_breaks) - 2 - npos
      #pp = "BRW-RdBu"
      #ppos = colorRampPalette(pal_pos[[pp]])(npos+1)[1:npos]      # retira a última cor (muito escura)
      #pn = "BRW-OrRd"
      #pneg = colorRampPalette(pal_neg[[pn]])(nneg+1)[2:(nneg+1)]  # retira a primeira cor (muito clara)
      #my_colors = c("#FFFFFF", ppos, pneg)
      my_colors = c("#FFFFFF","#D1E5F0","#9ECBE1","#62A7CD","#3580B9","#1B5B9D","#FDE1BA","#FDC38C","#FC8D59","#E7533A","#BF100A","#7F0000")
    }
    if (my_var == "TMIN") {
      my_breaks = c(seq(0,5, by=.5), 10,1000)
      # Vou adicionar o branco então menos uma cor automática
      #npos=floor((length(my_breaks)-2)/2); nneg=length(my_breaks) - 2 - npos
      #pp = "cspace-Lisbon"
      #ppos = colorRampPalette(pal_pos[[pp]])(npos+1)[1:npos]      # retira a última cor (muito escura)
      #pn = "pals-ocean.oxy"
      #pneg = colorRampPalette(pal_neg[[pn]])(nneg+1)[2:(nneg+1)]  # retira a primeira cor (muito clara)
      #my_colors = c("#FFFFFF", ppos, pneg)
      my_colors = c("#FFFFFF","#FCFCD3","#D2D199","#A8A86F","#808055","#59593D","#E2C825","#D2930F","#BB6404","#A03303","#760B0B","#400505")
    }
    if (my_var == "TMAX") {
      my_breaks = c(seq(0,5, by=.5), 10,1000)
      # Vou adicionar o branco então menos uma cor automática
      # npos=floor((length(my_breaks)-2)/2); nneg=length(my_breaks) - 2 - npos
      # pp = "ggsci-blue-grey"
      # ppos = colorRampPalette(pal_pos[[pp]])(npos+1)[1:npos]      # retira a última cor (muito escura)
      # pn = "BRW-YlOrBr"
      # pneg = colorRampPalette(pal_neg[[pn]])(nneg+1)[2:(nneg+1)]  # retira a primeira cor (muito clara)
      # my_colors = c("#FFFFFF", ppos, pneg)
      my_colors = c("#FFFFFF","#EBEEF1","#BBC7CD","#8A9FA9","#64808C","#4C646E","#FEF0AD","#FECE65","#FE9929","#E1640E","#AA3C03","#662506")
    }
    if (my_var == 'U10M') {
      my_breaks = c(seq(0,5, by=.5), 10, 1000)
      n1 = floor((length(my_breaks)-1)/2)
      n2 = length(my_breaks) - 1 - n1
      my_colors1 = colorRampPalette(c("#FFFFFF", f.GetHexColor("hotpink4")))(n1) 
      my_colors2 = colorRampPalette(c("#E2C825","#D2930F","#BB6404","#A03303","#760B0B","#400505"))(n2)
      my_colors = c(my_colors1, my_colors2)
    }
    if (my_var == 'V10M') {
      my_breaks = c(seq(0,5, by=.5), 10, 1000)
      n1 = floor((length(my_breaks)-1)/2)
      n2 = length(my_breaks) - 1 - n1
      my_colors1 = colorRampPalette(c("#FFFFFF",f.GetHexColor("springgreen4")))(n1)
      my_colors2 = colorRampPalette(c("#E2C825","#D2930F","#BB6404","#A03303","#760B0B","#400505"))(n2)
      my_colors = c(my_colors1, my_colors2)
    }
    if (my_var == 'VEL10M') {
      my_breaks = c(seq(0,5, by=.5), 10, 1000)
      my_colors = colorRampPalette(c("#FFFFFF","#FCFCD3","#D2D199","#A8A86F","#808055","#59593D",
                                     "#E2C825","#D2930F","#BB6404","#A03303","#760B0B","#400505"))(length(my_breaks) - 1)
    }
  }
  
  # =======================================================
  # SBIAS
  # =======================================================  
  if (grepl("BIAS", metric)) {
    my_pointers = "UpDown"
    if (my_var == "PREC") {
      my_breaks <- c(-10000, seq(-20, -10, by=5), seq(-5, -1), 0, seq(1, 5), seq(10, 20, by=5), +10000)
      col_neg = colorRampPalette(c("red", "orange", 'yellow', "white"))(floor(length(my_breaks)/2))
      col_pos <- colorRampPalette(c("white", 'cornflowerblue', 'blueviolet', 'blue4'))(floor(length(my_breaks)/2))
      my_colors = c(col_neg, col_pos)
    }
    if (my_var %in% c('TMAX', 'TMIN')) {
      my_breaks = c(-10000, seq(-8, -1, by=1), 0, seq(1, 8, by=1), 10000)
      if (my_var == 'TMIN') {
        my_colors = c("#7C1D6F","#B02378","#D23377","#E04572","#E85E6E","#F28170","#FAAC7B","#FCDE9C","#FFFFFF","#FFFFFF","#B6EFB6","#A3D4AF","#90BBAA","#7CA1A4","#647F9D","#4A5D95","#293578","#080E5B")
      }
      if (my_var == 'TMAX') {
        my_colors = c("#7F3B08","#9C4B06","#B95D08","#D37510","#E8912A","#F8B057","#FDC986","#FEE0B6","#FFFFFF","#FFFFFF","#ECE6F6","#C8B8E4","#A085D2","#815BC2","#6438B6","#5830AC","#4827A1","#311A92")
      }
    }
    if (my_var == 'U10M') {
      my_breaks = c(-10000, seq(-4, -.5, by=.5), 0, seq(.5, 4, by=.5), 10000)
      col_neg <- rev(brewer.pal(floor(length(my_breaks)/2), "YlOrRd"))
      col_pos <- brewer.pal(floor(length(my_breaks)/2), "PuBuGn")
      my_colors = c(col_neg, col_pos)
    }    
    if (my_var == 'V10M') {
      my_breaks = c(-10000, seq(-4, -.5, by=.5), 0, seq(.5, 4, by=.5), 10000)
      col_neg <- rev(brewer.pal(floor(length(my_breaks)/2), "YlOrRd"))
      col_pos <- brewer.pal(floor(length(my_breaks)/2), "GnBu")
      my_colors = c(col_neg, col_pos)
    }    
    if (my_var == 'VEL10M') {
      my_breaks = c(-10000, seq(-4, -.5, by=.5), 0, seq(.5, 4, by=.5), 10000)
      col_neg <- rev(brewer.pal(floor(length(my_breaks)/2), "YlOrRd"))
      col_pos <- colorRampPalette(c("white", 'cornflowerblue', 'blueviolet', 'blue4'))(floor(length(my_breaks)/2))
      col_pos <- brewer.pal(floor(length(my_breaks)/2), "BuPu")
      my_colors = c(col_neg, col_pos)
    }    
  }
  # =======================================================
  # AVG
  # =======================================================  
  if (metric == "AVG") {
    my_pointers = "UpDown"
    if (my_var == "PREC") {
      my_breaks <- c(-10000, seq(-20, -10, by=5), seq(-5, -1), 0, seq(1, 5), seq(10, 20, by=5), +10000)
      col_neg = colorRampPalette(c("red", "orange", 'yellow', "white"))(floor(length(my_breaks)/2))
      col_pos <- colorRampPalette(c("white", 'cornflowerblue', 'blueviolet', 'blue4'))(floor(length(my_breaks)/2))
      my_colors = c(col_neg, col_pos)
    }
    if (my_var %in% c('TMIN')) {
      vMax = 1000
      # Uma só paleta e breaks para tmax e tmin (para serem comparáveis)
      my_breaks = c(-vMax, seq(2, 40, by=2), vMax) 
      my_colors = c("#0C46A0","#1465BF","#1976D2","#1E87E5","#2096F2","#41A5F4","#64B4F6","#90CAF8","#BADEFA","#E3F2FD","#FFFFFF","#F6E8C3","#EBD7A3","#E1C684","#D4AC62","#C68F3E","#B37625","#9D6115","#854D09","#6C3E07","#543005")
      # verde roxo
      #my_colors = c("#1D4F60", "#236169", "#2B7472", "#36877A", "#459980", "#57AA88", "#6DBC90", "#88CA9D", "#A5D8AE",
      #              "#C4E6C3", "#FFFFFF", "#E7D4E8", "#D6BFDC", "#C6AAD1", "#B493C3", "#A27BB3", "#9160A2", "#814190",
      #              "#70257C", "#581263", "#40004B")
      
      # Duas - Padrão Saulo Freitas
      #seq_neg = c(-vMax, seq(10, 19.5, by=.5))
      #seq_pos = c(seq(20.5, 30, by=.5), vMax)
      #my_breaks = c(seq_neg, 20, seq_pos)
      #my_colors = c(colorRampPalette(c("#1D4F60","#C4E6C3"))(length(seq_neg)-1), rep("#FFFFFF", 2), 
      #              colorRampPalette(c("#E7D4E8","#40004B"))(length(seq_pos)-1))
    }
    if (my_var %in% c('TMAX')) {
      vMax = 1000
      # Uma só paleta e breaks para tmax e tmin (para serem comparáveis)
      my_breaks = c(-vMax, seq(2, 40, by=2), vMax) 
      my_colors = c("#0C46A0","#1465BF","#1976D2","#1E87E5","#2096F2","#41A5F4","#64B4F6","#90CAF8","#BADEFA","#E3F2FD","#FFFFFF","#F6E8C3","#EBD7A3","#E1C684","#D4AC62","#C68F3E","#B37625","#9D6115","#854D09","#6C3E07","#543005")
      # verde roxo
      #my_colors = c("#1D4F60", "#236169", "#2B7472", "#36877A", "#459980", "#57AA88", "#6DBC90", "#88CA9D", "#A5D8AE",
      #                "#C4E6C3", "#FFFFFF", "#E7D4E8", "#D6BFDC", "#C6AAD1", "#B493C3", "#A27BB3", "#9160A2", "#814190",
      #                "#70257C", "#581263", "#40004B")
      
      # Duas - Padrão Saulo Freitas
      #seq_neg = c(-vMax, seq(20, 29.5, by=.5)) 
      #seq_pos = c(seq(30.5, 40, by=.5), vMax)
      #my_breaks = c(seq_neg, 30, seq_pos)
      #my_colors = c(colorRampPalette(c("#1D4F60","#C4E6C3"))(length(seq_neg)-1), rep("#FFFFFF", 2), 
      #              colorRampPalette(c("#E7D4E8","#40004B"))(length(seq_pos)-1))
    }
    if (my_var == 'U10M') {
      my_breaks <- c(-vMax, seq(-10, 10, by=1), vMax)
      col_pos = c("#ADD8E6FF",                                    # Lightblue
                  "#5F8DE8", "#3955C3", "#131C9D",                # AZUL 
                  "#98FB98FF", "#B6C100", "#008406", "#004F3B",   # VERDE
                  "#A271D0", "#8E39DE", "#551A8BFF")              # ROXO
      col_neg = rev(colorRampPalette(RColorBrewer::brewer.pal(9, "Oranges"))(11))
      my_colors = c(col_neg, col_pos)
      # my_colors = c("#FFFFFF",                                             # BRANCO
      #               "#7071E9", "#5F8DE8", "#3955C3", "#131C9D",            # AZUL 
      #               "#B6C100", "#54A400", "#008406", "#004F3B",            # VERDE
      #               "#FFB6C1", "#FF69B4", "#FF1493", "#CD1076",            # ROSA
      #               "#A271D0", "#8E39DE", "#7F49E5",                       # ROXO
      #               "#FFFF00", "#FFD200", "#FF7D00", "#FF4500", "#8B0000") # AMARELO ..VERMELHO
      # amarelo era EEEE00
    }
    if (my_var == 'V10M') {
      my_breaks <- c(-vMax, seq(-10, 10, by=1), vMax)
      col_pos = c("darkseagreen1",                                # Lightgreen
                  "seagreen1", "#B6C100", "#008406", "#004F3B",   # VERDE
                  "#5F8DE8", "#3955C3", "#131C9D",                # AZUL 
                  "#A271D0", "#8E39DE", "#551A8BFF")              # ROXO
      col_neg = rev(colorRampPalette(RColorBrewer::brewer.pal(9, "Reds"))(11))
      my_colors = c(col_neg, col_pos)
      # my_colors = c("#FFFFFF",                                             # BRANCO
      #               "#B6C100", "#54A400", "#008406", "#004F3B",            # VERDE 
      #               "#7071E9", "#5F8DE8", "#3955C3", "#131C9D",            # AZUL 
      #               "#A271D0", "#8E39DE", "#7F49E5",                       # ROXO
      #               "#FFB6C1", "#FF69B4", "#FF1493", "#CD1076",            # ROSA
      #               "#FFFF00", "#FFD200", "#FF7D00", "#FF4500", "#8B0000") # AMARELO ..VERMELHO
    }
    if (my_var == 'VEL10M') {
      my_breaks <- c(seq(0, 10, by=.5), 10000)
      my_colors = c("#FFFFFF", "#CDCDCD",                                  # BRANCO E CINZA
                    "#FFB6C1", "#FF69B4", "#CD1076",                       # ROSA
                    "#A271D0", "#8E39DE", "#7F49E5",                       # ROXO
                    "#7071E9", "#5F8DE8", "#3955C3",  "#131C9D",           # AZUL 
                    "#B6C100", "#54A400", "#008406",  "#004F3B",           # VERDE
                    "#FFFF00", "#FFD200", "#FF7D00", "#FF4500", "#8B0000") # AMARELO ..VERMELHO
    }
  }
  res = list()
  res$breaks   = my_breaks
  res$colors   = my_colors
  res$pointers = my_pointers
  return(res)
}
#f.GetHexColor("deeppink")


# par(mfrow=c(2,3))
# f.pie("violet")
# f.pie("pink")
# f.pie("purple")
# f.pie("magenta")
# f.pie("orchid")
# colors()

# Draws a pie char from colors passed as arguments:
# if multiple colors, draws them
# if just one color, search it in colors() and show all containing the string, e.g. f.pie("blue")
f.pie = function(my_col) {
  if (length(my_col) == 1) {
    my_colors = colors()[grep(my_col, colors())]
    my_colors = my_colors[order(colMeans(col2rgb(my_colors)), decreasing=T)]
  } else {
    my_colors = my_col
  }
  pie(rep(1, length(my_colors)), col=my_colors, labels=paste0(1:length(my_colors), ") ", my_colors))
  return(my_colors)
}
