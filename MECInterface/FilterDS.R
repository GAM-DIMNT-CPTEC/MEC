# ==============================================================================  
# Filtra o dataset escolhido de acordo com as opções escolhidas para em input
# ------------------------------------------------------------------------------
# DS.NAME='EVAL_CONT'; dateBeg=input$anoMesBeg; dateEnd=input$anoMesEnd
# ATENÇÃO *** MÉDIA DAS MÉDIAS (DE DADOS AGRUPADOS) **** ================================
# ATENÇÃO: media(total) != media de (MediaParcia1 + MediaParcial2 + MediaParcial3)
# CORRETO: [(N1*MP1)+(N2*MP2)+(N3*MP3)/(N1+N2+N3), onde N=qtd de observacoes em cada media
# Exemplo:
#v <- rnorm(3000)
#mean(v)
#m1=mean(v[1:1000]); m2=mean(v[1001:2000]); m3=mean(v[2001:3000]); Cat(m1, m2, m3, '\n')
#mean(m1,m2,m3); Cat((m1+m2+m3)/3, '\n')
#((1000*m1) + (1000*m2) + (1000*m3)) / (3000)
# allLTimes = T, força a abertura de todos os aruivos de lead time, para algum processamento
# que requeira todos juntos
# ==============================================================================
f.FilterDS <- function (DS.NAME, input, type="PER", allLTimes=F, updateProgress=NULL) { # PER | LAST_DAY | LAST_WEEK | LAST_MONTH
  #validate(need(!is.null(input$mainRegId), F)) # Forces validation
  # nenhum valor pode ser FALSE
  req(input$evsetId, input$modId, input$maskId, input$anoMesBeg, input$anoMesEnd, input$subRegId)
  # FILTRO DOS LTIMES ==========================================================
  # Appenda todos os LEAD TIMES num só DS
  # ----------------------------------------------------------------------------
  RES <- data.table()
  #DS.NAME='EVAL_CONT'; input$runTimeId="00"; input$ltimes=c(1,1); type="PER"; allLTimes=F

  # Verificar se isso fica pesado, se ficar fazer um por um, tirar as medias e appendar
  #allLTimes=F
  if (!allLTimes) {
    ltimes <- input$ltimes[1]:input$ltimes[2]
  } else {
    ltimes <- 1:max(CFG$ltimes)
  }
  
  idx.ltime=idx.rtime=1
  for (idx.ltime in 1:length(ltimes)) {
    ltime = ltimes[idx.ltime]
    for (idx.rtime in 1:length(input$runTimeId)) {
      rtime = input$runTimeId[idx.rtime]
      try(
        if (is.function(updateProgress)) {
          cur.step <- idx.ltime * idx.rtime
          updateProgress(cur.step)
        })
      RES <- rbind.data.frame(
        RES,
        get(load(paste0(EVAL.CUR, '/', DS.NAME, '_', ltime, '_', rtime, '.RData'))))
      gc()
    }
  }
  if (DS.NAME == 'EVAL_CONT') {
    rm(EVAL.CONT) # Apaga o EVAL.CONT local (desta função)
  } else {
    rm(EVAL.CAT) # Apaga o EVAL.CAT local (desta função)
  }
  gc()
  
  # FILTRO DAS DATAS ===========================================================
  # Pega qq qtd de char dos inputs e filtra os maiores= que a data inicial e 
  # os menores= que a data final
  # ----------------------------------------------------------------------------
  # RES <- EVAL.CONT[
  #   (substring(format(DT, '%Y%m%d'), 1, nchar(input$anoMesBeg)) >= input$anoMesBeg) & 
  #     (substring(format(DT, '%Y%m%d'), 1, nchar(input$anoMesEnd)) <= input$anoMesEnd), ]
  switch(
    type,
    PER={
      dateBeg <- input$anoMesBeg
      dateEnd <- input$anoMesEnd
    },
    LAST_DAY={
      dateBeg <- format(max(RES$DT), format="%Y%m%d")
      dateEnd <- format(max(RES$DT), format="%Y%m%d")
    },
    LAST_WEEK={
      dateBeg <- format(max(RES$DT)-7, format="%Y%m%d")
      dateEnd <- format(max(RES$DT), format="%Y%m%d")
    },
    LAST_MONTH={
      dateBeg <- format(max(RES$DT)-30, format="%Y%m%d")
      dateEnd <- format(max(RES$DT), format="%Y%m%d")
    }
  )
  #dateBeg="2021"; dateEnd="2021"
  
  RES <- RES[
    (substring(format(DT, '%Y%m%d'), 1, nchar(dateBeg)) >= dateBeg) & 
      (substring(format(DT, '%Y%m%d'), 1, nchar(dateEnd)) <= dateEnd), ]

  # FILTROS: CONJUNTO DE AVALIAÇÃO, MODELOS, REGs ==============================
  if (input$ViewMode=='SC') {
    regions <- GetSubRegs(input) # Se ScoreCard retorna todas as subregiões (pois ele já separa)
  } else {
    if (input$ViewMode=='Hist') {
      regions <- GetSubRegs(input) # Se ScoreCard retorna todas as subregiões (pois ele já separa)
    } else {
      regions <- input$subRegId
    }
  }
  RES <- RES[EV_SET==input$evsetId & MOD %in% input$modId & REG %in% regions, ]
  my_var = unlist(strsplit(input$evsetId, "-"))[1]
  if (DS.NAME == 'EVAL_CAT' & length(THRES[[my_var]]) != length(input$thresId)) {
    RES <- RES[THRES %in% input$thresId]
  }

  # FILTROS MASK ===============================================================
  # Deve considerar com oceano somente quando se tratar dos quadrantes, caso
  # contrário deve sempre considerar "CONT"
  # ----------------------------------------------------------------------------
  #if (input$mainRegId == 9) {
  #  my_mask = input$maskId
  #} else {
  my_mask = 'CONT'
  #}
  RES <- RES[MASK==my_mask, ]

  # Verifica qual valid time de acordo com a obs
  if (input$evsetId == "PREC-MERGE") {
    valid_time="12"
  } else {
    valid_time=input$runTimeId #"00"
  }

  if (DS.NAME == 'EVAL_CONT') {
    #RES <- RES[, list(LTIME, REG, MOD, TYPE, DT, HH, VALUE, LCI95, UCI95)]
    RES <- RES[, list(LTIME, REG, MOD, TYPE, DT, HH, VALUE)]
    # Carrega e atualiza o DS de OBS correspondente
    load(paste0(EVAL.CUR, "/OBS_", gsub("-", "_", input$evsetId), ".RData"))
    .GlobalEnv$DS_OBS_DIA <- OBS[(substring(format(DT, '%Y%m%d'), 1, nchar(dateBeg)) >= dateBeg) & 
                                   (substring(format(DT, '%Y%m%d'), 1, nchar(dateEnd)) <= dateEnd) &
                                   REG %in% regions & MASK==my_mask & HH==valid_time, ]
    .GlobalEnv$DS_OBS_DIA = droplevels(.GlobalEnv$DS_OBS_DIA)
  } else {
    RES <- droplevels(RES[, list(LTIME, REG, MOD, THRES, DT, HH, HITS, FAL_AL, MISS, COR_NEG)])
  }
  setkey(RES)
  RES
}
