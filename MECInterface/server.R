#rm(list=ls())
#source('global.R')
options(shiny.reactlog=TRUE) 
function(input, output, session) {
  # ============================================================================
  # Reactive values, se for alterado qualquer procedimento que dependa dele é refeito
  # ============================================================================
  config <- reactiveValues(
    #models = rep(T, length(CFG$models)),
    DSCONT_DIA = NULL,
    DS_LTIME = NULL,
    max.ets = 0.5,
    metricId = 'BIAS',
    tabColWd = 50,
    regType = 'REG',
    imgRes = 'High',
    allInOne = F,
    tshift=0,  # time shift, according to var
    fld_x = NA, # xy position on image
    fld_y = NA
  )
  
  # ============================================================================
  # Criação dos controles 
  # ============================================================================
  # output$area1NameControl <- renderUI({
  #   #CFG$area0.names
  #   Cat(input$mainRegId, '\n')
  #   area1.name <- switch(
  #     input$mainRegId, 
  #     '12'='as sub-áreas',
  #     '3'='a região',
  #     '4'='as regiões',
  #     '5'='os países',
  #     '9'='os quadrantes'
  #   )
  #   h5(strong(paste0('Escolha ', area1.name, ' de interesse:')))
  # })
  
  # ============================================================================
  # Captura as sub regiões para mostrar no componente de UI na linha amarela
  # de acordo com a opção selecionada em input$mainRegId
  # ============================================================================
  output$subRegsControl <- renderUI({
    sub.regs <- GetSubRegs(input)
    sub.regs.names <- substring(sub.regs, ifelse(input$mainRegId==4, 7, 3))
    
    if (input$ViewMode == "MMODEL" || !input$facetReg) {
      prettyRadioButtons(
        inputId="subRegId", NULL,
        choiceNames=sub.regs.names, choiceValues=sub.regs, selected=sub.regs[1], 
        inline=T, status="danger", fill=T)
    } else {
      prettyCheckboxGroup(
        inputId="subRegId", NULL,
        choiceNames=sub.regs.names, choiceValues=sub.regs, selected=sub.regs[1],
        inline=T, status="danger", fill=T)
    }
  })
  
  #output$divTelaControl <- renderUI({
  #  checkboxInput("facetReg", label=NULL, value=F, width="100px")
  #})
  
  # ============================================================================
  # Controle dos modelos - foi para a UI
  # ============================================================================
  # output$modControl <- renderUI({
  #   mods = CFG$models
  #   if (input$pwd == "W1R2F3") {
  #       mods = CFG$models[grepl("WRF|GFS|BAM", CFG$models)]
  #   }
  #   print(mods)
  #   checkboxGroupButtons("modId", choiceNames=f.Models(mods), choiceValues=mods,
  #                        selected = mods, justified=F, individual=T, 
  #                        checkIcon = list(yes = icon("ok", class="alert-success", lib="glyphicon"),
  #                                         no = icon("remove", class="alert-dark", lib="glyphicon")))
  # })
  
  output$monthImgControl <- renderUI({
    strSearch = switch(input$aeTypeId,
                       PREVxOBS= "FCT",
                       BIAS= "BIAS",
                       SRMSE= "SRMSE",
                       SCORR= "SCORR")
    
    
    patt <- paste0("^Img_", strSearch, "_", GetVar(), "_(.*)_[0-9]{6}_(.*)_", input$runTimeId, "\\.jpeg")
    monthImgs <- dir(path=paste0(CFG$ename, '/IMG/'), pattern=patt)
    monthImgs <- sort(unique(regmatches(monthImgs, regexpr("[0-9]{6}", monthImgs))))
    #monthImgs <- gsub(pattern = paste0("Img_FCT_", GetVar(), "_"), replacement = "", monthImgs)
    #monthImgs <- gsub(pattern = paste0("(", paste0(CFG$models, collapse = "|"), ")_"), replacement = "", monthImgs)
    #monthImgs <- sort(unique(substring(monthImgs, 1, 6)), decreasing = T)
    #selectInput("monthImgId", titSty(tl("Mês")), 
    #            choices = monthImgs, selected = monthImgs[1])
    pickerInput("monthImgId", titSty(tl("Mês")),
                choices = monthImgs, selected = monthImgs[1],
                options = list(`live-search`=T)
    )
  })
  
  # Busca por arquivos de EV_SET de estações do ano que possuem nome padrão
  output$seasonImgControl <- renderUI({
    patt <- paste0("^Img_FCT_", GetVar(), "_(.*)_[0-9]{4}\\.[1-4]-(.*)_", input$runTimeId, "\\.jpeg")
    seasonImgs <- dir(path=paste0(CFG$ename, '/IMG/'), pattern=patt)
    seasonImgs <- gsub(pattern = paste0("Img_FCT_", GetVar(), "_"), replacement = "", seasonImgs)
    seasonImgs <- gsub(pattern = paste0("(", paste0(CFG$models, collapse = "|"), ")_"), replacement = "", seasonImgs)
    seasonImgs <- sort(unique(substring(seasonImgs, 1, 10)), decreasing = T)
    pickerInput("seasonImgId", titSty(tl("Estação")),
                choices = seasonImgs, selected = seasonImgs[1],
                options = list(`live-search`=T)
    )
  })
  
  # Busca por arquivos de EV_SET de estações do ano que possuem nome padrão
  output$rainyImgControl <- renderUI({
    patt <- paste0("^Img_FCT_", GetVar(), "_(.*)_[0-9]{4}\\.5-(.*)_", input$runTimeId, "\\.jpeg")
    imgs <- dir(path=paste0(CFG$ename, '/IMG/'), pattern=patt)
    imgs <- gsub(pattern = paste0("Img_FCT_", GetVar(), "_"), replacement = "", imgs)
    imgs <- gsub(pattern = paste0("(", paste0(CFG$models, collapse = "|"), ")_"), replacement = "", imgs)
    imgs <- sort(unique(substring(imgs, 1, 12)), decreasing = T)
    pickerInput("rainyImgId", titSty(tl("Chuvosa")),
                choices = imgs, selected = imgs[1],
                options = list(`live-search`=T)
    )
  })
  
  # ============================================================================
  # CONSUMO DE MEMÓRIA (ESCONDIDO AO LADO DO TÍTULO)
  # ============================================================================
  #output$txtMemory <- renderText({
  #  input$runBt
  #  input$ViewMode
  #  env=globalenv()
  #  mem2 = paste0(round(sum(unlist(lapply(ls(env), function(x) {
  #    object.size(get(x, envir=env, inherits=T))
  #  })))/1024/1024, 2), 'MB')
  #  paste0("V1=", round(pryr::mem_used()/1024/1024, 2), "MB  V2=", mem2)
  #})
  
  #output$showLtimesSlider <- reactive({ 'LTIME' %in% input$facetContId })
  #outputOptions(output, 'showLtimesSlider', suspendWhenHidden=FALSE) 
  
  
  # ============================================================================
  # OBSERVERS
  # ============================================================================
  observeEvent(input$mainRegId, {
    updateRadioButtons(session = session, inputId="regRadId", label='Regiões', 
                       choices=REGIONS$NAME, selected=input$mainRegId, inline=T)
  })
  
  observeEvent(input$showReg, {
    config$regTypes <- ifelse(input$showReg, 'REG', 'NREG')
  })
  
  # ----------------------------------------------------------------------------
  # Sets the width of the cols of the model config table
  # ----------------------------------------------------------------------------
  observeEvent(input$incTabColWd, {
    config$tabColWd <- config$tabColWd + 10
  })
  observeEvent(input$decTabColWd, {
    if (config$tabColWd >= 10) {
      config$tabColWd <- config$tabColWd - 10
    }
  })
  output$tabColWd <- renderText({config$tabColWd})
  output$tabColRGBsum <- renderText({input$tabColR+input$tabColG+input$tabColB})
  
  # ----------------------------------------------------------------------------
  # Sets the ETS range in the categorical metrics
  # ----------------------------------------------------------------------------
  observeEvent(input$incMaxEts, {
    if (config$max.ets < .9) {
      config$max.ets <- config$max.ets + 0.1
    }
  })
  observeEvent(input$decMaxEts, {
    if (config$max.ets > .2) {
      config$max.ets <- config$max.ets - 0.1
    }
  })
  output$max.ets<- renderText({config$max.ets})
  
  output$numFrames <- renderText({
    if (input$ViewMode == "Taylor") {
      num.graf=length(input$subRegId)
    } else {
      num.graf=(input$ltimes[2]-input$ltimes[1]+1) * length(input$subRegId);
    }
    paste0("(", num.graf, " ", tl("gráfico"), ifelse(num.graf==1, "", "s"),")")
  })
  
  # Parte do nome das imagens, mostra ou não os quadrantes das regioes
  ShowReg <- reactive({
    ifelse(input$showReg, 'REG', 'NREG')
  })
  # Retorna o nome do conjunto de avaliação trocando "-" por "_", para nome da img
  GetEvalSet <- reactive({
    gsub('-', '_', input$evsetId)
  })
  # Retorna a variavel que está no eval set
  GetVar <- reactive({
    unlist(strsplit(input$evsetId, '-'))[1]
  })
  # Retorna o tipo da observacao que está no eval set
  GetObs <- reactive({
    unlist(strsplit(input$evsetId, '-'))[2]
  })
  
  # Retorna as cores correspondentes aos modelos
  #CoresMod <- reactive({
  #  #input$modId <- CFG$models[c(1,3,5)]
  #  cores.mod[which(CFG$models  %in% input$modId)]
  #})
  observeEvent(input$evsetId, {
    config$tshift = ifelse(GetVar() == "PREC", 12, 0)
  })
  
  
  GetCorr <- function (data, v1='OBS', v2='AVG') {
  }
  
  # ============================================================================
  # Retorna a referência de tempo das imagens de acordo com as opções escolhidas
  # Diario | mensal | Sazonal
  # ============================================================================
  GetImgRef <- function() {
    req(input$aeTypeId)
    switch (
      input$aeTempoId,
      "D"=paste0("D", ifelse(input$dayImgId=="0", "-", ""), input$dayImgId),
      "M"=input$monthImgId,
      "S"=input$seasonImgId,
      "R"=input$rainyImgId
    )
  }
  
  # ============================================================================
  # Retorna o nome do arquivos de imagem da observação de acordo com as opções escolhidas
  # ============================================================================
  GetObsImgName <- function() {
    #hh.obs <- ifelse(GetObs()=="MERGE", "12", input$runTimeId)
    #fname <- paste0(EVAL.CUR, '/IMG/', GetEvalSet(), '_', GetImgRef(), '_', input$maskId, '_', config$imgRes, '_', ShowReg(), '_', hh.obs, '.jpeg')
    fname <- paste0(EVAL.CUR, '/IMG/', GetEvalSet(), '_', GetImgRef(), '_', input$maskId, '_', config$imgRes, '_', ShowReg(), '_', input$runTimeId, '.jpeg')
    #cat("OBS:", fname, "\n")
    fname
  }
  
  # ============================================================================
  # Retorna o nome dos arquivos de imagem de acordo com as opções escolhidas
  # ============================================================================
  GetImgName <- function(idx.mod, bestOfType="XXX") {
    fname <- ''
    aeTypeId = input$aeTypeId
    strSearch = switch(aeTypeId,  # 'PREVxOBS','BIAS',"SRMSE","SCORR"
                       PREVxOBS= "FCT",
                       BIAS= "BIAS",
                       SRMSE= "SRMSE",
                       SCORR= "SCORR")
    #print(strSearch)
    if (aeTypeId == 'PREVxOBS') {
      fname <- paste0(EVAL.CUR, "/IMG/Img_", strSearch, "_", GetVar(), '_', input$modId[idx.mod])
    } else {
      fname <- paste0(EVAL.CUR, "/IMG/Img_", strSearch, "_", GetVar(), '_', input$modId[idx.mod],'x', GetObs())
    }
    fname <- paste0(fname, '_', GetImgRef(), '_', input$maskId, '_', config$imgRes, '_', ShowReg(), '_', input$runTimeId, '.jpeg' )
    #cat(fname, "\n")
    # Quando sai do ViewMode AED ou AEM este código tb é realizado
    fname
  }
  
  # ============================================================================
  # PlotTS
  # ============================================================================
  output$PlotTS <- renderPlot({
    # Fica dependente do botão "Atualizar" somente e evita primeira execução, 
    # sem apertar o botão - ABORTADO, É INTERESSANTE TER A PRIMEIRA EXECUÇÃO
    #if (input$runBt == 0)  
    #  return()
    input$runBt
    isolate({   # Torna o código não reativo aos inputs pois o código é pesado
      req(input$subRegId, input$contId, input$ncolsContId) # Se input$xxx pode ser vazio, não usar aqui
      config$DSCONT_DIA <- f.FilterDS('EVAL_CONT', input)
      OBS <- .GlobalEnv$DS_OBS_DIA
      DS <- copy(config$DSCONT_DIA) 
      #unique(EVAL.CONT$TYPE)
      #validate(need('DS' %in% ls() && nrow(DS) > 0, F)) # Forces validation
      # Cuidado, pode ser que não haja modelos em alguns dias deste período abaixo, TRATAR na correlação
      
      # Manter DS diário para calcular a correlação e as outras métricas nele e não no agrupado
      #.GlobalEnv$DSCONT_DIA <- copy(DS)
      
      # Cria chave dos TYPES que irão para o DS (não precisa ir todos pois o DSCONT_DIA já tem todos)
      if (input$contId == 'FCT x OBS') {
        type.key <- 'AVG'
      } else {
        type.key <- input$contId
      }
      
      # Agrupa semana, mensal ou sazonalmente ====================================
      # Problema mesma semana em dois meses
      #{DS <- DSCONT_DIA[, list(VALUE=mean(VALUE)), by=list(LTIME, REG, MOD, TYPE, DT=paste0(year(DT), '-', sprintf('%02d', week(DT)), '-', month.abb[month(DT)]))]},
      
      switch(
        input$groupTsId,
        'Semana'={
          DS <- config$DSCONT_DIA[, list(VALUE=mean(VALUE)), by=list(LTIME, REG, MOD, TYPE, DT=paste0(year(DT), '-', sprintf('%02d', week(DT))), HH)]
          OBS <- .GlobalEnv$DS_OBS_DIA[, list(VALUE=mean(VALUE)), by=list(REG, TYPE, DT=paste0(year(DT), '-', sprintf('%02d', week(DT))), HH)]
        },
        'Mês'={
          if (input$appendYear) {
            DS <- config$DSCONT_DIA[, list(VALUE=mean(VALUE)), by=list(LTIME, REG, MOD, TYPE, DT=paste0(year(DT), '-', sprintf('%02d', month(DT))), HH)]
            OBS <- .GlobalEnv$DS_OBS_DIA[, list(VALUE=mean(VALUE)), by=list(REG, TYPE, DT=paste0(year(DT), '-', sprintf('%02d', month(DT))), HH)]
          } else {
            DS <- config$DSCONT_DIA[, list(VALUE=mean(VALUE)), by=list(LTIME, REG, MOD, TYPE, DT=sprintf('%02d', month(DT)), HH)]
            OBS <- .GlobalEnv$DS_OBS_DIA[, list(VALUE=mean(VALUE)), by=list(REG, TYPE, DT=sprintf('%02d', month(DT)), HH)]
          }
        },
        'Estação'={
          config$DSCONT_DIA$EST <- sapply(config$DSCONT_DIA$DT, f.Date2Season, app.year=input$appendYear)
          DS <- config$DSCONT_DIA[, list(VALUE=mean(VALUE)), by=list(LTIME, REG, MOD, TYPE, DT=EST, HH)]
          .GlobalEnv$DS_OBS_DIA$EST <- sapply(.GlobalEnv$DS_OBS_DIA$DT, f.Date2Season, app.year=input$appendYear)
          OBS <- .GlobalEnv$DS_OBS_DIA[, list(VALUE=mean(VALUE)), by=list(REG, TYPE, DT=EST, HH)]
        }
      )
      DS <- DS[TYPE %in% type.key, ]
      setkey(DS)
      OBS <- OBS[TYPE %in% type.key, ]
      setkey(OBS)
      
      # ATENÇÃO *** MÉDIA DE DADOS AGRUPADOS **** ================================
      # ATENÇÃO: media(total) != media de (MediaParcia1 + MediaParcial2 + MediaParcial3)
      # CORRETO: [(N1*MP1)+(N2*MP2)+(N3*MP3)/ / (N1+N2+N3), onde N=qtd de observacoes em cada media
      # Exemplo:
      #v <- rnorm(3000)
      #mean(v)
      #m1=mean(v[1:1000]); m2=mean(v[1001:2000]); m3=mean(v[2001:3000]); Cat(m1, m2, m3, '\n')
      #mean(m1,m2,m3); Cat((m1+m2+m3)/3, '\n')
      #((1000*m1) + (1000*m2) + (1000*m3)) / (3000)
      if (!input$contId %in% c('FCT x OBS', "SD", "VAR")) { # QUANDO FOR METRICAS DE ERRO ******************
        #p <- ggplot(DS[TYPE==input$contId], aes(x=DT, y=VALUE, color=MOD, group=MOD, linetype=HH))
        p <- ggplot(DS[TYPE==type.key], aes(x=DT, y=VALUE, fill=MOD, color=MOD, group=MOD)) +
          geom_line(size=1) + 
          geom_point(size=1, show.legend=F)
      } else {                                            # QUANDO FOR FCT X OBS ou SD ou VAR *****************
        # MODELS
        p = ggplot(DS[TYPE==type.key], aes(x=DT, y=VALUE, color=MOD, fill=MOD, group=MOD)) +
          geom_line(size=1) + 
          geom_point(size=1, show.legend=F)
        # OBSERVATION
        p = p + geom_line(data=OBS, inherit.aes=F, aes(x=DT, y=VALUE), size=1, color='black') + 
          geom_point(data=OBS, inherit.aes=F, aes(x=DT, y=VALUE), size=1, color='black')
        
        # Verifica se vai mostrar os valores da linha da observação
        if (input$showVal) {
          p = p + 
            geom_text(data=OBS, inherit.aes=F, aes(x=DT, y=VALUE, label=round(VALUE, 2)), vjust='bottom', nudge_y = .005, 
                      check_overlap=T, color='black', size=5, show.legend=F)
        }
        
      }
      # Verifica se vai mostrar os valores dos pontos no gráfico das previsões
      if ((input$showVal)) {
        p <- p + 
          geom_text(aes(label=round(VALUE, 2)), vjust='bottom', nudge_y = .005,
                    show.legend=F, check_overlap = T, size=5) 
      }
      
      # ==========================================================================
      # CONFIDENCE INTERVAL
      # ==========================================================================
      if ((input$showCI)) {
        p = p + geom_ribbon(aes(ymin=LCI95, ymax=UCI95), alpha=0.5, colour = NA)
      }
      
      p = p + 
        scale_color_manual(values=c(cores.mod[f.GetMainIdxMod(unique(DS$MOD))], "black"),
                           limits=c(as.character(unique(DS$MOD)), "OBS")) +
        scale_y_continuous(name=input$contId, expand=c(.05,.05)) #+
      
      #scale_x_date(breaks = breaks_pretty(input$numTicks), date_labels = "%d\n%b\n%Y") #function(x) seq.Date(from = min(x), 
      #  to = max(x), length.out = input$numTicks)), date_labels = "%d\n%b\n%Y")
      
      # ==========================================================================
      # TICKS
      # ==========================================================================
      if (input$groupTsId == 'Dia') {
        good <- mday(unique(DS$DT)) %% as.integer(input$ticksMult) == 0
        #which(DS[, mday(DT)] %% 15 == 0)
        if (sum(good)==0) {
          my_ticks <- rep(T, times=length(good))
        }
        my_ticks <- unique(c(min(DS$DT), unique(DS$DT)[good], max(DS$DT)))
        #if (length(my_ticks) > 4 && my_ticks[length(my_ticks)]-my_ticks[length(my_ticks)-1] < 3) {
        #  my_ticks <- c(my_ticks[1:(length(my_ticks)-2)], my_ticks[length(my_ticks)])
        #}
        my_labels <- format(my_ticks, "%d\n%m\n%y")
        p <- p +
          scale_x_date(breaks=my_ticks, labels=my_labels, expand=c(0.02,0.02))
      } else {
        switch(
          input$groupTsId,
          # ------------------------------------------------------------------------
          'Semana'= {
            good <- as.integer(substring(DS$DT, 6, 7)) %% as.integer(input$ticksMult) == 0
            my_ticks <- unique(c(min(DS$DT), DS$DT[good], max(DS$DT)))
            my_labels <- unlist(lapply(strsplit(my_ticks, '-'), function(x) paste0(x[1],'\n',x[2])))
          },
          # ------------------------------------------------------------------------
          'Mês'= {
            if (input$appendYear) {
              my_ticks <- unique(c(min(DS$DT), unique(DS$DT), max(DS$DT)))
              my_labels <- paste0(month.abb[as.numeric(substr(my_ticks, 6, 7))], '\n', substr(my_ticks, 0, 4))
            } else {
              my_ticks <- unique(DS$DT)
              my_labels <- month.abb[as.numeric(my_ticks)]
            }
          },
          # ------------------------------------------------------------------------
          'Estação'={
            my_ticks <- unique(DS$DT)
            my_labels <- gsub("-", "\n", my_ticks)
            my_labels <- gsub("a|b|c|d", "", my_labels)
            my_labels <- gsub("-", "", my_labels)
          }
        )
        #print(my_ticks)
        #print(my_labels)
        p <- p +
          scale_x_discrete(breaks=my_ticks, labels=my_labels, expand=c(0.02,0.02))
      }
      
      # TODO: Tratar todo x como data, causa muito eero e ajustes
      # TODO: Melhorar visualização das métricas
      
      #p <- p + geom_errorbar(aes(ymin=, ymax=g,color="LINE2"), width=0.1, size=.8) + 
      
      # Se for facetear
      if (!config$allInOne) {
        # Se facetear por LTIME+REG, DS.FRAME deve ser indexado por LTIME+REG
        if (length(input$ncolsContId) == 0 || as.integer(input$ncolsContId) == 0) {
          p = p + facet_wrap(LTIME ~ REG, scales = 'fixed', 
                             labeller=labeller(LTIME=function(x) {x=round(as.integer(x)+config$tshift/24,1); paste('Fct. ', x*24, 'h (', x, 'd)', sep='')}, 
                                               REG=function(x) substring(x, ifelse(input$mainRegId==4, 7, 3)), 
                                               .multi_line=F))
        } else {
          p <- p + facet_wrap(LTIME ~ REG, scales = 'fixed', ncol=as.integer(input$ncolsContId),
                              labeller=labeller(LTIME=function(x) {x=round(as.integer(x)+config$tshift/24,1); paste('Fct. ', x*24, 'h (', x, 'd)', sep='')}, 
                                                REG=function(x) substring(x, ifelse(input$mainRegId==4, 7, 3)), 
                                                .multi_line=F))
        }
      }
      
      # Calcula quantos quadros o grid de gráficos terá, para calcular o tamanho
      # da letra dos valores e separar as métricas
      nFacet <- 1
      nFacet <- nFacet * length(unique(DS$LTIME))
      nFacet <- nFacet * length(unique(DS$REG))
      dev='screen'
      
      if (input$contId == 'BIAS') {
        p <- p + geom_hline(yintercept=0, color=cor.mark, size=1, linetype=2)
      }
      
      # RETANGULOS MES A MES - ABORTADO POR ENQUANTO
      #dates <- as.data.frame(DS[, list(min(DT), max(DT)), by=month(DT)])
      #p + geom_rect(data=dates, inherit.aes = F, aes(xmin=V1, xmax=V2, ymin=-Inf, ymax=Inf, fill=month, group=month), alpha=0.4, show.legend=F)
      
      p <- p +
        theme(
          axis.text=element_text(size=12),
          axis.title=element_text(size=16, face="bold"),
          legend.text=element_text(size=14),
          strip.text = element_text(size = 16, face='bold'),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))
      
      # Retirar legenda
      p <- p + 
        theme(legend.position="none") +
        xlab(NULL)
      
      # PLOT EVENTS ON THE CHART
      if (length(find('EVENTS')) >= 1 && !config$allInOne) {
        EVENTS2 <- EVENTS[EVENTS$MOD %in% unique(DS$MOD),]
        pos.events <- NA
        switch(
          input$groupTsId,
          'Dia'={pos.events <- EVENTS2$DT+c(input$ltimes[1]:input$ltimes[2])},
          'Semana'={
            str.events <- paste0(year(EVENTS2$DT), '-', week(EVENTS2$DT+c(input$ltimes[1]:input$ltimes[2])))
            pos.events <- which(unique(DS$DT) == str.events)
          },
          'Mês'={
            if (input$appendYear) {
              str.events <- paste0(year(EVENTS2$DT+c(input$ltimes[1]:input$ltimes[2])), '-', sprintf('%02d', month(EVENTS2$DT+c(input$ltimes[1]:input$ltimes[2]))))
              pos.events <- which(unique(DS$DT) == str.events)
            }
          },
          'Estação'={pos.events <- which(unique(DS$DT)==f.Date2Season(EVENTS2$DT, input$appendYear))}
        )
        # Se não achou a posição do evento entre os tick marks, não mostra.
        if (length(pos.events) != 0 && !is.na(pos.events[1])) {
          # multiplica cada data dos eventos pela qtd de regiões pois é esta ordem
          # em que os gráficos são mostrados quando facet "LTIME ~ REG"
          pos.events <- rep(pos.events, each=length(unique(DS$REG)))
          #Cat(pos.events, '\n')
          if (!config$allInOne) {
            p <- p +
              geom_vline(data=EVENTS2, aes(xintercept=pos.events, color=MOD), size=.2, linetype=2)
          } else {
            p <- p +
              geom_vline(xintercept=pos.events, size=.2, linetype=2)
            # geom_vline(xintercept=pos.events, color=cores.mod[f.GetModIdxMain(unique(DS$MOD))], size=.2, linetype=2)
          }
        }
      }
      plot(p)
      #ggplotly(p)
    }) # isolate
    
  }, height=550) # altura da parte branca da tela
  
  
  # ============================================================================
  # Mostra a tabela com a sumarização das métricas
  # ============================================================================
  output$TabSummMetric <- renderUI({
    # Dependente de config$DSCONT_DIA, que é um reactiveValue
    #config$DSCONT_DIA <- f.FilterDS('EVAL_CONT', input)
    req(config$DSCONT_DIA)
    my_key <- expand.grid(unique(config$DSCONT_DIA$LTIME), unique(config$DSCONT_DIA$REG), unique(config$DSCONT_DIA$MOD), 
                          c("BIAS", "COR", "MAE", "RMSE")) #, "SD", "URMSE", "VAR"))
    
    # ABS() PARA BIAS?
    DS2 <- config$DSCONT_DIA[.(my_key)]
    ndays = as.integer(diff(range(DS2$DT, na.rm = T)))
    DS.FRAME = dcast.data.table(DS2, formula = LTIME+REG+MOD ~ TYPE,
                                fun.aggregate = function(x) round(mean(x), 2), value.var = 'VALUE')
    DS.FRAME = DS.FRAME[!is.na(RMSE)]
    
    #mod=unique(DS.FRAME$MOD)[1]; ltime=unique(DS.FRAME$LTIME)[1]; reg=unique(DS.FRAME$REG)[1]
    # *** Just a single fail in daily TS causes error in correlation
    # CORRELATION CALCULATION
    #print(unique(config$DSCONT_DIA$TYPE))
    for (ltime in unique(DS.FRAME$LTIME)) {
      for (reg in unique(DS.FRAME$REG)) {
        DT_COR = .GlobalEnv$DS_OBS_DIA[.(reg, "CONT", "AVG"), list(DT, OBS=VALUE)]
        setkey(DT_COR, DT)
        #print(DT_COR)
        for (mod in unique(DS2$MOD)) {
          #config$DSCONT_DIA[.(ltime, reg, mod, "AVG"), list(DT, VALUE)]  # INDEX
          #unique(config$DSCONT_DIA$TYPE)
          DT_COR$MOD = NA_real_
          
          if (!is.na(config$DSCONT_DIA[.(ltime, reg, mod, 'AVG'), VALUE][1])) {
            DT_COR[.(config$DSCONT_DIA[.(ltime, reg, mod, "AVG"), DT]),
                   MOD := config$DSCONT_DIA[.(ltime, reg, mod, 'AVG'), VALUE]]
            COR <- cor(DT_COR$OBS, DT_COR$MOD, use = 'pairwise.complete.obs')
            DS.FRAME[.(ltime, reg, mod),]$COR <- round(COR, 2)
          }
        }
      }
    }
    # Retira o prefixo do nome da região
    DS.FRAME$REG = substring(DS.FRAME$REG, ifelse(input$mainRegId==4, 7, 3))
    setkey(DS.FRAME, LTIME, REG, MOD)
    #save(DS.FRAME, file="DS.FRAME.RData")
    
    summTab = c("<html><head></head><body>")
    summTab = c(summTab, "<style> table, th, tr, td {border: 1px solid blue; } </style>")
    summTab = c(summTab, "<table style='width:50%' class='TabSummMetric'>",
                "<tr>",
                paste0("<th colspan=", ncol(DS.FRAME),">", tl("DESEMPENHO MÉDIO PREVISÃO EM"), " ", ndays, " ", tl("DIAS"), "</th>"), 
                "</tr>")
    summTab = c(summTab, "<tr background-color:", f.GetHexColor("lightskyblue1"), ">",
                paste0("<th>", names(DS.FRAME), "</th>"), "</tr>")
    
    ltimes = unique(DS.FRAME$LTIME)
    ltime=ltimes[1]
    for (ltime in ltimes) {
      DS_LTIME = DS.FRAME[LTIME==ltime]
      
      rspan.ltime = nrow(DS_LTIME)
      summTab = c(summTab, "<tr>")
      summTab = c(summTab, "<td rowspan=", rspan.ltime, ">", ltime, "</td>")
      
      regs = unique(DS_LTIME$REG)
      reg=regs[1]
      for (reg in regs) {
        DS_REG = DS_LTIME[REG==reg]
        
        rspan.reg = nrow(DS_REG)
        summTab = c(summTab, "<td rowspan=", rspan.reg, ">", reg, "</td>")
        
        # Adiciona linha que indica qual índice é o melhor de cada métrica (para ser pintado de cor verde)
        min_max = apply(DS_REG[, list(BIAS=abs(BIAS),COR,MAE,RMSE)], MARGIN = 2, range)
        min_max = rbind.data.frame(min_max, c(1,2,1,1))
        row.names(min_max) = c("MIN", "MAX", "BEST")
        
        mods = unique(DS_REG$MOD)
        mod=mods[1]
        for (mod in mods) {
          if (mod != mods[1]) {
            summTab = c(summTab, "<tr>")
          }
          summTab = c(summTab, paste0("<td style=\"color:", cores.mod[f.GetMainIdxMod(mod)],
                                      "; font-weight:bold;\">",
                                      DS_REG[MOD==mod, list(as.character(MOD))], "</td>"))
          
          # Colore as células dos valores de acordo com a performance de cada modelo: BIAS,COR,MAE,MSE,RMSE,SD,URMSE,VAR (na ordem)
          metric=colnames(min_max)[1]
          for (metric in colnames(min_max)) {
            my_bgcolor = "#FFFFFF"  # Default background color is white
            my_fgcolor = "#000000"  # Default foreground color is black
            
            # min_max vem com abs() para BIAS, devo colorir considerando abs(value) mas mostrar o valor real
            value = DS_REG[MOD==mod][[metric]]
            if (metric=="BIAS") {
              idx_mm = which(min_max[c("MIN", "MAX"), metric]==abs(value))
            } else {
              idx_mm = which(min_max[c("MIN", "MAX"), metric]==value)
            }
            if (length(idx_mm)[1] > 0) {               # Valor é igual ao mínimo ou ao máximo
              if (idx_mm[1] == min_max["BEST", metric][1]) {   # Achou o best
                my_bgcolor = "#32CD32"  # BEST (limegreen)
                my_fgcolor = "#000000"
              } else {
                my_bgcolor = "#CD5555"  # WORST (indianred3)
                my_fgcolor = "#FFFFFF"
              }
            }
            
            sty = paste0("background-color:", my_bgcolor, "; color:", my_fgcolor,";")
            summTab = c(summTab, paste0("<td style=\"", sty,"\">", value, "</td>"))
          }
        } # FOR MODS
        summTab = c(summTab, "</tr>")
      } # FOR REGS
    } # FOR LTIMES
    
    res = HTML(paste0(c(summTab, "</table></body></html>"), collapse=""))
    #writeLines(res, "TestHTML.html")
    res
  })
  
  # ============================================================================
  # Captura o DS completo para os gráficos de Métricas vs. Lead Time
  # Captura e grava num objeto reactive, ou seja, assim que é alterado as funções
  # que dele dependem são executadas
  # ============================================================================
  observeEvent(input$runBt, {
    req(input$subRegId) # Se input$xxx pode ser vazio, não usar aqui
    if (input$ViewMode == "LT") {
      # Create a Progress object
      progress <- shiny::Progress$new(min=0, max=length(input$runTimeId)*max(CFG$ltimes))
      progress$set(message = "Computing data", value=0)
      # Close the progress when this reactive exits (even if there's an error)
      on.exit(progress$close())
      
      # Create a closure to update progress.
      updateProgress <- function(value, detail = NULL) {
        progress$set(value = value, detail = detail)
      }
      #DS = RES
      DS <- f.FilterDS('EVAL_CONT', input, allLTimes=T, updateProgress=updateProgress)
      #DS <- f.FilterDS('EVAL_CONT', input, allLTimes=T)
      # FALTA COR
      #DS2 = DS[TYPE %in% c("BIAS", "RMSE"), 
      #         list(VALUE=mean(VALUE), LCI95=mean(LCI95), UCI95=mean(UCI95)), by=list(MOD, REG, LTIME, TYPE)]
      DS2 = DS[TYPE %in% c("BIAS", "RMSE"), 
               list(VALUE=mean(VALUE)), by=list(MOD, REG, LTIME, TYPE)]
      # Appends rows for COR
      #DS2 = rbind.data.frame(DS2,
      #                       expand.grid(MOD=unique(DS2$MOD), REG=unique(DS$REG), LTIME=unique(DS$LTIME),
      #                                   TYPE="COR", VALUE=NA_real_, LCI95=NA_real_, UCI95=NA_real_))
      DS2 = rbind.data.frame(DS2,
                             expand.grid(MOD=unique(DS2$MOD), REG=unique(DS$REG), LTIME=unique(DS$LTIME),
                                         TYPE="COR", VALUE=NA_real_))
      setkey(DS2, MOD, REG, LTIME, TYPE)
      
      #mod=unique(DS2$MOD)[4]; ltime=unique(DS2$LTIME)[4]; reg=unique(DS2$REG)[1]
      for (ltime in unique(DS2$LTIME)) {
        for (reg in unique(DS2$REG)) {
          DT_COR = .GlobalEnv$DS_OBS_DIA[.(reg, "CONT", "AVG"), list(DT, OBS=VALUE)]
          setkey(DT_COR, DT)
          #print(DT_COR)
          for (mod in unique(DS2$MOD)) {
            #config$DSCONT_DIA[.(ltime, reg, mod, "AVG"), list(DT, VALUE)]  # INDEX
            #unique(config$DSCONT_DIA$TYPE)
            DT_COR$MOD = NA_real_
            if (!is.na(DS[.(ltime, reg, mod, 'AVG'), VALUE][1])) {
              DT_COR[.(DS[.(ltime, reg, mod, "AVG"), DT]),
                     MOD := DS[.(ltime, reg, mod, 'AVG'), VALUE]]
              COR <- cor(DT_COR$OBS, DT_COR$MOD, use = 'pairwise.complete.obs')
              DS2[.(mod, reg, ltime, "COR")]$VALUE <- round(COR, 2)
              #ci_lims = f.CIr(COR, length(DT_COR$OBS))
              #DS2[.(mod, reg, ltime, "COR"), LCI95 := ci_lims[1]]
              #DS2[.(mod, reg, ltime, "COR"), UCI95 := ci_lims[2]]
            }
          }
        }
      }
      
      DS2 = DS2[!is.na(VALUE)]
      DS2$LTIME = as.factor(DS2$LTIME)
      #if ((input$showCI)) {
      #  DS2 = DS2[complete.cases(DS2)] // apaga LINHA QUANDO NÃO HOUVER LCI95 OU UCI9R
      #}  
      setkey(DS2, MOD, REG, LTIME, TYPE)
      
      # Creates a column with skill scores      
      #if (input$doSkillScore) {
      #input$modRefId="Eta_08km"
      
      # Skill Score = (score - score_ref) / (perf_score - score_ref)
      DS2$SS = (DS2[, ifelse(TYPE=="BIAS", abs(VALUE), VALUE)] - DS2[.(input$modRefId, DS2[, list(REG, LTIME, TYPE)]), ifelse(TYPE=="BIAS", abs(VALUE), VALUE)]) / 
        (PERF_SC[.(DS2$TYPE), VALUE] - DS2[.(input$modRefId, DS2[, list(REG, LTIME, TYPE)]), ifelse(TYPE=="BIAS", abs(VALUE), VALUE)])
      
      # Conferencia via pacote SpecsVerification      
      #DS_REF = DS2[.(input$modRefId)]
      #setkey(DS_REF, REG, LTIME, TYPE)
      #print(DS_REF)
      #
      #DS2$SS2 = 0.0
      #idx_ds2 = which(DS2$LTIME <= 6)
      #DS2[idx_ds2]
      #idx=1
      #for (idx in idx_ds2) {
      #  print(DS2[idx])
      #  sc = DS2[idx, VALUE]
      #  sc_ref = DS_REF[.(DS2[idx, list(REG, LTIME, TYPE)]), VALUE]
      #  cat("sc:", sc,  "  sc_ref:", sc_ref, "  perf_sc:", PERF_SC[.(DS2[idx, list(TYPE)]), VALUE], 
      #      "  ss:", (sc-sc_ref) / (PERF_SC[.(DS2[idx, list(TYPE)]), VALUE] - sc_ref))
      #  DS2[idx, SS2 := as.numeric(SkillScore(DS2[idx]$VALUE, sc_ref, score.perf = PERF_SC[.(DS2[idx, list(TYPE)]), VALUE])[1])]
      #}
      #print(DS2)
      
      #}
      config$DS_LTIME = DS2
    }
    
    if (input$ViewMode == "LT2") {
      
      #DS2 = EVAL.CONT.PER[EV_SET==input$evsetId & MOD %in% input$modId & 
      #                      REG %in% input$subRegId & MASK=="CONT", list(LTIME, REG, MOD, TYPE, PER, HH, VALUE)]
      #DS2$LTIME = as.factor(DS2$LTIME)
      #setkey(DS2, MOD, REG, LTIME, TYPE)
      
      DS2 = melt(EVAL.CONT.PER[EV_SET==input$evsetId & MOD %in% input$modId & REG %in% input$subRegId & MASK=="CONT"], 
                 id.vars=c("LTIME","REG","MOD"), measure.vars=c("SBIAS","SCORR","SRMSE"),
                 variable.name="TYPE", value.name="VALUE")
      
      # DS2$LCI95 = melt(EVAL.CONT.PER[EV_SET==input$evsetId & MOD %in% input$modId & REG %in% input$subRegId & MASK=="CONT"], 
      #                  id.vars=c("LTIME","REG","MOD"), measure.vars=c("LCI95_SBIAS","LCI95_SCORR","LCI95_SRMSE"),
      #                  variable.name="CI_TYPE", value.name="LCI95")[, LCI95]
      # DS2$UCI95 = melt(EVAL.CONT.PER[EV_SET==input$evsetId & MOD %in% input$modId & REG %in% input$subRegId & MASK=="CONT"], 
      #                  id.vars=c("LTIME","REG","MOD"), measure.vars=c("UCI95_SBIAS","UCI95_SCORR","UCI95_SRMSE"),
      #                  variable.name="CI_TYPE", value.name="UCI95")[, UCI95]
      
      DS2$LTIME = as.factor(DS2$LTIME)
      setkey(DS2, MOD, REG, LTIME, TYPE)
      
      # Creates a column with skill scores      
      # Skill Score = (score - score_ref) / (perf_score - score_ref)
      DS2$SS = (DS2$VALUE - DS2[.(input$modRefId, DS2[, list(REG, LTIME, TYPE)]), VALUE]) / 
        (PERF_SC[.(DS2$TYPE), VALUE] - DS2[.(input$modRefId, DS2[, list(REG, LTIME, TYPE)]), VALUE])
      config$DS_LTIME2 = DS2
    }
  })
  
  # Plots METRICS vs. LTIME ----------------------------------------------------
  output$PlotBiasVsLTime <- renderPlot({
    req(config$DS_LTIME) 
    f.PlotMetricVsLTime("BIAS")
  })
  output$PlotCorVsLTime <- renderPlot({
    req(config$DS_LTIME) 
    f.PlotMetricVsLTime("COR")
  })
  output$PlotRmseVsLTime <- renderPlot({
    req(config$DS_LTIME) 
    f.PlotMetricVsLTime("RMSE")
  })
  
  
  # Plots METRICS vs. LTIME2 ----------------------------------------------------
  output$PlotSCORRxLT2 <- renderPlot({
    req(config$DS_LTIME2) 
    f.PlotMetricVsLT2("SCORR")
  })
  output$PlotSRMSExLT2 <- renderPlot({
    req(config$DS_LTIME2) 
    f.PlotMetricVsLT2("SRMSE")
  })
  
  # Plots METRICS vs. CATEGS ---------------------------------------------------
  output$PlotMetVsCat <- renderPlot({
    f.PlotMetricVsCateg(input$catId)
  })
  
  # ============================================================================
  # Novo plot: Y=METRICA, X=LEAD TIME (estilo ETS, exemplo do IVMW-2020)
  # ============================================================================
  #metric="ME"
  f.PlotMetricVsLTime = function(metric) {
    
    if (input$doSkillScore) {
      p <- ggplot(config$DS_LTIME[TYPE==metric], aes(x=LTIME, y=SS, color=MOD, fill=MOD, group=MOD))
    } else {
      p <- ggplot(config$DS_LTIME[TYPE==metric], aes(x=LTIME, y=VALUE, color=MOD, fill=MOD, group=MOD))
    }
    
    p = p + geom_line(size=1) + geom_point(size=1, show.legend=F)
    
    # Verifica se vai mostrar os valores dos pontos no gráfico
    if (input$showVal) {
      if (input$doSkillScore) {
        p <- p +
          geom_text(aes(label=round(SS, 2)), vjust='bottom', nudge_y = .005,
                    show.legend=F, check_overlap=T, size=5)
      } else {
        p <- p +
          geom_text(aes(label=round(VALUE, 2)), vjust='bottom', nudge_y = .005,
                    show.legend=F, check_overlap=T, size=5)
      }
    }
    
    # ==========================================================================
    # CONFIDENCE INTERVAL
    # ==========================================================================
    if ((input$showCI)) {
      p = p + geom_ribbon(aes(ymin=LCI95, ymax=UCI95), alpha=0.5, colour = NA)
    }
    
    p = p +
      scale_color_manual(values=cores.mod[f.GetMainIdxMod(unique(config$DS_LTIME$MOD))]) +
      scale_y_continuous(name=tl(metric), expand=c(.05,.05)) 
    
    # Faceteia automaticamente
    if (length(input$ncolsContId) == 0 || as.integer(input$ncolsContId) == 0) {
      p = p + facet_wrap(~REG, scales = 'fixed',
                         labeller=labeller(REG=function(x) substring(x, ifelse(input$mainRegId==4, 7, 3)),
                                           .multi_line=F))
    } else {
      p = p + facet_wrap(~REG, scales = 'fixed', ncol=as.integer(input$ncolsContId),
                         labeller=labeller(REG=function(x) substring(x, ifelse(input$mainRegId==4, 7, 3)),
                                           .multi_line=F))
    }
    
    # Includes 0 hline in ME frame
    if (metric == "BIAS") {
      p = p + geom_hline(aes(yintercept=0), color=cor.mark, size=1, linetype=2)
    }
    
    p = p +
      theme(
        axis.text=element_text(size=12),
        axis.title=element_text(size=16, face="bold"),
        legend.text=element_text(size=14),
        strip.text = element_text(size = 16, face='bold'),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))
    
    # Retirar legenda
    p <- p +
      theme(legend.position="none") +
      xlab(tl("Dias previsão"))
    p
  }
  
  
  # ============================================================================
  # Mostra a tabela com a sumarização das métricas por ltime
  # ============================================================================
  output$TabSummLTime <- renderUI({
    # Dependente de config$DSCONT_DIA, que é um reactiveValue
    #config$DSCONT_DIA <- f.FilterDS('EVAL_CONT', input)
    req(config$DS_LTIME)
    #my_key <- expand.grid(unique(config$DSCONT_DIA$LTIME), unique(config$DSCONT_DIA$REG), unique(config$DSCONT_DIA$MOD), 
    #                      c("ME", "COR", "RMSE", "URMSE*"))
    my_ltimes = unique(config$DS_LTIME$LTIME)
    my_key <- expand.grid(unique(config$DS_LTIME$MOD), unique(config$DS_LTIME$REG), 
                          my_ltimes, c("BIAS", "COR", "RMSE"))
    DS2 <- config$DS_LTIME[.(my_key)]
    DS.FRAME = dcast.data.table(DS2, formula = REG+TYPE+MOD~LTIME,
                                fun.aggregate = function(x) round(mean(x), 2), value.var = 'VALUE')
    
    # MÁXIMOS E MÍNIMOS POR REG + TYPE + LTIME (PARA COLORIR)
    DS.MIN = dcast.data.table(DS2, formula = REG+TYPE~LTIME, fun.aggregate = function(x) round(min(abs(x), na.rm=T), 2), value.var = 'VALUE')
    DS.MAX = dcast.data.table(DS2, formula = REG+TYPE~LTIME, fun.aggregate = function(x) round(max(abs(x), na.rm=T), 2), value.var = 'VALUE')
    
    # Retira o prefixo do nome da região
    DS.MAX$REG = substring(DS.MAX$REG, ifelse(input$mainRegId==4, 7, 3))
    DS.MIN$REG = substring(DS.MIN$REG, ifelse(input$mainRegId==4, 7, 3))
    DS.FRAME$REG = substring(DS.FRAME$REG, ifelse(input$mainRegId==4, 7, 3))
    setkey(DS.MIN, REG, TYPE)
    setkey(DS.MAX, REG, TYPE)
    setkey(DS.FRAME, REG, TYPE, MOD)
    
    summTab = c("<html><head></head><body>")
    summTab = c(summTab, "<table style='width:100%;' class='TabSummMetric'>",
                "<tr>",
                paste0("<th colspan=", ncol(DS.FRAME),">", tl("DESEMPENHO POR DIA DE PREVISÃO"), "</th>"),
                "</tr>")
    summTab = c(summTab, "<tr background-color:", f.GetHexColor("lightskyblue1"), ">",
                paste0("<th>", names(DS.FRAME), "</th>"), "</tr>")
    idx.metric=idx.reg=idx.mod=1
    regs = unique(DS.FRAME$REG)
    metrics = unique(DS.FRAME$TYPE)
    mods = unique(DS.FRAME$MOD)
    for (idx.reg in 1:length(regs)) {
      rspan.reg = DS.FRAME[.(regs[idx.reg]), .N]
      summTab = c(summTab, "<tr>")
      summTab = c(summTab, "<td rowspan=", rspan.reg, ">", gsub(" +", "\n", regs[idx.reg]), "</td>")
      for (idx.metric in 1:length(metrics)) {
        rspan.metric   = DS.FRAME[.(regs[idx.reg], metrics[idx.metric]), .N]
        metric = metrics[idx.metric]
        summTab = c(summTab, "<td rowspan=", rspan.metric, ">", as.character(metrics[idx.metric]), "</td>")
        
        for (idx.mod in 1:length(mods)) {
          summTab = c(summTab, paste0("<td style=\"color:", cores.mod[f.GetMainIdxMod(mods[idx.mod])],
                                      "; font-weight:bold;\">", as.character(mods[idx.mod]), "</td>"))
          ltime=1
          for (ltime in as.integer(my_ltimes)) {
            my_bgcolor = "#FFFFFF"  # Default background color is white
            my_fgcolor = "#000000"  # Default foreground color is black
            
            # min_max vem com abs() para BIAS, devo colorir considerando abs(value) mas mostrar o valor real
            value = DS.FRAME[.(regs[idx.reg], metrics[idx.metric], mods[idx.mod])][[ltime+3]]
            if (!is.na(value)) {
              value_abs = abs(value)
              val_min = DS.MIN[.(regs[idx.reg], metrics[idx.metric])][[ltime+2]]
              val_max = DS.MAX[.(regs[idx.reg], metrics[idx.metric])][[ltime+2]]
              
              # Quanto menor melhor
              if (metric %in% c("BIAS", "RMSE")) {
                if (value_abs == val_max) {
                  my_bgcolor = "#CD5555"  # WORST (indianred3)
                  my_fgcolor = "#FFFFFF"
                }
                if (value_abs == val_min) {
                  my_bgcolor = "#32CD32"  # BEST (limegreen)
                  my_fgcolor = "#000000"
                }
              }
              
              # Quanto maior melhor
              if (metric=="COR") {
                if (value_abs == val_max) {
                  my_bgcolor = "#32CD32"  # BEST (limegreen)
                  my_fgcolor = "#000000"
                }
                if (value_abs == val_min) {
                  my_bgcolor = "#CD5555"  # WORST (indianred3)
                  my_fgcolor = "#FFFFFF"
                }
              }
            }
            sty = paste0("background-color:", my_bgcolor, "; color:", my_fgcolor,";")
            summTab = c(summTab, paste0("<td style=\"", sty,"\">", ifelse(is.na(value),"",value), "</td>"))
            
          }
          #summTab = c(summTab, paste0("<td>", sapply(DS.FRAME[.(regs[idx.reg], metrics[idx.metric], mods[idx.mod]), (1:length(my_ltimes))+3, with=F], 
          #                                           function(x) ifelse(is.na(x), "", x)), "</td>"))
          summTab = c(summTab, "</tr>")
        }
      }
    }
    
    res = HTML(paste0(c(summTab, "</table></body></html>"), collapse=""))
    #writeLines(res, "TestHTML.html")
    res
  })
  
  # ============================================================================
  # Mostra o help das avaliações por lead time (só o skill score)
  # ============================================================================
  output$TabHelpLTime <- renderUI({
    html = c("<html><head></head><body><table style=\"color:#000000; background-color:#FFFFFF;\">")
    if (input$doSkillScore) {
      metric="SS"
      hlp = f.GetHelp(metric)
      
      html = c("<html><head></head><body><table style=\"color:#000000; background-color:#FFFFFF;\">")
      
      html = c(html, "<tr><td><h4 style=\"color:#0000FF\">", hlp[[metric]][["Names"]], "</h4></td><td rowspan=3 style=\"text-align:left;background-color:#FFFFFF;\">", 
               "<img src=\"", metric, ".png\"></td></tr>")
      
      help_key = "Answers"
      html = c(html, paste0("<tr><td style=\"background-color:#FFFFFF;\"><strong>", help_key, ":</strong>", hlp[[metric]][[help_key]], "</td></tr>"))
      
      help_key = "Range"
      html = c(html, paste0("<tr><td style=\"background-color:#FFFFFF;\"><strong>", help_key, ":</strong>", hlp[[metric]][[help_key]], "</td></tr>"))
      
      help_key = "Characteristics"
      html = c(html, paste0("<tr><td style=\"background-color:#FFFFFF;\"><strong>", help_key, ":</strong>", hlp[[metric]][[help_key]], "</td></tr>"))
      
    } else {
      metrics = c("BIAS", "RMSE", "COR")
      html = c(html, paste0("<tr>", paste0("<td width=\"33.33%\" style=\"text-align:center;background-color:#FFFFFF;\"><img src=\"", as.character(sapply(sapply(metrics, f.GetHelp), "[", "Img")), "\"></td>", collapse = ""), "</tr>"))
      
      help_key="Names"
      html = c(html, paste0("<tr>", paste0("<td><h4 style=\"color:#0000FF;\">", as.character(sapply(sapply(metrics, f.GetHelp), "[", help_key)), "</h4></td>", collapse = ""), "</tr>"))
      help_key="Answers"
      html = c(html, paste0("<tr>", paste0("<td><strong>", help_key,": </strong>", sapply(sapply(metrics, f.GetHelp), "[", help_key), "</td>", collapse = ""), "</tr>"))
      help_key="Range"
      html = c(html, paste0("<tr>", paste0("<td><strong>", help_key,": </strong>", sapply(sapply(metrics, f.GetHelp), "[", help_key), "</td>", collapse = ""), "</tr>"))
      help_key="Characteristics"
      html = c(html, paste0("<tr>", paste0("<td><strong>", help_key,": </strong>", sapply(sapply(metrics, f.GetHelp), "[", help_key), "</td>", collapse = ""), "</tr>"))
    }
    html = c(html, "</table>")
    html = c(html, "<br><p style=\"color:#000000; background-color:#FFFFFF;\"><strong style=\"color:#0000FF\">Source:</strong> WWRP/WGNE Joint Working Group on Forecast Verification Research, online at <a href=\"https://www.cawcr.gov.au/projects/verification/\">https://www.cawcr.gov.au/projects/verification/</a></p>")
    res = HTML(paste0(c(html, "</body></html>"), collapse=""))
    res
    #writeLines(res, "HelpLTime.html")
  })
  
  # ============================================================================
  # Novo plot: Y=METRICA CONSOLIDADA, X=LEAD TIME (estilo ETS, exemplo do IVMW-2020)
  # ============================================================================
  #metric="SCORR"
  f.PlotMetricVsLT2 = function(metric) {
    if (input$doSkillScore) {
      p <- ggplot(config$DS_LTIME2[TYPE==metric], aes(x=LTIME, y=SS, color=MOD, fill=MOD, group=MOD))
    } else {
      p <- ggplot(config$DS_LTIME2[TYPE==metric], aes(x=LTIME, y=VALUE, color=MOD, fill=MOD, group=MOD))
    }
    
    p = p + geom_line(size=1) + geom_point(size=1, show.legend=F)
    
    # Verifica se vai mostrar os valores dos pontos no gráfico
    if (input$showVal) {
      if (input$doSkillScore) {
        p <- p +
          geom_text(aes(label=round(SS, 2)), vjust='bottom', nudge_y = .005,
                    show.legend=F, check_overlap=T, size=5)
      } else {
        p <- p +
          geom_text(aes(label=round(VALUE, 2)), vjust='bottom', nudge_y = .005,
                    show.legend=F, check_overlap=T, size=5)
      }
    }
    
    # ==========================================================================
    # CONFIDENCE INTERVAL
    # ==========================================================================
    if ((input$showCI)) {
      p = p + geom_ribbon(aes(ymin=LCI95, ymax=UCI95), alpha=0.5, colour = NA)
    }
    
    p = p +
      scale_color_manual(values=cores.mod[f.GetMainIdxMod(unique(config$DS_LTIME2$MOD))]) +
      scale_y_continuous(name=tl(metric), expand=c(.05,.05)) 
    
    # Faceteia automaticamente
    if (length(input$ncolsContId) == 0 || as.integer(input$ncolsContId) == 0) {
      p = p + facet_wrap(~REG, scales = 'fixed',
                         labeller=labeller(REG=function(x) substring(x, ifelse(input$mainRegId==4, 7, 3)),
                                           .multi_line=F))
    } else {
      p = p + facet_wrap(~REG, scales = 'fixed', ncol=as.integer(input$ncolsContId),
                         labeller=labeller(REG=function(x) substring(x, ifelse(input$mainRegId==4, 7, 3)),
                                           .multi_line=F))
    }
    
    p = p +
      theme(
        axis.text=element_text(size=12),
        axis.title=element_text(size=16, face="bold"),
        legend.text=element_text(size=14),
        strip.text = element_text(size = 16, face='bold'),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))
    
    # Retirar legenda
    p <- p +
      theme(legend.position="none") +
      xlab(tl("Dias previsão"))
    p
  }
  
  # ============================================================================
  # Mostra a tabela com a sumarização das métricas por ltime
  # ============================================================================
  output$TabSummLT2 <- renderUI({
    
    req(config$DS_LTIME2)
    #my_key <- expand.grid(unique(config$DSCONT_DIA$LTIME), unique(config$DSCONT_DIA$REG), unique(config$DSCONT_DIA$MOD), 
    #                      c("ME", "COR", "RMSE", "URMSE*"))
    my_ltimes = unique(config$DS_LTIME2$LTIME)
    my_key <- expand.grid(unique(config$DS_LTIME2$MOD), unique(config$DS_LTIME2$REG), 
                          my_ltimes, c("SCORR", "SRMSE"))
    DS2 <- config$DS_LTIME2[.(my_key)]
    DS.FRAME = dcast.data.table(DS2, formula = REG+TYPE+MOD~LTIME,
                                fun.aggregate = function(x) round(mean(x), 2), value.var = 'VALUE')
    
    # MÁXIMOS E MÍNIMOS POR REG + TYPE + LTIME (PARA COLORIR)
    DS.MIN = dcast.data.table(DS2, formula = REG+TYPE~LTIME, fun.aggregate = function(x) round(min(abs(x), na.rm=T), 2), value.var = 'VALUE')
    DS.MAX = dcast.data.table(DS2, formula = REG+TYPE~LTIME, fun.aggregate = function(x) round(max(abs(x), na.rm=T), 2), value.var = 'VALUE')
    
    # Retira o prefixo do nome da região
    DS.MAX$REG = substring(DS.MAX$REG, ifelse(input$mainRegId==4, 7, 3))
    DS.MIN$REG = substring(DS.MIN$REG, ifelse(input$mainRegId==4, 7, 3))
    DS.FRAME$REG = substring(DS.FRAME$REG, ifelse(input$mainRegId==4, 7, 3))
    setkey(DS.MIN, REG, TYPE)
    setkey(DS.MAX, REG, TYPE)
    setkey(DS.FRAME, REG, TYPE, MOD)
    
    summTab = c("<html><head></head><body>")
    summTab = c(summTab, "<table style='width:100%;' class='TabSummMetric'>",
                "<tr>",
                paste0("<th colspan=", ncol(DS.FRAME),">", tl("DESEMPENHO POR DIA DE PREVISÃO"), "</th>"),
                "</tr>")
    summTab = c(summTab, "<tr background-color:", f.GetHexColor("lightskyblue1"), ">",
                paste0("<th>", names(DS.FRAME), "</th>"), "</tr>")
    idx.metric=idx.reg=idx.mod=1
    regs = unique(DS.FRAME$REG)
    metrics = unique(DS.FRAME$TYPE)
    mods = unique(DS.FRAME$MOD)
    for (idx.reg in 1:length(regs)) {
      rspan.reg = DS.FRAME[.(regs[idx.reg]), .N]
      summTab = c(summTab, "<tr>")
      summTab = c(summTab, "<td rowspan=", rspan.reg, ">", gsub(" +", "\n", regs[idx.reg]), "</td>")
      for (idx.metric in 1:length(metrics)) {
        rspan.metric   = DS.FRAME[.(regs[idx.reg], metrics[idx.metric]), .N]
        metric = metrics[idx.metric]
        summTab = c(summTab, "<td rowspan=", rspan.metric, ">", as.character(metrics[idx.metric]), "</td>")
        for (idx.mod in 1:length(mods)) {
          summTab = c(summTab, paste0("<td style=\"color:", cores.mod[f.GetMainIdxMod(mods[idx.mod])],
                                      "; font-weight:bold;\">", as.character(mods[idx.mod]), "</td>"))
          ltime=1
          for (ltime in as.integer(my_ltimes)) {
            my_bgcolor = "#FFFFFF"  # Default background color is white
            my_fgcolor = "#000000"  # Default foreground color is black
            
            # min_max vem com abs() para BIAS, devo colorir considerando abs(value) mas mostrar o valor real
            value = DS.FRAME[.(regs[idx.reg], metrics[idx.metric], mods[idx.mod])][[ltime+3]]
            if (!is.na(value)) {
              value_abs = abs(value)
              val_min = DS.MIN[.(regs[idx.reg], metrics[idx.metric])][[ltime+2]]
              val_max = DS.MAX[.(regs[idx.reg], metrics[idx.metric])][[ltime+2]]
              
              # Quanto menor melhor
              if (metric %in% c("SBIAS", "SRMSE")) {
                if (value_abs == val_max) {
                  my_bgcolor = "#CD5555"  # WORST (indianred3)
                  my_fgcolor = "#FFFFFF"
                }
                if (value_abs == val_min) {
                  my_bgcolor = "#32CD32"  # BEST (limegreen)
                  my_fgcolor = "#000000"
                }
              }
              
              # Quanto maior melhor
              if (metric=="SCORR") {
                if (value_abs == val_max) {
                  my_bgcolor = "#32CD32"  # BEST (limegreen)
                  my_fgcolor = "#000000"
                }
                if (value_abs == val_min) {
                  my_bgcolor = "#CD5555"  # WORST (indianred3)
                  my_fgcolor = "#FFFFFF"
                }
              }
            }
            sty = paste0("background-color:", my_bgcolor, "; color:", my_fgcolor,";")
            summTab = c(summTab, paste0("<td style=\"", sty,"\">", ifelse(is.na(value),"",value), "</td>"))
            
          }
          #summTab = c(summTab, paste0("<td>", sapply(DS.FRAME[.(regs[idx.reg], metrics[idx.metric], mods[idx.mod]), (1:length(my_ltimes))+3, with=F], 
          #                                           function(x) ifelse(is.na(x), "", x)), "</td>"))
          summTab = c(summTab, "</tr>")
        }
      }
    }
    
    res = HTML(paste0(c(summTab, "</table></body></html>"), collapse=""))
    #writeLines(res, "TestHTML.html")
    res
  })
  
  # ============================================================================
  # DIAGRAMA DE TAYLOR
  # ============================================================================
  output$PlotTaylorDiag <- renderPlot({
    # Fica dependente do botão "Atualizar" somente e evita primeira execução, 
    # sem apertar o botão - ABORTADO, É INTERESSANTE TER A PRIMEIRA EXECUÇÃO
    if (input$runBt == 0)  
      return()
    input$runBt # Fica dependente do botão "Atualizar"
    isolate({   # Torna o código não reativo aos inputs pois o código é pesado
      req(input$subRegId) # Se input$xxx pode ser vazio, não usar aqui
      DS <- f.FilterDS('EVAL_CONT', input)
      DS <- DS[TYPE=='AVG'] # Só FCT AVG interessa
      DS = merge.data.table(DS[, list(LTIME, REG, MOD, DT, FCT=VALUE)], 
                            .GlobalEnv$DS_OBS_DIA[TYPE=='AVG', list(REG, DT, OBS=VALUE)], 
                            by=c("REG", "DT"))[, list(LTIME, REG, MOD, DT, FCT, OBS)]
      DS = DS[complete.cases(DS)]
      setkey(DS, LTIME, REG, MOD, DT)
      
      validate(need('DS' %in% ls() && nrow(DS) > 0, F)) # Forces validation
      if ('taylor' %in% ls()) {
        rm(taylor) 
      }
      
      # ============================================================================
      # Captura todos os viéses das distribuições (de cada ponto) antes de plotar
      # Para poder montar a barra de bias
      # ============================================================================
      bFCT <- DS[, mean(FCT), by=list(LTIME, REG, MOD)]$V1
      bOBS <- DS[, mean(OBS), by=list(LTIME, REG, MOD)]$V1
      biases <- pretty(bFCT - bOBS)
      regions <- unique(DS$REG)  #f.GetRegions(input)
      
      # # Calcula qual a divisão de tela para oa Taylors
      # v1 <- list(  1   ,   2   ,   3   ,   4   , c(5,6), c(7,8,9))
      # v2 <- list(c(1,1), c(1,2), c(1,3), c(2,2), c(2,3), c(3,3))
      # idx.grid <- grep(length(regions), v1)
      # if (idx.grid != 0) {
      #   grid <- unlist(v2[idx.grid])
      # } else {
      #   grid <- c(3, ceiling(length(regions)/3))
      # }
      
      # Calcula qual a divisão de tela para oa Taylors
      # V2 = linhas com 3 gráficos
      grid <- c(ceiling(length(regions)/4), 4)
      
      # Configura a área gráfica
      my_mar <- length(unique(DS$MOD))+5
      par(mfrow=grid, pty='s', xpd=T, mar=c(my_mar,4,2,1), oma=c(0,0,0,0))
      
      reg=regions[1]
      for (reg in regions) {
        idx.mod=1;idx.ltime=1
        if ('taylor' %in% ls()) {
          rm(taylor) 
        }
        
        for (idx.mod in 1:length(unique(DS$MOD))) {
          mod <- unique(DS$MOD)[idx.mod]
          for (idx.ltime in 1:length(CFG$ltimes)) {
            ltime <- unique(DS$LTIME)[idx.ltime]
            
            vFCT <- DS[.(ltime, reg, mod), FCT]
            vOBS <- DS[.(ltime, reg, mod), OBS]
            
            #par(mfrow=c(1,1))
            if (!'taylor' %in% ls()) {
              taylor <- TaylorDiag(
                ref = vOBS, model = vFCT, 
                mod=mod, mod.idx.input=idx.mod, pch=as.character(ltime), asp=1,
                main = f.GetRegName(input, reg),
                axis.pt=input$taylorTextSize,
                bias.scale = biases,      # Numeros da escala do bias
                models=unique(DS$MOD))
              #legend(x=1.9, y=1.75, legend=unique(DS$MOD), pch=19, col=cores.mod, xpd=T)
              # Mostra o titulo após a última pos
              #          bias.x <- -.4; axes.cex=1.1#; bias.col <- colors()[36];
              pos.y <- bias.y-(length(unique(DS$MOD))*.04/2)
              #text(x=-0.02, y=pos.y, labels='Bias', cex=axes.cex+.2, col=bias.col, xpd=T, pos=1, srt=90)
              pos.y <- bias.y-.02-(length(unique(DS$MOD))*.04)-.05
              pos.x <- f.Scale(0, biases)
              lines(x=c(pos.x,pos.x), y=c(pos.y,bias.y), col=cor.mark) # linha do 0
            } else {
              TaylorDiag(vOBS, vFCT, normalize=T, mod=mod, mod.idx.input=idx.mod, pch=as.character(ltime), 
                         add=T, axis.pt=input$taylorTextSize, bias.scale=biases, models=unique(DS$MOD))
              # normalize=T; col=cores.mod[idx.mod]; pch=as.character(as.numeric(ltime)/24); add=T; pcex=1.5; bias.value=bias.value
            }
          } # LTIMES
        } # MODELS
      } # regions
    }) # isolate
  }) #, height=600) # altura da parte branca da tela, depende do num de linhas do grid
  
  
  # ============================================================================
  # Captura o DS completo para as metricas categóricas
  # Captura e grava num objeto reactive, ou seja, assim que é alterado as funções
  # que dele dependem são executadas
  # ============================================================================
  DSCat <- reactive({
    if (input$ViewMode == "Perf" || input$ViewMode == "Categ") {
      req(input$subRegId, input$catId, input$ncolsContId) # Se input$xxx pode ser vazio, não usar aqui
      # Create a Progress object
      
      RES <- f.FilterDS('EVAL_CAT', input)[, list(HITS=sum(HITS), FAL_AL=sum(FAL_AL), 
                                                  MISS=sum(MISS), COR_NEG=sum(COR_NEG)), 
                                           by=list(MOD, REG, LTIME, THRES)]
      
      RES$LTIME = as.factor(RES$LTIME)
      setkey(RES, MOD, REG, LTIME, THRES)
      RES
    }
  })
  
  
  # ============================================================================
  # Novo plot: Y=METRICA CAT, X=CATEGORIAS DA VARIAVEL
  # ============================================================================
  #metric="FBIAS"
  f.PlotMetricVsCateg = function(metric) {
    #DS <- f.FilterDS('EVAL_CAT', input, allLTimes=F, updateProgress=updateProgress)
    #DS <- f.FilterDS('EVAL_CAT', input)
    # Soma as TCs
    #DS = DS[, list(HITS=sum(HITS), FAL_AL=sum(FAL_AL), MISS=sum(MISS), COR_NEG=sum(COR_NEG)), 
    #                      by=list(MOD, REG, LTIME, THRES)]
    input$runBt
    isolate({   # Torna o código não reativo aos inputs pois o código é pesado
      
      if (input$ViewMode == "Categ") {
        DS = DSCat()
        # ETS (Equitable Threat Score) or GSS (Gilbert Skill Score) --------------------
        # -1/3:1 (no skill=0, perfect=1)
        if (metric == "ETS") {
          DS[, HRAND := (as.numeric(HITS+MISS)*as.numeric(HITS+FAL_AL)) / as.numeric(HITS+FAL_AL+MISS+COR_NEG), # Hits random
             by=list(MOD,LTIME,REG,THRES)]
          DS[, SCORE := as.numeric(HITS-HRAND) / as.numeric(HITS+FAL_AL+MISS-HRAND), 
             by=list(MOD,LTIME,REG,THRES)]
        }
        # ACCURACY (FRACTION|PERCENT CORRECT) --------------------------------------------------
        # 0:1 (perfect=1)
        if (metric == "PC") {
          DS[, SCORE := as.numeric(HITS+COR_NEG)/as.numeric(HITS+FAL_AL+MISS+COR_NEG), 
             by=list(MOD,LTIME,REG,THRES)]
        }
        
        # BIAS SCORE (FREQUENCY BIAS) --------------------------------------------------
        # 0:INF (perfect=1)
        if (metric == "FBIAS") {
          DS[, SCORE := as.numeric(HITS + FAL_AL) / as.numeric(HITS + MISS), 
             by=list(MOD,LTIME,REG,THRES)]
        }
        
        # POD (Prob of Detection) or HR (Hit Rate) or H --------------------------------
        # 0:1 (perfect=1)
        if (metric == "POD") {
          DS[, SCORE := HITS / as.numeric(HITS + MISS), 
             by=list(MOD,LTIME,REG,THRES)]
        }
        
        # FAR (False Alarme Ratio) -----------------------------------------------------
        # 0:1 (perfect=0)
        if (metric == "FAR") {
          DS[, SCORE := FAL_AL / as.numeric(HITS + FAL_AL), 
             by=list(MOD,LTIME,REG,THRES)]
        }
        
        # POFD (Prob of False Detection) or false alarme rate or F ---------------------
        # 0:1 (perfect=0)
        if (metric == "POFD") {
          DS[, SCORE := FAL_AL / as.numeric(COR_NEG + FAL_AL), 
             by=list(MOD,LTIME,REG,THRES)]
        }
        
        # SR (Success Ratio) -----------------------------------------------------------
        # 0:1 (perfect=1)
        if (metric == "SR") {
          DS[, SCORE := HITS / as.numeric(HITS + FAL_AL), 
             by=list(MOD,LTIME,REG,THRES)]
        }
        
        # TS (Threat Score) or CSI (Critical Success Index) ----------------------------
        # 0:1 (no skill=0, perfect=1)
        if (metric == "TS") {
          DS[, SCORE := HITS / as.numeric(HITS + MISS + FAL_AL), 
             by=list(MOD,LTIME,REG,THRES)]
        }
        
        # HK (Hanssen and Kuipers discriminant) or TSS (True Skill Statistics) or ------
        # PSS (Peirce's Skill Score)
        # -1:1 (no skill=0, perfect=1)
        if (metric == "HK") {
          DS[, SCORE := (HITS/as.numeric(HITS+MISS)) - (FAL_AL / as.numeric(FAL_AL+COR_NEG)), 
             by=list(MOD,LTIME,REG,THRES)]
        }
        
        # HSS (Heidke Skill Score) -----------------------------------------------------
        # -1:1 (no skill=0, perfect=1)
        # Expected correct random
        if (metric == "HSS") {
          DS[, SCORE := 1/as.numeric(HITS+FAL_AL+MISS+COR_NEG) * ((as.numeric(HITS+MISS)*as.numeric(HITS+FAL_AL)) + 
                                                                    (as.numeric(COR_NEG+MISS)*as.numeric(COR_NEG+FAL_AL))), 
             by=list(MOD,LTIME,REG,THRES)]
          
          DS[, SCORE := as.numeric(HITS + COR_NEG - ExpCorRand) / (as.numeric(HITS+FAL_AL+MISS+COR_NEG) - ExpCorRand), 
             by=list(MOD,LTIME,REG,THRES)]
        }
        
        #DS$x = match(DS$THRES, THRES[[GetVar()]])
        # Eixo X deve ser equidistante 
        DS$x = match(DS$THRES, input$thresId)
        p = ggplot(DS, aes(x=x, y=SCORE, color=MOD, group=MOD)) +
          geom_line(size=1) + geom_point(size=1, show.legend=F) +
          scale_x_discrete(limits=input$thresId)
        #scale_x_discrete(limits=as.factor(THRES[[GetVar()]]))
        
        # Verifica se vai mostrar os valores dos pontos no gráfico
        if (input$showVal) {
          p <- p +
            geom_text(aes(label=round(SCORE, 2)), vjust='bottom', nudge_y = .005,
                      show.legend=F, check_overlap=T, size=5)
        }
        
        p = p +
          scale_color_manual(values=cores.mod[f.GetMainIdxMod(unique(DS$MOD))]) +
          scale_y_continuous(name=metric, expand=c(.05,.05)) 
        
        # Faceteia automaticamente
        if (length(input$ncolsContId) == 0 || as.integer(input$ncolsContId) == 0) {
          nCols = NULL
        } else {
          nCols = as.integer(input$ncolsContId)
        }
        p = p + facet_wrap(~REG+LTIME, scales = 'fixed', ncol=nCols,
                           labeller=labeller(REG=function(x) substring(x, ifelse(input$mainRegId==4, 7, 3)),
                                             .multi_line=F))
        p = p +
          xlab(tl("Limiares")) + ylab(metric) +
          theme(
            axis.text=element_text(size=12),
            axis.title=element_text(size=16, face="bold"),
            legend.text=element_text(size=14),
            strip.text = element_text(size = 16, face='bold'),
            axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
            legend.position="none"
          )
        p
      } # input$ViewMode == "Categ"
    }) # isolate
  }
  
  # ============================================================================
  # Mostra o help das avaliações categóricas, de acordo com a métrica escolhida
  # ============================================================================
  output$TabHelpCat <- renderUI({
    metric=input$catId
    #metric="SS"
    hlp = f.GetHelp(metric)
    
    html = c("<html><head></head><body style=\"color:#000000; background-color:#FFFFFF;\"><table style=\"color:#000000; background-color:#FFFFFF;\">")
    
    html = c(html, "<tr><td><h2 style=\"color:#0000FF\">", hlp[[metric]][["Names"]], "</h2></td><td rowspan=3 style=\"text-align:left;background-color:#FFFFFF;\">", 
             "<img src=\"", metric, ".png\"></td></tr>")
    
    help_key = "Answers"
    html = c(html, paste0("<tr><td style=\"background-color:#FFFFFF;\" colspan=2><strong>", help_key, ": </strong>", hlp[[metric]][[help_key]], "</td></tr>"))
    
    help_key = "Range"
    html = c(html, paste0("<tr><td style=\"background-color:#FFFFFF;\" colspan=2><strong>", help_key, ": </strong>", hlp[[metric]][[help_key]], "</td></tr>"))
    
    help_key = "Characteristics"
    html = c(html, paste0("<tr><td style=\"background-color:#FFFFFF;\" colspan=2><strong>", help_key, ": </strong>", hlp[[metric]][[help_key]], "</td></tr>"))
    
    html = c(html, "</table>")
    html = c(html, "<br><p style=\"color:#000000; background-color:#FFFFFF;\"><strong style=\"color:#0000FF\">Source:</strong> WWRP/WGNE Joint Working Group on Forecast Verification Research, online at <a href=\"https://www.cawcr.gov.au/projects/verification/\">https://www.cawcr.gov.au/projects/verification/</a></p>")
    res = HTML(paste0(c(html, "</body></html>"), collapse=""))
    #writeLines(res, "HelpCat.html")
    res
  })
  
  
  # ============================================================================
  # DIAGRAMA DE PERFORMANCE
  # ============================================================================
  output$PlotPerfDiag <- renderPlot({
    input$runBt
    isolate({   # Torna o código não reativo aos inputs pois o código é pesado
      req(input$subRegId, input$ncolsContId) # Se input$xxx pode ser vazio, não usar aqui
      
      DS <- DSCat()
      DS[, POD := HITS / (HITS + MISS),     by=list(LTIME, REG, MOD, THRES)]
      DS[, FAR := FAL_AL / (HITS + FAL_AL), by=list(LTIME, REG, MOD, THRES)]
      validate(need('DS' %in% ls() && nrow(DS) > 0, F)) # Forces validation
      
      p = ggplot(DS, aes(x=1-FAR, y=POD, color=MOD)) +
        geom_point(aes(size=as.character(THRES), shape=as.character(THRES))) +
        scale_x_continuous(limits = c(0,1), expand=c(0,0)) +
        scale_y_continuous(limits = c(0,1), expand=c(0,0)) +
        scale_shape_manual(name=tl("Limiares"), values=c(1, 16, 3, 17, 2, 15, 4), breaks=THRES[[GetVar()]]) +
        scale_size_discrete(name=tl("Limiares"), breaks=THRES[[GetVar()]]) +
        guides(color="none")
      
      p = p +
        scale_color_manual(values=cores.mod[f.GetMainIdxMod(unique(DS$MOD))]) +
        geom_abline(intercept=0, slope=1, linetype=2, color="grey60") +
        coord_fixed()
      
      # Faceteia automaticamente
      if (length(input$ncolsContId) == 0 || as.integer(input$ncolsContId) == 0) {
        nCols = NULL
      } else {
        nCols = as.integer(input$ncolsContId)
      }
      p = p + facet_wrap(~REG+LTIME, scales = 'fixed', ncol=nCols,
                         labeller=labeller(REG=function(x) substring(x, ifelse(input$mainRegId==4, 7, 3)),
                                           .multi_line=F))
      p = p +
        theme(
          axis.text=element_text(size=12),
          axis.title=element_text(size=16, face="bold"),
          legend.text=element_text(size=14),
          strip.text = element_text(size = 16, face='bold'),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))
        )
      p
    }) # isolate
  }, height=650) # altura da parte branca da tela
  
  # ============================================================================
  # Mostra o help do diagrama de performance
  # ============================================================================
  output$TabHelpPerf <- renderUI({
    input$runBt
    isolate({   # Torna o código não reativo aos inputs pois o código é pesado
      html = c("<html><head></head><body><table style=\"color:#000000; background-color:#FFFFFF;\">")
      metrics = c("POD", "FAR")
      html = c(html, paste0("<tr>", paste0("<td width=\"50%\" style=\"text-align:center;\"><img src=\"", as.character(sapply(sapply(metrics, f.GetHelp), "[", "Img")), "\"></td>", collapse = ""), "</tr>"))
      html = c(html, paste0("<tr>", paste0("<th><h2 style=\"color:#0000FF; background-color:#FFFFFF;\">", as.character(sapply(sapply(metrics, f.GetHelp), "[", "Names")), "</h2></th>", collapse = ""), "</tr>"))
      html = c(html, paste0("<tr>", paste0("<td><strong>Answers:</strong> ", as.character(sapply(sapply(metrics, f.GetHelp), "[", "Answers")), "</td>", collapse = ""), "</tr>"))
      html = c(html, paste0("<tr>", paste0("<td><strong>Range:</strong> ", as.character(sapply(sapply(metrics, f.GetHelp), "[", "Range")), "</td>", collapse = ""), "</tr>"))
      html = c(html, paste0("<tr>", paste0("<td><strong>Characteristics:</strong> ", as.character(sapply(sapply(metrics, f.GetHelp), "[", "Characteristics")), "</td>", collapse = ""), "</tr>"))
      html = c(html, "</table>")
      html = c(html, "<br><p style=\"color:#000000; background-color:#FFFFFF;\"><strong style=\"color:#0000FF\">Source:</strong> WWRP/WGNE Joint Working Group on Forecast Verification Research, online at <a href=\"https://www.cawcr.gov.au/projects/verification/\">https://www.cawcr.gov.au/projects/verification/</a></p>")
      res = HTML(paste0(c(html, "</body></html>"), collapse=""))
      res
    })
    #writeLines(res, "HelpLTime.html")
  })
  
  
  # ============================================================================
  # LEGENDA DO DIAGRAMA DE PERFORMANCE
  # ============================================================================
  output$PerfDiagLeg <- renderPlot({ 
    if (input$ViewMode == "Perf") {
      par(mfrow=c(1,1), mar=c(0,0,0,0), oma=c(0,0,0,0), pty='m')
      if (input$typeLegPerfDiag == tl('Círculos')) {
        # repetição de bolinha vazada e bolinha preenchida (par e ímpar)
        my_pch  <- c(1, 16)
      } else {
        # símbolo do ponto é por threshold (símbolos pensados para serem visualmente diferentes dos vizinhos)
        my_pch  <- CFG$symbols[1:length(CFG$thres.prec.bin)]
      }
      
      plot(0, type='n', axes=F, xlab='', ylab='')
      if (input$typeLegPerfDiag == tl('Círculos')) {
        legend("topleft", legend = paste0(">", format(CFG$thres.prec.bin, width=5, justify = 'right')), 
               cex = .9, pt.cex=CFG$cex.thres.leg, pch=my_pch, x.intersp = 1.1, y.intersp = 1, xpd=T, title = tl('Limiares'))
      } else {
        legend("topleft", y=30, legend = paste0(">", format(CFG$thres.prec.bin, width=5, justify = 'right')),
               cex = .9, pt.cex=1.8, pch=my_pch, x.intersp = 1.1, y.intersp = 1, xpd=T, title = tl('Limiares'))
      }
    }
  }, height=150)
  
  # ============================================================================
  # HISTOGRAMAS
  # ============================================================================
  output$PlotHist <- renderPlot({
    input$runBt # Fica dependente do botão "Atualizar"
    isolate({   # Torna o código não reativo aos inputs pois o código é pesado
      #req(input$catId, input$catNumBreaksId) # Se input$xxx puder ser vazio, não usar aqui
      options("scipen" = 100, "digits" = 2)
      #
      #
      # TODO: método está errado!!!!!! Soma pela média
      #
      #
      # Classifica (DS já existe!!!)
      DS.HIST <- f.FilterDS('EVAL_CONT', input)
      DS.HIST <- DS.HIST[TYPE %in% c('AVG', 'OBS'), ] # Só FCT e OBS interessam
      idx.thres <- 
        my_breaks <- THRES$PREC[which(THRES$PREC==input$thresId[1]):which(THRES$PREC==input$thresId[2])]
      
      DS.HIST[, CAT := findInterval(x=VALUE, vec=my_breaks, left.open=T, rightmost.closed=F)]
      DS2 <- DS.HIST[TYPE=='AVG', .N, by=list(LTIME, REG, MOD, CAT)]
      DS3 <- DS.HIST[TYPE=='OBS', .N, by=list(LTIME, REG, MOD, CAT)]
      DS3$MOD <- 'OBS'
      DS4 <- rbind.data.frame(DS2, DS3)
      
      my_ticks <- 1:length(my_breaks)
      p <- ggplot(data=DS4, aes(x=CAT+.5, y=N, fill=MOD)) +
        geom_col(position=position_dodge(width=0.8), width=.7, orientation="x") + 
        scale_fill_manual(values=c(cores.mod[f.GetMainIdxMod(unique(DS4$MOD))], 'black')) +
        scale_x_continuous(breaks=my_ticks, labels=my_breaks) + 
        theme(legend.position="none") +
        ylab(paste(tl('Ocorrências'), '(x0E6)')) + 
        xlab(paste(tl('Categorias de'), toupper(GetVar()), UNID[[toupper(GetVar())]])) +
        theme(
          axis.text=element_text(size=12),
          axis.title=element_text(size=16, face="bold"),
          legend.text=element_text(size=14),
          strip.text.x = element_text(size = 16, face='bold'),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
          legend.title=element_text(size=16, face='bold'))
      
      #geom_text(aes(y=FREQ,label=paste0(round(PERC,1),"%")), position = position_dodge(1), check_overlap = T,# size=4,
      #          color=rev(rep(c(cores.mod, 'black'), times=nrow(FilterHist())/4)), vjust=0) +
      #scale_y_continuous(labels = f.FormatLabel)
      if (length(input$ncolsContId) == 0 || as.integer(input$ncolsContId) == 0) {
        p <- p + facet_wrap(LTIME ~ REG, scales = 'fixed', 
                            labeller=labeller(LTIME=function(x) {x=round(as.integer(x)+config$tshift/24,1); paste('Fct. ', x*24, 'h (', x, 'd)', sep='')}, 
                                              REG=function(x) substring(x, ifelse(input$mainRegId==4, 7, 3)), 
                                              .multi_line=F))
      } else {
        p <- p + facet_wrap(LTIME ~ REG, scales = 'fixed', ncol=as.integer(input$ncolsContId),
                            labeller=labeller(LTIME=function(x) {x=round(as.integer(x)+config$tshift/24,1); paste('Fct. ', x*24, 'h (', x, 'd)', sep='')}, 
                                              REG=function(x) substring(x, ifelse(input$mainRegId==4, 7, 3)), 
                                              .multi_line=F))
      }
      plot(p)
    }) # isolate
  }, height=550) # altura da parte branca da tela
  
  # ============================================================================
  # Plot do mapa das áreas
  # ============================================================================
  f.PlotRegioes <- function(zoom=F) {
    #jpeg(filename=paste0(EVAL.CUR, '/IMG/Map.jpeg'), width=150, height=220)
    req(input$subRegId)
    #validate(need(!is.null(input$subRegId), F))
    #par(mfrow=c(1,zoom+1), mar=c(0,0,0,0), oma=c(0,0,0,0))
    par(mfrow=c(1,1), mar=c(0,0,0,0), oma=c(0,0,0,0))
    par(mfrow=c(1,1), mar=c(0,0,0,0))
    ##
    #reg.type='9'
    #input$mainRegId=c(CFG$reg.names, REGIONS$NAME)[which(substring(c(CFG$reg.names, REGIONS$NAME),1,1)==reg.type)[2]]
    #reg.name=input$subRegId; input$maskId='ALL';
    ##
    
    # Extrai o tipo e o nome do item a ser mostrado
    #validate(need(!is.na(input$mainRegId), F))
    reg.type <- unique(substring(input$subRegId,1,1))[1]
    reg.name <- input$subRegId
    
    # ==========================================================================
    # Mapa da A.S.
    # ==========================================================================
    # lim deve ser: X1, X2, Y1, Y2
    lim <- c(min(CFG$lon), max(CFG$lon), min(CFG$lat), max(CFG$lat))
    #f.PlotMap(reg.type, reg.name, lim, input$maskId)
    
    # # ==========================================================================
    # # Mapa ZOOM
    # # ==========================================================================
    # # Captura os limites da área escolhida
    if (zoom) {
      #' switch(
      #'   reg.type,
      #'   '1'={ lim <-c(range(CFG$lon), range(CFG$lat)) },
      #'   #'2'={ lim = map("world", xlim=range(REGIONS$LON1, REGIONS$LON2), ylim=range(REGIONS$LAT1, REGIONS$LAT2),
      #'   #                regions=substring(reg.name, 3), plot=F)$range },
      #'   '2'={ lim = st_bbox(shp.br.uf)},
      #'   '3'={ lim2 = st_bbox(shp.br.uf[shp.br.uf$sigla %in% substring(reg.name, 3),]);
      #'   lim=as.vector(t(lim2)); lim=lim[c(1,3,2,4)] },
      #'   '4'={ lim2 = st_bbox(shp.br.reg[shp.br.reg$sigla %in% substring(reg.name, 7),]) ;
      #'   lim=as.vector(t(lim2)); lim=lim[c(1,3,2,4)] },
      #'   '5'={ lim = map("world", xlim=range(CFG$lon), ylim=range(CFG$lat),
      #'                   regions=substring(reg.name, 3), plot=F)$range },
      #'   '9'={ idx.reg <- which(REGIONS$NAME %in% reg.name);
      #'   lim <- c(min(REGIONS[idx.reg,'LON1'])-1, max(REGIONS[idx.reg,'LON2'])+1,
      #'            min(REGIONS[idx.reg,'LAT1'])-1, max(REGIONS[idx.reg,'LAT2'])+1)}
      #' )
      #' # lim deve ser: X1, X2, Y1, Y2, bbox() retorna (X1, Y1, X2, Y2)
      #' ext <- 2  # extensão dos limites do plot
      #' lim[1]<-lim[1]-ext; lim[2]<-lim[2]+ext;
      #' lim[3]<-lim[3]-ext; lim[4]<-lim[4]+ext;
      lim = f.GetMapLim(reg.type, reg.name)
      rect(lim[1], lim[3], lim[2], lim[4], lwd=2, border='red') # retangulo no sem zoom
      f.PlotMap(reg.type, input$subRegId, lim, input$maskId)
      graphics::box(col='red', lwd=2)
    }
    #map.text('world', xlim=range(REGIONS$LON1, REGIONS$LON2), ylim=range(REGIONS$LAT1, REGIONS$LAT2), add=T)
    #dev.off()
  }
  
  # ============================================================================
  # SCORECARD PERIOD
  # ============================================================================
  output$ScoreCard01_per <- renderUI({ 
    input$runBt # Fica dependente do botão "Atualizar"
    isolate({   # Torna o código não reativo aos inputs pois o código é pesado
      # Create a Progress object
      progress <- shiny::Progress$new(style = 'notification')
      progress$set(message = "Computing data", value = 0)
      # Close the progress when this reactive exits (even if there's an error)
      on.exit(progress$close())
      
      # Create a closure to update progress.
      updateProgress <- function(value, detail = NULL) {
        progress$set(value = value, detail = detail)
      }
      # Compute the new data, and pass in the updateProgress function so
      # that it can update the progress indicator.
      #      f.GenScoreCard01(input$anoMesBeg, 
      #                       input$anoMesEnd, 
      #                       paste('Média do período -', input$anoMesBeg, 'a', input$anoMesEnd),
      #                       input, updateProgress, config)
      f.GenScoreCard01("PER", input, updateProgress, config)
    })
  })
  
  # ============================================================================
  # SCORECARD LAST DAY
  # ============================================================================
  output$ScoreCard01_lastday <- renderUI({ 
    input$runBt # Fica dependente do botão "Atualizar"
    isolate({   # Torna o código não reativo aos inputs pois o código é pesado
      # Create a Progress object
      progress <- shiny::Progress$new(style = 'notification')
      progress$set(message = "Computing data", value = 0)
      # Close the progress when this reactive exits (even if there's an error)
      on.exit(progress$close())
      
      # Create a closure to update progress.
      updateProgress <- function(value, detail = NULL) {
        progress$set(value = value, detail = detail)
      }
      # Compute the new data, and pass in the updateProgress function so
      # that it can update the progress indicator.
      # f.GenScoreCard01(format(max(EVAL.CONT$DT), format="%Y%m%d"), 
      #                  format(max(EVAL.CONT$DT), format="%Y%m%d"), 
      #                  paste('Último dia  - ', max(EVAL.CONT$DT)), 
      #                  input, updateProgress2, config)
      f.GenScoreCard01("LAST_DAY", input, updateProgress, config)
    })
  })
  
  # ============================================================================
  # SCORECARD LAST WEEK
  # ============================================================================
  output$ScoreCard01_lastweek <- renderUI({ 
    input$runBt # Fica dependente do botão "Atualizar"
    isolate({   # Torna o código não reativo aos inputs pois o código é pesado
      # Create a Progress object
      progress <- shiny::Progress$new(style = 'notification')
      progress$set(message = "Computing data", value = 0)
      # Close the progress when this reactive exits (even if there's an error)
      on.exit(progress$close())
      
      # Create a closure to update progress.
      updateProgress <- function(value, detail = NULL) {
        progress$set(value = value, detail = detail)
      }
      # Compute the new data, and pass in the updateProgress function so
      # that it can update the progress indicator.
      # f.GenScoreCard01(format(max(EVAL.CONT$DT), format="%Y%m%d"), 
      #                  format(max(EVAL.CONT$DT), format="%Y%m%d"), 
      #                  paste('Último dia  - ', max(EVAL.CONT$DT)), 
      #                  input, updateProgress2, config)
      f.GenScoreCard01("LAST_WEEK", input, updateProgress, config)
    })
  })
  
  # ============================================================================
  # RENDERIZAÇÕES DIVERSAS
  # ============================================================================
  # TODO: Criar estas imagens para mostrar ao invés de criá-las toda hora
  output$ColorScale <- renderPlot({ 
    if (input$ViewMode == "SC") {
      if (input$colScaleId == 'GRAD') {
        f.ColScaleBestWorst(grad=T, cols=colors.prec.bias) 
        #f.ArrowedColorBar('H', 'UpDown', .1, .9, .1, .2, .2, 
        #                  nums=breaks.prec.bias, cols=colors.prec.bias) 
      } else {
        f.ColScaleBestWorst(grad=F, cols=c('red', 'orange', 'darkgreen')) 
      }
    }
  }, height=70)
  
  output$PlotRegioes <- renderPlot({
    width=240
    height=240
    f.PlotRegioes(zoom=T)
  }, execOnResize=T)
  
  # output$PlotMap <- renderPlot({
  #   if (input$ViewMode == 'Reg') {
  #     f.PlotRegioes(zoom=T)
  #   }
  # }, execOnResize=T, height=500)
  
  # ============================================================================
  # ANÁLISE ESPACIAL - OBS + FCTs
  # ============================================================================
  output$PlotObs <- renderImage({
    fname <- GetObsImgName()
    list(src = fname)
  }, deleteFile = FALSE)
  output$PlotFct1 <- renderImage({
    fname <- GetImgName(1)
    list(src = fname)
  }, deleteFile = FALSE)
  output$PlotFct2<- renderImage({
    fname <- GetImgName(2)
    list(src = fname)
  }, deleteFile = FALSE)
  output$PlotFct3 <- renderImage({
    fname <- GetImgName(3)
    list(src = fname)
  }, deleteFile = FALSE)
  output$PlotFct4 <- renderImage({
    fname <- GetImgName(4)
    list(src = fname)
  }, deleteFile = FALSE)
  
  # ANÁLISE ESPACIAL - BIAS, SRMSE, SCORR
  output$PlotBias1 <- renderImage({
    fname <- GetImgName(1)
    list(src = fname)
  }, deleteFile = FALSE)
  output$PlotBias2 <- renderImage({
    fname <- GetImgName(2)
    list(src = fname)
  }, deleteFile = FALSE)
  output$PlotBias3 <- renderImage({
    fname <- GetImgName(3)
    list(src = fname)
  }, deleteFile = FALSE)
  output$PlotBias4 <- renderImage({
    fname <- GetImgName(4)
    list(src = fname)
  }, deleteFile = FALSE)
  
  # ANÁLISE ESPACIAL - MMODEL
  output$PlotBestOf1 <- renderImage({
    fname <- GetImgName(0, "SBIAS")
    list(src = fname)
  }, deleteFile = FALSE)
  output$PlotBestOf2 <- renderImage({
    fname <- GetImgName(0, "SCORR")
    list(src = fname)
  }, deleteFile = FALSE)
  output$PlotBestOf3 <- renderImage({
    fname <- GetImgName(0, "SRMSE")
    list(src = fname)
  }, deleteFile = FALSE)
  
  output$PlotSpAnalisys <- renderPlot({ 
    M = matrix(c(1,1,2,4,3,5,6,6), ncol=4)
    M = matrix(c(1,2,3,6,4,7,5,8,9,9), ncol=5)
    # Matrix has 4 cols, so we need a vector of 4 elements, each one is a relative width wrt the total width
    # divide to total width in n parts and the 1st col gets n parts and the 2nd n parts
    layout(M, widths=c(2.4,2.4,2.4,2.4,.4))
    #layout.show(9)
    f.PlotSpBestOfScore()
    f.PlotSpBestOfModels()
    f.PlotSpModel(1)
    f.PlotSpModel(2)
    f.PlotSpModel(3)
    f.PlotSpModel(4)
    f.PlotSpModel(5)
    f.PlotSpModel(6)
    f.PlotSpLeg()
  }, height=1000)
  
  #idx_mod=2
  f.PlotSpModel = function(idx_mod) {
    if (length(input$modId) >= idx_mod) {
      res = f.GetColorsAndBreaks(GetVar(), input$contSpId)
      my_breaks = res$breaks
      my_colors = res$colors
      h_shift = ifelse(GetVar()=="PREC", 12, 0)
      ltime_str = sprintf('%03d', input$ltimes2[1] * 24 + h_shift)
      #ltime_str = "180"
      my_mar = c(2,2,2,1)
      par(mar=my_mar)
      mod=input$modId[idx_mod]
      fname = paste0(EVAL.CUR, "/FLDS/FLD_", input$contSpId, "_", mod, "_", GetVar(), "_", ltime_str, "_", input$perId, ".RData")
      if (file.exists(fname)) {
        load(fname)
        # Só existe avaliação de mask=ALL para região 1-South_America
        reg_type = substring(input$subRegId,1,1)
        if (reg_type %in% c("1","2")) {
          my_ext=0
        } else {
          my_ext=1
        }
        lim = f.GetMapLim(reg.type=reg_type, reg.name=input$subRegId, ext=my_ext)
        my_mask = ifelse(substring(input$subRegId,1,1) == "1", input$maskId, "CONT")
        # Todas as máscaras vêm com o nome de MASK.CONT
        if (reg_type == "1") {
          idx.lon = 1:length(CFG$lon)
          idx.lat = 1:length(CFG$lat)
          if (my_mask == "CONT") {
            load(paste0(EVAL.CUR, "/MASKS/", CFG$mask.cont.prefix, CFG$domain, '_', CFG$area1.names[1], '_', CFG$mask.cont.suffix, '.RData'))
          }
        } else {
          if (reg_type == "9") { # QUADRANTES
            idx.reg <- which(REGIONS$NAME == input$subRegId)
            idx.lon  <- which(CFG$lon >= REGIONS$LON1[idx.reg] & CFG$lon <= REGIONS$LON2[idx.reg])
            idx.lat  <- which(CFG$lat >= REGIONS$LAT1[idx.reg] & CFG$lat <= REGIONS$LAT2[idx.reg])
            # Carrega a máscara do contorno da AS, para aplicar nos quadrantes
            load(paste0(EVAL.CUR, "/MASKS/", CFG$mask.cont.prefix, CFG$domain, '_', CFG$area1.names[1], '_', CFG$mask.cont.suffix, '.RData'))
          } else {
            load(paste0(EVAL.CUR, "/MASKS/", CFG$mask.cont.prefix, CFG$domain, '_', input$subRegId, '_', CFG$mask.cont.suffix, '.RData'))
            idx.lon  <- which(CFG$lon >= lim[1] & CFG$lon <= lim[2])
            idx.lat  <- which(CFG$lat >= lim[3] & CFG$lat <= lim[4])
          }
        }
        #par(mfrow=c(1,1), mar=c(0,0,0,0))
        # cria um break e adiciona uma cor se for para filtrar o SCORR
        if (input$contSpId == "SCORR" && input$doFilterScorr) {
          scorrThres = as.numeric(input$scorrThres)
          my_breaks = sort(c(my_breaks, scorrThres))               
          posThres = findInterval(scorrThres, my_breaks)
          my_colors = c(rep("#FFFFFF", posThres-1), my_colors[(posThres-1):length(my_colors)])
        }
        
        if (input$coloredFLD || input$numIsoLevels > 0) {
          map("world", xlim=c(lim[1], lim[2]), ylim=c(lim[3], lim[4]), xpd=F, mar=my_mar)
        }
        #cat("A Model 1:", my_ext, lim, lim[1], lim[2], lim[3], lim[4], range(idx.lon), range(idx.lat), par("mar"), par("xpd"), par("usr"), par("oma"), "\n")      
        # FLD tem que ser ajustado pois pode mostrar campo colorido e isolinhas
        if (my_mask == "CONT") {
          FLD = FLD[idx.lon,idx.lat] + MASK.CONT[idx.lon,idx.lat]
        }
        
        if (input$coloredFLD) {
          image(x=round(CFG$lon[idx.lon],2), y=round(CFG$lat[idx.lat],2), z=FLD, useRaster=T, col=my_colors, xlab='', ylab='', pty='m', axes=F, 
                add=T, breaks=my_breaks, mar=my_mar)
        }
        
        # ESTADOS E QUADRANTES
        if (reg_type %in% c('3','9')) {  
          plot(shp.br.uf$geometry, add=T, lwd=.5, mar=my_mar)
        } 
        
        # Isolinhas
        if (input$contSpId == "SCORR" && input$doFilterScorr && input$numIsoLevels > 0) {
          #lev1 = (trunc(scorrThres*10)+1)/10
          #lev2 = min(trunc(max(FLD, na.rm=T)*10)/10+.1, 1)
          #my_levels = seq(lev1, lev2, length.out=input$numIsoLevels)
          my_levels = round(seq(scorrThres, 1, length.out=input$numIsoLevels), 2)
          if (input$isolineColor == "Black") {
            colIsolines = "#000000"
          } else {
            colIsolines = colorRampPalette(my_colors[posThres:length(my_colors)])(length(my_levels))
          }
          contour(x=round(CFG$lon[idx.lon],2), y=round(CFG$lat[idx.lat],2), z=FLD, zlim=c(scorrThres, 1), add=T,
                  levels=my_levels, col=colIsolines, labels=my_levels, labcex=1.5, drawlabels=input$isolineLabels, mar=my_mar)
        }
        # REFORÇA OS TRAÇOS DO MAPA PARA FICAREM SOBRE TUDO
        map("world", xlim=c(lim[1], lim[2]), ylim=c(lim[3], lim[4]), add=T, col='black', lwd=2, xpd=F, mar=my_mar)
        suppressWarnings({plot(shp.br.reg, add=T, pal=NA, mar=my_mar)})
        par(mar=my_mar) # O COMANDO ACIMA MUDA A MARGEM
        graphics::box(); axis(1); axis(2, las=2)
        #cat("A Model 2:", my_ext, lim, lim[1], lim[2], lim[3], lim[4], range(idx.lon), range(idx.lat), par("mar"), par("xpd"), par("usr"), par("oma"), "\n")      
        leg = f.BoxedText(x=grconvertX(1, from='npc'), y=max(CFG$lat[idx.lat]), text=mod, cex=2.5, yjust=0, col.fg=cores.mod[which(CFG$models==mod)])
        leg = f.BoxedText(x=grconvertX(1, from='npc'), y=leg$rect$top-leg$rect$h, text=paste0(substring(input$contSpId, 2), " ", ltime_str, "h", collapse = ""), 
                          col.fg=cores.mod[which(CFG$models==mod)], cex=2)
        metr_val = round(EVAL.CONT.PER[.(input$evsetId, mod, input$ltimes2[1], input$subRegId, my_mask, input$perId, input$runTimeId)][[input$contSpId]], 2)
        leg = f.BoxedText(x=grconvertX(1, from='npc'), y=leg$rect$top-leg$rect$h, text=paste0("Média = ", metr_val), col.fg=cores.mod[which(CFG$models==mod)], 
                          cex=2)
      } else {
        plot(0, type='n', axes=F)
      }
    } else {
      plot(0, type='n', axes=F)
    }
  }
  
  f.PlotSpBestOfScore <- function() {
    if (length(input$modId) > 0) {
      res = f.GetColorsAndBreaks(GetVar(), input$contSpId)
      my_breaks = res$breaks
      my_colors = res$colors
      h_shift = ifelse(GetVar()=="PREC", 12, 0)
      ltime_str = sprintf('%03d', input$ltimes2[1] * 24 + h_shift)
      #ltime_str = "180"
      my_mar = c(2,2,2,1)
      par(mar=my_mar)
      patt = paste0("FLD_", input$contSpId, "_(", paste0(input$modId, collapse="|"), ")_", GetVar(), "_", ltime_str, "_", input$perId, ".RData")
      files = dir(path = paste0(EVAL.CUR, "/FLDS"), pattern = patt)
      models = gsub(paste0("(FLD", "_|", input$contSpId, "_|_", GetVar(), "|_", ltime_str, "|_", input$perId, "\\.RData", ")"), "", files)
      FLDS <- array(NA, c(length(CFG$lon), length(CFG$lat), length(files)))
      fld_dim = dim(FLDS)
      idx_fld=1
      for (idx_fld in 1:fld_dim[3]) {
        #MEC_model = input$modId[idx_fld]
        # Filenames of the consolidated 2D matrix of each model
        fname = paste0(EVAL.CUR, "/FLDS/", files[idx_fld])
        #print(fname)
        load(fname)  # Loads as "FLD", already checked if exists
        FLDS[,,idx_fld] = copy(FLD)
      }
      # Captura o índice do melhor modelo, ponto a ponto
      if (input$contSpId == "SCORR") {            # Higher corr is better
        MMODEL = matrix(as.numeric(apply(FLDS, c(1,2), function(x) {i=which.max(x); return(x[i])})), nrow=fld_dim[1])
      } else {
        MMODEL = matrix(as.numeric(apply(FLDS, c(1,2), function(x) {i=which.min(abs(x)); return(x[i])})), nrow=fld_dim[1])
      }
      # Só existe avaliação de mask=ALL para região 1-South_America
      reg_type = substring(input$subRegId,1,1)
      if (reg_type %in% c("1","2")) {
        my_ext=0
      } else {
        my_ext=1
      }
      lim = f.GetMapLim(reg.type=reg_type, reg.name=input$subRegId, ext=my_ext)
      my_mask = ifelse(substring(input$subRegId,1,1) == "1", input$maskId, "CONT")
      # Todas as máscaras vêm com o nome de MASK.CONT
      if (reg_type == "1") {
        idx.lon = 1:length(CFG$lon)
        idx.lat = 1:length(CFG$lat)
        if (my_mask == "CONT") {
          load(paste0(EVAL.CUR, "/MASKS/", CFG$mask.cont.prefix, CFG$domain, '_', CFG$area1.names[1], '_', CFG$mask.cont.suffix, '.RData'))
        }
      } else {
        if (reg_type == "9") { # QUADRANTES
          idx.reg <- which(REGIONS$NAME == input$subRegId)
          idx.lon  <- which(CFG$lon >= REGIONS$LON1[idx.reg] & CFG$lon <= REGIONS$LON2[idx.reg])
          idx.lat  <- which(CFG$lat >= REGIONS$LAT1[idx.reg] & CFG$lat <= REGIONS$LAT2[idx.reg])
          # Carrega a máscara do contorno da AS, para aplicar nos quadrantes
          load(paste0(EVAL.CUR, "/MASKS/", CFG$mask.cont.prefix, CFG$domain, '_', CFG$area1.names[1], '_', CFG$mask.cont.suffix, '.RData'))
        } else {
          load(paste0(EVAL.CUR, "/MASKS/", CFG$mask.cont.prefix, CFG$domain, '_', input$subRegId, '_', CFG$mask.cont.suffix, '.RData'))
          idx.lon  <- which(CFG$lon >= lim[1] & CFG$lon <= lim[2])
          idx.lat  <- which(CFG$lat >= lim[3] & CFG$lat <= lim[4])
        }
      }
      #par(mfrow=c(1,1), mar=c(0,0,0,0))
      # cria um break e adiciona uma cor se for para filtrar o SCORR
      if (input$contSpId == "SCORR" && input$doFilterScorr) {
        scorrThres = as.numeric(input$scorrThres)
        my_breaks = sort(c(my_breaks, scorrThres))               
        posThres = findInterval(scorrThres, my_breaks)
        my_colors = c(rep("#FFFFFF", posThres-1), my_colors[(posThres-1):length(my_colors)])
      }
      
      if (input$coloredFLD || input$numIsoLevels > 0) {
        map("world", xlim=c(lim[1], lim[2]), ylim=c(lim[3], lim[4]), xpd=F, mar=my_mar)
      }
      #cat("B Score 1:", my_ext, lim, lim[1], lim[2], lim[3], lim[4], range(idx.lon), range(idx.lat), par("mar"), par("xpd"), par("usr"), par("oma"), "\n")      
      # FLD tem que ser ajustado pois pode mostrar campo colorido e isolinhas
      if (my_mask == "CONT") {
        MMODEL = MMODEL[idx.lon,idx.lat] + MASK.CONT[idx.lon,idx.lat]
      }
      
      if (input$coloredFLD) {
        image(x=round(CFG$lon[idx.lon],2), y=round(CFG$lat[idx.lat],2), z=MMODEL, useRaster=T, col=my_colors, xlab='', ylab='', pty='m', axes=F, 
              add=T, breaks=my_breaks, mar=my_mar)
        #cat("B Score 2:", my_ext, lim, lim[1], lim[2], lim[3], lim[4], range(idx.lon), range(idx.lat), par("mar"), par("xpd"), par("usr"), par("oma"), "\n")      
      }
      
      # ESTADOS E QUADRANTES
      if (reg_type %in% c('3','9')) {  
        plot(shp.br.uf$geometry, add=T, lwd=.5, mar=my_mar)
        #cat("B Score 3:", my_ext, lim, lim[1], lim[2], lim[3], lim[4], range(idx.lon), range(idx.lat), par("mar"), par("xpd"), par("usr"), par("oma"), "\n")      
      } 
      
      # Isolinhas
      if (input$contSpId == "SCORR" && input$doFilterScorr && input$numIsoLevels > 0) {
        #lev1 = (trunc(scorrThres*10)+1)/10
        #lev2 = min(trunc(max(FLD, na.rm=T)*10)/10+.1, 1)
        #my_levels = seq(lev1, lev2, length.out=input$numIsoLevels)
        my_levels = round(seq(scorrThres, 1, length.out=input$numIsoLevels), 2)
        if (input$isolineColor == "Black") {
          colIsolines = "#000000"
        } else {
          colIsolines = colorRampPalette(my_colors[posThres:length(my_colors)])(length(my_levels))
        }
        contour(x=round(CFG$lon[idx.lon],2), y=round(CFG$lat[idx.lat],2), z=MMODEL, zlim=c(scorrThres, 1), add=T,
                levels=my_levels, col=colIsolines, labels=my_levels, labcex=1.5, drawlabels=input$isolineLabels, mar=my_mar)
        #cat("B Score 4:", my_ext, lim, lim[1], lim[2], lim[3], lim[4], range(idx.lon), range(idx.lat), par("mar"), par("xpd"), par("usr"), par("oma"), "\n")      
        
      }
      
      # REFORÇA OS TRAÇOS DO MAPA PARA FICAREM SOBRE TUDO
      map("world", xlim=c(lim[1], lim[2]), ylim=c(lim[3], lim[4]), add=T, col='black', lwd=2, xpd=F, mar=my_mar)
      #cat("B Score 5:", my_ext, lim, lim[1], lim[2], lim[3], lim[4], range(idx.lon), range(idx.lat), par("mar"), par("xpd"), par("usr"), par("oma"), "\n")      
      suppressWarnings({plot(shp.br.reg, add=T, pal=NA, mar=my_mar)}); 
      par(mar=my_mar) # O COMANDO ACIMA MUDA A MARGEM
      #cat("B Score 6:", my_ext, lim, lim[1], lim[2], lim[3], lim[4], range(idx.lon), range(idx.lat), par("mar"), par("xpd"), par("usr"), par("oma"), "\n")      
      graphics::box(col="red", lwd=10); axis(1); axis(2, las=2)
      #cat("B Score 7:", my_ext, lim, lim[1], lim[2], lim[3], lim[4], range(idx.lon), range(idx.lat), par("mar"), par("xpd"), par("usr"), par("oma"), "\n")      
      #title(main=paste0("Multi model em ", substring(input$contSpId, 2)), cex.main=2.5, line=-2)
      leg = f.BoxedText(x=grconvertX(1, from='npc'), y=max(CFG$lat[idx.lat]), text="Multi Model", cex=2.5, yjust=0)
      leg = f.BoxedText(x=grconvertX(1, from='npc'), y=leg$rect$top-leg$rect$h, text=paste0(substring(input$contSpId, 2), " ", ltime_str, "h"), cex=2)
      metr_val = round(mean(MMODEL, na.rm=T), 2)
      leg = f.BoxedText(x=grconvertX(1, from='npc'), y=leg$rect$top-leg$rect$h, text=paste0("Média = ", metr_val), cex=2)
    } else {
      plot(0, type='n', axes=F)
    }
  }
  
  f.PlotSpBestOfModels <- function() {
    if (length(input$modId) > 0) {
      my_pal = cores.mod #c("purple3", "gold", "dodgerblue2", "darkgreen")
      my_colors = my_pal[unlist(lapply(input$modId, grep, CFG$models))]
      h_shift = ifelse(GetVar()=="PREC", 12, 0)
      ltime_str = sprintf('%03d', input$ltimes2[1] * 24 + h_shift)
      #ltime_str = "180"
      my_mar = c(2,2,2,1)
      par(mar=my_mar)
      #input$mainRegId <- '4'; input$regBrId = "SE"
      #input$subRegId <- GetSubRegs(input)[2]
      patt = paste0("FLD_", input$contSpId, "_(", paste0(input$modId, collapse="|"), ")_", GetVar(), "_", ltime_str, "_", input$perId, ".RData")
      files = dir(path = paste0(EVAL.CUR, "/FLDS"), pattern = patt)
      models = gsub(paste0("(FLD", "_|", input$contSpId, "_|_", GetVar(), "|_", ltime_str, "|_", input$perId, "\\.RData", ")"), "", files)
      FLDS <- array(NA, c(length(CFG$lon), length(CFG$lat), length(files)))
      fld_dim = dim(FLDS)
      idx_fld=1
      for (idx_fld in 1:fld_dim[3]) {
        #MEC_model = input$modId[idx_fld]
        # Filenames of the consolidated 2D matrix of each model
        fname = paste0(EVAL.CUR, "/FLDS/", files[idx_fld])
        #print(fname)
        load(fname)  # Loads as "FLD", already checked if exists
        FLDS[,,idx_fld] = copy(FLD)
      }
      # Cores dos modelos disponíveis
      my_colors = my_pal[unlist(lapply(models, grep, CFG$models))]
      
      if (input$contSpId == "SCORR") {            # Higher corr is better
        MMODEL = matrix(as.integer(apply(FLDS, c(1,2), which.max)), nrow=fld_dim[1])
      } else {
        MMODEL = matrix(as.integer(apply(abs(FLDS), c(1,2), which.min)), nrow=fld_dim[1])
      }
      
      # Só existe avaliação de mask=ALL para região 1-South_America
      reg_type = substring(input$subRegId,1,1)
      if (reg_type %in% c("1","2")) {
        my_ext=0
      } else {
        my_ext=1
      }
      lim = f.GetMapLim(reg.type=reg_type, reg.name=input$subRegId, ext=my_ext)
      my_mask = ifelse(substring(input$subRegId,1,1) == "1", input$maskId, "CONT")
      # Todas as máscaras vêm com o nome de MASK.CONT
      if (reg_type == "1") {
        idx.lon = 1:length(CFG$lon)
        idx.lat = 1:length(CFG$lat)
        if (my_mask == "CONT") {
          load(paste0(EVAL.CUR, "/MASKS/", CFG$mask.cont.prefix, CFG$domain, '_', CFG$area1.names[1], '_', CFG$mask.cont.suffix, '.RData'))
        }
      } else {
        if (reg_type == "9") { # QUADRANTES
          idx.reg <- which(REGIONS$NAME == input$subRegId)
          idx.lon  <- which(CFG$lon >= REGIONS$LON1[idx.reg] & CFG$lon <= REGIONS$LON2[idx.reg])
          idx.lat  <- which(CFG$lat >= REGIONS$LAT1[idx.reg] & CFG$lat <= REGIONS$LAT2[idx.reg])
          # Carrega a máscara do contorno da AS, para aplicar nos quadrantes
          load(paste0(EVAL.CUR, "/MASKS/", CFG$mask.cont.prefix, CFG$domain, '_', CFG$area1.names[1], '_', CFG$mask.cont.suffix, '.RData'))
        } else {
          load(paste0(EVAL.CUR, "/MASKS/", CFG$mask.cont.prefix, CFG$domain, '_', input$subRegId, '_', CFG$mask.cont.suffix, '.RData'))
          idx.lon  <- which(CFG$lon >= lim[1] & CFG$lon <= lim[2])
          idx.lat  <- which(CFG$lat >= lim[3] & CFG$lat <= lim[4])
        }
      }
      if (my_mask == "CONT") {
        MMODEL = MMODEL[idx.lon,idx.lat] + MASK.CONT[idx.lon,idx.lat]
      }
      map("world", xlim=c(lim[1], lim[2]), ylim=c(lim[3], lim[4]), xpd=F, mar=my_mar)
      #cat("B Model 1:", my_ext, lim, lim[1], lim[2], lim[3], lim[4], range(idx.lon), range(idx.lat), par("mar"), par("xpd"), par("usr"), par("oma"), "\n")      
      image(x=round(CFG$lon[idx.lon],2), y=round(CFG$lat[idx.lat],2), z=MMODEL, useRaster=T, col=my_colors, xlab='', ylab='', pty='m', axes=F, add=T)
      #cat("B Model 2:", my_ext, lim, lim[1], lim[2], lim[3], lim[4], range(idx.lon), range(idx.lat), par("mar"), par("xpd"), par("usr"), par("oma"), "\n")      
      # REFORÇA OS TRAÇOS DO MAPA PARA FICAREM SOBRE TUDO
      map("world", xlim=c(lim[1], lim[2]), ylim=c(lim[3], lim[4]), add=T, col='black', lwd=2, xpd=F, mar=my_mar)
      #cat("B Model 3:", my_ext, lim, lim[1], lim[2], lim[3], lim[4], range(idx.lon), range(idx.lat), par("mar"), par("xpd"), par("usr"), par("oma"), "\n")      
      graphics::box(col="black", lwd=10); axis(1); axis(2, las=2)
      #cat("B Model 4:", my_ext, lim, lim[1], lim[2], lim[3], lim[4], range(idx.lon), range(idx.lat), par("mar"), par("xpd"), par("usr"), par("oma"), "\n")      
      suppressWarnings({plot(shp.br.reg, add=T, pal=NA, mar=my_mar)}); 
      par(mar=my_mar) # O COMANDO ACIMA MUDA A MARGEM
      #cat("B Model 5:", my_ext, lim, lim[1], lim[2], lim[3], lim[4], range(idx.lon), range(idx.lat), par("mar"), par("xpd"), par("usr"), par("oma"), "\n")      
      if (reg_type %in% c('3','9')) {  # ESTADOS E QUADRANTES
        plot(shp.br.uf$geometry, add=T, lwd=.5, mar=my_mar)
        par(mar=my_mar) # O COMANDO ACIMA MUDA A MARGEM
        #cat("B Model 6:", my_ext, lim, lim[1], lim[2], lim[3], lim[4], range(idx.lon), range(idx.lat), par("mar"), par("xpd"), par("usr"), par("oma"), "\n")      
      } 
      #metr_val = round(EVAL.CONT.PER[.(input$evsetId, input$modId, input$ltimes2[1], input$subRegId, my_mask, input$perId, input$runTimeId)][[input$contSpId]], 2)
      # Considering only non -1 grid points (-1 is no data)
      perc = paste0(round(prop.table(table(MMODEL, useNA="no"))*100, 1), "%")
      #my_leg = paste0(input$modId, "\n",perc, paste0(" (",  metr_val, ")"))
      my_leg = paste(models, perc)
      legend(x=grconvertX(1, from='npc'), y=grconvertY(0, from='npc'), legend=my_leg, fill=my_colors, xpd=T, pt.cex=2, horiz=F, xjust=1, yjust=0,
             bg="white", y.intersp=1.5, cex=1.5)
      #title(main=paste0("Best of modelos em ", substring(input$contSpId, 2)), cex.main=2.5, line=-2)
    }
  }
  
  f.PlotSpLeg = function() {
    res = f.GetColorsAndBreaks(GetVar(), input$contSpId)
    my_breaks = res$breaks
    my_colors = res$colors
    if (input$contSpId == "SCORR" && input$doFilterScorr) {
      scorrThres = as.numeric(input$scorrThres)
      my_breaks = sort(c(my_breaks, scorrThres))               # cria um break e adiciona uma cor
      my_colors = c(rep("#FFFFFF", which(my_breaks==scorrThres)-1), my_colors[(which(my_breaks==scorrThres)-1):length(my_colors)])
    }
    f.ArrowedColorBar(pointers=res$pointers, colors=my_colors, breaks=my_breaks, add=F)
  }
  
  output$SavePlotEvalCont <- downloadHandler(
    filename =  function() {
      fname <- c("MetCont", toupper(input$evsetId))
      
      #if (input$facetContId != 'NONE') {
      #  fname <- c(fname, paste0(input$facetContId, collapse = 'x'))
      #} 
      #if (!'TYPE' %in% input$facetContId) {
      #  fname <- c(fname, input$contId)
      #}
      fname <- gsub('\\*', '', fname)
      #if (!'REG' %in% input$facetContId) {
      #  fname <- c(fname, input$mainRegId)
      #}
      #fname <- c(fname, input$scaleContId)
      fname <- paste0(paste0(fname, collapse = "_"), ".jpeg")
    },
    # content is a function with the argument file that writes the plot to the device
    content = function(file) {
      jpeg(file, width = 1200, height = 800, quality = 100)
      #jpeg(file, width = 20, height = 10, units = 'in', res=600)
      # ggsave não tá gravando escala y da COR corretamente 
      #ggsave(file, device = "jpeg", width = 20, height = 10, units = "in")
      f.PlotMetricsCont('file')
      dev.off()
    }
  )
  #}) # observeEvent input$run # Colocar aqui dentro tudo que for render()
}
