#https://community.rstudio.com/t/how-do-i-add-tabs-to-the-dashboardheader/39674/2
#https://rinterface.com/shiny/shinydashboard/
#https://rstudio.github.io/shinydashboard/appearance.html#skins
#pie(c(1,1), color=c("#F0F0F0", "#000000"))
dashboardPage(
  #skin = "midnight", #"blue", "blue-light", "black", "black-light", "purple", "purple-light",
  #"green", "green-light", "red", "red-light", "yellow", "yellow-light", "midnight",
  
  # ============================================================================
  # HEADER
  # ============================================================================
  header = dashboardHeader(
    title=paste0("MEC - CPTEC/INPE", ifelse(grepl('DEV', getwd()), ' *** (DEV) ***', ''), collapse = ""),
    # Dropdown menu for messages
    tags$li(
      # PWD ====================================================================
      conditionalPanel(
        "input.pwd!='MNT'",
        style="padding:0; margin:10px 0px 0px 0px; display:inline-block; ",
        h3('Pwd'), textInput("pwd", label = NULL, value = ifelse(grepl('DEV', getwd()) || EVAL.CUR == "CPTEC_DAILY", 
                                                                 'MNT', ''), width='200px')
      ),
      conditionalPanel(
        "input.pwd=='MNT' || input.pwd=='W1R2F3'",
        
        # Button =================================================================
        conditionalPanel(
          "input.ViewMode != 'AE'",
          style="display:inline-block; margin:10px 0px 0px 0px",
          actionBttn("runBt", tl("Atualizar Tela"), icon=icon("refresh"), style="simple",
                     color="primary", size="sm")
          
        ),
        div(style="height:35px; display:inline-block;",
            #uiOutput("modControl")
            checkboxGroupButtons("modId", choiceNames=f.Models(), choiceValues=CFG$models,
                                 selected = CFG$models, justified=F, individual=T,
                                 checkIcon = list(yes = icon("ok", class="alert-dark", lib="glyphicon"),
                                                  no = icon("remove", class="alert-danger", lib="glyphicon")))
        ),
        # ------------------------------------------------------------------------
        # CONJUNTO DE AVALIAÇÃO VAR+OBS (EVAL_SET)
        # ------------------------------------------------------------------------
        div(style="height:35px; display:inline-block;",
            #selectInput("evsetId", label = NULL, choices = CFG$ev.sets,
            #            selected = CFG$ev.sets[1], width="150px")
            pickerInput(
              inputId = "evsetId", NULL, choices=as.character(CFG$ev.sets), selected = as.character(CFG$ev.sets[1]),
              width = "150px")
        ),
        div(style="height:35px; display:inline-block;",
            radioGroupButtons(
              inputId = "runTimeId", NULL, choices = c("00"), selected="00",
              status = "primary", checkIcon = list(
                yes = icon("ok"   , lib="glyphicon"),
                no = icon("remove", lib = "glyphicon"))
            )
        )#,
        #div(style="height:35px; display:inline-block;",
        #    span(textOutput('txtMemory'), style='color:black')
        #)
      ), # condit panel pwd
      class='dropdown') # tags$li
  ), # dashboardHeader
  
  # ============================================================================
  # SIDEBAR
  # ============================================================================
  sidebar = dashboardSidebar(
    # ------------------------------------------------------------------------
    # CONJUNTO DE AVALIAÇÃO VAR+OBS (EVAL_SET) e MODELOS
    # ------------------------------------------------------------------------
    #htmlOutput("modControl"),
    #uiOutput("modControl"))
    #fluidRow(column(6, checkboxInput("inp1", NULL)), 
    #column(6, checkboxInput("inp2", NULL))),
    #uiOutput("modControl"),
    
    # ------------------------------------------------------------------------
    # DIAS DE PREVISÃO
    # ------------------------------------------------------------------------
    conditionalPanel(
      "input.pwd=='MNT'",
      
      conditionalPanel(
        "input.ViewMode != 'AE' && input.ViewMode != 'LT' && input.ViewMode != 'LT2' && input.ViewMode != 'MMODEL'", 
        sliderInput("ltimes", label=HTML(
          paste(titSty(tl("Dias previsão"), extraSty="display:inline-block;"), 
                h5(strong(textOutput("numFrames")), 
                   style="display:inline-block; color:yellow;"),
                collapse="")), 
          min = 1, max=max(CFG$ltimes), 
          value=c(1,1), step=1, dragRange=F),
        #h5(strong(textOutput("numFrames")), style="text-align:center; color:yellow;")
      ),
      
      conditionalPanel(
        "input.ViewMode == 'MMODEL'", 
        sliderInput("ltimes2", label=HTML(
          paste(titSty(tl("Dias previsão"), extraSty="display:inline-block;"), 
                collapse="")), 
          min = 1, max=max(CFG$ltimes), 
          value=1, step=1, dragRange=F),
        #h5(strong(textOutput("numFrames")), style="text-align:center; color:yellow;")
      ),
      
      conditionalPanel(
        "input.ViewMode == 'Taylor'",
        fluidRow(column(12, sliderInput("taylorTextSize", tl("Tam. texto"), min=1.5, max=3.5, step=.5, value=2)))
      ),
      
      # ------------------------------------------------------------------------
      # PERÍODO DA ST (YYYYMMDD)
      # ------------------------------------------------------------------------
      conditionalPanel(
        "input.ViewMode != 'AE' && input.ViewMode != 'LT2' && input.ViewMode != 'MMODEL'", 
        div(style="display:inline-block; ", 
            textInput("anoMesBeg", label=titSty(tl("Período")),
                      value = CFG$anomes.beg, width='110px')
        ),
        div(style="display:inline-block; ", 
            textInput("anoMesEnd", label = NULL, 
                      value = CFG$anomes.end, width='110px')
        )
      ),
      
      # ------------------------------------------------------------------------
      # PERÍODOS CONSOLIDADOS
      # ------------------------------------------------------------------------
      conditionalPanel(
        "input.ViewMode == 'LT2' || input.ViewMode == 'MMODEL'", 
        pickerInput("perId", label=titSty(tl("Período consolidado")), 
                    choices=CFG$pers, selected=CFG$pers[1])
      ),
      
      # ------------------------------------------------------------------------
      # TEMPORALIDADE DA ST
      # ------------------------------------------------------------------------
      conditionalPanel(
        "input.ViewMode == 'ST'", 
        div(style="height:100px",
            radioGroupButtons("groupTsId", label=titSty(tl("Temporalidade")), 
                              choiceNames=c(tl('D'),tl('S'),tl('M'),'S'),
                              choiceValues=c('Dia', 'Semana', 'Mês', 'Estação'), selected='Dia', 
                              direction = "horizontal", status = "primary")
        ),
        
        conditionalPanel(
          "input.groupTsId == 'Mês' || input.groupTsId == 'Estação'", 
          div(style="height:35px; margin:-15px 0px 0px 0px; ",
              prettyCheckbox("appendYear", tl('Ciclo'), value=F, status="success", fill=T)
          )
        )
      ),
      
      # ------------------------------------------------------------------------
      # ÁREAS (AGRUPAMENTO DE REGIÕES) (Radio)
      # ------------------------------------------------------------------------
      conditionalPanel(
        "input.ViewMode != 'AE'", 
        #style="border:1px solid red;", 
        #radioButtons(inputId="mainRegId", label=tl('Áreas'), 
        #             choiceNames=CFG$area0.names, 
        #             choiceValues=CFG$area0.ids, selected=CFG$area0.ids[1]),
        prettyRadioButtons(
          inputId="mainRegId", label=titSty(tl("Áreas")),
          choiceNames=CFG$area0.names, 
          choiceValues=CFG$area0.ids, selected=CFG$area0.ids[1],
          inline = F, status="danger", fill=T, width="100%"
        )
      ),
      
      # ------------------------------------------------------------------------
      # ST METRICAS CONTINUAS
      # ------------------------------------------------------------------------
      conditionalPanel(
        "input.ViewMode == 'ST'", 
        #style="border:1px solid blue;", 
        div(#style="height:80px;",
          pickerInput("contId", label = titSty(tl("Métricas")), 
                      choices=metrics.cont, selected = metrics.cont[1])
        )
      ),
      # ------------------------------------------------------------------------
      # ST METRICAS CONTINUAS
      # ------------------------------------------------------------------------
      
      conditionalPanel(
        "input.ViewMode == 'MMODEL'", 
        #style="border:1px solid blue;", 
        div(#style="height:80px;",
          pickerInput("contSpId", label = titSty(tl("Métricas espaciais")), 
                      choices=metrics.sp, selected = metrics.sp[1])
        ),
        # TODO:
        #prettyRadioButtons("spFilterType", tl("Filtro"),
        #                   choiceNames = c(tl('Tudo'), tl('Valor maior que'), tl("95% IC")), 
        #                   choiceValues = c('TUDO', 'VAL_MAIOR', "CI95"), selected='TUDO', 
        #                   inline=T, status="danger", fill=T, width="auto"),
        #conditionalPanel(
        #  "input.spFilterType == 'VAL_MAIOR'", 
        #  textInput("scorrThres", label=tl("Limiar SCORR 95% IC"), value =".349", width='150px'),
        #  sliderInput("numIsoLevels", label=tl("# Isolinhas"), value=0, min=0, max=5, step=1),
        #),
        
        conditionalPanel(
          "input.contSpId == 'SCORR'", 
          prettyCheckbox("coloredFLD", label=tl("Campo colorido"), value=T, status="success", fill=T, inline=T),
          prettyCheckbox("doFilterScorr", label=tl('Filtra SCORR'), value=F, status="success", fill=T, inline=T),
          conditionalPanel(
            "input.doFilterScorr", 
            textInput("scorrThres", label=tl("Limiar SCORR 95% IC"), value =".349", width='150px'),
            sliderInput("numIsoLevels", label=tl("# Isolinhas"), value=0, min=0, max=5, step=1),
          ),
          conditionalPanel(
            "input.numIsoLevels > 0", 
            prettyRadioButtons("isolineColor", label=tl('Cores isolinhas'), choices=c("Scale", "Black"), inline=T),
            prettyCheckbox("isolineLabels", label=tl('Mostra níveis'), value=F, inline=T)
          )
        )
      ),
      
      # ------------------------------------------------------------------------
      # LT METRICAS CONTINUAS
      # ------------------------------------------------------------------------
      conditionalPanel(
        "input.ViewMode == 'ST' || input.ViewMode == 'LT' || input.ViewMode == 'LT2' || input.ViewMode == 'Categ'", 
        prettyCheckbox("showVal", tl('Valores dos pontos'), value=F, status="success", fill=T),
        prettyCheckbox("showCI", tl('95% IC'), value=F, status="success", fill=T)
      ),
      
      
      conditionalPanel(
        "input.ViewMode == 'LT' || input.ViewMode == 'LT2' ", 
        div(
          style="margin:-15px 0 0 0; ",
          prettyCheckbox("doSkillScore", tl('Modo Skill Score'),
                         value=F, status="success", fill=T)
        ),
        conditionalPanel(
          "input.doSkillScore", 
          div(
            style="margin:-15px 0 0 0; ",
            textInput("modRefId", label=titSty("Referência"),
                      value ="GFS_30km", width='200px'))
        )
      ),
      
      # ========================================================================
      # CONTROLES DO SCORE CARD: ESCALA DE CORES
      # Se colocar tudo num conditional panel só um controle não vai para outra 
      # linha sozinho
      # ========================================================================
      conditionalPanel(
        "input.ViewMode == 'SC'", 
        div(style="display:inline-block;",
            #radioButtons(inputId="colScaleId", label=titSty(tl("Escala de Cores")), 
            #             choiceNames=c(tl('Gradiente do bias'), tl('Melhor vs. Pior')), 
            #             choiceValues=c('GRAD', 'FIXED'), selected='FIXED', width = "160px"),
            prettyRadioButtons(
              inputId="colScaleId", label=titSty(tl("Escala de Cores")),
              choiceNames=c(tl('Gradiente do bias'), tl('Melhor vs. Pior')), 
              choiceValues=c('GRAD', 'FIXED'), selected='FIXED', width="160px",
              status="primary", fill=T)
            
        ),
        div(style="display:inline-block; vertical-align:top; margin: 15px 0 0 0; ",
            plotOutput('ColorScale', height='70px', width="20px")
        )
      ),
      
      # ========================================================================
      # CONTROLES DA ANALISE ESPACIAL
      # Se colocar tudo num conditional panel só um controle não vai para outra 
      # linha sozinho
      # ========================================================================
      conditionalPanel(
        "input.ViewMode == 'AE'", 
        radioGroupButtons("aeTempoId", label=titSty(tl("Temporalidade")), 
                          choiceNames  = c(tl('Dia'), tl('Mês'), tl('Saz.'), tl('Chuv.')), 
                          choiceValues = c('D','M','S',"R"), selected='M', 
                          direction = "horizontal", status = "primary"),
        
        radioGroupButtons("aeTypeId", label=titSty(tl("Tipo")), 
                          choiceNames = c(tl('Prev vs. Obs'),'Bias (P-O)','SRMSE','SCORR'), 
                          choiceValues = c('PREVxOBS','BIAS',"SRMSE","SCORR"), selected='PREVxOBS',
                          direction="horizontal", status="danger"),
        # ABORTADO, MOSTRA TODOS OS MODELOS JÁ 
        #conditionalPanel(
        #  "input.aeTypeId == 'MMODEL'", 
        #  radioGroupButtons("aeTypeMMODEL", label=titSty(tl("Tipo Best Of")), 
        #                    choices = c("SBIAS",'SRMSE','SCORR'), selected='SBIAS',
        #                    direction="horizontal", status="warning"),
        #),
        
        # Daily Images -----------------------------------------------------------
        conditionalPanel(
          "input.aeTempoId=='D'", 
          sliderTextInput("dayImgId", label=titSty(tl('Dia')), 
                          choices =(0:-30), selected=0)
          
          # radioGroupButtons("dayImgId", label=titSty(tl("Dia")), 
          #                   choiceNames=(0:-30),
          #                   choiceValues=paste0("D-", 0:30), selected = "D-0", 
          #                   direction = "horizontal", status = "primary",
          #                   justified=T, individual=T)
        ),
        # Monthly Images ---------------------------------------------------------
        conditionalPanel(
          "input.aeTempoId=='M'", 
          uiOutput("monthImgControl")
        ),
        
        # Seasonal Images --------------------------------------------------------
        conditionalPanel(
          "input.aeTempoId=='S'", 
          uiOutput("seasonImgControl")
        ),
        
        # Rainy seasonal Images --------------------------------------------------
        conditionalPanel(
          "input.aeTempoId=='R'", 
          uiOutput("rainyImgControl")
        ),
        
        # SHOW REG for FIELDS ----------------------------------------------------
        div(style="height:35px; margin:-15px 0px 0px 0px; ",
            prettyCheckbox("showReg", tl(' quadrantes'), 
                           value=F, status="success", fill=T, width="auto", inline=T)
            
            
        )
      ), 
      # FIM DOS CONTROLE DE ANÁLISE ESPACIAL =====================================
      
      # INICIO MÉTRICA CATEGÓRICAS ===============================================
      # Histograma e análise categórica ------------------------------------------
      conditionalPanel(
        "input.ViewMode == 'Hist' || input.ViewMode == 'Categ'",
        div(#style="height:80px;",
          pickerInput("catId", label = titSty(tl("Métricas")), 
                      choices=metrics.cat, selected = "ETS")
        ),
        checkboxGroupButtons("thresId", label=titSty(tl('Categorias')), 
                             choices=THRES$PREC, selected=THRES$PREC, justified=F, individual=T, size="xs",
                             status = "success", 
                             checkIcon = list(
                               yes = icon("ok", lib = "glyphicon"),
                               no = icon("remove", lib = "glyphicon")))
      ),
      
      # Diagrama de performance --------------------------------------------------
      conditionalPanel(
        "input.ViewMode == 'Perf'", # Abas de visualização dos campos
        radioButtons(inputId="typeLegPerfDiag", label=titSty(tl('Tipo do ponto')),
                     choices=c(tl('Círculos'), tl('Símbolos')),
                     selected=tl('Círculos'), width = "90px")#,
        #plotOutput('PerfDiagLeg', height="145px", width="70px")
      ),
      
      # Mapinha --------------------------------------------------
      conditionalPanel(
        "input.ViewMode != 'AE'", 
        plotOutput(tl('PlotRegioes'), width="150px", height="220px")
      ),
      
      # BOTÃO DE HELP  --------------------------------------------------
      conditionalPanel(
        "input.ViewMode != 'AE' && input.ViewMode != 'MMODEL'", 
        prettyCheckbox("showHelp", 'Help',
                       value=F, status="success", fill=T)
      )
      
    ) # Condit panel pwd
  ), #dashboardSidebar
  
  
  # ============================================================================
  # BODY
  # ============================================================================
  body = dashboardBody(
    style="padding:0; margin:0;",
    # NÃO FUNCIONA
    #setBackgroundColor(
    #  color = c("#F7FBFF", "#2171B5"),
    #  gradient = "linear",
    #  direction = "bottom"
    #),
    shinyDashboardThemes(
      theme = "grey_dark" # purple_gradient
    ),
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="Styles.css")
    ),
    #tags$head(tags$style(".main-header .navbar{ margin-left: 0px !important;}")),
    # This is to align left header components
    tags$head(tags$style(".navbar-custom-menu, .main-header .navbar-right {float:left !important;}")),
    
    conditionalPanel(
      "input.pwd=='MNT'",
      
      # Begin of fluidRow #2 - Some control components =============================
      conditionalPanel(
        "input.ViewMode == 'ST' || input.ViewMode == 'LT' || input.ViewMode == 'LT2' ", 
        class="compo-line",
        fluidRow(
          column(
            12, 
            div(class="line-compo", #style="height:30px; ", 
                textInputAddon("ncolsContId", NULL, value=0, addon="# cols", width="100px"), #icon("at"))
            ),
            conditionalPanel(
              "input.ViewMode == 'ST'", 
              class="line-compo", style="border:0px solid white",
              div(class="def-gap", style="border:0px solid red"),
              div(class="line-cond-panel", style="border:0px solid blue",
                  titSty(tl("Resolução eixo-X"), extraClass = "line-text")
              ),
              div(class="line-compo", #style="height:35px", #style="border:1px solid; margin:0px 0px 0px 0px; padding:0px 4px 0px 2px; vertical-align:top; height:20px; ",
                  style="border:0px solid green",
                  radioGroupButtons("ticksMult", NULL, choices=c(1,2,5,10,15,30), selected=10, 
                                    direction = "horizontal", status = "primary")),
              div(class="line-cond-panel", style="border:0px solid purple",
                  titSty(paste0("(", tl("dias"), ")"), extraClass="line-text")
              )
            )
          )
        )
      ), # End of fluidRow #2 - Some control components =============================
      
      # Begin of fluidRow #3 - Region selector =====================================
      div(
        class="compo-line", 
        fluidRow(
          column(
            12, 
            # painel 1 -------------------------------------------------------------
            conditionalPanel(
              "input.ViewMode != 'AE'",
              class="line-cond-panel", 
              titSty(paste0(tl('Região'), ":"), extraClass="line-text-in-panel"),
              conditionalPanel(
                #3 = estados do brasil, vai mostrá-los por região
                "input.mainRegId == '3'", class="line-cond-panel", 
                div(class="line-compo-sml", #style="display:inline-block; border:1px solid; margin:0px 0px 0px 0px; padding:0px 4px 0px 2px; vertical-align:top; width:250px; height:20px; ",
                    #radioButtons("regBrId", label = NULL, 
                    #             choices=names(CFG$RegUf), selected=names(CFG$RegUf)[1], 
                    #             inline=T),
                    prettyRadioButtons("regBrId", NULL,
                                       choices=names(CFG$RegUf), selected=names(CFG$RegUf)[1],
                                       inline=T, status="primary", fill=T)
                )
              ),
              conditionalPanel(
                #"(input.ViewMode == 'ST' || input.ViewMode == 'Taylor' || input.ViewMode == 'Hist' || input.ViewMode == 'MMODEL') && input.mainRegId == '3'",
                "input.mainRegId == '3'",
                class="line-cond-panel",
                div(class="def-gap"),
                titSty(paste0(tl('Estados'), ":"), extraClass="line-text-in-panel"),
              ),
            ),
            
            # painel 2 -------------------------------------------------------------
            conditionalPanel(
              "input.ViewMode != 'AE'",
              class="line-cond-panel", 
              uiOutput("subRegsControl", class="line-compo-sml"), #, style="border:1px solid; margin:0px 0px 0px 0px; padding:0px 4px 0px 2px; vertical-align:top; height:20px; ") 
              conditionalPanel(
                "input.ViewMode != 'MMODEL'",
                class="line-compo-sml", #style="display:inline-block; border:1px solid; margin:0px 0px 0px 0px; padding:0px 4px 0px 2px; vertical-align:top; width:250px; height:20px; ",
                prettyCheckbox("facetReg", tl('Separar regiões'), 
                               value=F, status="success", fill=T)
              )
            ),
            
            # Inclui Oceano ====================================================
            conditionalPanel(
              "((input.ViewMode == 'ST' || input.ViewMode == 'Taylor' || input.ViewMode == 'Hist') && input.mainRegId == '9') ||
          (input.ViewMode == 'AE' || (input.ViewMode == 'MMODEL' && input.mainRegId == '1'))",
              class="line-cond-panel", 
              div(class="def-gap"),
              titSty(paste0(tl('Incluir oceano'), ":"), extraClass="line-text-in-panel"),
              div(class="line-compo-sml", #style="display:inline-block; border:1px solid; margin:0px 0px 0px 0px; padding:0px 4px 0px 2px; vertical-align:top; width:250px; height:20px; ",
                  prettyRadioButtons("maskId", NULL,
                                     choiceNames = c(tl('Não'),tl('Sim')), choiceValues=c('CONT', 'ALL'), selected='CONT', 
                                     inline=T, status="danger", fill=T, width="auto"
                  )
              )
            )
          ) # Column 12
        ), # fluidRow #3
      ), # ConditionalPanel region selector
      
      conditionalPanel(
        "input.pwd=='MNT'", 
        #style = "border:1px solid; overflow-x:hidden; overflow-y:scroll; height:620px; padding:0; margin:0;",
        style = "border:1px solid; overflow-x:hidden; overflow-y:scroll; height:1250px; padding:0; margin:0;",
        #      wellPanel(
        #style = "border:0px solid; overflow-x:hidden; overflow-y:scroll; height:620px; padding:0; margin:0;",
        #style = "border:0px solid; overflow-x:hidden; overflow-y:scroll; height:850px; padding:0; margin:0;",
        tabsetPanel(
          #title = NULL, width = 12,
          id = 'ViewMode', selected = 'ST',
          tabPanel(
            tl("Série Temporal"),
            value = 'ST',
            wellPanel(fluidRow(column(12, plotOutput('PlotTS', height="auto")))),
            wellPanel(
              style = "overflow-y:hidden; overflow-y:scroll; height:250px; ",
              fluidRow(column(12, htmlOutput("TabSummMetric")))
            )
          ), # tabPanel ST
          tabPanel(
            tl("Análise LT da ST"),
            value = 'LT',
            conditionalPanel(
              "input.showHelp===false", 
              wellPanel(fluidRow(column(6, plotOutput('PlotBiasVsLTime', height="400px")),
                                 column(6, plotOutput('PlotCorVsLTime', height="400px"))),
                        fluidRow(column(6, plotOutput('PlotRmseVsLTime', height="400px")),
                                 column(6, htmlOutput("TabSummLTime")))
              )
            ),
            conditionalPanel(
              "input.showHelp===true", 
              wellPanel(htmlOutput("TabHelpLTime"))
            )
          ), # tabPanel LT
          tabPanel(
            tl("Análise LT Espacial"),
            value = 'LT2',
            conditionalPanel(
              "input.showHelp===false",
              wellPanel(fluidRow(
                column(6, plotOutput('PlotSRMSExLT2', height="400px"),
                       plotOutput('PlotSCORRxLT2', height="400px")),
                column(6, htmlOutput("TabSummLT2")))
              )
            ),
            conditionalPanel(
              "input.showHelp===true",
              wellPanel(htmlOutput("TabHelpLT2"))
            )
          ), # tabPanel LT2
          tabPanel(
            "Taylor",
            value = 'Taylor',
            fluidRow(column(12, plotOutput("PlotTaylorDiag", height="800px")))
          ), # tabPanel Taylor
          tabPanel(
            tl("Análise Espacial"),
            value = 'AE',
            fluidRow(
              conditionalPanel(
                "input.aeTypeId=='PREVxOBS'",
                column(3, imageOutput('PlotObs', width='100%', height='100%')), 
                column(9,
                       wellPanel(
                         style = "border:1px solid; overflow-x:scroll; overflow-y:scroll; max-height:1250px; padding:0; margin:0;",
                         fluidRow(
                           column(3, imageOutput('PlotFct1', width='100%', height='100%')), 
                           column(3, imageOutput('PlotFct2', width='100%', height='100%')),
                           column(3, imageOutput('PlotFct3', width='100%', height='100%')),
                           column(3, imageOutput('PlotFct4', width='100%', height='100%'))
                         )
                       )
                )
              ),
              conditionalPanel(
                "input.aeTypeId!='PREVxOBS'",
                wellPanel(
                  style = "border:1px solid; overflow-x:scroll; overflow-y:scroll; max-height:1250px; padding:0; margin:0;",
                  column(3, imageOutput('PlotBias1', width='100%', height='100%')), 
                  column(3, imageOutput('PlotBias2', width='100%', height='100%')),
                  column(3, imageOutput('PlotBias3', width='100%', height='100%')),
                  column(3, imageOutput('PlotBias4', width='100%', height='100%'))
                )
              )
            ), # fluidRow
          ), # tabPanel Analise Espacial
          tabPanel(
            tl("Multi Model"),
            value = "MMODEL",
            fluidRow( 
              column(
                12, 
                plotOutput('PlotSpAnalisys', width='100%', height='100%')
              )
            )
          ),
          tabPanel(
            tl("Análise categórica"),
            value = 'Categ',
            wellPanel(plotOutput('PlotMetVsCat', height="800px")),
            conditionalPanel(
              "input.showHelp===true", 
              wellPanel(htmlOutput("TabHelpCat"))
            )
          ), # tabPanel ST
          tabPanel(
            tl("Performance"),
            value = 'Perf',
            wellPanel(plotOutput("PlotPerfDiag", height="auto")),
            conditionalPanel(
              "input.showHelp===true", 
              wellPanel(htmlOutput("TabHelpPerf"))
            )
          ) # tabPanel Performance Diagram
          
          # tabPanel(
          #   tl("Histogramas"),
          #   value = 'Hist',
          #   wellPanel(
          #     fluidRow(column(12, plotOutput('PlotHist', height="auto")))
          #   )
          
          #tabPanel(
          #  tl("Regiões"),   
          #  value = 'Reg',
          #  fluidRow(column(12, plotOutput('PlotMap')))
          #) # tabPanel Regioes
        ) # main tabSetPanel 
        #      ) # wellPanel
      ) # main conditionalPanel
    ) # condit panel pwd
  ) # dashBoardBody
  
  # ============================================================================
  # RIGHT SIDEBAR
  # ============================================================================
  #controlbar = dashboardControlbar()  
) # dashboardPage
