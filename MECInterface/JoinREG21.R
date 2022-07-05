library(data.table)
rm(list=ls())
dd=1

# Para REG21 vão todos os modelos exceto WRFG (versão antiga)
# Para WRF  vão os WRFs e o GFS
feed_modes = c("REG21", "MEC_WRF")
feed_mode = feed_modes[1]
for (feed_mode in feed_modes) {
  print("")
  print(paste0(feed_mode, " ==> EVAL BY DAY ================================================================="))
  for (dd in 1:9) {
    cat("\n\nCONT Day:", dd, "\n")
    EV.CONT21 = data.table()
    EV.CAT21 = data.table()
  
    t="P"
    for (t in c("P", "T", "UVP")) {
       load(paste0("REG21", t,"/EVAL_CONT_", dd,"_00.RData"))
       if (feed_mode == "REG21") {
         EVAL.CONT = EVAL.CONT[MOD != "WRFG_07km"]
       } else {
         EVAL.CONT = EVAL.CONT[MOD %in% c("BAM_20km", "GFS_30km", "WRFG_07km", "WRF_07km")]
       }
       cat(t, ": ", nrow(EV.CONT21), " + ", nrow(EVAL.CONT), " = ", sep="")
       EV.CONT21 = rbind.data.frame(EV.CONT21, EVAL.CONT)
       cat(nrow(EV.CONT21), "\n")
    }
    cat("Saving day", dd)
    good = colnames(EV.CONT21)[!grepl("(U|L)CI", colnames(EV.CONT21))]
    EVAL.CONT = EV.CONT21[, good, with=F]
    setkey(EVAL.CONT, NULL)
    setkey(EVAL.CONT)
    save(EVAL.CONT, file = paste0(feed_mode, "/EVAL_CONT_", dd,"_00.RData"))
  
    # TODO: Unir quando o plot das categorias estiverem prontas
    #cat("\n\nCAT Day:", dd, "\n")
    #load(paste0("REG21P/EVAL_CAT_", dd,"_00.RData"))
    #cat(t, ": ", nrow(EV.CAT21), " + ", nrow(EVAL.CAT), " = ", sep="")
    #EV.CAT21 = rbind.data.frame(EV.CAT21, EVAL.CAT)
    #cat(nrow(EV.CAT21), "\n")
    #save(EVAL.CAT, file = paste0("REG21/EVAL_CAT_", dd,"_00.RData"))
  }
  
  print("")
  print(paste0(feed_mode, " ==> EVAL BY PERIOD - ONLY ONE DS FOR ALL LTIMES ================================="))
  EV.CONT21=data.table()
  dd=1
  for (dd in 1:9) {
    cat("\n\nDay:", dd, "\n")
    
    t="P"
    for (t in c("P", "T", "UVP")) {
      load(paste0("REG21", t,"/EVAL_CONT_PER_", dd,"_00.RData"))
      if (feed_mode == "REG21") {
        EVAL.CONT.PER = EVAL.CONT.PER[MOD != "WRFG_07km"]
      } else {
        EVAL.CONT.PER = EVAL.CONT.PER[MOD %in% c("BAM_20km", "GFS_30km", "WRFG_07km", "WRF_07km")]
      }
      cat(t, ": ", nrow(EV.CONT21), " + ", nrow(EVAL.CONT.PER), " = ", sep="")
      # Evita colunas de rate, do BESTOF, por enquanto
      #good = colnames(EVAL.CONT.PER)[!grepl("RATE", colnames(EVAL.CONT.PER))]
      #EV.CONT21 = rbind.data.frame(EV.CONT21, EVAL.CONT.PER[, ..good])
      EV.CONT21 = rbind.data.frame(EV.CONT21, EVAL.CONT.PER)
      cat(nrow(EV.CONT21), "\n")
    }
    #cat("Saving day", dd)
    #EVAL.CONT.PER = EV.CONT21
    #save(EVAL.CONT.PER, file = paste0(feed_mode, "/EVAL_CONT_PER_", dd,"_00.RData"))
  }
  # CONT PER É UM SÓ POIS É PEQUENO
  good = colnames(EV.CONT21)[!grepl("(U|L)CI", colnames(EV.CONT21))]
  EVAL.CONT.PER = EV.CONT21[, good, with=F]
  setkey(EVAL.CONT.PER, NULL)
  setkey(EVAL.CONT.PER)
  cat(" = ", nrow(EVAL.CONT.PER), "\n")
  save(EVAL.CONT.PER, file = paste0(feed_mode, "/EVAL_CONT_PER_00.RData"))
}   
