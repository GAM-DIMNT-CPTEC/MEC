def_cfg_method=NA
if (!"MEC_eval_name" %in% ls()) {
  MEC_eval_name=Sys.getenv("MEC_eval_name")  # needs export MEC_eval_name=xxx before
}
fname_defcfg=paste0(MEC_eval_name, '/LastEvalCfg.R')
if (!"MEC_dont_check_args" %in% ls() || !MEC_dont_check_args) {
  # Checks if arguments are correct
  if (interactive()) {
    if (!"MEC_eval_name" %in% ls() || MEC_eval_name=="") {
      stop("Variable MEC_eval_name is not defined in the environment!")
    }
    if (!file.exists(fname_defcfg)) {
      stop(paste0("File ", fname_defcfg, " does not exist!"))
    }
    def_cfg_method="source"
  } else {
    args <- commandArgs(trailingOnly=T)
    if (length(args) == 1) { # MEC_eval_name as argument is enough
      if (!file.exists(fname_defcfg)) {
        stop(paste0("File ", fname_defcfg, " does not exist!"))
      }
      def_cfg_method="source"
      MEC_eval_name=args[1]
    } else {
      if (length(args) %in% (2:6)) {
        cat("\nMEC - Model Evaluation Comparator\n")
        cat("At least the evaluation name must be informed or all 6 arguments:\n")
        cat(". Rscript <script>.R <EVAL_NAME>\n")
        cat(". Rscript <script>.R <EVAL_NAME> <MOD> <VAR> <LTIME> <PER> <OBS> <EVAL_MODE>\n\n")
        stop("Wrong number of arguments!")
      } else {
        def_cfg_method="comm_line"
        MEC_eval_name=args[1]
      }
    }
  }
}
source(paste0(MEC_eval_name, "/EVAL_SET.R"))
source('LoadCommonFunctions.R')
source('LoadCommonDSs.R')
source('LoadPkgs.R')

if (!is.na(def_cfg_method) && def_cfg_method == "comm_line") {
  # "dt" é recebida no tipo string no formato YYYYMMDDHH via script 05-Eval.ksh e
  # no formato YYYYMMDD via LastEvalCfg.R, por isso é convertida para 8 chars sempre
  CFG$eval_name <- args[1]
  CFG$model <- args[2]; CFG$var <- args[3]; CFG$ltime <- args[4]; 
  CFG$dt <- substring(args[5],1,8); CFG$hs_run <- substring(args[5], 9, 10); 
  CFG$obs <- args[6]; CFG$eval_mode = args[7]
} else {
  source(fname_defcfg)
  CFG$hs_run = CFG$init_time
}

CFG$verb = T
if (class(CFG$dt) == "Date") {
  CFG$dt.str = format(CFG$dt, format="%Y%m%d")
} else {
  CFG$dt.str = CFG$dt
}
CFG$dt = as.Date(CFG$dt, format='%Y%m%d') # Converte "dt" para tipo Date
CFG$hh = CFG$hs_run
CFG$init_time = CFG$hs_run
#f.Cat("* DT detected: ", as.character(CFG$dt), " ", CFG$dt.str, "\n")

# Qtd de dias do mes (quando CFG$dt.str for string)
#CFG$num.days <- as.numeric(format(seq(as.Date(substr(CFG$dt.str,1,8), format='%Y%m%d'), by = "month", length = 2)[2]-1, '%d'))
# Qtd de dias do mes (quando CFG$dt.str for POSIX)
day1 <- as.Date(paste0(year(CFG$dt),'-',sprintf('%02d', month(CFG$dt)), '-01'))
day2 <- seq.Date(day1, by="month", length=2)[2]-1
CFG$num.days <- mday(seq.Date(day1, by="month", length=2)[2]-1)
CFG$dates    <- seq.Date(day1, day2, by='day')

source('LoadCfgSet.R') 

if (CFG.SET$obs1 == "MERGE") {
  hh.targ <- "12"
} else {
  hh.targ <- CFG$hh
}
