#source(paste0(MEC_eval_name, "/EVAL_SET.R"))
source('LoadCommonFunctions.R')
#source('LoadCommonDSs.R')
source('LoadPkgs.R')
source('LoadCfgSet.R') 

CFG$verb     = T
CFG$dt       = as.Date(CFG$val_dt1, format='%Y%m%d')
CFG$dt.str   = format(CFG$dt, format="%Y%m%d")
CFG$hh       = CFG$init_time
CFG$day1     = as.Date(CFG$val_dt1, format="%Y%m%d")
CFG$day2     = as.Date(CFG$val_dt2, format="%Y%m%d")
CFG$num.days = CFG$day2 - CFG$day1 + 1
CFG$dates    = seq.Date(CFG$day1, CFG$day2, by='day')

if (CFG.SET$obs1 == "MERGE") {
  hh.targ = "12"
} else {
  hh.targ = CFG$hh
}
