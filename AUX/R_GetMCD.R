args <- commandArgs(trailingOnly=T)
eval_name=args[1]
dir_files="CONFIG/"
DOMAINS=read.csv(paste0(dir_files, eval_name, "_DOMAINS.txt"), header=T, sep=";")
writeLines(paste0(max(DOMAINS$LON1), " ", min(DOMAINS$LON2)), con=paste0(dir_files, eval_name, "_MCD_LON.txt"))
writeLines(paste0(max(DOMAINS$LAT1), " ", min(DOMAINS$LAT2)), con=paste0(dir_files, eval_name, "_MCD_LAT.txt"))
