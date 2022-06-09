# =================================================
# Definition of MCD (Maximum Common Domain)
# Se deixar automático o grads pode gerar uma observação além dos limites
# dos modelos, então....
# OBS: Isso deve ser feito para cada observação
# =================================================
# 1) Read all model and observed domains from a file and write LON and LAT MCDs
Rscript R_GetMCD.R ${MEC_eval_name}
MEC_MCD_lon=`cat ${MEC_D_cfg}/${MEC_eval_name}_MCD_LON.txt`
MEC_MCD_lat=`cat ${MEC_D_cfg}/${MEC_eval_name}_MCD_LAT.txt`

# NO GRADS
# ga-> open MERGE_PREC_2021030412.ctl
# Setar LON e LAT de acordo com o conteúdo dos arquivos acima
# ga-> set lon 270 339.92
# ga-> set lat -55 19.4
# ga-> q dims
# X is varying   Lon = 270 to 339.92   X = 301.5 to 1000.7
# Y is varying   Lat = -55 to 19.4   Y = 51.5 to 795.5
# Diminuir todas as bordas para o inteiro anterior ou próximo 
# ga_> set x 302 1000
# LON set to 270.05 339.85 
# ga-> set y 52 795
# LAT set to -54.95 19.35 
# E capturar os novos LONs (270.05 339.85)e LATs (-54.95 19.35)
# E usá-los ao abrir os gribs
