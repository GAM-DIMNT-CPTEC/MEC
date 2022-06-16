* Menor domínio em 1o.
'open /dados/dmdpesq/roberto.garcia/MEC_DATA/REG21/TEMP/MERGE_PREC_2021010112.ctl'
'set lon 270.05 339.85'
'set lat -54.95 19.35'

* Maior domínio em 2o.
'open /oper/ioper/tempo/GFS/0p25/brutos/2020/12/31/00//gfs025gr.pgrb2.2020123100.ctl'

* Acumula o modelo
'FCT=prec.2(t=13)-prec.2(t=5)'

* Regrida modelo na grade da OBS com lterp(src, dest)
'FCTNEW=lterp(FCT(t=1),lat.1(t=1))'

* Grava saída regridada em GRIB2
'./GS_g2grb_prec FCTNEW ./GFS_30km_PREC_036_2021010100.grib2 d=2021010100:APCP:0 m above ground:anl'

* Grava obs regridada em GRIB2
'PP=precsfc.1(t=1)'
'./GS_g2grb_prec PP ./MERGE_PREC_2021010112.grib2 d=2021010112:PREC:surface:anl'

'quit'
~                                                                                                                                
~                    
