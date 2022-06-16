* Menor domínio em 1o.
'open /dados/grpeta/dsk003/dados/Eta8km_tempo/binctl/2020122500/2D/Eta_8km_BMJ_FER_2020122500_2D.ctl'
'set lon LON'
'set lat LAT'

* Acumula o modelo
'ACC_COMMAND'

* Maior domínio em 2o.
'open CTL_OBS'

* Regrida modelo na grade da OBS com lterp(src, dest)
'FCTNEW=lterp(FCT(t=1),lat.2(t=1))'

* Grava saída regridada em GRIB2
'set dfile 2'
'./GS_g2grb_prec FCTNEW DIR_OUT/MODELO.grib2 d=DT:APCP:0 m above ground:anl'

* Grava obs regridada em GRIB2
'PP=prec(t=1)'
'./GS_g2grb_prec PP DIR_OUT/OBS_NEW.grib2 d=dt_obs:PREC:surface:anl'

'quit'
~                                                                                                                                
~                    
