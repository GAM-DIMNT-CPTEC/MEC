* Menor domínio em 1o.
'OPEN_COMM CTL_MODEL'
'set lon LONGITUDE'
'set lat LATITUDE'

* Acumula o modelo
'ACC_COMMAND'

* Maior domínio em 2o.
'open CTL_OBS'

* Regrida modelo na grade da OBS com lterp(src, dest)
'FCTNEW=lterp(FCT(t=1),lat.2(t=1))'

* Grava saída regridada em GRIB2
'set dfile 2'
'./GS_g2grb_prec FCTNEW DIR_OUT/MODELO.grib2 d=DT:APCP:0 m above ground:anl'

'quit'
~                                                                                                                                
~                    
