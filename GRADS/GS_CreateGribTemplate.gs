function main(args)
ctl=subwrd(args,1)
var=subwrd(args,2)
out=subwrd(args,3)
data=subwrd(args,4)

'open 'ctl 
'VAR='var'(t=1)'
'GS_g2grb_prec VAR 'out'.grib2 d='data':PREC:surface:anl'
'quit'
return
