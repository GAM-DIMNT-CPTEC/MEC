'sdfopen GFS_30km_PREC_180_2021011100.nc'
'q ctlinfo'
lin=sublin(result, 5)
grid=subwrd(lin, 5)
lin=sublin(result, 10)
var=subwrd(lin, 4)
*'set display color white'
*'set parea 0 10 0 10'
*'set vpage 2 9 0 8.5'
'set xsize 600 600'
'set stat on'
'd 'var
lin=sublin(result, 8)
min=subwrd(lin, 4)
max=subwrd(lin, 5)
lin=sublin(result, 11)
mean=subwrd(lin, 2)
'q dim'
lin=sublin(result, 2)
lon1=subwrd(lin, 6)
lon2=subwrd(lin, 8)
lin=sublin(result, 3)
lat1=subwrd(lin, 6)
lat2=subwrd(lin, 8)
'set stat off'
'c'
'd 'var
'draw title GFS_30km_PREC_180_2021011100.nc 'var' ('grid')'
'set string 1 c 2'
'draw string 5.5 0.3 VAR: 'var
'set string 1 r 2'
'set strsiz 0.2'
'draw string 2.5 4.5 max: 'max
'draw string 2.5 4.0 avg: 'mean
'draw string 2.5 3.5 min: 'min
'draw string 2.5 2.5 'lon1' 'lon2
'draw string 2.5 2.0 'lat1' 'lat2
'printim MODN.png'
'quit'
