dset ^GFS_25.grib2
index ^GFS_25.grib2.idx
undef 9.999E+20
title GFS_25.grib2
* produced by g2ctl v0.0.8.2
* command line options: GFS_25.grib2
* griddef=1:0:(1441 x 721):grid_template=0:winds(N/S): lat-lon grid:(1441 x 721) units 1e-06 input WE:SN output WE:SN res 48 lat -90.000000 to 90.000000 by 0.250000 lon 0.000000 to 0.000000 by 0.250000 #points=1038961:winds(N/S)

dtype grib2
ydef 721 linear -90.000000 0.25
xdef 1441 linear -360 0.250000
tdef 1 linear 00Z01mar2021 1mo
zdef 1 linear 1 1
vars 1
PRECsfc  0,1   0,15,5 ** surface Precipitation [kg/m^2]
ENDVARS
