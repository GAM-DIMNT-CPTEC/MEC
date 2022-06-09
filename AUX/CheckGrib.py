import pygrib

# Open a grib/grib2 file
file = "/dados/dmdpesq/roberto.garcia/MEC_DATA/REG21_GFS/2020/12/22/00/gfs.t00z.pgrb2.0p25.f276.2020122200.grib2"

gr = pygrib.open(file)

# Print an inventory of file
for g in gr:
    print(g)


#The process will print out each record stored in the file, similar to the output you would see from using the wgrib/wgrib2 programs.
#To print some more detailed variable attributes, such as variable name, level, coordinate level, valid date, etc. :

#for g in gr:
#  print (g.typeOfLevel, g.level, g.name, g.validDate, g.analDate, g.forecastTime)
