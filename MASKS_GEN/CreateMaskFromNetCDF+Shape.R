args <- commandArgs(trailingOnly=T)
if (length(args) != 1) {
  cat("\nUse:\n")
  cat("\nRscript CreateMaskFromNetCDF+Shape.R <EVAL_NAME>\n")
  cat("\n<EVAL_NAME>   A name for the evaluation, root directory of the main output directory\n\n")
  stop("Wrong number of argments!")
}

library(spatial)  # over()
library(maptools) # map()
library(raster)   # shapefile() and coordinates()
library(ncdf4)    # over()
rgdal::set_thin_PROJ6_warnings(F) 

do.plot <- F
plot.num <- 1

if (do.plot) {
  library(fields)  # image()
  dev.off()
  dev.new()
}
MEC_eval_name=args[1]   # REG21 | Eta | ...
source(paste0("../LoadCommonFunctions.R"))
source(paste0("../", MEC_eval_name, "/EVAL_SET.R"))

nc_templ_path = paste0(MEC_D_root, "/DATA_OBS/NCDF")
nc_templ_fname = sort(dir(nc_templ_path, "*.nc", recursive=T))[1]
file.nc <- nc_open(paste0(nc_templ_path, "/", nc_templ_fname))
lon <- ncvar_get(file.nc, varid = 'lon')
lat <- ncvar_get(file.nc, varid = 'lat')
lon <- lon[lon>180] <- lon-360

# ==============================================================================
cat('\nReading shapefiles ... ')
# ==============================================================================
shape.world <- shapefile(paste0(MEC_D_shp, '/countries.shp'))
shape.br.reg <- shapefile(paste0(MEC_D_shp, '/BR/regioes/regioes_2010.shp'))
shape.br.uf <- shapefile(paste0(MEC_D_shp, '/BR/estados/estados_2010.shp'))
cat('Ok!\n')
# ==============================================================================
dominio <- '1-South America'
# ==============================================================================
# Load contries' names
countries <- readLines(con='ListSACountries.txt')
countries <- countries[substring(countries,1,1) %in% LETTERS] # wipe out comments
# Include countries that wont be evaluated individually but must be part of the SA
idx.as <- which(shape.world$COUNTRY %in% c(countries, "Suriname", "French Guiana", "Guyana"))
#idx.as <- which(shape.world$COUNTRY %in% countries) # keep only countries in list
#idx.as <- which(shape.world$CONTINENT == substring(dominio, 3)) # keep only countries in list
shape.as <- shape.world[idx.as, ]

cat('\n', dominio, ': ', sep='')
shape.item <- shape.as
mask.name <- dominio
source('CreateMaskFromNetCDF+Shape_Itens.R')

# ==============================================================================
dominio <- '2-Brazil'
# ==============================================================================
cat('\n', dominio, ': ', sep='')
shape.item <- shape.as[which(shape.as$COUNTRY == 'Brazil'), ]
mask.name <- dominio
source('CreateMaskFromNetCDF+Shape_Itens.R')

# ==============================================================================
# 03-BR_UP, 04-BR_REG, 05-AS_PAIS
dominios <- c('BR_UF', 'BR_REG', 'PAIS')
# ==============================================================================
dominio=dominios[1]
for (dominio in dominios) {
  cat('\n', dominio, ':', sep='')

  if (dominio == 'BR_UF') {  
    shape.dom <- shape.br.uf
    attrib <- 'sigla'
    ordem <- '3'
  }
  if (dominio == 'BR_REG') {  
    shape.dom <- shape.br.reg
    attrib <- 'sigla'
    ordem <- '4-REG'
  }
  if (dominio == 'PAIS') {  
    #shape.dom <- shape.as[which(shape.as$COUNTRY != 'Brazil'), ] # Brazil is already done above.
    shape.dom <- shape.as[!shape.as$COUNTRY %in% c('Brazil', "Suriname", "French Guiana", "Guyana"), ] # Brazil is already done above.
    attrib <- 'COUNTRY'
    ordem <- '5'
  }
  itens <- shape.dom[[attrib]]
  max.len <- max(nchar(itens))
  item=itens[1]
  for (item in itens) {
    cat('\n  - ', f.PadR(item, max.len, pch='.'), ': ', sep='')
    idx.item <- which(shape.dom[[attrib]] == item) # keep only current item's shape: a country, a br_region or a br_uf
    shape.item <- shape.dom[idx.item, ]
    mask.name <- paste0(ordem, '-', item)
    source('CreateMaskFromNetCDF+Shape_Itens.R')
  }
  cat('\n')
}

