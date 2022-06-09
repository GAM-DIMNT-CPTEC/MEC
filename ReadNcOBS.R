# ==============================================================================
# Read observation NetCDF files
# ------------------------------------------------------------------------------
# Jose Roberto M. Garcia (garcia.cptec@gmail.com)
# DMD / CPTEC / INPE / MCTI
# ==============================================================================
if (CFG.SET$obs1 == "MERGE") {
  fname.obs <- paste0(CFG.SET$obs1, '_', CFG$var, '_', paste0(substring(CFG$dt.str,1,8), "12"))
} else {
  fname.obs <- paste0(CFG.SET$obs1, '_', CFG$var, '_', paste0(substring(CFG$dt.str,1,8), CFG$init_time))
}

# Se for avaliação diária, coloca o YYYYMM como diretório para buscar o arquivo
if (CFG$etype=='daily') {
  fname.obs.full <- paste0(DIR$obs, substr(CFG$dt.str, 1, 6), '/', fname.obs, '.nc')
} else {
  fname.obs.full <- paste0(DIR$obs, fname.obs, '.nc')
}

if (file.exists(fname.obs.full)) {
  nc.obs <- nc_open(fname.obs.full)
  
  if (any(names(nc.obs$dim)=='longitude')) {
    CFG.SET$lon <- ncvar_get(nc.obs, varid = "longitude")
  } else {
    CFG.SET$lon <- ncvar_get(nc.obs, varid = "lon")
  }
  CFG.SET$lon[CFG.SET$lon > 180] <- CFG.SET$lon - 360   # Ajusta lon 0..360 para -180..+180
  CFG.SET$lon.obs <- CFG.SET$lon
  CFG$lon <- CFG.SET$lon
  if (any(names(nc.obs$dim)=='latitude')) {
    CFG.SET$lat <- ncvar_get(nc.obs, varid = "latitude")
  } else {
    CFG.SET$lat <- ncvar_get(nc.obs, varid = "lat")
  }
  CFG$lat <- CFG.SET$lat
  CFG.SET$lat.obs <- CFG.SET$lat
  CFG.SET$lon.range <- range(CFG.SET$lon)
  CFG.SET$lat.range <- range(CFG.SET$lat)
  CFG.SET$lon.range[CFG.SET$lon.range > 180] <- CFG.SET$lon.range - 360   # Ajusta lon 0..360 para -180..+180
  #f.Cat('A=', length(CFG.SET$lon), 'x', length(CFG.SET$lat), ', ')
  
  CFG.SET$time.obs <- ncvar_get(nc.obs,"time")
  time.units <- unlist(strsplit(ncatt_get(nc.obs,"time","units")$value, ' '))
  CFG.SET$time.unit.obs <- time.units[1]
  CFG.SET$time.ref.obs <- as.Date(time.units[3], format="%Y-%m-%d", tz = "UTC")
  if (CFG.SET$time.unit.obs=='days') {
    CFG.SET$time.scale.obs <- 1
  } else {
    if (CFG.SET$time.unit.obs=='hours') {
      CFG.SET$time.scale.obs <- 24
    } else {
      if (CFG.SET$time.unit.obs == 'seconds') {
        CFG.SET$time.scale.obs <- 24 * 60 * 60
      }
    }
  }
  f.SaveCfgSet()
  
  #f.Cat('T=', length(CFG.SET$time.obs), ', ')
  #f.Cat('V=', CFG$var)
  NC.OBS <- ncvar_get(nc.obs, varid = CFG$var)
  
  # Não há chuva negativa, undeff (que é um número negativo grande) atrapalha
  # Transformando todos em NA
  if (CFG$var == 'PREC') {
   NC.OBS[NC.OBS < 0] <- NA
  }
}
