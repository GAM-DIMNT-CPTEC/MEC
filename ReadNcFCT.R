# ==============================================================================
# Read model NetCDF files
# ------------------------------------------------------------------------------
# Jose Roberto M. Garcia (garcia.cptec@gmail.com)
# DMD / CPTEC / INPE / MCTI
# ==============================================================================
#t2 <- Sys.time()
#f.Cat('\n- ReadNcFCT: ')
fname.fct <- paste0(CFG$model, '_', CFG$var, '_', CFG$ltime, '_', CFG$dt.str, CFG$hh)

if (CFG$etype=='daily') {
  fname.fct.full <- paste0(DIR$mod, substr(CFG$dt.str, 1, 6), '/', CFG$init_time, "/", fname.fct, '.nc')
} else {
  fname.fct.full <- paste0(DIR$mod, fname.fct, '.nc')
}

if (file.exists(fname.fct.full)) {
  nc.fct <- nc_open(fname.fct.full)
  
  if (any(names(nc.fct$dim)=='longitude')) {
    CFG.SET$lon <- ncvar_get(nc.fct, varid = "longitude")
  } else {
    CFG.SET$lon <- ncvar_get(nc.fct, varid = "lon")
  }
  CFG.SET$lon[CFG.SET$lon > 180] <- CFG.SET$lon - 360   # Ajusta lon 0..360 para -180..+180
  CFG.SET$lon.fct <- CFG.SET$lon
  CFG$lon <- CFG.SET$lon
  if (any(names(nc.fct$dim)=='latitude')) {
    CFG.SET$lat <- ncvar_get(nc.fct, varid = "latitude")
  } else {
    CFG.SET$lat <- ncvar_get(nc.fct, varid = "lat")
  }
  CFG$lat <- CFG.SET$lat
  CFG.SET$lat.fct <- CFG.SET$lat
  CFG.SET$lon.range <- range(CFG.SET$lon)
  CFG.SET$lat.range <- range(CFG.SET$lat)
  CFG.SET$lon.range[CFG.SET$lon.range > 180] <- CFG.SET$lon.range - 360   # Ajusta lon 0..360 para -180..+180
  #f.Cat('A=', length(CFG.SET$lon), 'x', length(CFG.SET$lat), ', ')
  
  CFG.SET$time.fct <- ncvar_get(nc.fct,"time")
  time.units <- unlist(strsplit(ncatt_get(nc.fct,"time","units")$value, ' '))
  CFG.SET$time.unit.fct <- time.units[1]
  CFG.SET$time.ref.fct <- as.Date(time.units[3], format="%Y-%m-%d", tz = "UTC")
  if (CFG.SET$time.unit.fct=='days') {
    CFG.SET$time.scale.fct <- 1
  } else {
    if (CFG.SET$time.unit.fct=='hours') {
      CFG.SET$time.scale.fct <- 24
    } else {
      if (CFG.SET$time.unit.fct == 'seconds') {
        CFG.SET$time.scale.fct <- 24 * 60 * 60
      }
    }
  }
  f.SaveCfgSet()
  NC.FCT <- ncvar_get(nc.fct, varid = CFG$var)
  
  # Os modelos podem gerar valores negativo muito pequenos para chuva, que devem ser postos como 0
  if (CFG$var == 'PREC') {
     NC.FCT[NC.FCT < 0] <- 0
  }
} 
