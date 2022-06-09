# Get all coords of the polygon in order to save time when spatializing
cat('coords, ')
polys = attr(shape.item,'polygons')
npolys = length(polys)
coords <- data.frame()
for (i in 1:npolys){
  poly = polys[[i]]
  polys2 = attr(poly,'Polygons')
  npolys2 = length(polys2)
  for (j in 1:npolys2){
    coords <- rbind.data.frame(coords, coordinates(polys2[[j]]))
  }
}

#Set the projection of the SpatialPointsDataFrame using the projection of the shapefile
cat('spatializing, ')
# Creates a DF with all LONs and LATs of the spatial domain
idx.lon <- which(lon >= min(coords$V1) & lon <= max(coords$V1))
idx.lat <- which(lat >= min(coords$V2) & lat <= max(coords$V2))

# Expande area do item em um PG pois pode ter cortado regiao
if (idx.lon[1] != 1) idx.lon <- c(idx.lon[1]-1, idx.lon)
if (idx.lat[1] != 1) idx.lat <- c(idx.lat[1]-1, idx.lat)
if (idx.lon[length(idx.lon)] != length(lon)) idx.lon <- c(idx.lon, idx.lon[length(idx.lon)]+1)
if (idx.lat[length(idx.lat)] != length(lat)) idx.lat <- c(idx.lat, idx.lat[length(idx.lat)]+1)

lon2 <- lon[idx.lon]
lat2 <- lat[idx.lat]
grid <- expand.grid(LON=lon2, LAT=lat2)
coordinates(grid) <- ~ LON + LAT
proj4string(grid) <- proj4string(shape.item)

# Tests rather each point is over the map
cat('checking overlap, ')
res <- over(grid, shape.item)          # DEMORA DE ACORDO COM A MATRIZ
#idx.cont <- which(!is.na(res$OBJECTID)) # Get the index of only the is.over() ones
idx.cont <- which(!is.na(res[, 1]))  # Get the index of only the is.over() ones

cat('building spatial matrix, ')
grid.pos <- as.matrix(expand.grid(LON=1:length(lon2), LAT=1:length(lat2)))
grid.pos <- grid.pos[idx.cont, ]

# Teste de posicionamento, NÃO APAGAR
#plot(shape.item); box(); axis(1); axis(2)
#abline(v=range(coords$V1), col='blue', lwd=3); abline(h=range(coords$V2), col='blue', lwd=3); 
#abline(v=range(lon2), col='red'); abline(h=range(lat2), col='red')

MASK.CONT <- matrix(NA, nrow=length(lon), ncol=length(lat)) # matrix do dominio total
MASK2 <- matrix(NA, nrow=length(lon2), ncol=length(lat2))   # matrix do dominio do pais
MASK2[grid.pos] <- 0
MASK.CONT[idx.lon, idx.lat] <- MASK2                        # Sobrepoe

cat('saving ... ')
save(MASK.CONT, file=paste0(MEC_D_masks, "/MASK_CONT_AS_", mask.name,'_0.1.RData'))
cat('Ok!')

#if (do.plot) image(MASK.CONT, add=substring(mask.name,1,1)!='1', col=as.numeric(substring(mask.name,1,1)))
if (do.plot) image(MASK.CONT, add=substring(mask.name,1,1)!='1', col=plot.num)
plot.num <- plot.num + 1

