library(sp)
library(rgdal)
library(rgeos)


setwd('/Users/Henrik/Repositories/bopriskarta/')


wgs84.crs <- CRS("+init=epsg:4326")
sweref.crs <- CRS("+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

sweden <- readOGR(dsn="data/shp", layer="sweden-simplified")
sweden <- spTransform(sweden, sweref.crs)
municipalities <- readOGR(dsn='data/shp', layer='Kommun_Sweref99TM_region')
municipalities <- spTransform(municipalities, sweref.crs)

swe.hull <- gConvexHull(sweden)
swe.grid <- spsample(swe.hull, cellsize = c(100, 100), type = "regular")
gridded(swe.grid) <- TRUE

hulls <- list()
rasters <- list()
for (i in 1:length(municipalities)) {
	hull <- gConvexHull(municipalities[i, ])
	hull <- gBuffer(hull, width=3000, id=i)
	hulls <- c(hulls, hull)
	raster <- swe.grid[hull]
	rasters <- c(rasters, raster)
}
hulls <- as(do.call(rbind, hulls), "SpatialPolygonsDataFrame")
rasters <- as(do.call(rbind, rasters), "SpatialPixelsDataFrame")



