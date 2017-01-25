library(sp)
library(plyr)
library(raster)
library(ggplot2)
library(ggmap)
library(rgdal)
library(automap)
library(FRK)


setwd("/Users/Henrik/Repositories/bopriskarta/")

load("data/Rdata/sold-aggregate.Rdata")


##  Create spatial points object in WGS 84
wgs84.crs <- CRS("+init=epsg:4326")
coordinates(sold.agg) <- ~longitude+latitude
proj4string(sold.agg) <- wgs84.crs

##  Project to SWEREF99
sweref.crs <- CRS("+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
sold.sweref <- spTransform(sold.agg, sweref.crs)


##  Create output grid
#convex_hull <- chull(coordinates(df)[,1], coordinates(df)[,2])
#convex_hull <- c(convex_hull, convex_hull[1]) # Close the polygon
#d <- Polygon(coordinates(df)[convex_hull, ]) 
d <- readOGR(dsn="data/shp/", layer="sthlm-ch-buf")
d <- spTransform(d, sweref.crs)
out <- spsample(d, 60000, type = "regular")
gridded(out) = TRUE


##  Filter data by chosen output grid
sold.lan <- sold.sweref[d, ]
df <- sold.lan[sold.lan$soldDate > as.POSIXct("2015-01-01"), ]
df <- df[df$objectType == 4, ]  # Apartments


## Krige using autoKrige
xx <- autoKrige(indexAdjustedSoldPrice ~ 1, df[sample(length(df), 10000), ], out)
xx$krige_output$id <- 1:length(xx$krige_output)


##  Krige using Fixed Rank Kriging
crs(df) <- NA  # Bug in FRK requires proj4string to be unset
S <- FRK(f = indexAdjustedSoldPrice ~ 1,
		 list(df),
		 cellsize = c(100, 100),
		 n_EM = 100)
Pred <- SRE.predict(SRE_model = S, obs_fs = TRUE)
proj4string(df) <- sweref.crs
proj4string(Pred) <- sweref.crs
Pred@data$id <- 1:length(Pred)

save(S, file = 'data/Rdata/FRK-SRE-1000x1000.Rdata')
save(Pred, file = 'data/Rdata/FRK-Pred-1000x1000.Rdata')


##  Create raster from center points
pred.data <- Pred@data
raster.data <- data.frame(id = pred.data$id)
Pred@data <- raster.data
r <- raster(Pred)
Pred@data <- pred.data

##  Create tile polygons from raster
spdf <- as(r, 'SpatialPolygonsDataFrame')
spdf@data <- join(spdf@data, pred.data, by = "id")
proj4string(spdf) <- sweref.crs
spdf <- spTransform(spdf, wgs84.crs)
tiles.df <- fortify(spdf)
tiles.df <- tiles.df[, !names(tiles.df) %in% c("group", "hole", "order", "piece")]

##  Create contour spatial lines from raster
nlevels <- round(Pred@data$mu) - min(Pred@data$mu)) / 5000)
contours <- rasterToContour(r, nlevels = nlevels)



proj4string(contours) <- sweref.crs
contours <- spTransform(contours, wgs84.crs)
contours.df <- fortify(contours)
contours.df <- join(contours.df, contours@data, by = "id")
contours.df$level <- as.numeric(as.character(contours.df$level))










##  Plot

m <- get_map(location = c(lon = mean(sold.objects.agg@coords[,1]), lat = mean(sold.objects.agg@coords[,2])), zoom = 11)
heatmap <- geom_polygon(data = tiles.df, aes(x=long, y=lat, fill=sqmprice, group=id), alpha=0.3)
plot_contours <- geom_path(data = contours.df, aes(x=long, y=lat, group=group, colour=level))

#ggplot(data = tiles.df, aes(x=long, y=lat)) + geom_polygon(aes(fill=sqmprice, group=id))

#ggplot(data = as.data.frame(kr.output), aes(x=x1, y=x2)) + geom_raster(aes(fill = var1.pred)) + geom_contour(aes(z = var1.pred), binwidth = 5000)

#plot.points.df <- data.frame(lat = sold.objects.agg$latitude, lng = sold.objects.agg$longitude)
#all.points <- geom_point(data = plot.points.df, aes(x=lng, y=lat), color = "#cc0000", size = 0.05)

ggmap(m) +
heatmap +
scale_fill_gradient(low="blue", high="red") +
plot_contours +
scale_colour_gradient(low="blue", high="red")


# Output json

dsd
