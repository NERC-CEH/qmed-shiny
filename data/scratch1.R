# Adam Griffin 2018-01-22
# demo for selecting 

library(sp)
library(raster)
library(rgdal)

setwd("W:/sgsys/Newton Fund - Flood Estimation India/")

polygons <- readOGR("./Data/New GIS files/A1 Summaries","Catchments")
polygons <- polygons[order(polygons@data$Area0, decreasing=T),]
plot(polygons)

xy <- click(polygons, xy=T, type='p')
df <- data.frame(xy[[1]]@coords)
sp <- SpatialPoints(df)
proj4string(sp) <- proj4string(polygons)
A <- polygons %over% sp
K <- polygons[!is.na(A),]
L <- K[which.min(K@data[,'Area0']),]
plot(K, xlim=c(300000,1360000), ylim=c(11000000, 12600000))
plot(polygons, border="grey50")
points(sp, col=2, pch=4)
plot(L, add=T, border=2, lwd=2)
