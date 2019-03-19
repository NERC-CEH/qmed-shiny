# Adam Griffin, 2019-03-19
# Shiny app for QMED estimation in India, as part of SUNRISE project.
# Includes point-and-click for selecting catchments, selecting nearest
# downstream station.
# TODO: pooling or estimates at ungauged locations. Would require determinining
# of catchments draining into a given point. This is not fast in R.

library(shiny)
library(ggplot2)
library(shinythemes)
library(sp)
library(raster)
library(rgdal)  # Lancaster needs rgdal =< 1.2-20, as has gdal 1.11.3
library(leaflet)
library(dplyr)
library(htmltools)
library(here)
library(shinycssloaders)
library(lmom)

### SETUP ###

polygons <- readOGR("./data","NotProjectedPolygons")
polygons <- polygons[order(polygons@data$Area0, decreasing=T),]
summary <- read.csv("./data/stationSummary_20171006.csv", 
                    stringsAsFactors=F, header=T, row.names=1)
#terrain <- raster("./data/terrainA.grd")
rivers <- readOGR("./data","riversGK")
amax_dir <- dir("./data/AMAX_New", full.names=T, pattern=".csv")
load(here("data/amax_list_app.RDa"))

yf <- function(f){ - log((1 - f) / f) }  # Reduced logistic variate
yfgum <- function(f){ -1 * log(-1 * log(f))} # Gumbel variate