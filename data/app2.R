# Adam Griffin, 2018-??-??, updated 2019-03-15
# Shiny app for QMED estimation


require(shiny)
library(ggplot2)
library(ggmap)
library(shinythemes)
library(sp)
library(raster)
library(rgdal)
library(leaflet)
library(dplyr)
library(htmltools)
library(here)

### SETUP ###

polygons <- readOGR("./data","NotProjectedPolygons")
polygons <- polygons[order(polygons@data$Area0, decreasing=T),]
summary <- read.csv("./data/stationSummary_20171006.csv", 
                    stringsAsFactors=F, header=T, row.names=1)
terrain <- raster("./data/terrainA.grd")
#rivers <- readOGR("data/simpleGKriversP.shp")
rivers <- readOGR("./data","riversGK")

#### Define UI for application that draws a histogram ####
ui <- fluidPage(
    theme = shinytheme("sandstone"),
    
    
    #### ROW OF LOGOS ####
    fluidRow(
        column(width = 5,
               img(
                  src = "CEH_RGB_PMS.png",
                  width = "381",
                  height = "92"
               )
        ),
        column(width = 5,
               h2("QMED estimation app")
        )
    ), 
    
    fluidRow(
        #### OPTIONS COLUMN ####
        column(width = 3,
            h3("Overlay options"),
            
            helpText("Select the descriptors to be presented in summary"),
            
            checkboxGroupInput(  
                ### DESCRIPTORS FOR QMED SUMMARY
                "descriptorGroup",
                "Set of Descriptors",
                choices = list(
                    "Area" = 1,
                    "AAR" = 2,
                    "Mean Aspect" = 3,
                    "Soil Permeability" = 4,
                    "Mean drainage path length" = 5
                ), 
                selected = 1
            ), 
            
            # helpText("Select map overlay to show in point selection:"),
            ### ASPECTS TO INCLUDE ON THE MAP
            # checkboxInput("catchmentOn", "Catchments shown", F),
            # checkboxInput("riversOn", "Rivers shown", F),
            # selectInput("mapGroup", "Map Overlay",
            #             choices=list("None"=1,
            #                          "Terrain"=2,
            #                          "Rivers"=3),
            #             selected=1),
            
            
            h3("Manual input of coordinates"),
            ### COORDINATE TEXT BOXES
            textInput("easting", "Degrees East:"),
            textInput("northing", "Degrees North:"),
            
            actionButton("enLocation", "Go to location")
        ), 
        
        column(width = 9,
            ### MAP DISPLAY
            h4(paste("Drag to move view, double-click or scroll to zoom.",
                     "Click to select point.")),
            leafletOutput("map", height = 600)
        )
    ), 
    
    
    fluidRow(
        h3("Estimates and Predictors"),
        column(width = 4,
               tableOutput("tableOP")),
        column(width = 4,
               tableOutput("tableCD")),
        column(width = 2,
            actionButton("growthCurve", "DOES NOT WORK: Growth Curve")
        )
    )
) 








server <- function(input, output){
    
  
    ### SETUP ###
    df <- data.frame(long=c(), lat=c())
    
    ranges <- reactiveValues(x = extent(polygons)[1:2], 
                             y = extent(polygons)[3:4])
    
    location <- reactiveValues(upstream = NULL, 
                               overlap=NULL,
                               polys=NULL,
                               zCode=NULL, 
                               zNumber=NULL, 
                               gridRef=NULL)
    
    mapPoint <- reactiveValues(mapP = NULL,
                               station = NULL)
    
    errorPoint <- reactiveValues(er = F)
    
    observeEvent(input$enLocation, {  ## MANUAL COORD INPUT
        leafletProxy('map') %>%
            clearGroup('markers') %>%
            clearGroup('catchments')
      
        if (!is.null(input$easting) & !is.null(input$northing)) {
            inLong <- as.numeric(input$easting)
            inLat <- as.numeric(input$northing) 
            errorPoint$er <- F
            
            if (inLong > 90 || inLong < 40 || inLat < 8 || inLat > 25) {
                errorPoint$er <- T
            }
            
        }else{
            errorPoint$er <- T
            inLong <- 70
            inLat <- 17
        }
        
        mapPoint$mapP <- SpatialPoints(data.frame(x=inLong, y=inLat))
        proj4string(mapPoint$mapP) <-  proj4string(polygons)
        location$overlap <- polygons %over% mapPoint$mapP
        
        ### IF CLICK IS INSIDE AT LEAST ONE CATCHMENT
        if (length(location$overlap)>0 && sum(!is.na(location$overlap))>0) {
          
            location$polys <- polygons[!is.na(location$overlap), ]
            location$upstream <- location$polys[
              which.min(location$polys@data[, 'Area0']), ]
            location$zCode <- location$upstream@data[,'zCode']
            #cat(file=stderr(), "Zcode ", location$zCode, "\n")
            
            location$zNumber <- 
              which(summary$zCode == as.character(location$zCode))
            #cat(file=stderr(), "Zno ", 
            #    as.character(summary$Location[location$zNumber]), "\n")
            
            location$gridRef <- 
              as.matrix(summary[location$zNumber,14:13], "\n")
            #cat(file=stderr(), "gridRef ", location$gridRef, "\n")
            #location$gridRef <- project(location$gridRef, proj4string(polygons))
            
            mapPoint$station <- SpatialPoints(
                data.frame(x=location$gridRef[1,1], 
                           y=location$gridRef[1,2])
            )
            proj4string(mapPoint$station) <- proj4string(polygons)
        }else{
            location$polys <- NULL
            location$upstream <- NULL
            location$zCode <- NULL
            location$zNumber <- NULL
            location$gridRef <- NULL
            mapPoint$station <- NULL
        }
        if (!is.null(location$zCode)) {
            zn <- which(summary$zCode == as.character(location$zCode))
            zq <- as.character(summary$QMED[zn])
            zl <- as.character(summary$Location[zn])
        }else{
            zn <- NA
            zq <- NA
            zl <- "No station"
        }
        
        leafletProxy('map') %>%
            addMarkers(lng=clng, lat=clat, group='markers')
        if (!is.null(location$zCode)) {
            #cat(file=stderr(), "Z", location$gridRef, "\n")
            lab <- as.character(summary$Location[location$zNumber])
            leafletProxy('map') %>%
                addMarkers(lng=location$gridRef[1], lat=location$gridRef[2], 
                           group="markers", 
                           label = htmlEscape(lab), 
                           labelOptions = labelOptions(noHide = T)) %>%
                addPolygons(data=location$upstream,
                            fillColor="purple", 
                            opacity=0.5,
                            group="catchments", 
                            options=pathOptions(clickable=F))
        }
    })
    
    observeEvent(input$map_click, {  ## LOCATION UPDATE BY MOUSE CLICK
        ## Get the click info like had been doing
        leafletProxy('map') %>%
        clearGroup('markers') %>%
        clearGroup('catchments')
        
        click <- input$map_click
        clat <- click$lat
        clng <- click$lng
        
        mapPoint$mapP <- SpatialPoints(data.frame(x=clng,y=clat))
        proj4string(mapPoint$mapP) <-  proj4string(polygons)
        location$overlap <- polygons %over% mapPoint$mapP

        if (length(location$overlap) > 0 && sum(!is.na(location$overlap)) > 0) {
            location$polys <- polygons[!is.na(location$overlap), ]
            location$upstream <- 
              location$polys[which.min(location$polys@data[,'Area0']), ]
            
            location$zCode <- location$upstream@data[,'zCode']
            cat(file=stderr(), "Z ", location$zCode, "\n")
            
            location$zNumber <- 
              which(summary$zCode == as.character(location$zCode))
            location$gridRef <- as.matrix(summary[location$zNumber,14:13], "\n")
            
            mapPoint$station <- SpatialPoints(
                data.frame(x=location$gridRef[1,1], 
                           y=location$gridRef[1,2])
            )
            proj4string(mapPoint$station) <-  proj4string(polygons)
        }else{
            location$polys <- NULL
            location$upstream <- NULL
            location$zCode <- NULL
            location$zNumber <- NULL
            location$gridRef <- NULL
            mapPoint$station <- NULL
        }
        if (!is.null(location$zCode)) {
            zn <- which(summary$zCode == as.character(location$zCode))
            zq <- as.character(summary$QMED[zn])
            zl <- as.character(summary$Location[zn])
        }else{
            zn <- NA
            zq <- NA
            zl <- "No station"
        }
        
        leafletProxy('map') %>%
            addMarkers(lng=clng, lat=clat, group='markers')
        if (!is.null(location$zCode)) {
            #cat(file=stderr(), "Z", location$gridRef, "\n")
            lab <- as.character(summary$Location[location$zNumber])
            
        leafletProxy('map') %>%
            addMarkers(lng=location$gridRef[1],
                       lat=location$gridRef[2], 
                       group="markers", 
                       label = htmlEscape(lab), 
                       labelOptions = labelOptions(noHide = T)) %>%
            addPolygons(data=location$upstream,
                        fillColor="purple", 
                        opacity=0.5,
                        group="catchments", 
                        options=pathOptions(clickable=F))
        }
        # if(!is.null(mapPoint$mapP)){
        #     leafletProxy('map') %>% 
        #     addPolygons(data=location$upstream,
        #                 fillcolor="purple",
        #                 opacity=0.5) %>%
        #     addMarkers(mapPoint$station, col="red", popup = zl)
        # }
    })
    
    observeEvent(errorPoint$er,{
                 showNotification(
                   paste("Please choose a point in 40-90 degrees East,",
                         "8-25 degrees North"), 
                   duration=5,
                   closeButton=T,
                   type="error")
      })
    
    
    
    ### TABLE OF LOCATION INFO
    output$tableOP <- renderTable({
        if (!is.null(location$zCode)) {
            zn <- which(summary$zCode == as.character(location$zCode))
            zq <- as.character(summary$QMED[zn])
            zl <- as.character(summary$Location[zn])
            #zc <- round(mapPoint$mapP,0)
        }else{
            zn <- NA
            zq <- NA
            zl <- "No location"
            #zc <- c(0,0)
        }
        if (!is.null(mapPoint$mapP)) {
            zc <- c(round(mapPoint$mapP$x, 2),
                    round(mapPoint$mapP$y, 2))
        }else{
            zc <- c(0,0)
        }
        dt <- data.frame(matrix(NA,3,2))
        colnames(dt) <- c("Field", "Value")
        dt[,1] <- c("Clicked Location", 
                    "Nearest downstream station", 
                    "QMED estimate:")
        dt[1,2] <- paste(zc[2], "N", zc[1], "E")
        dt[2,2] <- zl
        dt[3,2] <- paste(zq, "m³/s")
        dt
    })

    
    
    ### MAP DISPLAYED
    output$map <- renderLeaflet({
      leaflet() %>% 
        addTiles(options = providerTileOptions(minZoom = 6, maxZoom = 9)) %>%
        setView(lng=78, lat=18, zoom=6) %>%
        setMaxBounds(lng1=40,lng2=90,lat1=8,lat2=25) %>%
        addPolygons(data=polygons,
                    weight=1, col="red", fill=F,
                    options=pathOptions(clickable=F),
                    group="Watersheds") %>%
        addPolylines(data=rivers, weight=1, col="blue",
                     options=pathOptions(clickable=F),
                     group="Rivers") %>%
        addLayersControl(
          overlayGroups = c("Rivers", "Watersheds"))
    })
    
    
    
    ### CATCHMENT DESCRIPTORS TABLE
    output$tableCD = renderTable({
      
        if (!is.null(location$zCode)) {
            zn <- which(summary$zCode == as.character(location$zCode))
            za <- as.character(round(summary$Area[zn],0))
            zaar <- as.character(round(summary$Annual_rain_mean[zn],0))
            #zasp <- as.character(round(summary$Asp_mean[zn],2))
            zasp <- round(((summary$Asp_mean[zn]*180)/pi),2)
            zasp <- ifelse(zasp < 0, zasp + 360, zasp)
            zsq <-as.character(round(summary$SQ4_mean[zn],2))
            zlm <- as.character(round(summary$Length_mean[zn],0))
            #zc <- round(mapPoint$mapP,0)
        }else{
            zn <- NA
            za <- NA
            zaar <- NA
            zasp <- NA
            zsq <- NA
            zlm <- NA
        }
        
        ### CD TABLE OUTPUT BASED ON SELECTIONS
        if (length(input$descriptorGroup)==0) {
            dt <- data.frame(Descriptor="None selected", Value=NA)
        }else{
            dt <- data.frame(matrix(NA,5,2))
            colnames(dt) <- c("Descriptor", "Value")
            
            dt[1,] <- c("Area",paste(za, "km²"))
            dt[2,] <- c("AAR", paste(zaar, "mm"))
            dt[3,] <- c("Mean Aspect", paste(zasp, "deg clockwise from North"))
            dt[4,] <- c("Mean permeability", zsq)
            dt[5,] <- c("Mean channel length", paste(zlm, "km"))
            dt <- dt[input$descriptorGroup,]
            dt
        }
      
    })
}



# Run the application 
shinyApp(ui = ui, server = server)