#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(ggmap)
library(shinythemes)
library(sp)
library(raster)
library(rgdal)
library(leaflet)
library(dplyr)
library(htmltools)

#setwd("W:/sgsys/Newton Fund - Flood Estimation India/ShinyApp/QMED_1/testLeaflet")

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
    fluidRow(column(
        width = 5,
        img(
            src = "CEH_RGB_PMS.png",
            width = "381",
            height = "92"
        )
    ),
    column(width = 5,
           h2(
               "QMED estimation app"
           ))), 
    # Application title
    #titlePanel("QMED Estimation Shiny App"),
    fluidRow(
        column(
            width = 3,
            h3("Overlay options"),
            # Sidebar with options for display
            # sidebarLayout(
            #    sidebarPanel(
            #        h3("Overlay options"),
            helpText("Select the descriptors to be presented in summary"),
            checkboxGroupInput(
                "descriptorGroup",
                "Set of Descriptors",
                choices = list(
                    "Area" = 1,
                    "AAR" = 2,
                    "Mean Aspect" =
                        3,
                    "Soil Permeability" =
                        4,
                    "Mean drainage path length" =
                        5
                ), 
                selected = 1
            ), 
            # helpText("Select map overlay to show in point selection:"),
            # checkboxInput("catchmentOn", "Catchments shown", F),
            # checkboxInput("riversOn", "Rivers shown", F),
            # selectInput("mapGroup", "Map Overlay",
            #             choices=list("None"=1,
            #                          "Terrain"=2,
            #                          "Rivers"=3),
            #             selected=1),
            h3("Manual input of coordinates"),
            helpText("Please choose a point in 40-90 degrees East, 8-25 degrees North"),
            textInput("easting", "Degrees East:"),
            textInput("northing", "Degrees North:"),
            actionButton("enLocation", "Go to location")
        ), 
        
        # Show a plot of the generated distribution
        # mainPanel(
        column(
            width = 9,
            h4("Drag to move view, double-click or scroll to zoom. Click to select point."),
            leafletOutput("map", height = 600)
        )
    ), 
    fluidRow(
        h3("Estimates and Predictors"),
        column(width = 4,
               tableOutput("tableOP")),
        column(width = 4,
               tableOutput("tableCD")),
        column(
            width = 2,
            actionButton("growthCurve", "DOES NOT WORK: Growth Curve")
        )
    )
) 

#### Define server logic required to draw a histogram  ####
server <- function(input, output) {
    
    df <- data.frame(long=c(), lat=c())
    
    ranges <- reactiveValues(x = extent(polygons)[1:2], 
                             y = extent(polygons)[3:4])
    location <- reactiveValues(L = NULL, 
                               A=NULL,
                               K=NULL,
                               Z=NULL, 
                               zn0=NULL, 
                               ne=NULL)
    mapPoint <- reactiveValues(mapP = NULL,
                               station = NULL)
    errorPoint <- reactiveValues(er = F)
    
    output$map <- renderLeaflet({
        leaflet() %>% 
            addTiles(options = providerTileOptions(minZoom = 6, maxZoom = 9)) %>%
            setView(lng=78, lat=18, zoom=6) %>%
            setMaxBounds(lng1=40,lng2=90,lat1=8,lat2=25) %>%
            addPolygons(data=polygons, weight=1, col="red", fill=F,
                        options=pathOptions(clickable=F), group="Watersheds") %>%
            addPolylines(data=rivers, weight=1, col="blue",
                        options=pathOptions(clickable=F), group="Rivers") %>%
            addLayersControl(
                overlayGroups = c("Rivers", "Watersheds")
            )
    })
    observeEvent(input$enLocation, {
        leafletProxy('map') %>%
            clearGroup('markers') %>%
            clearGroup('catchments')
        if(!is.null(input$easting) & !is.null(input$northing)){
            clng <- as.numeric(input$easting)
            clat <- as.numeric(input$northing) 
            errorPoint$er <- F
            if(clng > 90 || clng < 40 || 
               clat < 8 || clat > 25){
                errorPoint$er <- T
            }
        }else{
            errorPoint$er <- T
            clng <- 70
            clat <- 17
        }
        
        mapPoint$mapP <- SpatialPoints(
            data.frame(x=clng,
                       y=clat))
        proj4string(mapPoint$mapP) <-  proj4string(polygons)
        location$A <- polygons %over% mapPoint$mapP
        if(length(location$A)>0 && sum(!is.na(location$A))>0){
            location$K <- polygons[!is.na(location$A),]
            location$L <- location$K[which.min(location$K@data[,'Area0']),]
            location$Z <- location$L@data[,'zCode']
            cat(file=stderr(), "Z ", location$Z, "\n")
            location$zn0 <- which(summary$zCode == as.character(location$Z))
            #cat(file=stderr(), "Zno ", 
            #    as.character(summary$Location[location$zn0]), "\n")
            location$ne <- as.matrix(summary[location$zn0,14:13], "\n")
            #cat(file=stderr(), "ne ", location$ne, "\n")
            #location$ne <- project(location$ne, proj4string(polygons))
            mapPoint$station <- SpatialPoints(
                data.frame(x=location$ne[1,1], 
                           y=location$ne[1,2])
            )
            proj4string(mapPoint$station) <-  proj4string(polygons)
        }else{
            location$K <- NULL
            location$L <- NULL
            location$Z <- NULL
            location$zn0 <- NULL
            location$ne <- NULL
            mapPoint$station <- NULL
        }
        if(!is.null(location$Z)){
            zn <- which(summary$zCode == as.character(location$Z))
            zq <- as.character(summary$QMED[zn])
            zl <- as.character(summary$Location[zn])
        }else{
            zn <- NA
            zq <- NA
            zl <- "No station"
        }
        
        leafletProxy('map') %>%
            addMarkers(lng=clng, lat=clat, group='markers')
        if(!is.null(location$Z)){
            #cat(file=stderr(), "Z", location$ne, "\n")
            lab <- as.character(summary$Location[location$zn0])
            leafletProxy('map') %>%
                addMarkers(lng=location$ne[1], lat=location$ne[2], 
                           group="markers", 
                           label = htmlEscape(lab), 
                           labelOptions = labelOptions(noHide = T)) %>%
                addPolygons(data=location$L, fillColor="purple", 
                            opacity=0.5, group="catchments", 
                            options=pathOptions(clickable=F))
        }
    })
    
    observeEvent(input$map_click, {
        ## Get the click info like had been doing
        leafletProxy('map') %>%
            clearGroup('markers') %>%
            clearGroup('catchments')
        
        click <- input$map_click
        clat <- click$lat
        clng <- click$lng
        
        mapPoint$mapP <- SpatialPoints(
            data.frame(x=clng,
                       y=clat))
        
        proj4string(mapPoint$mapP) <-  proj4string(polygons)
        location$A <- polygons %over% mapPoint$mapP
        cat(file=stderr(), "length ", length(location$A), 
            " ", is.null(location$A), "\n")
        if(length(location$A)>0 && sum(!is.na(location$A))>0){
            location$K <- polygons[!is.na(location$A),]
            location$L <- location$K[which.min(location$K@data[,'Area0']),]
            location$Z <- location$L@data[,'zCode']
            cat(file=stderr(), "Z ", location$Z, "\n")
            location$zn0 <- which(summary$zCode == as.character(location$Z))
            location$ne <- as.matrix(summary[location$zn0,14:13], "\n")
            mapPoint$station <- SpatialPoints(
                data.frame(x=location$ne[1,1], 
                           y=location$ne[1,2])
            )
            proj4string(mapPoint$station) <-  proj4string(polygons)
        }else{
            location$K <- NULL
            location$L <- NULL
            location$Z <- NULL
            location$zn0 <- NULL
            location$ne <- NULL
            mapPoint$station <- NULL
        }
        if(!is.null(location$Z)){
            zn <- which(summary$zCode == as.character(location$Z))
            zq <- as.character(summary$QMED[zn])
            zl <- as.character(summary$Location[zn])
        }else{
            zn <- NA
            zq <- NA
            zl <- "No station"
        }
        
        leafletProxy('map') %>%
            addMarkers(lng=clng, lat=clat, group='markers')
        if(!is.null(location$Z)){
            #cat(file=stderr(), "Z", location$ne, "\n")
            lab <- as.character(summary$Location[location$zn0])
        leafletProxy('map') %>%
            addMarkers(lng=location$ne[1], lat=location$ne[2], 
                       group="markers", 
                       label = htmlEscape(lab), 
                       labelOptions = labelOptions(noHide = T)) %>%
            addPolygons(data=location$L, fillColor="purple", 
                        opacity=0.5, group="catchments", 
                        options=pathOptions(clickable=F))
        }
        # if(!is.null(mapPoint$mapP)){
        #     leafletProxy('map') %>% 
        #     addPolygons(data=location$L, fillcolor="purple", opacity=0.5) %>%
        #     addMarkers(mapPoint$station, col="red", popup = zl)
        # }
    })
    
    #observeEvent(errorPoint$er,{
    #             showNotification("Please choose a point in 40-90 degrees East, 8-25 degrees North", duration=5, closeButton=T, type="error")
    #    })
    
    output$tableOP = renderTable({
        if(!is.null(location$Z)){
            zn <- which(summary$zCode == as.character(location$Z))
            zq <- as.character(summary$QMED[zn])
            zl <- as.character(summary$Location[zn])
            #zc <- round(mapPoint$mapP,0)
        }else{
            zn <- NA
            zq <- NA
            zl <- "No location"
            #zc <- c(0,0)
        }
        if(!is.null(mapPoint$mapP)){
            zc <- c(round(mapPoint$mapP$x,2), round(mapPoint$mapP$y,2))
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
    #Table of location and results
    
    #output$tableCD  # Table of Catchment Descriptors
    output$tableCD = renderTable({
        if(!is.null(location$Z)){
            zn <- which(summary$zCode == as.character(location$Z))
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
        if(length(input$descriptorGroup)==0){
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

