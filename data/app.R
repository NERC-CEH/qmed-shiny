all# Adam Griffin, 2018-??-??, updated 2019-03-15
# Shiny app for QMED estimation


require(shiny)
library(ggplot2)
library(shinythemes)
library(sp)
library(raster)
library(rgdal)
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

##### UI #####
ui <- fluidPage(
    theme = shinytheme("sandstone"),
    
    
    ## ROW OF LOGOS ##
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
        ## OPTIONS COLUMN ##
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
                selected = 1:5
            ),
            
            radioButtons("distribution", "Distribution Choice",
                         choiceValues=c("gpa", "pe3", "both"),
                         choiceNames=c("Generalised Pareto", "Pearson Type III",
                                        "Both"), selected="pe3"),
            
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
            textInput("easting", label="Degrees East:", value="70"),
            textInput("northing", label="Degrees North:", value="17"),
            
            actionButton("enLocation", "Go to location")
        ), 
        
        column(width = 9,
            ### MAP DISPLAY
            h4(paste("Drag to move view, double-click or scroll to zoom.",
                     "Click to select point.")),
            leafletOutput("map", height = 600) %>% withSpinner(type=4)
        )
    ), 
    
    
    fluidRow(
        h3("Estimates and Predictors"),
        column(width = 3,
               tableOutput("tableOP")),
        column(width = 4,
               plotOutput("growthCurve")),
        column(width = 3,
               conditionalPanel(
                 condition = "(typeof input.location.zNumber !== 'undefined')",
                 downloadButton("saveGrowthCurve", "download as PDF")
               ))
    )
) 






##### SERVER #####

server <- function(input, output){
    
  
    ### SETUP ###
    userPoint <- reactiveValues(inLong=NULL,
                                inLat=NULL)
  
    overlap <- reactiveValues(point = NULL)
    
    location <- reactiveValues(upstream = NULL,
                               polys=NULL,
                               zCode=NULL, 
                               zNumber=NULL,
                               name=NULL,
                               gridRef=NULL)
    
    mapPoint <- reactiveValues(mapP = NULL,
                               station = NULL)
    
    errorPoint <- reactiveValues(er = F)
    
    showButton <- reactive({is.null(location$zCode)})
    
    ### CURVE PLOTTING FUNCTION
    curvePlot <- reactive({
      
      x <- amax_list[[which(place_list == location$name)]]
      x <- sort(x[!is.na(x)])
      n <- length(x)
      y <- -log(-log(((1:n) - 0.44)/(n + 0.12)))  
      # Gringorten plotting positions, as for GLO
      fx <- ((1:n) - 0.44)/(n + 0.12) # F_i = F(x_i)
      yfx <- yfgum(fx)  # y(F_i)
      
      # axes and labels
      xlm <- c(min(0,yfx), max(yfx))
      ylm <- c(min(0,x), max(x))
      ylb <- expression("Peak flow  " * (m^3/s))
      xlb <- expression("Gumbel reduced variate,  " * -log( -log(1 - (1/T))))
      
      fcurve <- seq(0,1, length.out=1000)[2:999]
      ycurve <- yfgum(fcurve)
      
      lx <- samlmu(x)
      
      #curve calculation using precomputed parameters
      if (input$distribution == "gpa") {
        param <- param_list[[which(place_list == location$name)]]$paramGPA
        xcurve <- lmom::quagpa(fcurve,param)
      }else if (input$distribution == "pe3") {
        param <- param_list[[which(place_list == location$name)]]$paramPE3
        xcurve <- lmom::quape3(fcurve, param)
      }else if (input$distribution == "both") {
        param <- param_list[[which(place_list == location$name)]]$paramPE3
        paramGPA <- param_list[[which(place_list == location$name)]]$paramGPA
        xcurve <- lmom::quape3(fcurve, param)
        xcurveGPA <- lmom::quagpa(fcurve,paramGPA)
      }
      
      #graphing
      par(mar=c(4,4.3,1,1))
      plot(y, x, xlab = xlb, ylab = ylb, xlim = xlm, ylim = ylm,
           type='p', pch=4)
      lines(ycurve, xcurve)
      if(input$distribution=="both"){
        lines(ycurve, xcurveGPA, lty=3)
      }
      parusr <- par("usr")
      rp.lab <- c(2, 5, 10, 20, 50, 100, 200, 500, 1000, 10000, 
                  1e+05, 1e+06, 1e+07, 1e+08)
      rp.tic <- -log(-log(1 - 1/rp.lab))
      crit <- (rp.tic >= parusr[1] & rp.tic <= parusr[2])
      rp.tic <- rp.tic[crit]
      rp.lab <- rp.lab[crit]
      rp.ypos <- parusr[3] + (parusr[4] - parusr[3]) * 0.05
      axis(side = 3, at = rp.tic, labels = rp.lab, pos = rp.ypos, cex.axis=0.8)
      text((min(rp.tic) + max(rp.tic)) * 0.5,
           rp.ypos + 0.9*(par("cxy")[2] * par("mgp")[1]),
           "Return period (years)",
           adj = c(0.5, 0), cex=0.8)
      legend("topleft", 
             legend=switch(input$distribution, "gpa"=c("GPA"),
                           "pe3"=c("PE3"),
                           "both"=c("GPA", "PE3")),
             col = 1,
             lty = if (input$distribution != "both") 1 else c(1,3),
             bty="n"
      )
    })
    
    curvePlot2 <- function(){
      
      x <- amax_list[[which(place_list == location$name)]]
      x <- sort(x[!is.na(x)])
      n <- length(x)
      y <- -log(-log(((1:n) - 0.44)/(n + 0.12)))  
      # Gringorten plotting positions, as for GLO
      fx <- ((1:n) - 0.44)/(n + 0.12) # F_i = F(x_i)
      yfx <- yfgum(fx)  # y(F_i)
      
      # axes and labels
      xlm <- c(min(0,yfx), max(yfx))
      ylm <- c(min(0,x), max(x))
      ylb <- expression("Peak flow  " * (m^3/s))
      xlb <- expression("Gumbel reduced variate,  " * -log( -log(1 - (1/T))))
      
      fcurve <- seq(0,1, length.out=1000)[2:999]
      ycurve <- yfgum(fcurve)
      
      lx <- samlmu(x)
      
      #curve calculation using precomputed parameters
      if (input$distribution == "gpa") {
        param <- param_list[[which(place_list == location$name)]]$paramGPA
        xcurve <- lmom::quagpa(fcurve,param)
      }else if (input$distribution == "pe3") {
        param <- param_list[[which(place_list == location$name)]]$paramPE3
        xcurve <- lmom::quape3(fcurve, param)
      }else if (input$distribution == "both") {
        param <- param_list[[which(place_list == location$name)]]$paramPE3
        paramGPA <- param_list[[which(place_list == location$name)]]$paramGPA
        xcurve <- lmom::quape3(fcurve, param)
        xcurveGPA <- lmom::quagpa(fcurve,paramGPA)
      }
      
      #graphing
      par(mar=c(4,4.3,1,1))
      plot(y, x, xlab = xlb, ylab = ylb, xlim = xlm, ylim = ylm,
           type='p', pch=4)
      lines(ycurve, xcurve)
      if(input$distribution=="both"){
        lines(ycurve, xcurveGPA, lty=3)
      }
      parusr <- par("usr")
      rp.lab <- c(2, 5, 10, 20, 50, 100, 200, 500, 1000, 10000, 
                  1e+05, 1e+06, 1e+07, 1e+08)
      rp.tic <- -log(-log(1 - 1/rp.lab))
      crit <- (rp.tic >= parusr[1] & rp.tic <= parusr[2])
      rp.tic <- rp.tic[crit]
      rp.lab <- rp.lab[crit]
      rp.ypos <- parusr[3] + (parusr[4] - parusr[3]) * 0.05
      axis(side = 3, at = rp.tic, labels = rp.lab, pos = rp.ypos, cex.axis=0.8)
      text((min(rp.tic) + max(rp.tic)) * 0.5,
           rp.ypos + 0.9*(par("cxy")[2] * par("mgp")[1]),
           "Return period (years)",
           adj = c(0.5, 0), cex=0.8)
      legend("topleft", 
             legend=switch(input$distribution, "gpa"=c("GPA"),
                           "pe3"=c("PE3"),
                           "both"=c("GPA", "PE3")),
             col = 1,
             lty = if (input$distribution != "both") 1 else c(1,3),
             bty="n"
      )
    }
    
    
    
    ### LOCATION UPDATE BY TEXT INPUT
    observeEvent(input$enLocation, {
      if (!(is.null(input$easting) | nchar(input$easting)==0) &
          !(is.null(input$northing) | nchar(input$northing)==0)) {
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
      
      userPoint$inLong <- inLong
      userPoint$inLat <- inLat
      
    }, ignoreInit = TRUE) 
    
    
    ### LOCATION UPDATE BY MOUSE CLICK
    observeEvent(input$map_click, {  
      ## Get the click info like had been doing
      userPoint$inLat <- input$map_click$lat
      userPoint$inLong <- input$map_click$lng
      
    }, ignoreInit = TRUE)
    
    
    ### FIND NEAREST DOWNSTREAM STATION
    observeEvent(userPoint$inLat, {
      leafletProxy('map') %>%
        clearGroup('markers') %>%
        clearGroup('catchments')
      
      mapPoint$mapP <- SpatialPoints(data.frame(x=userPoint$inLong,
                                                y=userPoint$inLat))
      proj4string(mapPoint$mapP) <-  proj4string(polygons)
      overlap$point <- polygons %over% mapPoint$mapP
      
      if (length(overlap$point)>0 && sum(!is.na(overlap$point))>0) {
        
        location$polys <- polygons[!is.na(overlap$point), ]
        location$upstream <- location$polys[
          which.min(location$polys@data[, 'Area0']), ]
        location$zCode <- location$upstream@data[,'zCode']
        location$zNumber <- 
          which(summary$zCode == as.character(location$zCode))
        location$name <- summary$Location[location$zNumber]
        location$gridRef <- 
          as.matrix(summary[location$zNumber,14:13], "\n")
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
        location$name <- NULL
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
      
      ### ADD TO MAP
      leafletProxy('map') %>%
        addMarkers(lng=userPoint$inLong, lat=userPoint$inLat, group='markers')
      if (!is.null(location$zCode)) {
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
    
    
    ### ERROR POPUP
    observeEvent(errorPoint$er,{
                 showNotification(
                   paste("Please choose a point in 40-90 degrees East,",
                         "8-25 degrees North"), 
                   duration=5,
                   closeButton=T,
                   type="error")
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
    
    
    ### GROWTH CURVE PLOT
    output$growthCurve <- renderPlot({
      if( !is.null(location$zCode)){
        curvePlot()
      }
    })
    
    ### TABLE OF LOCATION INFO
    output$tableOP <- renderTable({
      if (!is.null(location$zCode)) {
        zn <- which(summary$zCode == as.character(location$zCode))
        zq <- as.character(summary$QMED[zn])
        zl <- as.character(summary$Location[zn])
        za <- as.character(round(summary$Area[zn],0))
        zaar <- as.character(round(summary$Annual_rain_mean[zn],0))
        #zasp <- as.character(round(summary$Asp_mean[zn],2))
        zasp <- round(((summary$Asp_mean[zn]*180)/pi),2)
        zasp <- ifelse(zasp < 0, zasp + 360, zasp)
        zsq <-as.character(round(summary$SQ4_mean[zn],2))
        zlm <- as.character(round(summary$Length_mean[zn],0))
      }else{
        zn <- NA
        zq <- NA
        za <- NA
        zaar <- NA
        zasp <- NA
        zsq <- NA
        zlm <- NA
        zl <- "No location"
      }
      if (!is.null(mapPoint$mapP)) {
        zc <- c(round(mapPoint$mapP$x, 2),
                round(mapPoint$mapP$y, 2))
      }else{
        zc <- c(0,0)
      }
      dt <- data.frame(matrix(NA,8,2))
      colnames(dt) <- c("Field", "Value")
      
      dt[1,] <- c("Clicked Location", paste(zc[2], "N", zc[1], "E"))
      dt[2,] <- c("Nearest downstream station", zl)
      dt[3,] <- c("QMED estimate", paste(zq, "m³/s"))
      dt[4,] <- c("Area",paste(za, "km²"))
      dt[5,] <- c("AAR", paste(zaar, "mm"))
      dt[6,] <- c("Mean Aspect", paste(zasp, "deg clockwise from North"))
      dt[7,] <- c("Mean permeability", zsq)
      dt[8,] <- c("Mean channel length", paste(zlm, "km"))
      if(length(input$descriptorGroup)==0){
        dt <- dt[1:3,]
      }else{
        #browser()
        dt <- dt[c(1:3, 3+as.numeric(input$descriptorGroup)),]
      }
      dt
    })
    
    output$saveGrowthCurve <- downloadHandler(
      filename = function(){paste0(location$name, "_growthcurve.pdf")},
      content = function(file){
        pdf(file=file, width=7, height=7, pointsize=10)
          curvePlot2()
        dev.off()
      }
    )
    
    # ### CATCHMENT DESCRIPTORS TABLE
    # output$tableCD = renderTable({
    #   
    #     if (!is.null(location$zCode)) {
    #         zn <- which(summary$zCode == as.character(location$zCode))
    # 
    #         #zc <- round(mapPoint$mapP,0)
    #     }else{
    #         zn <- NA
    #         za <- NA
    #         zaar <- NA
    #         zasp <- NA
    #         zsq <- NA
    #         zlm <- NA
    #     }
    #     
    #     ### CD TABLE OUTPUT BASED ON SELECTIONS
    #     if (length(input$descriptorGroup)==0) {
    #         dt <- data.frame(Descriptor="None selected", Value=NA)
    #     }else{
    #         dt <- data.frame(matrix(NA,5,2))
    #         colnames(dt) <- c("Descriptor", "Value")
    #         
    #         dt[1,] <- c("Area",paste(za, "km²"))
    #         dt[2,] <- c("AAR", paste(zaar, "mm"))
    #         dt[3,] <- c("Mean Aspect", paste(zasp, "deg clockwise from North"))
    #         dt[4,] <- c("Mean permeability", zsq)
    #         dt[5,] <- c("Mean channel length", paste(zlm, "km"))
    #         dt <- dt[input$descriptorGroup,]
    #         dt
    #     }
    #   
    # })
}



# Run the application 
shinyApp(ui = ui, server = server)
