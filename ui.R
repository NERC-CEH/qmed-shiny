# Adam Griffin, 2019-03-19
# Shiny app for QMED estimation in India, as part of SUNRISE project.
# Includes point-and-click for selecting catchments, selecting nearest
# downstream station.
# TODO: pooling or estimates at ungauged locations. Would require determinining
# of catchments draining into a given point. This is not fast in R.


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