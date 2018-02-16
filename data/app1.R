h#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(shinythemes)
#library(Cairo)

#### UI SCRIPT ####
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("sandstone"),
    #### HEADER ####
   fluidRow(
       column(width=5,
               img(src="CEH_RGB_PMS.png", width="381", height="92")),
       column(width=5,
              h2("QMED estimation app"))
   ),
   #### OPTIONS COL ####
   fluidRow(
       column(width=3,
              h3("Overlay options"),
              helpText("Select the descriptors to be presented in summary"),
              checkboxGroupInput("descriptorGroup",
                                 "Set of Descriptors",
                                 choices = list("Area"=1,
                                                "AAR"=2,
                                                "Mean Aspect"=3,
                                                "Soil Permeability"=4,
                                                "Mean drainage path length"=5),
                                 selected=1),
              helpText("Select map overlay to show in point selection:"),
              selectInput("mapGroup", "Map Overlay",
                          choices=list("Catchments"=1,
                                       "Terrain"=2,
                                       "Rivers"=3,
                                       "Soil Permeability"=4,
                                       "Rainfall model output"=5),
                          selected=1),
              h3("Manual input of coordinates"),
              textInput("easting", "Decimal degrees east:"),
              textInput("northing", "Decimal degrees north:"),
              actionButton("enLocation", "Go to location")
       ),
    #### MAPS OF STUFF ####
       
              h3("MAP OF THINGS"),
              helpText("Drag region and double-click to zoom. Double-click without selected region to reset zoom. Single click to select point."),
              plotOutput("plot1", height = 600,
                         dblclick = "plot1_dblclick",
                         click = "plot1_click",
                         brush = brushOpts(
                             id = "plot1_brush",
                             resetOnNew = TRUE
                         )
              )

       )
   ),
   #### NUMERICAL OUTPUTS ####
   fluidRow(
       column(width=9, offset=3,
              h3("Estimates and Predictors"),
              textOutput("outputsText")),
       h4("Points near click"),
       verbatimTextOutput("click_info")
   )
   # Application title

   # 
   # # Sidebar with options for display
   # sidebarLayout(
   #    sidebarPanel(
   #        
   #    ),
   #    
   #    # Show a plot of the generated distribution
   #    mainPanel(
   # 
   #    )
   # )
)

#### SERVER SCRIPT ####

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   # output$distPlot <- renderPlot({
   #    # generate bins based on input$bins from ui.R
   #    x    <- faithful[, 2] 
   #    bins <- seq(min(x), max(x), length.out = input$bins + 1)
   #    
   #    # draw the histogram with the specified number of bins
   #    hist(x, breaks = bins, col = 'darkgray', border = 'white')
   # })
    
    ranges <- reactiveValues(x = NULL, y = NULL)
    
    output$plot1 <- renderPlot({
        ggplot(mtcars, aes(wt, mpg)) +
            geom_point() +
            coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
    })
    
    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$plot1_dblclick, {
        brush <- input$plot1_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
    })
    output$click_info <- renderPrint({
        # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
        # were a base graphics plot, we'd need those.
        nearPoints(mtcars, input$plot1_click, addDist = TRUE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

