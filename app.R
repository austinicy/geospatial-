#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(rgdal)
library(readr)
library(spatstat)
library(maptools)
library(raster)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  titlePanel("Accessibility Visualisation Tool by Acces(S)ingapore"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("layers",
                         "Toggle layers to be displayed:",
                         c("Kernel Density" = "kde",
                           "Buffer Analysis" = "buffer")),
      fileInput('userfiles',
                'Select file to upload for analysis:'),
      selectInput('origin', 
                  'Select origin locations:', 
                  c(Choose='','SingPost Offices',list.files(pattern="user_")),
                  selectize=FALSE),
      selectInput('dest', 
                  'Select destination locations:', 
                  c(Choose='','SingPost Offices','SCDF Ambulances'),
                  selectize=FALSE),
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      sliderInput("dist",
                  "Distance to judge accessibility (m):",
                  min = 100,
                  max = 2000,
                  value = 500,
                  step = 25)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("mymap"),
      p(),
      h4('List of uploaded files:')
      ,verbatimTextOutput('fileList')
    )
  )
)

server <- function(input, output, session) {
  coast <- readShapePoly("www/Shapefiles/CoastalOutline.shp")
  coast_sd <- as(coast, "SpatialPolygons")
  W <- as(coast_sd, "owin")
  
  # loads SingPost locations into spatial points data frame
  SingPost <- read_csv("www/default/Singpost_Locations.csv")
  coordinates(SingPost) <- c("X-COORDINATES","Y-COORDINATES")
  proj4string(SingPost) <- CRS("+init=epsg:3414")
  longlat.df <- as.data.frame(spTransform(SingPost,CRS("+proj=longlat")))
  SingPost$LONGITUDE <- longlat.df$X.COORDINATES
  SingPost$LATITUDE <- longlat.df$Y.COORDINATES
  
  SingPost.df <- as.data.frame(SingPost)
  
  output$mymap <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      setView(103.8509, 1.2800, zoom = 10) %>%
      addMarkers(data=SingPost.df)
  })
  
  # updates SelectInput when user uploads a new file
  observeEvent(input$userfiles, {  
    if (is.null(input$userfiles)) {    return(NULL)  }  
    file.copy(from = input$userfiles$datapath, to =  paste0('user_',input$userfiles$name )  ) 
    isolate(updateSelectInput(session,"origin",choices = c('SingPost Offices', list.files(pattern='user_'))))
    isolate(updateSelectInput(session,"dest",choices = c('SingPost Offices', list.files(pattern='user_'))))
  })
  
  # imports csv as SPDF, converts XY coordinates to Longlat coordinates
  observeEvent(input$origin, {  
    if (is.null(input$origin) | input$origin == "Choose" | input$origin == '') {    return(NULL)  }  
    originPt <- read_csv(input$origin)
    coordinates(originPt) <- c("X-COORDINATES","Y-COORDINATES")
    proj4string(originPt) <- CRS("+init=epsg:3414")
    originlonglat.df <- as.data.frame(spTransform(originPt,CRS("+proj=longlat")))
    originPt$LONGITUDE <- originlonglat.df$X.COORDINATES
    originPt$LATITUDE <- originlonglat.df$Y.COORDINATES
    originPt_sp <- as(originPt, "SpatialPoints")
    originPt_ppp <- as(originPt_sp, "ppp")[W]
    kde_originPt_1000 <- density(originPt_ppp,1000)
    kde_raster <<- raster(kde_originPt_1000)
    projection(kde_raster) <<- projection(SingPost)
  })
  
  observeEvent(input$layers, {
    if (is.null(input$layers)) { return (NULL)} 
    if ("kde" %in% input$layers) {
      leafletProxy("mymap") %>%
        addRasterImage(kde_raster, opacity = 0.5,group ="KDE") %>%
        showGroup("KDE")
    } else {
      leafletProxy("mymap") %>% hideGroup("KDE")
    }
  })
  
  output$fileList <- renderText({   
    input$userfiles
    dir(pattern = 'user_') 
  })
  
}

shinyApp(ui, server)