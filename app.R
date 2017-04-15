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
library(REAT)
library(dplyr)
library(RColorBrewer)
library(classInt)

#r_colors <- rgb(t(col2rgb(colors()) / 255))
#names(r_colors) <- colors()

ui <- fluidPage(
  titlePanel("Accessibility Visualisation Tool by Acces(S)ingapore"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("layers",
                         "Toggle layers to be displayed:",
                         c("SingPost Outlets" = "singpost", "Kernel Density" = "kde", "Buffer Analysis" = "buffer")),
      radioButtons("stats",
                   "Toggle statistics to be displayed:",
                   c("None selected" = "", "Hansen Accessibility" = "hansen", "Road Distance between Origin and Destination" = "huff")),
      selectInput('POI', 
                  'Select point of interest:', 
                  c(Choose='','SingPost Offices'="www/default/Singpost_Locations.csv", 'Office Buildings'="www/default/Office Locations.csv", 'Private Housing'="www/default/Geocoded_PrivateHousing.csv",'SCDF Ambulances'),
                  selectize=FALSE),
      br(),
      fileInput('userfiles',
                "Select file to upload for analysis (maximum 1000 rows for Hansen):"),
      selectInput('origin', 
                  'Select origin locations:', 
                  c(Choose='','Office Buildings'="www/default/Office Locations.csv",list.files(pattern="user_")),
                  selectize=FALSE),
      selectInput('dest', 
                  'Select destination locations:', 
                  c(Choose='','SingPost Offices'="www/default/Singpost_Locations.csv",'SCDF Ambulances'),
                  selectize=FALSE),
      actionButton("update", "Update Graph"),
      actionButton("updatestats", "Update Stats"),
      br(),
      sliderInput("travelt",
                  "Driving distance to judge accessibility (minuetes):",
                  min = 1,
                  max = 60,
                  value = 15),
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
      h4('Statistics:'),
      plotOutput('histplot'),
      h4('List of uploaded files:'),
      verbatimTextOutput('fileList'),
      h4('Errors:'),
      verbatimTextOutput('errorList')
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
  
  # default map
  icons <- awesomeIcons(
    icon = 'fa-envelope-square',
    iconColor = 'black',
    library = 'fa',
    markerColor = 'darkblue'
  )
  output$mymap <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      setView(103.8509, 1.2800, zoom = 10) %>%
      addAwesomeMarkers(data=SingPost.df, icon=icons, group="singpost")
  })
  
  observeEvent(input$update, {
    leafletProxy("mymap") %>% clearGroup("KDE")
    check = 1
    if (input$POI == "choose" &&  (is.null(input$layers))) {
      output$errorList <- renderText({"Please choose a point of interest and an analysis method"})
      check = 2
    }else if (is.null(input$layers) ) {
      output$errorList <-  renderText({"Please choose an analysis method"})
      check = 2
    }else if (input$POI == "choose" ){
      output$errorList <-  renderText({"Please choose a point of interest"})
      check = 2
    }
    
    if(check == 2){
      return (NULL)
    }
    
    if ("kde" %in% input$layers && input$POI != "choose") {
      #clear off previous result 
      leafletProxy("mymap") %>% clearGroup("KDE")
      output$errorList <-  renderText({""})
      
      originPt <- read_csv(input$POI)
      originPt[originPt==""] <- NA
      originPt <- na.omit(originPt)
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
      leafletProxy("mymap") %>%
        addRasterImage(kde_raster, opacity = 0.5,group ="KDE") %>%
        showGroup("KDE")
    }
  })
  
  observeEvent(input$updatestats, {
    if (input$stats == "hansen" && input$origin != "choose" && input$dest != "choose") {
      #clear off previous result
      leafletProxy("mymap") %>% clearGroup("stats")
      
      print ("YES")
      originPt <- read_csv(input$origin)
      destPt <- read_csv(input$dest)
      
      # check for empty values
      originPt[originPt==""] <- NA
      originPt <- na.omit(originPt)
      destPt[destPt==""] <- NA
      destPt <- na.omit(destPt)
      
      # sets XY coordinates & projection 
      coordinates(originPt) <- c("X-COORDINATES","Y-COORDINATES")
      coordinates(destPt) <- c("X-COORDINATES","Y-COORDINATES")
      proj4string(originPt) <- CRS("+init=epsg:3414")
      proj4string(destPt) <- CRS("+init=epsg:3414")
      
      # converts XY coordinates to LongLat coordinates
      originlonglat.df <- as.data.frame(spTransform(originPt,CRS("+proj=longlat")))
      destlonglat.df <- as.data.frame(spTransform(destPt,CRS("+proj=longlat")))
      originPt$LONGITUDE <- originlonglat.df$X.COORDINATES
      originPt$LATITUDE <- originlonglat.df$Y.COORDINATES
      destPt$LONGITUDE <- destlonglat.df$X.COORDINATES
      destPt$LATITUDE <- destlonglat.df$Y.COORDINATES
      
      originPt_df <- as.data.frame(originPt)
      destPt_df <- as.data.frame(destPt)
      
      distmat <- dist.mat(originPt_df, "POSTAL", "LATITUDE", "LONGITUDE", destPt_df, "POSTAL", "LATITUDE", "LONGITUDE", unit = "km")
      hansen_output <- hansen(distmat,"from","to",attrac=1,"distance",gamma=1, lambda=-2,dtype="exp",accnorm="TRUE")
      originPt_df <- left_join(originPt_df,hansen_output,by=c("POSTAL"="from"))
      
      output$histplot <- renderPlot({
        x <- originPt_df$accessibility
        hist(x,breaks=6)
      })
      
      originPt_df$info <- paste0("Accessibility: ", round(originPt_df$accessibility,3))
      
      pal <- brewer.pal(11,"Spectral")
      pal <- colorRampPalette(pal)
      palData <- classIntervals(originPt_df$accessibility,style="jenks")
      originPt_df$colors <- findColours(palData, pal(100))
      
      leafletProxy("mymap") %>%
        clearGroup("singpost") %>%
        addCircleMarkers(opacity = 1, data = originPt_df, color = ~colors, popup = ~info, group ="stats")
      #  %>%
      #  addLegend("bottomright", pal = palData, values = ~accessibility, title = "Hansen Accessibility")
    }
    else {
      leafletProxy("mymap") %>% clearGroup("stats")
    }
  })
      
      # updates SelectInput when user uploads a new file
      observeEvent(input$userfiles, {  
        if (is.null(input$userfiles)) {    return(NULL)  }  
        file.copy(from = input$userfiles$datapath, to =  paste0('user_',input$userfiles$name )  ) 
        isolate(updateSelectInput(session,"POI",choices = c('SingPost Offices', 'Private Housing', list.files(pattern='user_'))))
        isolate(updateSelectInput(session,"origin",choices = c('SingPost Offices', 'Private Housing', list.files(pattern='user_'))))
        isolate(updateSelectInput(session,"dest",choices = c('SingPost Offices', 'Private Housing', list.files(pattern='user_'))))
      })
      
      output$fileList <- renderText({   
        input$userfiles
        dir(pattern = 'user_') 
      })
      
}

shinyApp(ui, server)
