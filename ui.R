
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Accessibility Visualisation Tool by Acces(S)ingapore"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("layers",
                         "Toggle layers to be displayed:",
                         c("Kernel Density" = "kde",
                           "Buffer Analysis" = "buffer")),
      fileInput('userfiles',
                'Select file to upload'),
      selectInput('origin', 
                  'Select origin locations:', 
                  c(Choose='','SingPost Offices','SCDF Ambulances'),
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
      plotOutput("distPlot"),
      
    )
  )
))
