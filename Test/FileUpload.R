library(shiny) 

ui <- shinyUI(fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Select file to upload' )  
    ),
    mainPanel(
      h4('List of uploaded files:')
      ,verbatimTextOutput('fileList')
    )
  ))
)

server <- shinyServer(function(input, output) {
  
  observe({  
    if (is.null(input$file1) ) {    return(NULL)  }  
    file.copy(from = input$file1$datapath, to =  paste0('userFile_',input$file1$name )  ) 
  }) 
  
  output$fileList <- renderText({  
    input$file1
    dir(pattern = 'userFile_') 
  })
})

shinyApp(ui, server)