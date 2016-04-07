library(shiny)

shinyUI(
  fluidPage(    
    titlePanel("Persistent data test"),
    sidebarLayout(      
      sidebarPanel(
        numericInput("inputnum", "Type in a number:", 1000000000),
        actionButton("action", label = "Action"),
        downloadButton('downloadData', 'Download')
      ),
      mainPanel(
        tableOutput('table')  
      )
    )
  )
)
