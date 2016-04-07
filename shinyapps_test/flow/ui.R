####
##  Sequence of menus, updating data also
####
library(shiny)
shinyUI(fluidPage(
  titlePanel("Choose your own adventure"),
  fluidRow(
    
    column(3, wellPanel(
      uiOutput("ui")
    )),

    column(3, wellPanel(
      uiOutput("ui2")
    )),
    
    column(3,
           verbatimTextOutput("input_type_text")
    ),
    
    column(3, wellPanel(
      actionButton("submitted", label = "OK")
    ))
  )
))