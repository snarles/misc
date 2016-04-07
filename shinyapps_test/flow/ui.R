####
##  Sequence of menus, updating data also
####
library(shiny)
shinyUI(navbarPage("ROBUILDER", id = 'nav',
  tabPanel("New Game",
    fluidRow(
      column(3, textInput("p1name", "Player 1 name:", "Sente"),
             textInput("p2name", "Player 2 name:", "Gote"),
             actionButton("newgame", "New Game"))
  )),
  tabPanel("Build",
    fluidRow(
      column(4, textOutput('army1_comment'),
      tableOutput('army_table1'),
      textOutput('army2_comment'),
      tableOutput('army_table2'))
    ))
))