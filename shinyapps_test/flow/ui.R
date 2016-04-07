####
##  Sequence of menus, updating data also
####
library(shiny)
shinyUI(navbarPage("ROBUILDER",
  navbarMenu("Game",
    tabPanel("New",
      radioButtons("new_game_confirm", "Confirm new game?", c("No", "Yes"))),
    tabPanel("Save",
      textInput("save_file", "Filename?")),
    tabPanel("Load",
             textInput("load_file", "Filename?"))
  ),
  tabPanel("Build",
           textInput("lala","LALA"))
))