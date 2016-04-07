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
      tableOutput('army_table2'),
      textOutput('build_turn_message'),
      textOutput('build_funds_message'),
      textOutput('build_cost'),
      sliderInput('hp', 'Hit Points (HP):', min = 10, max = 70, value = 50),
      sliderInput('armor', 'Armor:', min = 0, max = 20, value = 0),
      sliderInput('en', 'Energy (EN):', min = 20, max = 60, value = 30),
      textInput('atk1_name', 'Attack 1 name:', 'Attack 1'),
      sliderInput('atk1_lv', 'Attack 1 level (0 for no attack)', min = 0, max = 10, value = 5),
      sliderInput('atk1_charge', 'Attack 1 charge time', min = 0, max = 5, value = 0),
      textInput('atk2_name', 'Attack 2 name:', 'Attack 1'),
      sliderInput('atk2_lv', 'Attack 2 level (0 for no attack)', min = 0, max = 10, value = 0),
      sliderInput('atk1_charge', 'Attack 2 charge time', min = 0, max = 5, value = 0),
      sliderInput('standby_en', 'EN recharge rate', min = 5, max = 20, value = 10),
      sliderInput('repair_time', 'Repair time', min = 2, max = 10, value = 5),
      actionButton('add_proto', 'ADD PROTOTYPE')
      )
    ))
))