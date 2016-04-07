####
##  Sequence of menus, updating data also
####

game_state <- list(tab = "main")
empty_army <- data.frame(name = "", cost = "", HP = "", Armor = "",
                            EN = "", repair = "", attack = "", dmg = "",
                            en_cost = "", charge = "")
empty_army <- empty_army[FALSE, ]
starting_funds <- 100

cost_formula <- function(hp, armor, en, atk1_lv, atk1_charge, atk2_lv, atk2_charge, standby_en,
                         repair_time) {
  hp + 5 * armor + en + 2 * atk1_lv + 2 * atk2_lv + 3 * standby_en + floor(60/repair_time)
}

shinyServer(function(input, output, session) {
  observeEvent(input$newgame, {
    game_state$names <<- c(input$p1name, input$p2name)
    game_state$funds <<- rep(starting_funds, 2)
    game_state$armies <<- list(p1 = empty_army, p2 = empty_army)
    game_state$current <<- 1
    updateNavbarPage(session, "nav", selected = "Build")
    output$army_table1 <- renderTable(game_state$armies[[1]])
    output$army_table2 <- renderTable(game_state$armies[[2]])
    output$army1_comment <- renderText(paste0(game_state$names[1], "'s army:"))
    output$army2_comment <- renderText(paste0(game_state$names[2], "'s army:"))
    output$build_turn_message <- renderText(paste0("It is ", game_state$names[1], "'s turn to design a prototype!"))
    output$build_funds_message <- renderText(paste0("You have $", game_state$funds[game_state$current]))
    output$build_cost <- renderText(cost())
  })
  cost <- reactive({
    paste0("This unit will cost $", 
      cost_formula(hp=input$hp, armor=input$armor, en=input$en, 
                   atk1_lv = input$atk1_lv, atk1_charge = input$atk1_charge,
                   atk2_lv = input$atk2_lv, atk2_charge = input$atk2_charge,
                   standby_en = input$standby$en, repair_time = input$repair_time),
      " to build in game.")
  })
})