####
##  Sequence of menus, updating data also
####

game_state <- list(tab = "main")
empty_army <- data.frame(name = "", HP = "",
                            EN = "", attack = "", dmg = "",
                            en_cost = "", charge = "")
empty_army <- empty_army[FALSE, ]
starting_funds <- 100

shinyServer(function(input, output, session) {
  observeEvent(input$newgame, {
    game_state$names <<- c(input$p1name, input$p2name)
    game_state$funds <<- rep(starting_funds, 2)
    game_state$armies <<- list(p1 = empty_army, p2 = empty_army)
    updateNavbarPage(session, "nav", selected = "Build")
    output$army_table1 <- renderTable(game_state$armies[[1]])
    output$army_table2 <- renderTable(game_state$armies[[2]])
    output$army1_comment <- renderText(paste0(game_state$names[1], "'s army:"))
    output$army2_comment <- renderText(paste0(game_state$names[2], "'s army:"))
  })
})