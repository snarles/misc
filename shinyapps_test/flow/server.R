####
##  Sequence of menus, updating data also
####

game_state <- list(tab = "main")

shinyServer(function(input, output) {
  output$ui <- renderUI({
    if (game_state$tab == "main") {
      return(
        textInput("name", "Enter your name", value = "anon")
      )
    }
  })
})