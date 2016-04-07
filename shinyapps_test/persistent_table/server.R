a <- data.frame(num = c(100, 200))

shinyServer(function(input, output) {
  ntext <- eventReactive(input$action, {
    a <<- rbind(a, input$inputnum)
  })
  output$table <- renderTable({
    ntext()
  })
  output$downloadData <- downloadHandler(
    filename = 'temp.csv',
    content = function(file) {
      write.csv(a, file)
    }
  )
})