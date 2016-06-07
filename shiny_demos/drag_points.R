library(shiny)

pts <- rbind(c(1,1), c(2,2), c(3,3))
sel_pt <- 0
DIST_THRES <- 0.1

shinyApp(
  ui = fluidPage(
    fluidRow(
      column(width = 4,
               uiOutput("plotUI1")))
  ),
  
  server = function(input, output, session) {
    curPoints <- reactive({
      if (!is.null(input$plot1_click)) {
        p1c <- c(input$plot1_click$x, input$plot1_click$y)
        dd <- colSums((t(pts) - p1c)^2)
        if (sel_pt != 0) {
          pts[sel_pt, ] <<- p1c
          sel_pt <<- 0
        }
        if (sel_pt == 0 && min(dd) < DIST_THRES) {
          sel_pt <<- which.min(dd)
        }
      }
      pts
    })
    output$plotUI1 <- renderUI({
      plotOutput("plot1", height = 300,
                 click = "plot1_click")
    })
    output$plot1 <- renderPlot({
      plot(curPoints(), xlab = "x", ylab = "y", xlim = c(0, 4), ylim = c(0, 4))
      if (sel_pt != 0) {
        points(curPoints()[sel_pt, , drop=FALSE], cex = 3, col = "red")
      }
    })
  }
)