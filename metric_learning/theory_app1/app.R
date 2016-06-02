library(shiny)
library(pracma)

KERNEL_CHOICES <- c("Optimal", "LR_knn", "LR_unif", "LR_gauss")

v0 <- 100 # variance for global mean

wts_lr <- function(xs, wts, x) {
  xwx <- t(xs) %*% diag(wts) %*% xs
  xw <- t(xs) %*% diag(wts)
  as.numeric(x %*% solve(xwx, xw))
}

shinyApp(
  ui = fluidPage(
    # Some custom CSS
    tags$head(
      tags$style(
        HTML(
          "
           .option-group {
           border: 1px solid #ccc;
           border-radius: 6px;
           padding: 0px 5px;
           margin: 5px -10px;
           background-color: #f5f5f5;
           }

           .option-header {
           color: #79d;
           text-transform: uppercase;
           margin-bottom: 5px;
           }
          "
        )
      )
    ),
    fluidRow(
      column(width = 3,
             div(class = "option-group",
                 div(class = "option-header", "Generation"),
                 numericInput("par_lambda", "Lambda: ", 1.5),
                 selectInput("par_lin", "Linearity: ", c(0, 99), selected = 99),
                 numericInput("par_n", "N: ", 20),
                 numericInput("par_s0", "Individual sigma: ", 5.2),
                 numericInput("par_seed", "Random seed: ", 555)
             )
      ),
      column(width = 3,
             div(class = "option-group",
                 div(class = "option-header", "Kernel"),
                 numericInput("par_h", "Bandwidth: ", 1.5),
                 numericInput("par_knn", "Neighbors: ", 5),
                 radioButtons("choice_k", "Kernel: ", KERNEL_CHOICES, selected = "Optimal")
             )
      ),
      column(width = 4,
             div(
             uiOutput("plotUI1"),
             uiOutput("plotUI2"),
             uiOutput("plotUI3")))
    )
  ),
  
  server = function(input, output, session) {
    curPoints <- reactive({
      set.seed(input$par_seed)
      xs <- randn(input$par_n, 2)
      xs
    })
    curWeights <- reactive({
      n <- input$par_n
      nn <- input$par_knn
      vN <- input$par_s0^2
      vX <- as.numeric(input$par_lin)
      hh <- input$par_lambda
      ans <- rep(1, n)/n
      xs <- curPoints()
      if (!is.null(input$plot1_click)) {
        p1c <- t(c(input$plot1_click$x, input$plot1_click$y))
        if (input$choice_k == "Optimal") {
          xs2 <- rbind(xs, p1c)
          dd0 <- pdist(xs2)
          covm <- v0 + exp(-dd0/(hh^2)) + vN * eye(n + 1) + 
            vX * (xs2[, 1] %*% t(xs2[, 1])) + vX * (xs2[, 2] %*% t(xs2[, 2]))
          ans <- as.numeric(solve(covm[-(n+1), -(n+1)], covm[-(n+1), (n+1)]))
        }
        if (input$choice_k == "LR_knn") {
          dd <- as.numeric(pdist2(p1c, xs))
          inds <- which(dd <= sort(dd)[nn])
          ans <- numeric(n)
          xsi <- xs[inds, , drop = FALSE]
          px <- p1c %*% solve(t(xsi) %*% xsi, t(xsi))
          ans[inds] <- px
        }
        if (input$choice_k == "LR_unif") {
          dd <- as.numeric(pdist2(p1c, xs))
          inds <- which(dd <= input$par_h)
          ans <- numeric(n)
          xsi <- xs[inds, , drop = FALSE]
          px <- p1c %*% solve(t(xsi) %*% xsi, t(xsi))
          ans[inds] <- px
        }
        if (input$choice_k == "LR_gauss") {
          dd <- as.numeric(pdist2(p1c, xs))
          ans <- wts_lr(xs, exp(-dd/(input$par_h)^2), p1c)
        }
      }
      ans
    })
    output$plotUI1 <- renderUI({
      plotOutput("plot1", height = 300,
                 click = "plot1_click")
    })
    output$plotUI2 <- renderUI({
      plotOutput("plot2", height = 300)
    })
    output$plotUI3 <- renderUI({
      plotOutput("plot3", height = 200)
    })
    output$plot1 <- renderPlot({
      plot(curPoints(), xlab = "x", ylab = "y")
    })
    output$plot2 <- renderPlot({
      xs <- curPoints()
      plot(xs, xlab = "x", ylab = "y", col = "white")
      if (!is.null(input$plot1_click)) {
        points(input$plot1_click$x, input$plot1_click$y, pch = "+", cex = 2)
      }
      wts <- curWeights()
      n <- input$par_n
      for (i in 1:n) {
        if (wts[i] >= 0) cl <- grey(level = 1 - pmax(0, wts[i]/max(wts)))
        if (wts[i] < 0) cl <- rgb(1 -wts[i]/min(wts), 1, 1-wts[i]/min(wts))
        points(xs[i, , drop = FALSE], col = cl)
      }
    })
    output$plot3 <- renderPlot({
      if (is.null(input$plot1_click)) {
        plot(NA, NA, xlim = c(0, 1), ylim = c(0, 1), ann = FALSE, axes = FALSE)
      } else {
        barplot(sort(curWeights()))
      }
    })
  }
)

