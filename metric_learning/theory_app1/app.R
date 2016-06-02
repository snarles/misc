library(shiny)

KERNEL_CHOICES <- c("Optimal", "LR_knn", "LR_unif", "LR_gauss")

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
                 numericInput("par_n", "N: ", 20),
                 numericInput("par_s0", "Individual sigma: ", 5.2),
                 numericInput("par_seed", "Random seed: ", 555)
             )
      )
    ),
    fluidRow(
      column(width = 3,
             div(class = "option-group",
                 div(class = "option-header", "Kernel"),
                 numericInput("par_h", "Bandwidth: ", 1.5),
                 numericInput("par_knn", "Neighbors: ", 5),
                 radioButtons("choice_k", "Kernel: ", KERNEL_CHOICES, selected = 1)
             )
      )
    )
  ),
  
  server = function(input, output, session) {
  }
)

