renderInputs <- function(prefix) {
  fluidRow(
    column(12, 
           fluidRow(column(4, numericInput(paste0(prefix, "_", "speed"), label = "Speed \\(\\frac{[\\text{km}]}{[\\text{h}]}\\)", min = 10, max = 100, value = 40, step = 1)),
                    column(4, numericInput(paste0(prefix, "_", "travel_cost"), label = "Travel cost per km \\(\\frac{[\\text{currency}]}{[\\text{km}]}\\)", min = 0.5, max = 5, value = 1.5, step = 0.1)),
                    column(4, numericInput(paste0(prefix, "_", "beta5"), label = "Travel comfort \\(\\beta_\\text{TT}\\)", min = 0, max = 0.2, value = 0.01, step = 0.001))
           )
    )   
  )
}

shinyUI(
  fluidPage(
    theme = "bootstrap.css", # Based on shinytheme("paper")
    withMathJax(),
    tags$style(type = "text/css",
               "label {font-size: 10px;}",
               ".recalculating {opacity: 1.0;}"
    ),
    h4("Simulation of Wider Economic Benefits", windowTitle = "CTS WEB"),
    h5("Centrum för Transportstudier, KTH"),
    helpText("Source code can be found ", a("here", href = "https://github.com/savemark/ctsweb")),
    hr(),
    fluidRow(
      source("sidebar.R", local = TRUE)$value,
      source("main.R", local = TRUE)$value
    ),
    fluidRow(
      source("results.R", local = TRUE)$value
    )
  )
)