renderInputs <- function(prefix) {
  fluidRow(
    column(12, 
           verbatimTextOutput(paste0(prefix, "_", "cityShow")),
           tags$style(type = "text/css", paste0("#", prefix, "_cityShow {background-color: rgba(255,255,255,1);}", sep = "")),
           fluidRow(column(4, numericInput(paste0(prefix, "_", "speed"), label = "Speed on all links \\(\\frac{[\\text{km}]}{[\\text{h}]}\\)", min = 10, max = 100, value = 40, step = 1)),
                    column(4, numericInput(paste0(prefix, "_", "travel_cost"), label = "Travel cost \\(\\frac{[\\text{currency}]}{[\\text{km}]}\\)", min = 0.5, max = 5, value = 1.5, step = 0.1)),
                    column(4, numericInput(paste0(prefix, "_", "beta5"), label = "Travel comfort \\(\\beta_\\text{TT}\\)", min = 0, max = 0.2, value = 0.07, step = 0.001))
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
    titlePanel("Simulation of Wider Economic Benefits", windowTitle = "CTS WEB"),
    h5("Centrum för Transportstudier"),
    helpText("Source code can be found ", a("here", href = "https://github.com/savemark/ctsweb")),
    hr(),
    fluidRow(
      source("sidebar.R", local = TRUE)$value,
      source("main.R", local = TRUE)$value
    )
  )
)