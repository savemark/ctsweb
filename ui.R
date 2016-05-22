renderInputs <- function(prefix) {
  tags$style(type = "text/css", paste0("#", prefix, "_cityShow", " {background-color: #ffffff;}", sep = ""))
  fluidRow(
    column(12, 
           verbatimTextOutput(paste0(prefix, "_", "cityShow")),
           h5("Travel controls"),
           fluidRow(column(6, sliderInput(paste0(prefix, "_", "speed"), label = "Speed on all links \\(\\frac{[\\text{km}]}{[\\text{h}]}\\)", min = 10, max = 100, value = 40, step = 1)),
                    column(6, sliderInput(paste0(prefix, "_", "travel_cost"), label = "Travel cost \\(\\frac{[\\text{currency}]}{[\\text{km}]}\\)", min = 0.5, max = 10, value = 1.5, step = 0.1))),
           h5("Travel comfort parameter of the utility function"),
           fluidRow(column(6, sliderInput(paste0(prefix, "_", "beta5"), label = "\\(\\beta_\\text{TT}\\)", min = 0, max = 1, value = 0.07, step = 0.001)))
    )   
  )
}

shinyUI(
  fluidPage(
    withMathJax(),
    tags$style(type="text/css",
               "label {font-size: 10px;}",
               ".recalculating {opacity: 1.0;}"
    ),
    titlePanel("Centrum för Transportstudier", windowTitle = "CTS WEB"),
    h5("Simulation of Wider Economic Benefits"),
    helpText("Questions, bugs? E-mail savemark (at) kth.se"),
    hr(),
    fluidRow(
      source("sidebar.R", local = T)$value,
      source("main.R", local = T)$value
    )
  )
)