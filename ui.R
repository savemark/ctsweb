renderInputs <- function(prefix) {
  wellPanel(
    fluidRow(
      column(6,
             h5("City controls"),
             fileInput(paste0(prefix, "_", "xycoords"), label = "Coordinate matrix"),
             helpText("This is not supported yet. Default cordinate matrix will be used."),
             fileInput(paste0(prefix, "_", "adjacency"), label = "Adjacency matrix"),
             helpText("This is not supported yet. Default adjacency matrix will be used.")
      ),
      column(6,
             h5("Travel controls"),
             sliderInput(paste0(prefix, "_", "speed"), label = "Velocity \\(\\frac{[\\text{km}]}{[\\text{h}]}\\)", min = 0, max = 100, value = 50, step = 1),
             sliderInput(paste0(prefix, "_", "travel_cost"), label = "Travel cost \\(\\frac{[\\text{currency}]}{[\\text{km}]}\\)", min = 0, max = 10, value = 1.5, step = 0.1),
             sliderInput(paste0(prefix, "_", "theta"), label = "\\(\\theta\\)", min = 0, max = 10, value = 0.33, step = 0.01),
             helpText("\\(\\theta\\) is the parameter for travel comfort, or utility gained from time spent travelling, in the utility function.")
      )
    ),
    hr(),
    plotOutput(paste0(prefix, "_", "cityPlot")),
    hr(),
    fluidRow(
      column(6,
             helpText("The running time of the simulation increases with increasing \\(N\\) and \\(|v|\\).")
      ),
      column(6,
             p(actionButton(paste0(prefix, "_", "recalc"), "Run/Re-run simulation", icon("random"))),
             sliderInput(paste0(prefix, "_", "maxit"), label = "Maximum number of iterations", min = 10, max = 1500, value = 50, step = 10)
      )
    )
  )
}

shinyUI(
  fluidPage(
    tags$style(type="text/css",
               #"th, td {padding: 5px; font-size: 10px;}",
               "label {font-size: 10px;}",
               ".recalculating {opacity: 1.0;}"
    ),
    
    titlePanel("CTS' Wider Economic Benefits"),
    helpText("Welcome to CTS' application for simulating wider economic benefits."),
    
    fluidRow(
      column(12, tags$h4("Utility function"))
    ),
    
    fluidRow(
      column(12,   
             wellPanel(
               fluidRow(
                 column(12,
                        withMathJax(),
                        h5("Utility function"),
                        p("The set of utility functions for every worker \\(n\\) is defined to be \\(\\{u^n_{ij}(W, L) : i, j = 1 \\dots |v|,\\, n = 1 \\dots N\\}\\) where \\(|v|\\) is the number of nodes in a graph and
                          $$u_{ij}^n(W, L) = \\alpha \\log(\\tau w_j^n W-p_iL-c_{ij}+G)+\\beta \\log(24-W-t_{ij})+\\gamma \\log(L)+\\theta t_{ij}+H^n_i+D^n_j$$
                          with domain $$C_{ij} = \\{(W, L) : W > 0,\\, L > 0,\\, 24 - t_{ij} > W > \\frac{p_i}{\\tau w^n_j} L + \\frac{c_{ij}-G}{\\tau w^n_j}\\}.$$
                          Maximizing over the set of utility functions, we get a point-set
                          $$\\{(W, L, \\max_{i,j} u^n_{ij}(W,L)) : W, L \\in \\bigcup_{i,j} C_{ij}\\}$$
                          Is this well-defined? More text...")
                 )
               ),
               fluidRow(
                 column(12,
                        h5("Parameter controls"),
                        fluidRow(
                          column(3,
                                 sliderInput("alpha", label = "\\(\\alpha\\)", min = 0, max = 10, value = 0.27, step = 0.001),
                                 div("\\(\\alpha\\) is the parameter for the log of the composite good money.")
                          ),
                          column(3,
                                 sliderInput("beta", label = "\\(\\beta\\)", min = 0, max = 10, value = 0.66, step = 0.001),
                                 helpText("\\(\\beta\\) is the parameter for the log of leisure time.")
                          ),
                          column(3,
                                 sliderInput("gamma", label = "\\(\\gamma\\)", min = 0, max = 10, value = 0.07, step = 0.001),
                                 helpText("\\(\\gamma\\) is the parameter for the log of land use.")
                          ),
                          column(3,
                                 sliderInput("tau", label = "\\(\\tau\\)", min = 0, max = 1, value = 0.70, step = 0.001),
                                 helpText("\\(\\tau\\) is the parameter for how much of income per hour each worker gets to keep for themselves.")
                          )
                        )
                 )
               )
             )
      )
    ),
    
    fluidRow(
      column(12, tags$h4("Population"))
    ),
    
    fluidRow(
      column(12,   
             wellPanel(
               fluidRow(
                 column(12,
                        h5("Number of workers"),
                        sliderInput("n", label = "Number of workers \\(N\\)", min = 100, max = 500, value = 300, step = 1),
                        helpText("Increasing the number of workers will increase the time of each simulation.")
                 )
               ),
               hr(),
               fluidRow(
                 column(3,
                        h5("Income distribution"),
                        helpText("This part controls the parameters of $$w^n_{ij} \\in \\ln N(\\mu, \\sigma)$$ 
                                   The number of observations N in the plot to the right
                                   will be equal to the number of workers times the number of nodes, as each worker is offered a random wage in 
                                   each node. The random number \\(w^n_{ij}\\) will be divided by the number of working days times hours
                                   to get a sensible hourly wage rate.")
                 ),
                 column(3,
                        h5("Income controls"),
                        sliderInput("median", label = "Median", min = 100000, max = 400000, value = 270225, step = 1000),
                        sliderInput("spread", label = "Spread", min = 1, max = 2, value = 1.15, step = 0.01),
                        sliderInput("days", label = "Number of working days per year", min = 200, max = 365, value = 224, step = 1),
                        sliderInput("hours", label = "Avarage working hours per day", min = 1, max = 16, value = 8, step = 0.01)
                 ),
                 column(6,
                        plotOutput("populationIncomeDensity")
                        
                 )
               ),
               hr(),
               fluidRow(
                 column(3,
                        h5("Preference distributions"),
                        helpText("This part controls the parameters of $$H_{i},\\, D_{j} \\in N(\\mu, \\sigma)$$ 
                                   The number of observations N in the plots to the right
                                   will be equal to the number of workers times the number of nodes. Every worker is assumed to have stated
                                   preferences for each residential area (node) and for each workplace (node).")
                 ),
                 column(3,
                        h5("Preference controls"),
                        h6("Residential node preference"),
                        sliderInput("h_mean", label = "\\(\\mu\\)", min = -10, max = 10, value = 0, step = 0.01),
                        sliderInput("h_sd", label = "\\(\\sigma\\)", min = 0, max = 10, value = 1, step = 0.01),
                        hr(),
                        h6("Work node preference"),
                        sliderInput("w_mean", label = "\\(\\mu\\)", min = -10, max = 10, value = 0, step = 0.01),
                        sliderInput("w_sd", label = "\\(\\sigma\\)", min = 0, max = 10, value = 1, step = 0.01)
                 ),
                 column(6,
                        plotOutput("populationPreferenceDensity")
                 )
               )
             )
      )
    ),
    
    fluidRow(
      column(6, tags$h4("Scenario A")),
      column(6, tags$h4("Scenario B"))
    ),
    fluidRow(
      column(6, renderInputs("a")),
      column(6, renderInputs("b"))
    ),
    
    fluidRow(
      column(12, tags$h4("Summary"))
    ),
    
    fluidRow(
      column(12,   
             wellPanel(
               tabsetPanel(
                 tabPanel("WEB",
                          column(3,
                                 tableOutput("WEB"),
                                 tableOutput("CS")
                          ),
                          column(9,
                                 tableOutput("WEBP")
                          )
                 ),
                 tabPanel("Scenario A: Population",
                          tableOutput("simulationPopulationA")
                 ),
                 tabPanel("Scenario A: City",
                          tableOutput("simulationCityA")
                 ),
                 tabPanel("Scenario B: Population",
                          tableOutput("simulationPopulationB")
                 ),
                 tabPanel("Scenario B: City",
                          tableOutput("simulationCityB")
                 ),
                 tabPanel("List of variables",
                          withTags({
                            table(border = 1,
                                  tr(
                                    td(strong("Abbrevation")),
                                    td("i"),
                                    td("j"),
                                    td("inc"),
                                    td("h"),
                                    td("tax"),
                                    td("lu")
                                  ),
                                  tr(
                                    td(strong("Variable")),
                                    td("Node of origin"),
                                    td("Node of destination"),
                                    td("Income per hour"),
                                    td("Work hours per day"),
                                    td("Amount of taxes paid per day"),
                                    td("Land use")
                                  ),
                                  tr(
                                    td(strong("Abbrevation")),
                                    td("u"),
                                    td("u.y"),
                                    td("u.lt"),
                                    td("u.lu"),
                                    td("u.tc"),
                                    td("u.H"),
                                    td("u.D"),
                                    td("du, du.y, du.lt, du.lt, du.tc, du.H, du.D")
                                  ),
                                  tr(
                                    td(strong("Variable")),
                                    td("utility"),
                                    td("utility (money)"),
                                    td("utility (leisure time)"),
                                    td("utility (land use)"),
                                    td("utility (travel comfort)"),
                                    td("utility (residential area)"),
                                    td("utility (workplace)"),
                                    td("delta utility (variable)")
                                  )
                            )
                          })                          
                 )
               )
             )
      )
    )
  )
)
