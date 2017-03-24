column(4,
       tabsetPanel(
         tabPanel("Global",
                  wellPanel(
                    fluidRow(
                      column(6, 
                             numericInput("n", label = "Number of classes \\(N\\)", min = 25, max = 100, value = 250, step = 1),
                             numericInput("delta", label = "Scale parameter \\(\\delta>0\\) for the error terms", value = 0.01, step = 0.001),
                             numericInput("y", label = "Exogenous income \\(y\\) per day", min = 0, value = 100, step = 10)
                      ),
                      column(6,
                             numericInput("tau", label = "Taxation rate \\(\\tau\\)", min = 0, max = 0.5, value = 0.30, step = 0.01),
                             numericInput("spillover.eps", label = "Spillover effect parameter \\(\\gamma\\)", min = 0, value = 0.03, step = 0.01),
                             numericInput("TIME", label = "Available time per day \\(T \\text{ [h]}\\)", min = 15, max = 24, value = 16, step = 1)
                      )
                    ),
                    fluidRow(
                      column(12, 
                             p("\\(N\\) is the size of the population, or the number of different choice probability matrices."),
                             p("\\(\\tau \\in [0, 1) \\) is the tax rate parameter. When it is set to 0, the taxation rate is 0%."),
                             p("\\(\\delta > 0\\) is the scale parameter for the Gumbel distributed error terms, i.e. \\(\\epsilon^n_{ij} \\sim \\text{Gu}(0, \\delta^2).\\)"),
                             p("When workers choose work place in the same node, it is possible to include a spillover effect. 
                    The function \\((1+\\frac{1}{A_{j}} N_j)^\\gamma\\) where \\(N_j = \\sum_{n, i} \\pi^n_{i,j}\\) is the number of persons working in node \\(j\\), will act as a factor,  
                    affecting wage rates in the node. To include it, set a value higher than 0 for \\(\\gamma\\).")
                      )
                    )
                  )
         ),
         tabPanel("Utility function",
                  wellPanel(
                    fluidRow(
                      column(4, 
                             numericInput("beta2", label = "Consumption parameter \\(\\beta_\\text{CO}\\)", min = 0, max = 1, value = 0.4, step = 0.001)
                      ),
                      column(4,
                             numericInput("beta4", label = "Land-use parameter \\(\\beta_\\text{LU}\\)", min = 0, max = 1, value = 0.12, step = 0.001)
                      ),
                      column(4,
                             numericInput("beta3", label = "Leisure time parameter \\(\\beta_\\text{LE}\\)", min = 0, max = 1, value = 0.48, step = 0.001)
                      )
                    ),
                    fluidRow(
                      column(12,
                             p("Note that \\(\\beta_\\text{CO}+\\beta_\\text{LE}+\\beta_\\text{LU}=1\\) must hold.")
                      )
                    )
                  )
         )
       ),
       tabsetPanel(
         tabPanel("Wage rates",
                  wellPanel(
                    fluidRow(
                      column(12,
                             plotOutput("populationWageRatesHistogram")
                      )
                    ),
                    hr(),
                    fluidRow(
                      column(6, 
                             numericInput("meanlog", label = "log(mean)", min = log(100000), max = log(400000), value = 12.84, step = 0.01),
                             numericInput("lowerbound", label = "Lower bound", min = 0, value = 150000, step = 1),
                             numericInput("days", label = "Work days per year", min = 200, max = 365, value = 228, step = 1)
                      ),
                      column(6, 
                             numericInput("sdlog", label = "log(standard deviation)", min = log(1), max = log(1000), value = 4.6, step = 1),
                             numericInput("upperbound", label = "Upper bound", min = 0, value = 2000000, step = 1),
                             numericInput("hours", label = "Hours per day", min = 4, max = 12, value = 8, step = 0.01)
                      )
                    ),
                    fluidRow(
                      column(12,
                             p("This part controls the parameters of $$\\omega^n_{j} \\sim e^{\\mu+\\sigma N(0, 1)}.$$",
                               "Here it is assumed that the median and the mean of the wage rate offers are known and the spread is",
                               "defined as \\(\\frac{\\text{mean}}{\\text{median}}\\).",
                               "The median and the spread are related to \\(\\mu\\) and \\(\\sigma\\) by the two equalities
                                                    \\(\\mu = \\log(\\text{median})\\) and \\(\\sigma = \\sqrt{2\\log(\\text{spread})}.\\)
                                                    The simulated numbers \\(\\omega^n_{j}\\) are divided by the number of working days and hours
                                                    to get approximated wage rates.
                                                    The number of observations in the plot to the right
                                                    will be equal to the number of workers \\(N\\) times the number of nodes \\(|V|\\), as each worker is offered a wage rate in 
                                                    each node.")
                      )
                    )
                  )
         ),
         tabPanel("Origin and destination qualities",
                  wellPanel(
                    fluidRow(
                      column(12,
                             plotOutput("populationPreferenceHistogram")
                      )
                    ),
                    hr(),
                    h5("Origin and destination qualities"),
                    fluidRow(
                      column(6,
                             numericInput("omean", label = "Origin \\(\\mu\\)", min = -10, max = 10, value = 0, step = 0.01),
                             numericInput("osd", label = "Origin \\(\\sigma\\)", min = 0, max = 10, value = 0.25, step = 0.01)
                      ),
                      column(6,
                             numericInput("dmean", label = "Destination \\(\\mu\\)", min = -10, max = 10, value = 0, step = 0.01),
                             numericInput("dsd", label = "Destination \\(\\sigma\\)", min = 0, max = 10, value = 0.25, step = 0.01)
                      )
                    ),
                    fluidRow(
                      column(12,
                             p("This part controls the parameters of $$q^n_{i},\\, q^n_{j} \\sim N(\\mu, \\sigma)$$",
                               "in the utility function above. The number of preferences in the plots to the right
                               will be equal to the number of workers \\(N\\) times the number of nodes \\(|V|\\). Every worker is assumed to have 
                               preferences for each residential area (node) and for each work place (node).")
                      )
                    )
                  )
         )
       ),
       tabsetPanel(
         tabPanel("Logit Transport Model",
                  wellPanel(
                    fluidRow(
                      column(4,
                             numericInput("sigma", label = "Calibration parameter \\(\\sigma\\)", min = 0.001, value = 0.001, step = 0.001)
                      )
                    ),
                    fluidRow(
                      column(12,
                             p("The parameter \\(\\sigma\\) is used when calcluating the elasticity of total output (pre-tax) with respect to accessibility. 
                               It needs to be set so that the logit transport model (in the summary) is consistent with avg. travel time in the city. 
                               This can be adjusted after the simulation has finished.")
                             )
                      )
                    )
                  )
       )
)