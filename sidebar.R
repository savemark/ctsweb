column(4,
       tabsetPanel(
         tabPanel("Global parameters",
                  wellPanel(
                    fluidRow(
                      column(6, 
                             numericInput("n", label = "Population size \\(N\\)", min = 2, max = 2000, value = 250, step = 1),
                             numericInput("delta", label = "Scale parameter \\(\\delta>0\\) for the error terms", min = 0.01, max = 0.1, value = 0.01, step = 0.001),
                             numericInput("y", label = "Exogenous income \\(y\\) per day", min = 0, max = 500, value = 100, step = 10)
                      ),
                      column(6,
                             numericInput("tau", label = "Taxation rate \\(\\tau\\)", min = 0, max = 0.5, value = 0.30, step = 0.01),
                             numericInput("spillover.eps", label = "Spillover effect parameter \\(\\gamma\\)", min = 0, max = 0.05, value = 0.01, step = 0.01),
                             numericInput("TIME", label = "Available time per day \\(T \\text{ [h]}\\)", min = 15, max = 24, value = 24, step = 1)
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
         tabPanel("Utility function parameters",
                  wellPanel(
                    fluidRow(
                      column(4, 
                             numericInput("beta2", label = "Consumption parameter \\(\\beta_\\text{CO}\\)", min = 0, max = 1, value = 0.265, step = 0.001)
                      ),
                      column(4,
                             numericInput("beta4", label = "Land-use parameter \\(\\beta_\\text{LU}\\)", min = 0, max = 1, value = 0.089, step = 0.001)
                      ),
                      column(4,
                             numericInput("beta3", label = "Leisure time parameter \\(\\beta_\\text{LE}\\)", min = 0, max = 1, value = 0.646, step = 0.001)
                      )
                    ),
                    fluidRow(
                      column(12,
                             p("Note that \\(\\beta_\\text{CO}+\\beta_\\text{LE}+\\beta_\\text{LU}=1\\) must hold.")
                      )
                    )
                  )
         ),
         tabPanel("Logit Transport Model parameters",
                  wellPanel(
                    fluidRow(
                      column(4,
                             numericInput("sigma", label = "Calibration parameter \\(\\sigma\\)", min = 0.001, max = 0.010, value = 0.004, step = 0.001)
                      )
                    ),
                    fluidRow(
                      column(12,
                             p("The parameter \\(\\sigma\\) is used when calcluating the elasticity of total output (pre-tax) with respect to accessibility. 
                               It needs to be set so that the logit transport model (in the summary) is consistent with avg. travel time in the city.")
                             )
                    )
                  )
         )
       ),
       tabsetPanel(
         tabPanel("City",
                  wellPanel(
                    fluidRow(
                      column(12,
                             plotOutput("cityPlot")
                      )
                    ),
                    hr(),
                    fluidRow(
                      column(4,                     
                             radioButtons("type", "City type:",
                                          c("Default" = "default",
                                            "Random" = "random"),
                                          inline = TRUE)
                      ),
                      column(4,
                             numericInput("nodes", label = "Nodes (only if Random)", value = 30, min = 10, max = 60, step = 1)
                      ),
                      column(4,
                             numericInput("scale", label = "Scale", value = 40, min = 1, max = 50, step = 0.1)
                      )
                    ),
                    fluidRow(
                      column(12,
                             p("Nodes are coloured red. Directed edges with high values (low values) of weighted betweenness centrality are coloured light blue (dark gray). Borders of zones are coloured black.")
                             )
                    )
                  )
         )
       ),
       tabsetPanel(
         tabPanel("Wage rate distribution",
                  wellPanel(
                    fluidRow(
                      column(12,
                             plotOutput("populationWageRatesHistogram")
                      )
                    ),
                    hr(),
                    fluidRow(
                      column(6, 
                             numericInput("median", label = "Median", min = 100000, max = 400000, value = 280000, step = 1000),
                             numericInput("spread", label = "Spread", min = 1, max = 2, value = 1.12, step = 0.01)
                      ),
                      column(6, 
                             numericInput("days", label = "Work days per year", min = 200, max = 365, value = 228, step = 1),
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
       )
)