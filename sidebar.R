column(4,
       wellPanel(
         h4("Global parameters"),
         fluidRow(
           column(6, 
                  sliderInput("n", label = "Population size \\(N\\)", min = 2, max = 2000, value = 250, step = 1),
                  sliderInput("delta", label = "Scale parameter \\(\\delta>0\\) for the error terms", min = 0.0005, max = 1, value = 0.01, step = 0.001)
           ),
           column(6,
                  sliderInput("tau", label = "Taxation rate \\(\\tau\\)", min = 0, max = 0.5, value = 0.30, step = 0.01),
                  sliderInput("spillover.eps", label = "Spillover effect parameter \\(\\gamma\\)", min = 0, max = 0.05, value = 0, step = 0.01)
           )
         ),
         fluidRow(
           column(12, 
                  tags$h5("Population size"),
                  p("\\(N\\) is the size of the population, or the number of different choice sets. 
                    All choice sets are equal in size and depend on the number of origins/destinations in the city."),
                  tags$h5("Taxation rate"),
                  p("\\(\\tau \\in [0, 1) \\) is the tax rate parameter. When it is set to 0, the taxation rate is 0%."),
                  tags$h5("Scale parameter for the random utility error terms"),
                  p("\\(\\delta > 0\\) is the scale parameter for the Gumbel distributed error terms, i.e. \\(\\epsilon^n_{ij} \\sim \\text{Gu}(0, \\delta^2)\\)."),
                  tags$h5("Spillover effect"),                                  
                  p("When workers choose work place in the same node, it is possible to include a spillover effect. 
                    The function \\((1+\\alpha_i N_j)^\\gamma\\) where \\(N_j\\) is the number of persons working in node \\(j\\), will act as a factor,  
                    affecting wage rates in the node. To include it, set a value higher than 0 for \\(\\gamma\\).")
           )
         )
       ),
       wellPanel(
         h4("City"),
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
                  sliderInput("nodes", label = "Nodes (only if Random)", value = 30, min = 10, max = 60, step = 1)
           ),
           column(4,
                  sliderInput("scale", label = "Scale", value = 7.5, min = 1, max = 25, step = 0.1)
           )
         )
       ),
       wellPanel(
         h4("Utility function parameters"),
         fluidRow(
           column(6, 
                  sliderInput("beta2", label = "Consumption parameter \\(\\beta_\\text{CO}\\)", min = 0, max = 1, value = 0.265, step = 0.001),
                  sliderInput("beta4", label = "Land-use parameter \\(\\beta_\\text{LU}\\)", min = 0, max = 1, value = 0.089, step = 0.001)
           ),
           column(6,
                  sliderInput("beta3", label = "Leisure time parameter \\(\\beta_\\text{LE}\\)", min = 0, max = 1, value = 0.646, step = 0.001)
           )
         ),
         fluidRow(
           column(12,
                  p("Note that \\(\\beta_\\text{CO}+\\beta_\\text{LE}+\\beta_\\text{LU}=1\\) must hold.")
           )
         )
       ),
       wellPanel(
         h4("Wage rate distribution"),
         fluidRow(
           column(12,
                  plotOutput("populationWageRatesHistogram")
           )
         ),
         hr(),
         fluidRow(
           column(6, 
                  sliderInput("median", label = "Median", min = 100000, max = 400000, value = 210000, step = 1000),
                  sliderInput("spread", label = "Spread", min = 1, max = 2, value = 1.12, step = 0.01)
           ),
           column(6, 
                  sliderInput("days", label = "Work days per year", min = 200, max = 365, value = 228, step = 1),
                  sliderInput("hours", label = "Hours per day", min = 1, max = 16, value = 8, step = 0.01)
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
       ),
       wellPanel(
         h4("Origin and destination qualities"),
         fluidRow(
           column(12,
                  plotOutput("populationPreferenceHistogram")
           )
         ),
         hr(),
         fluidRow(
           column(6,
                  sliderInput("omean", label = "Origin \\(\\mu\\)", min = -10, max = 10, value = 0, step = 0.01),
                  sliderInput("osd", label = "Origin \\(\\sigma\\)", min = 0, max = 10, value = 0.25, step = 0.01)
           ),
           column(6,
                  sliderInput("dmean", label = "Destination \\(\\mu\\)", min = -10, max = 10, value = 0, step = 0.01),
                  sliderInput("dsd", label = "Destination \\(\\sigma\\)", min = 0, max = 10, value = 0.25, step = 0.01)
           )
         ),
         fluidRow(
           column(12,
                  p("This part controls the parameters of $$q_{n,i},\\, q_{n,j} \\sim N(\\mu, \\sigma)$$",
                    "in the utility function above. The number of preferences in the plots to the right
                                     will be equal to the number of workers \\(N\\) times the number of nodes \\(|V|\\). Every worker is assumed to have 
                                     preferences for each residential area (node) and for each work place (node).")
           )
         )
       )
)