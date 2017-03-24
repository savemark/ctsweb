column(8,
       fluidRow(
         column(6,
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
                               column(6,                     
                                      radioButtons("type", "City type:",
                                                   c("Default" = "default",
                                                     "Random" = "random",
                                                     "Grid" = "grid"),
                                                   selected = "grid",
                                                   inline = TRUE)
                               ),
                               column(6,                     
                                      radioButtons("mode", "Network mode:",
                                                   c("Undirected" = "undirected",
                                                     "Directed" = "directed"),
                                                   selected = "undirected",
                                                   inline = TRUE)
                               )
                             ),
                             fluidRow(
                               conditionalPanel(
                                 condition = "input.type == 'default'",
                                 column(4,
                                        numericInput("scale", label = "Scale", value = 40, min = 1, max = 50, step = 1)
                                 )
                               ),
                               conditionalPanel(
                                 condition = "input.type == 'random'",
                                 column(4,
                                        numericInput("nodes", label = "Nodes", value = 30, min = 10, max = 60, step = 1)
                                 ),
                                 column(4,
                                        numericInput("scale", label = "Scale", value = 40, min = 1, max = 50, step = 1)
                                 )
                               ),
                               conditionalPanel(
                                 condition = "input.type == 'grid'",
                                 column(4,
                                        numericInput("sqrtnodes", label = "\\(\\sqrt(\\text{nodes})\\)", value = 2, min = 2, max = 10, step = 1)
                                 ),
                                 column(4,
                                        numericInput("scale", label = "Scale", value = 40, min = 1, max = 50, step = 1)
                                 )
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
                  tabPanel("Scenarios",
                           wellPanel(
                             p("Base scenario"),
                             renderInputs("a"),
                             p("Alternative scenario(s)"),
                             radioButtons("scenario", "Scenario type:",
                                          c("Multiple links, one alternative scenario" = "default",
                                            "Single link, permutations, several alternative scenarios" = "permutation"),
                                          inline = TRUE),
                             uiOutput("scenarioInput"),
                             renderInputs("b"),
                             fluidRow(
                               column(12,
                                      radioButtons("landUseOption", "Land use option",
                                                   c("Non-fixed" = "nonfixed",
                                                     "Fixed" = "fixed",
                                                     "Both" = "both"),
                                                   inline = TRUE)
                               )
                             ),
                             hr(),
                             fluidRow(
                               column(4,
                                      numericInput("guess", "Land price guess", value = 100, min = 1)
                               ),
                               column(8,
                                      p("Land price guess for all scenarios.")
                               )
                             ),
                             fluidRow(
                               column(12,
                                      p("Note that running the simulation usually takes 1-2 minutes. 
                                        Run time depends mostly on population size, the number of origins/destinations and if there is a spillover 
                                        effect in the economy. Also note that when the simulation has finished, some of the plots and tables might take some time to load because of the 
                                        large number of entries.")
                                      )
                                      ),
                             fluidRow(
                               column(4,
                                      actionButton("run_economy", "Run/Re-run", icon("refresh")),
                                      offset = 4
                               )
                             )
                           )
                  )
                )
         ),
         column(6,
                tabsetPanel(
                  tabPanel("Economy",
                           wellPanel(
                             tags$style(type = "text/css", "#economyShow {background-color: rgba(255,255,255,1);}"),
                             verbatimTextOutput("economyShow")
                           )
                  ),
                  tabPanel("Weights",
                           wellPanel(
                             tags$style(type = "text/css", "#weightsShow {background-color: rgba(255,255,255,1);}"),
                             verbatimTextOutput("weightsShow")
                           )
                  )
                )
         )
       )
)