column(8,
       fluidRow(
         column(6,
                tabsetPanel(
                  tabPanel("Base scenario",
                           wellPanel(
                             renderInputs("a")
                           )
                  )
                )
         ),
         column(6,
                tabsetPanel(
                  tabPanel("Do-something scenario",
                           wellPanel(
                             renderInputs("b")
                           )
                  )
                )
         )
       ),
       wellPanel(
         fluidRow(
           
           column(2,
                  numericInput("guess", "Land price guess", value = 50, min = 1, max = 100)
           ),
           column(10,
                  p("Land price guess for the Base scenario. The solution will be used as a guess for the Do-something scenario.")
           )
         ),
         fluidRow(
           column(2,
                  actionButton("run", "Run/Re-run", icon("refresh"), width = "90%")
           ),
           column(10,
                  p("Note that running the simulation usually takes 1-2 minutes. 
                    Run time depends mostly on population size, the number of origins/destinations and if there is a spillover 
                    effect in the economy. Also note that when the simulation has finished, some of the tables might take some time to load because of the 
                    large number of entries.")
           )
         )
       ),
       fluidRow(
         column(12, 
                tabsetPanel(
                  tabPanel("Summary",
                           wellPanel(
                             tags$style(type='text/css', '#summary {background-color: rgba(255,255,255,1);}'),
                             verbatimTextOutput("summary")
                           )
                  ),     
                  tabPanel("Plots",
                           tabsetPanel(
                             tabPanel("Demand plots",
                                      column(6,
                                             plotOutput("wageratedensity"),
                                             plotOutput("x2density"),
                                             plotOutput("x4density"),
                                             plotOutput("vktdensity"),
                                             plotOutput("opdensity")
                                      ),
                                      column(6,
                                             plotOutput("x1density"),
                                             plotOutput("x3density"),
                                             plotOutput("x5density"),
                                             plotOutput("incomedensity"),
                                             plotOutput("dpdensity")
                                      )
                             ),
                             tabPanel("Equity plots",
                                      fluidRow(
                                        column(12,
                                               plotOutput("equity")
                                        )
                                      ),
                                      hr(),
                                      fluidRow(
                                        column(3,
                                               numericInput("probs", "Number of income classes", value = 3, min = 1, max = 10, step = 1)
                                        )
                                      )
                             ),
                             tabPanel("3D Plots",
                                      column(6,
                                             plotOutput("residency3dA", width = "100%", height = "600px"),
                                             plotOutput("price3dA", width = "100%", height = "600px"),
                                             plotOutput("work3dA", width = "100%", height = "600px")
                                      ),
                                      column(6,
                                             plotOutput("residency3dB", width = "100%", height = "600px"),
                                             plotOutput("price3dB", width = "100%", height = "600px"),
                                             plotOutput("work3dB", width = "100%", height = "600px")
                                      )
                             )
                           )
                  ),
                  tabPanel("Tables",
                           tabsetPanel(
                             tabPanel("Base scenario",
                                      tabsetPanel(
                                        tabPanel("City",
                                                 dataTableOutput("simulationCityA")
                                        ),
                                        tabPanel("Population",
                                                 dataTableOutput("simulationPopulationA")
                                        )
                                      )
                             ),
                             tabPanel("Do-something scenario",
                                      tabsetPanel(
                                        tabPanel("City",
                                                 dataTableOutput("simulationCityB")
                                        ),
                                        tabPanel("Population",
                                                 dataTableOutput("simulationPopulationB")
                                        )
                                      )
                             )
                           )
                  )
                )
         )
       )
)