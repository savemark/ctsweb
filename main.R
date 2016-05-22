column(8,
       fluidRow(
         column(6,
                wellPanel(
                  h4("Base scenario"),
                  renderInputs("a")
                )
         ),
         column(6,
                wellPanel(
                  h4("Do-something scenario"),
                  renderInputs("b")
                )
         )
       ),
       fluidRow(
         column(2,
                actionButton("run", "Run/Re-run simulation", icon("random"))
         ),
         column(10,
                p("Note that running the simulation can take several minutes. 
                  Run time depends mostly on population size, the number of origins/destinations and if there is a spillover 
                  effect in the economy.")
                )
       ),
       tags$br(),
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
                  tabPanel("Base scenario",
                           tabsetPanel(
                             tabPanel("City",
                                      dataTableOutput("simulationCityA")
                             ),
                             tabPanel("Population",
                                      dataTableOutput("simulationPopulationA")
                             ),
                             tabPanel("Plots",
                                      column(6,
                                             "-" #webGLOutput("cityPrice")
                                             #plotOutput("cityPlotA")
                                      ),
                                      column(6, 
                                             "-" #webGLOutput("cityPopulationDensity")
                                      )
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
                             ),
                             tabPanel("Plots",
                                      column(6,
                                             #plotOutput("cityPopulationPlotA"),
                                             "-" #webGLOutput("cityPrice")
                                             #plotOutput("cityPlotA")
                                      ),
                                      column(6, 
                                             "-" #webGLOutput("cityPopulationDensity")
                                      )
                             )
                           )
                  )
                )
         )
       )
)