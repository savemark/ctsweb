column(12, 
       tabsetPanel(
         tabPanel("Summary",
                  wellPanel(
                    tags$style(type='text/css', '#summary {background-color: rgba(255,255,255,1);}'),
                    tags$style(type='text/css', '#summary_economy {background-color: rgba(255,255,255,1);}'),
                    tags$style(type='text/css', '#summary_economy_fixed {background-color: rgba(255,255,255,1);}'),
                    fluidRow(
                      column(2,
                             checkboxInput("showConvergenceMessage", "Show convergence message(s)", value = FALSE)
                      )
                    ),
                    verbatimTextOutput("summary_economy"),
                    verbatimTextOutput("summary_economy_fixed")
                  )
         ),     
         tabPanel("Plots",
                  tabsetPanel(
                    tabPanel("Demand",
                             h5("Non-fixed"),
                             verticalLayout(
                               splitLayout(
                                 plotOutput("argmax1", width = 400),
                                 plotOutput("argmax2", width = 400),
                                 plotOutput("argmax3", width = 400),
                                 plotOutput("argmax4", width = 400)
                               ),
                               splitLayout(
                                 plotOutput("argmax5", width = 400),
                                 plotOutput("wageratedensi", width = 400),
                                 plotOutput("incomedensi", width = 400),
                                 plotOutput("vktdensi", width = 400)
                               )
                             ),
                             hr(),
                             h5("Fixed"),
                             verticalLayout(
                               splitLayout(
                                 plotOutput("argmax1_fixed", width = 400),
                                 plotOutput("argmax2_fixed", width = 400),
                                 plotOutput("argmax3_fixed", width = 400),
                                 plotOutput("argmax4_fixed", width = 400)
                               ),
                               splitLayout(
                                 plotOutput("argmax5_fixed", width = 400),
                                 plotOutput("wageratedensi_fixed", width = 400),
                                 plotOutput("incomedensi_fixed", width = 400),
                                 plotOutput("vktdensi_fixed", width = 400)
                               )
                             )
                    ),
                    tabPanel("Non-fixed vs. fixed",
                             plotOutput("scatter_nonfixed_vs_fixed", width = 400)
                    ),
                    tabPanel("Equity",
                             fluidRow(
                               column(3,
                                      numericInput("probs", "Number of income classes", value = 3, min = 1, max = 10, step = 1)
                               )
                             ),
                             hr(),
                             fluidRow(
                               column(12,
                                      plotOutput("equity", width = 400)
                               )
                             )
                    ),
                    tabPanel("3D",
                             column(4,
                                    uiOutput("scenario_ID_base_plot"),
                                    plotOutput("price3dA", width = "100%", height = "600px"),
                                    plotOutput("residency3dA", width = "100%", height = "600px"),
                                    plotOutput("work3dA", width = "100%", height = "600px")
                             ),
                             column(4,
                                    uiOutput("scenario_ID_alt_plot"),
                                    plotOutput("price3dB", width = "100%", height = "600px"),
                                    plotOutput("residency3dB", width = "100%", height = "600px"),
                                    plotOutput("work3dB", width = "100%", height = "600px")
                             ),
                             column(4,
                                    uiOutput("scenario_ID_fixed_plot"),
                                    plotOutput("price3d_fixed", width = "100%", height = "600px"),
                                    plotOutput("residency3d_fixed", width = "100%", height = "600px"),
                                    plotOutput("work3d_fixed", width = "100%", height = "600px")
                             )
                    )
                  )
         ),
         tabPanel("Tables",
                  tabsetPanel(
                    tabPanel("Base scenario",
                             uiOutput("scenario_ID_base"),
                             tabsetPanel(
                               tabPanel("City",
                                        dataTableOutput("simulateEconomyCityA")
                               ),
                               tabPanel("Paths",
                                        br(),
                                        p("Note that Total cost (time) is the sum of link costs (times) and within-zone costs (times)."),
                                        br(),
                                        dataTableOutput("simulateEconomyPathA")
                               ),
                               tabPanel("Population",
                                        dataTableOutput("simulateEconomyPopulationA")
                               )
                             )
                    ),
                    tabPanel("Alternative scenario(s)",
                             uiOutput("scenario_ID_alt"),
                             tabsetPanel(
                               tabPanel("City",
                                        dataTableOutput("simulateEconomyCityB")
                               ),
                               tabPanel("Paths",
                                        br(),
                                        p("Note that Total cost (time) is the sum of link costs (times) and within-zone costs (times)."),
                                        br(),
                                        dataTableOutput("simulateEconomyPathB")
                               ),
                               tabPanel("Population",
                                        dataTableOutput("simulateEconomyPopulationB")
                               )
                             )
                    ),
                    tabPanel("Fixed land-use", 
                             uiOutput("scenario_ID_fixed"),
                             tabsetPanel(
                               tabPanel("City",
                                        dataTableOutput("simulateEconomyCityC")
                               ),
                               tabPanel("Paths",
                                        br(),
                                        p("Note that Total cost (time) is the sum of link costs (times) and within-zone costs (times)."),
                                        br(),
                                        dataTableOutput("simulateEconomyPathC")
                               ),
                               tabPanel("Population",
                                        dataTableOutput("simulateEconomyPopulationC")
                               )
                             )
                    )
                  )
         )
       )
)