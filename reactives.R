# Reactive expressions
cityInput <- reactive({
  if (input$type == "default") x <- city(input$scale*xy, adjacency, mode = input$mode)
  if (input$type == "random") x <- city.random(input$scale*matrix(c(runif(input$nodes), runif(input$nodes)), input$nodes, 2), mode = input$mode)
  if (input$type == "grid") x <- city.grid(input$sqrtnodes, scale = input$scale, mode = input$mode)
  x <- setEdgeSpeed(x, value = input$a_speed)  
  x <- setEdgeCostFactor(x, value = input$a_travel_cost)
  x <- setEdgeComfort(x, value = input$a_beta5)
  return(x)
})

populationInput <- reactive({ # Population
  x <- population(input$n, 
                  getNodeCount(cityInput()),
                  kn = input$kn,
                  lowerbound = input$lowerbound,
                  upperbound = input$upperbound,
                  meanlog = input$meanlog, 
                  sdlog = input$sdlog, 
                  days = input$days,
                  hours = input$hours,
                  omean = input$omean,
                  osd = input$osd, 
                  dmean = input$dmean,
                  dsd = input$dsd)
  return(x)
})

output$scenarioInput <- renderUI({
  if (input$scenario == "default") {
    selectInput("linkids", "Link ids", 1:getEdgeCount(cityInput()), selected = 1:getEdgeCount(cityInput()), multiple = TRUE,
                selectize = TRUE, width = NULL, size = NULL)
  } else if (input$scenario == "permutation") {
    selectInput("linkids", "Link ids", 1:getEdgeCount(cityInput()), selected = NULL, multiple = TRUE,
                selectize = TRUE, width = NULL, size = NULL)
  }
})

networkWeightsInput <- reactive({
  base_speed <- matrix(input$a_speed, 1, getEdgeCount(cityInput()))
  base_costfactor <- matrix(input$a_travel_cost, 1, getEdgeCount(cityInput()))
  base_comfort <- matrix(input$a_beta5, 1, getEdgeCount(cityInput()))
  if (input$scenario == "default") {
    alternative_speed <- matrix(input$a_speed, 1, getEdgeCount(cityInput()))
    alternative_costfactor <- matrix(input$a_travel_cost, 1, getEdgeCount(cityInput()))
    alternative_comfort <- matrix(input$a_beta5, 1, getEdgeCount(cityInput()))
    ids <- as.numeric(input$linkids)
    alternative_speed[1, ids] <- input$b_speed
    alternative_costfactor[1, ids] <- input$b_travel_cost
    alternative_comfort[1, ids] <- input$b_beta5
  } else if (input$scenario == "permutation") {
    alternative_speed <- matrix(input$a_speed, length(input$linkids), getEdgeCount(cityInput()))
    alternative_costfactor <- matrix(input$a_travel_cost, length(input$linkids), getEdgeCount(cityInput()))
    alternative_comfort <- matrix(input$a_beta5, length(input$linkids), getEdgeCount(cityInput()))
    ids <- as.numeric(input$linkids)
    for (i in seq_along(ids)) {
      alternative_speed[i, ids[i]] <- input$b_speed
      alternative_costfactor[i, ids[i]] <- input$b_travel_cost
      alternative_comfort[i, ids[i]] <- input$b_beta5
    }
  }
  speeds <- rbind(base_speed, alternative_speed)
  costfactors <- rbind(base_costfactor, alternative_costfactor)
  comforts <- rbind(base_comfort, alternative_comfort)
  x <- abind(speeds, costfactors, comforts, along = 3)
  return(x)
})

observeEvent(input$run_economy, {
  output$scenario_ID_base <- renderUI({ # This could perhaps be turned into a module instead
    radioButtons("scenario_id_base", "Scenario ID", 1, inline = TRUE)
  })
}, ignoreInit = TRUE)

observeEvent(input$run_economy, {
  output$scenario_ID_alt <- renderUI({
    if (nrow(networkWeightsInput()) == 1) return(radioButtons("scenario_id_alt", "Scenario ID", 1, inline = TRUE))
    radioButtons("scenario_id_alt", "Scenario ID", 2:nrow(networkWeightsInput()), inline = TRUE)
  })
}, ignoreInit = TRUE)

observeEvent(input$run_economy, {
  output$scenario_ID_fixed <- renderUI({
    if (nrow(networkWeightsInput()) == 1) return(radioButtons("scenario_id_fixed", "Scenario ID", 1, inline = TRUE))
    radioButtons("scenario_id_fixed", "Scenario ID", 2:nrow(networkWeightsInput()), inline = TRUE)
  })
}, ignoreInit = TRUE)

observeEvent(input$run_economy, {
  output$scenario_ID_base_plot <- renderUI({
    radioButtons("scenario_id_base_plot", "Scenario ID", 1, inline = TRUE)
  })
}, ignoreInit = TRUE)

observeEvent(input$run_economy, {
  output$scenario_ID_alt_plot <- renderUI({
    if (nrow(networkWeightsInput()) == 1) return(radioButtons("scenario_id_alt_plot", "Scenario ID", 1, inline = TRUE))
    radioButtons("scenario_id_alt_plot", "Scenario ID", 2:nrow(networkWeightsInput()), inline = TRUE)
  })
}, ignoreInit = TRUE)

observeEvent(input$run_economy, {
  output$scenario_ID_fixed_plot <- renderUI({
    if (nrow(networkWeightsInput()) == 1) return(radioButtons("scenario_id_fixed_plot", "Scenario ID", 1, inline = TRUE))
    radioButtons("scenario_id_fixed_plot", "Scenario ID", 2:nrow(networkWeightsInput()), inline = TRUE)
  })
}, ignoreInit = TRUE)

economyInput <- reactive({
  x <- economy(cityInput(), 
               populationInput(), 
               networkWeightsInput(),
               utilityWrapper(c(input$beta2, input$beta3, input$beta4, input$tau, input$y, input$TIME, input$delta)),
               probability = probabilityClosure(input$delta),
               spillover = spilloverClosure(input$spillover.eps, getArea(cityInput()), getDistance(cityInput())))
  return(x)
})

parameters <- reactiveValues()
observeEvent(input$run_economy, {
  parameters$tau <- input$tau
  parameters$a_speed <- input$a_speed
  parameters$b_speed <- input$b_speed
  parameters$a_travel_cost <- input$a_travel_cost
  parameters$b_travel_cost <- input$b_travel_cost
  parameters$a_beta5 <- input$a_beta5
  parameters$b_beta5 <- input$b_beta5
  parameters$delta <- input$delta
  parameters$type <- input$type
})

simulateEconomyInput <- eventReactive(input$run_economy, { # Simulation
  if (input$landUseOption == "nonfixed" || input$landUseOption == "both") {x <- simulate(economyInput(), input$guess)}
  else {return()}
  return(x)
})

simulateEconomyFixedInput <- eventReactive(input$run_economy, { # Simulation
  if (input$landUseOption == "fixed" || input$landUseOption == "both") {x <- simulate(economyInput(), input$guess, fixed.landuse = TRUE)}
  else {return()}
  return(x)
})