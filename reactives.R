# Reactive expressions

# Cities for simulations A and B
cityInput <- reactive({
  if (input$type == "default") x <- city(input$scale*xy, adjacency)
  if (input$type == "random") x <- city.delunay(input$scale*matrix(c(runif(input$nodes), runif(input$nodes)), input$nodes, 2), 
                                                speed = 1, 
                                                costfactor = 1)
  return(x)
})

cityInputA <- reactive({
  x <- cityInput()
  setSpeed(x) <- input$a_speed
  setCostFactor(x) <- input$a_travel_cost
  return(x)
})

cityInputB <- reactive({
  x <- cityInput()
  setSpeed(x) <- input$b_speed
  setCostFactor(x) <- input$b_travel_cost
  return(x)
})

populationInput <- reactive({ # Population
  x <- population(input$n, 
                  getNodeCount(cityInput()), 
                  median = input$median, 
                  spread = input$spread, 
                  days = input$days,
                  hours = input$hours,
                  omean = input$omean,
                  osd = input$osd, 
                  dmean = input$dmean,
                  dsd = input$dsd)
  return(x)
})

utilityFunctionInputA <- reactive({ # Utility function
  x <- utilityOptimClosure(input$beta2, 
                           input$beta3, 
                           input$beta4, 
                           input$a_beta5, 
                           1-input$tau)
  return(x)
})

utilityFunctionInputB <- reactive({ # Utility function
  x <- utilityOptimClosure(input$beta2, 
                           input$beta3, 
                           input$beta4, 
                           input$b_beta5, 
                           1-input$tau)
  return(x)
})

simulationInput <- reactive({ # Simulation
  input$run
  if (input$run == 0) {
    return(NULL)
  } else {      
    isolate({
      simA <- simulation(guess = rep(50, getNodeCount(cityInputA())), 
                         city = cityInputA(),
                         population = populationInput(),
                         utility = utilityFunctionInputA(),
                         delta = input$delta,
                         spillover.eps = input$spillover.eps)
      simB <- simulation(guess = simA$solution$par[1:getNodeCount(cityInputA())], 
                         city = cityInputB(),
                         population = simA$population,
                         utility = utilityFunctionInputB(),
                         delta = input$delta,
                         spillover.eps = input$spillover.eps)
      x <- list(solutionA = simA$solution, 
                solutionB = simB$solution, 
                populationA = simA$population, 
                populationB = simB$population,
                cityA = cityInputA(),
                cityB = cityInputB())
      return(x)
    })
  }
})