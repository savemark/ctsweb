# Reactive expressions
cityInput <- reactive({
  if (input$type == "default") x <- city(input$scale*xy, adjacency)
  if (input$type == "random") x <- city.random(input$scale*matrix(c(runif(input$nodes), runif(input$nodes)), input$nodes, 2))
  if (input$type == "grid") x <- city.grid(input$sqrtnodes, scale = input$scale)
  return(x)
})

cityInputA <- reactive({ # Cities for simulations A and B
  x <- cityInput()
  x <- setEdgeSpeed(x, value = input$a_speed)  
  x <- setEdgeCostFactor(x, value = input$a_travel_cost)
  return(x)
})

cityInputB <- reactive({
  x <- cityInput()
  x <- setEdgeSpeed(x, value = input$b_speed)
  x <- setEdgeCostFactor(x, value = input$b_travel_cost)
  return(x)
})

populationInput <- reactive({ # Population
  x <- population(input$n, 
                  getNodeCount(cityInput()),
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

parameters <- reactiveValues()

observeEvent(input$run, {
  parameters$tau <- input$tau
  parameters$a_speed <- input$a_speed
  parameters$b_speed <- input$b_speed
  parameters$a_travel_cost <- input$a_travel_cost
  parameters$b_travel_cost <- input$b_travel_cost
  parameters$a_beta5 <- input$a_beta5
  parameters$b_beta5 <- input$b_beta5
  parameters$delta <- input$delta
  parameters$sigma <- input$sigma
  parameters$type <- input$type
})

simulationInput <- eventReactive(input$run, { # Simulation
  simA <- simulation(guess = rep(input$guess, getNodeCount(cityInputA())), 
                     city = cityInputA(),
                     population = populationInput(),
                     utility = utilityWrapper(c(input$beta2, input$beta3, input$beta4, input$a_beta5, 1-input$tau, input$y, input$TIME)),
                     probability = probabilityClosure(input$delta),
                     spillover = spilloverClosure(input$spillover.eps, getArea(cityInputA())))
  simB <- simulation(guess = rep(input$guess, getNodeCount(cityInputB())), 
                     city = cityInputB(),
                     population = populationInput(),
                     utility = utilityWrapper(c(input$beta2, input$beta3, input$beta4, input$b_beta5, 1-input$tau, input$y, input$TIME)),
                     probability = probabilityClosure(input$delta),
                     spillover = spilloverClosure(input$spillover.eps, getArea(cityInputB())))
  scale <- array(apply(array(rep(colSums(aperm(getProbability(simA$population), c(2, 1, 3))), each = getNodeCount(simA$city)), 
                             c(getNodeCount(simA$city), getNodeCount(simA$city), getNumberOfClasses(simA$population))), 3, t), 
                 c(getNodeCount(simA$city), getNodeCount(simA$city), getNumberOfClasses(simA$population)))
  sim_fixed <- simulation(guess = rep(input$guess, getNodeCount(cityInputB())), 
                          city = cityInputB(),
                          population = populationInput(),
                          utility = utilityWrapper(c(input$beta2, input$beta3, input$beta4, input$b_beta5, 1-input$tau, input$y, input$TIME)),
                          probability = probabilityClosure(input$delta),
                          spillover = spilloverClosure(input$spillover.eps, getArea(cityInputB())),
                          scale = scale)
  x <- list(solutionA = simA$solution, 
            solutionB = simB$solution,
            solution_fixed = sim_fixed$solution,
            populationA = simA$population, 
            populationB = simB$population,
            population_fixed = sim_fixed$population,
            cityA = simA$city,
            cityB = simB$city,
            city_fixed = sim_fixed$city)
  return(x)
})