# Reactive expressions
# Cities for simulations A and B
cityInput <- reactive({
  if (input$type == "default") x <- city(input$scale*xy, adjacency)
  if (input$type == "random") x <- city.delunay(input$scale*matrix(c(runif(input$nodes), runif(input$nodes)), input$nodes, 2))
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
})

simulationInput <- eventReactive(input$run, { # Simulation
  simA <- simulation(guess = rep(input$guess, getNodeCount(cityInputA())), 
                     city = cityInputA(),
                     population = populationInput(),
                     utility = utilityWrapper(c(input$beta2, input$beta3, input$beta4, input$a_beta5, 1-input$tau, input$y, input$TIME)),
                     probability = probabilityClosure(input$delta),
                     spillover = spilloverClosure(input$spillover.eps, getArea(cityInputA())))
  simB <- simulation(guess = simA$solution$par[1:getNodeCount(cityInputA())], 
                     city = cityInputB(),
                     population = simA$population,
                     utility = utilityWrapper(c(input$beta2, input$beta3, input$beta4, input$b_beta5, 1-input$tau, input$y, input$TIME)),
                     probability = probabilityClosure(input$delta),
                     spillover = spilloverClosure(input$spillover.eps, getArea(cityInputA())))
  x <- list(solutionA = simA$solution, 
            solutionB = simB$solution, 
            populationA = simA$population, 
            populationB = simB$population,
            cityA = simA$city,
            cityB = simB$city)
  return(x)
})

fixedLanduseInput <- eventReactive(input$run, {
  flu <- fixedLandUse(x = list(city = simulationInput()$cityA, population = simulationInput()$populationA), 
                      y = list(city = simulationInput()$cityB, population = simulationInput()$populationB), 
                      sigma = parameters$delta)
  return(list(city = flu$city, population = flu$population))
})