output$a_cityShow <- renderPrint({
  return(print(cityInputA()))
})

output$b_cityShow <- renderPrint({
  return(print(cityInputB()))
})

output$populationShow <- renderPrint({
  return(print(populationInput()))
})

output$summary <- renderPrint({
  if (is.null(simulationInput())) return()
  if (input$showConvergenceMessage) {
    cat("Base scenario: Land prices and wage rates...", simulationInput()$solutionA$message, "\n")
    cat("feval (BBSolve): ", simulationInput()$solutionA$feval, "\n", "iter (BBSolve): ", simulationInput()$solutionA$iter, "\n")
    cat("Do-something scenario: Land prices and wage rates...", simulationInput()$solutionB$message, "\n")
    cat("feval (BBSolve): ", simulationInput()$solutionB$feval, "\n", "iter (BBSolve): ", simulationInput()$solutionB$iter, "\n")
    cat("--------------------------------------------------------------------------------", "\n")
  }
  nodes <- getNodeCount(simulationInput()$cityA)
  N <- getSize(simulationInput()$populationA)
  cat("Elasticities of VKT w.r.t. ...", "\n")      
  log.vkt.ratio <- log(sum(odDemand(getProbability(simulationInput()$populationB))*getDistance(simulationInput()$cityB))/sum(odDemand(getProbability(simulationInput()$populationA))*getDistance(simulationInput()$cityA)))
  log.time.ratio <- log({1/parameters$b_speed}/{1/parameters$a_speed})
  log.cost.ratio <- log(parameters$b_travel_cost/parameters$a_travel_cost)
  tab <- cbind(Estimate = round(c(log.vkt.ratio/log.time.ratio, 
                                  log.vkt.ratio/log.cost.ratio), 2), "Reference Value" = c(-0.20, -0.20))
  rownames(tab) <- c("Travel time", "Travel cost")
  print(tab)
  
  cat("Elasticity of total output w.r.t. ...", "\n")
  tab4 <- cbind(Estimate = round(logElasticityProductionAccessibility(list(city = simulationInput()$cityA, population = simulationInput()$populationA), 
                                                                      list(city = simulationInput()$cityB, population = simulationInput()$populationB), 0.004)
                                 , 2), "Reference Value" = 0.04)
  rownames(tab4) <- c("Accessibility")
  print(tab4) 
  cat("--------------------------------------------------------------------------------", "\n")
  cat("Differences in the economy from a top-down point of view", "\n")
  lor.a <- sum(getArea(simulationInput()$cityA)*getVertexPrice(simulationInput()$cityA))
  lor.b <- sum(getArea(simulationInput()$cityB)*getVertexPrice(simulationInput()$cityB))
  lor.diff <- lor.b-lor.a        
  tax.a <- tax(simulationInput()$cityA, simulationInput()$populationA, parameters$tau)
  tax.b <- tax(simulationInput()$cityB, simulationInput()$populationB, parameters$tau)
  tax.diff <- tax.b-tax.a   
  wage.sum <- wageSum(simulationInput()$populationA, simulationInput()$populationB, parameters$tau)
  tab4 <- round(cbind(Estimate = c(tax.diff, lor.diff, wage.sum$Base, wage.sum$Scale)), 2)
  rownames(tab4) <- c("Tax revenues", "Land-owner revenues", "Wages: Matching and Work hours", "Wages: Spillovers")
  print(tab4)
  cat("--------------------------------------------------------------------------------", "\n")
  cat("Equivalent variation approximation", "\n")
  ev <- equivalentVariation(list(city = simulationInput()$cityA, population = simulationInput()$populationA), 
                            list(city = simulationInput()$cityB, population = simulationInput()$populationB),
                            sigma = parameters$delta)
  tab3 <- round(cbind(Estimate = ev), 4)
  rownames(tab3) <- c("EV approximation")     
  if (input$fixedLanduse) {
    evflu <- equivalentVariation(list(city = simulationInput()$cityA, population = simulationInput()$populationA), 
                                 list(city = fixedLanduseInput()$city, population = fixedLanduseInput()$population),
                                 sigma = parameters$delta)   
    tab3 <- round(cbind(Estimate = c(ev, evflu)), 4)
    rownames(tab3) <- c("EV approximation", "EV approximation (FL)")   
  }
  print(tab3)
  cat("--------------------------------------------------------------------------------", "\n")
  cat("ROAH benefits with WEI", "\n")
  roah3 <- roah3(list(population = simulationInput()$populationA, city = simulationInput()$cityA, comfort = parameters$a_beta5), 
                 list(population = simulationInput()$populationB, city = simulationInput()$cityB, comfort = parameters$b_beta5))
  tab_roah3 <- round(cbind(Estimate = c(roah3, sum(roah3))))#, "(1)/(7) (%)" = 100*ev/roah3, "Relative error (%)" = 100*(roah3-ev)/ev), 2)
  rownames(tab_roah3) <- c("1. Travel costs", "2. Travel times", "3. Travel comfort", "4. Land prices", "5. Wage rate offers", "Sum")
  if (input$fixedLanduse) {
    roah3flu <- roah3(list(population = simulationInput()$populationA, city = simulationInput()$cityA, comfort = parameters$a_beta5), 
                      list(population = fixedLanduseInput()$population, city = fixedLanduseInput()$city, comfort = parameters$b_beta5))
    tab_roah3 <- round(cbind(Estimate = c(roah3, sum(roah3)), "Estimate (FL)" = c(roah3flu, sum(roah3flu))), 2)
    rownames(tab_roah3) <- c("1. Travel costs", "2. Travel times", "3. Travel comfort", "4. Land prices", "5. Wage rate offers", "Sum")
  }
  print(tab_roah3)
  cat("--------------------------------------------------------------------------------", "\n")
  cat("Averages", "\n")
  tab5 <- round(avarages(list(city = simulationInput()$cityA, population = simulationInput()$populationA), 
                         list(city = simulationInput()$cityB, population = simulationInput()$populationB)), 2)
  colnames(tab5) <- c("Base scenario", "Do-something scenario")
  tab5 <- cbind(tab5, "Reference value" = c(8, NA, 12, 0.48, 11.66, NA))
  print(tab5)
  cat("\n", "logit Transport Model", "\n")
  tab6 <- round(matrix(c(logitTransportModel(list(city = simulationInput()$cityA, population = simulationInput()$populationA), parameters$sigma),
                         logitTransportModel(list(city = simulationInput()$cityB, population = simulationInput()$populationB), parameters$sigma)), 2, 1), 2)
  rownames(tab6) <- c("Base scenario", "Do-something scenario")
  colnames(tab6) <- "Avg. travel time"
  print(tab6)
  cat("--------------------------------------------------------------------------------", "\n")
  cat("Inequality measures, post-taxation", "\n")
  wage.x <- array(rep(t(getWageRate(simulationInput()$populationA)), each = getNodeCount(simulationInput()$cityA)), 
                  dim = c(getNodeCount(simulationInput()$cityA), getNodeCount(simulationInput()$cityA), N))
  wage.y <- array(rep(t(getWageRate(simulationInput()$populationB)), each = getNodeCount(simulationInput()$cityB)), 
                  dim = c(getNodeCount(simulationInput()$cityB), getNodeCount(simulationInput()$cityB), N))
  gini.x <- ineq(apply((1-parameters$tau)*getProbability(simulationInput()$populationA)*wage.x*getArgMax(simulationInput()$populationA)[ , , , 1], 3, sum))
  gini.y <- ineq(apply((1-parameters$tau)*getProbability(simulationInput()$populationB)*wage.y*getArgMax(simulationInput()$populationB)[ , , , 1], 3, sum))
  tab_ineq <- round(cbind(gini.x, gini.y), 4)
  colnames(tab_ineq) <- c("Base scenario", "Do-something scenario")
  rownames(tab_ineq) <- c("Gini")
  print(tab_ineq)
})