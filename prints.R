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
  if (is.null(simulationInput()))
    return(cat("Press run..."))      
  isolate({
    cat("Base scenario: Land prices and wage rates...", simulationInput()$solutionA$message, "\n")
    cat("Do-something scenario: Land prices and wage rates...", simulationInput()$solutionB$message, "\n")
    nodes <- getNodeCount(simulationInput()$cityA)
    N <- getSize(simulationInput()$populationA)
    cat("--------------------------------------------------------------------------------")
    cat("\n", "Elasticities of VKT w.r.t. ...", "\n")      
    log.vkt.ratio <- log(sum(odDemand(getProbability(simulationInput()$populationB))*getDistance(simulationInput()$cityB))/sum(odDemand(getProbability(simulationInput()$populationA))*getDistance(simulationInput()$cityA)))
    log.time.ratio <- log({1/input$b_speed}/{1/input$a_speed})
    log.cost.ratio <- log(input$b_travel_cost/input$a_travel_cost)
    tab <- cbind(Estimate = round(c(log.vkt.ratio/log.time.ratio, 
                                    log.vkt.ratio/log.cost.ratio), 2), "Reference Value" = c(-0.20, -0.20))
    rownames(tab) <- c("Travel time", "Travel cost")
    print(tab)
    
    cat("\n", "Elasticity of total output w.r.t. ...", "\n")
    tab4 <- cbind(Estimate = round(logElasticityProductionAccessibility(list(city = simulationInput()$cityA, population = simulationInput()$populationA), 
                                                                          list(city = simulationInput()$cityB, population = simulationInput()$populationB), 0.01)
                                   , 2), "Reference Value" = 0.04)
    rownames(tab4) <- c("Accessibility")
    print(tab4)       
    cat("--------------------------------------------------------------------------------")
    cat("\n", "Equivalent variation approximation", "\n")
    ev <- ev2(list(city = simulationInput()$cityA, population = simulationInput()$populationA), 
              list(city = simulationInput()$cityB, population = simulationInput()$populationB),
              sigma = input$delta)     
    lor.a <- sum(getArea(simulationInput()$cityA)*simulationInput()$solutionA$par[1:nodes])
    lor.b <- sum(getArea(simulationInput()$cityB)*simulationInput()$solutionB$par[1:nodes])
    lor.diff <- lor.b-lor.a        
    tax.a <- tax(simulationInput()$cityA, simulationInput()$populationA, input$tau)
    tax.b <- tax(simulationInput()$cityB, simulationInput()$populationB, input$tau)
    tax.diff <- tax.b-tax.a        
    tab3 <- round(cbind(Estimate = ev), 2)
    rownames(tab3) <- c("Difference")        
    print(tab3)
    cat("--------------------------------------------------------------------------------")
    cat("\n", "Differences in the economy from a top-down point of view", "\n")
    wage.sum <- wageSum(simulationInput()$populationA, simulationInput()$populationB, input$tau)
    tab4 <- round(cbind(Estimate = c(tax.diff, lor.diff, wage.sum$Base, wage.sum$Scale)), 2)
    rownames(tab4) <- c("Tax revenues", "Land-owner revenues", "Wages: Matching and Work hours", "Wages: Spillovers")
    print(tab4)
    cat("--------------------------------------------------------------------------------")
    cat("\n", "Transport benefits", "\n", "- Only travel costs, travel times, travel comfort", "\n")
    roth2 <- roth2(list(city = simulationInput()$cityA, population = simulationInput()$populationA), 
                   list(city = simulationInput()$cityB, population = simulationInput()$populationB),
                   comfort = c(input$a_beta5, input$b_beta5)) 
    tab_roah2 <- round(cbind(Estimate = roth2), 2)
    rownames(tab_roah2) <- c("Rule of a Half approximation")
    print(tab_roah2)
    
    cat("\n", "Transport benefits with WEI", "\n")
    roah3 <- roah3(list(population = simulationInput()$populationA, city = simulationInput()$cityA, price = simulationInput()$solutionA$par, comfort = input$a_beta5), 
                   list(population = simulationInput()$populationB, city = simulationInput()$cityB, price = simulationInput()$solutionB$par, comfort = input$b_beta5))
    tab_roah3 <- round(cbind(Estimate = c(roah3, sum(roah3))))#, "(1)/(7) (%)" = 100*ev/roah3, "Relative error (%)" = 100*(roah3-ev)/ev), 2)
    rownames(tab_roah3) <- c("1. Travel costs", "2. Travel times", "3. Travel comfort", "4. Land prices", "5. Wage rate offers", "Sum")
    print(tab_roah3)
    cat("--------------------------------------------------------------------------------")
    cat("\n", "Averages", "\n")
    tab5 <- round(avarages(list(city = simulationInput()$cityA, population = simulationInput()$populationA), 
                           list(city = simulationInput()$cityB, population = simulationInput()$populationB)), 2)
    colnames(tab5) <- c("Base scenario", "Do-something scenario")
    tab5 <- cbind(tab5, "Reference value" = c(8, NA, 12, 0.48, 11.66, NA))
    print(tab5)
    cat("\n", "logit Transport Model", "\n")
    tab6 <- round(matrix(c(logitTransportModel(list(city = simulationInput()$cityA, population = simulationInput()$populationA), 0.01),
                           logitTransportModel(list(city = simulationInput()$cityB, population = simulationInput()$populationB), 0.01)), 2, 1), 2)
    rownames(tab6) <- c("Base scenario", "Do-something scenario")
    colnames(tab6) <- "Avg. travel time"
    print(tab6)
  })
})