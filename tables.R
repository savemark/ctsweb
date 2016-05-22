# Base scenario
output$simulationPopulationA <- renderDataTable({
  if (is.null(simulationInput())) 
    return()
  return(populationDataFrame(simulationInput()$populationA, simulationInput()$cityA))
}, options = (list(lengthMenu = c(50, 200, 1000))))

output$simulationCityA <- renderDataTable({
  if (is.null(simulationInput()))
    return()
  return(cityDataFrame(simulationInput()$solutionA$par[1:getNodeCount(simulationInput()$cityA)], simulationInput()$populationA, simulationInput()$cityA))
}, options = (list(lengthMenu = c(50, 100))))

# Do-something scenario
output$simulationPopulationB <- renderDataTable({
  if (is.null(simulationInput())) 
    return()
  return(populationDataFrame(simulationInput()$populationB, simulationInput()$cityB))
}, options = (list(lengthMenu = c(50, 200, 1000))))

output$simulationCityB <- renderDataTable({
  if (is.null(simulationInput()))
    return()     
  return(cityDataFrame(simulationInput()$solutionB$par[1:getNodeCount(simulationInput()$cityB)], simulationInput()$populationB, simulationInput()$cityB))
}, options = (list(lengthMenu = c(50, 100))))