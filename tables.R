# Base scenario
output$simulationPopulationA <- renderDataTable({
  if (is.null(simulationInput())) return()
  return(format(populationDataFrame(simulationInput()$populationA, simulationInput()$cityA), digits = 2))
}, options = (list(lengthMenu = c(50, 200, 1000))))

output$simulationCityA <- renderDataTable({
  if (is.null(simulationInput())) return()
  return(format(cityDataFrame(simulationInput()$solutionA$par[1:getNodeCount(simulationInput()$cityA)], simulationInput()$populationA, simulationInput()$cityA), digits = 2))
}, options = (list(lengthMenu = c(50, 100))))

output$pathDataFrameA <- renderDataTable({
  if (is.null(simulationInput())) return()
  return(format(pathDataFrame(simulationInput()$cityA), digits = 2))
}, options = (list(lengthMenu = c(50, 100))))

# Do-something scenario
output$simulationPopulationB <- renderDataTable({
  if (is.null(simulationInput())) return()
  return(format(populationDataFrame(simulationInput()$populationB, simulationInput()$cityB), digits = 2))
}, options = (list(lengthMenu = c(50, 200, 1000))))

output$simulationCityB <- renderDataTable({
  if (is.null(simulationInput())) return()     
  return(format(cityDataFrame(simulationInput()$solutionB$par[1:getNodeCount(simulationInput()$cityB)], simulationInput()$populationB, simulationInput()$cityB), digits = 2))
}, options = (list(lengthMenu = c(50, 100))))

output$pathDataFrameB <- renderDataTable({
  if (is.null(simulationInput())) return()
  return(format(pathDataFrame(simulationInput()$cityB), digits = 2))
}, options = (list(lengthMenu = c(50, 100))))

# Fixed land-use
output$simulationPopulationC <- renderDataTable({
  if (is.null(simulationInput())) return()
  return(format(populationDataFrame(fixedLanduseInput(), simulationInput()$cityB), digits = 2))
}, options = (list(lengthMenu = c(50, 200, 1000))))