# Base scenario
output$simulationPopulationA <- renderDataTable({
  if (is.null(simulationInput())) return()
  return(format(populationDataFrame(simulationInput()$populationA, simulationInput()$cityA), digits = 2))
}, options = (list(lengthMenu = c(50, 200, 1000))))

output$simulationCityA <- renderDataTable({
  if (is.null(simulationInput())) return()
  return(format(cityDataFrame(simulationInput()$cityA, simulationInput()$populationA), digits = 2))
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
  return(format(cityDataFrame(simulationInput()$cityB, simulationInput()$populationB), digits = 2))
}, options = (list(lengthMenu = c(50, 100))))

output$pathDataFrameB <- renderDataTable({
  if (is.null(simulationInput())) return()
  return(format(pathDataFrame(simulationInput()$cityB), digits = 2))
}, options = (list(lengthMenu = c(50, 100))))

# Fixed land-use
output$simulationPopulationC <- renderDataTable({
  if (is.null(simulationInput()) || !input$fixedLanduse) return()
  return(format(populationDataFrame(simulationInput()$population_fixed, simulationInput()$city_fixed), digits = 2))
}, options = (list(lengthMenu = c(50, 200, 1000))))

output$simulationCityC <- renderDataTable({
  if (is.null(simulationInput()) || !input$fixedLanduse) return()     
  return(format(cityDataFrame(simulationInput()$city_fixed, simulationInput()$population_fixed), digits = 2))
}, options = (list(lengthMenu = c(50, 100))))

output$pathDataFrameC <- renderDataTable({
  if (is.null(simulationInput()) || !input$fixedLanduse) return()
  return(format(pathDataFrame(simulationInput()$city_fixed), digits = 2))
}, options = (list(lengthMenu = c(50, 100))))