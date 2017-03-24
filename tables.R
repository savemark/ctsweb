# Base scenario
output$simulateEconomyPopulationA <- renderDataTable({
  if (is.null(simulateEconomyInput())) return(invisible(NULL))
  if (is.null(input$scenario_id_base)) return(invisible(NULL))
  return(format(getDataFrame(simulateEconomyInput(), as.numeric(input$scenario_id_base), "population"), digits = 2))
}, options = (list(lengthMenu = c(50, 200, 1000))))

output$simulateEconomyCityA <- renderDataTable({
  if (is.null(simulateEconomyInput())) return(invisible(NULL))
  if (is.null(input$scenario_id_base)) return(invisible(NULL))
  return(format(getDataFrame(simulateEconomyInput(), as.numeric(input$scenario_id_base), "city"), digits = 2))
}, options = (list(lengthMenu = c(50, 200, 1000))))

output$simulateEconomyPathA <- renderDataTable({
  if (is.null(simulateEconomyInput())) return(invisible(NULL))
  if (is.null(input$scenario_id_base)) return(invisible(NULL))
  return(format(getDataFrame(simulateEconomyInput(), as.numeric(input$scenario_id_base), "path"), digits = 2))
}, options = (list(lengthMenu = c(50, 200, 1000))))

# Do-something scenario
output$simulateEconomyPopulationB <- renderDataTable({
  if (is.null(simulateEconomyInput())) return(invisible(NULL))
  if (is.null(input$scenario_id_alt)) return(invisible(NULL))
  return(format(getDataFrame(simulateEconomyInput(), as.numeric(input$scenario_id_alt), "population"), digits = 2))
}, options = (list(lengthMenu = c(50, 200, 1000))))

output$simulateEconomyCityB <- renderDataTable({
  if (is.null(simulateEconomyInput())) return(invisible(NULL))
  if (is.null(input$scenario_id_alt)) return(invisible(NULL))
  return(format(getDataFrame(simulateEconomyInput(), as.numeric(input$scenario_id_alt), "city"), digits = 2))
}, options = (list(lengthMenu = c(50, 200, 1000))))

output$simulateEconomyPathB <- renderDataTable({
  if (is.null(simulateEconomyInput())) return(invisible(NULL))
  if (is.null(input$scenario_id_alt)) return(invisible(NULL))
  return(format(getDataFrame(simulateEconomyInput(), as.numeric(input$scenario_id_alt), "path"), digits = 2))
}, options = (list(lengthMenu = c(50, 200, 1000))))

# Fixed land-use
output$simulateEconomyPopulationC <- renderDataTable({
  if (is.null(simulateEconomyFixedInput())) return(invisible(NULL))
  if (is.null(input$scenario_id_fixed)) return(invisible(NULL))
  return(format(getDataFrame(simulateEconomyFixedInput(), as.numeric(input$scenario_id_fixed), "population"), digits = 2))
}, options = (list(lengthMenu = c(50, 200, 1000))))

output$simulateEconomyCityC <- renderDataTable({
  if (is.null(simulateEconomyFixedInput())) return(invisible(NULL))
  if (is.null(input$scenario_id_fixed)) return(invisible(NULL))
  return(format(getDataFrame(simulateEconomyFixedInput(), as.numeric(input$scenario_id_fixed), "city"), digits = 2))
}, options = (list(lengthMenu = c(50, 200, 1000))))

output$simulateEconomyPathC <- renderDataTable({
  if (is.null(simulateEconomyFixedInput())) return(invisible(NULL))
  if (is.null(input$scenario_id_fixed)) return(invisible(NULL))
  return(format(getDataFrame(simulateEconomyFixedInput(), as.numeric(input$scenario_id_fixed), "path"), digits = 2))
}, options = (list(lengthMenu = c(50, 200, 1000))))