# Base scenario
output$simulateEconomyPopulationA <- DT::renderDT({
  if (is.null(simulateEconomyInput())) return(invisible(NULL))
  if (is.null(input$scenario_id_base)) return(invisible(NULL))
  return(format(getDataFrame(simulateEconomyInput(), as.numeric(input$scenario_id_base), "population"), digits = 2))
}, options = (list(lengthMenu = c(50, 200, 1000))))

output$simulateEconomyCityA <- DT::renderDT({
  if (is.null(simulateEconomyInput())) return(invisible(NULL))
  if (is.null(input$scenario_id_base)) return(invisible(NULL))
  return(format(getDataFrame(simulateEconomyInput(), as.numeric(input$scenario_id_base), "city"), digits = 2))
}, options = (list(lengthMenu = c(50, 200, 1000))))

output$simulateEconomyPathA <- DT::renderDT({
  if (is.null(simulateEconomyInput())) return(invisible(NULL))
  if (is.null(input$scenario_id_base)) return(invisible(NULL))
  return(format(getDataFrame(simulateEconomyInput(), as.numeric(input$scenario_id_base), "path"), digits = 2))
}, options = (list(lengthMenu = c(50, 200, 1000))))

# Do-something scenario
output$simulateEconomyPopulationB <- DT::renderDT({
  if (is.null(simulateEconomyInput())) return(invisible(NULL))
  if (is.null(input$scenario_id_alt)) return(invisible(NULL))
  return(format(getDataFrame(simulateEconomyInput(), as.numeric(input$scenario_id_alt), "population"), digits = 2))
}, options = (list(lengthMenu = c(50, 200, 1000))))

output$simulateEconomyCityB <- DT::renderDT({
  if (is.null(simulateEconomyInput())) return(invisible(NULL))
  if (is.null(input$scenario_id_alt)) return(invisible(NULL))
  return(format(getDataFrame(simulateEconomyInput(), as.numeric(input$scenario_id_alt), "city"), digits = 2))
}, options = (list(lengthMenu = c(50, 200, 1000))))

output$simulateEconomyPathB <- DT::renderDT({
  if (is.null(simulateEconomyInput())) return(invisible(NULL))
  if (is.null(input$scenario_id_alt)) return(invisible(NULL))
  return(format(getDataFrame(simulateEconomyInput(), as.numeric(input$scenario_id_alt), "path"), digits = 2))
}, options = (list(lengthMenu = c(50, 200, 1000))))

# Fixed land-use
output$simulateEconomyPopulationC <- DT::renderDT({
  if (is.null(simulateEconomyFixedInput())) return(invisible(NULL))
  if (is.null(input$scenario_id_fixed)) return(invisible(NULL))
  return(format(getDataFrame(simulateEconomyFixedInput(), as.numeric(input$scenario_id_fixed), "population"), digits = 2))
}, options = (list(lengthMenu = c(50, 200, 1000))))

output$simulateEconomyCityC <- DT::renderDT({
  if (is.null(simulateEconomyFixedInput())) return(invisible(NULL))
  if (is.null(input$scenario_id_fixed)) return(invisible(NULL))
  return(format(getDataFrame(simulateEconomyFixedInput(), as.numeric(input$scenario_id_fixed), "city"), digits = 2))
}, options = (list(lengthMenu = c(50, 200, 1000))))

output$simulateEconomyPathC <- DT::renderDT({
  if (is.null(simulateEconomyFixedInput())) return(invisible(NULL))
  if (is.null(input$scenario_id_fixed)) return(invisible(NULL))
  return(format(getDataFrame(simulateEconomyFixedInput(), as.numeric(input$scenario_id_fixed), "path"), digits = 2))
}, options = (list(lengthMenu = c(50, 200, 1000))))