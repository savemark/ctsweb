# City
output$cityPlot <- renderPlot({
  plot(cityInput())
})

# Wage rate offers
output$populationWageRatesHistogram <- renderPlot({
  hist(getWageRate(populationInput()), freq = FALSE, main = "", xlab = "Wage rate", 
       yaxs = "i", xaxs = "i",
       ylim = range(density(getWageRate(populationInput()))$y))
  lines(density(getWageRate(populationInput())), lty = "dotted")
})

# Preferences
output$populationPreferenceHistogram <- renderPlot({
  par(mfrow = c(1, 2))
  hist(getOriginPreference(populationInput()), probability = TRUE, 
       main = "", 
       xlab = "Residential zone quality", 
       ylim = range(density(getOriginPreference(populationInput()))$y))
  lines(density(getOriginPreference(populationInput())), lty = "dotted")
  hist(getDestinationPreference(populationInput()), probability = TRUE, main = "", 
       xlab = "Work place quality",
       ylim = range(density(getDestinationPreference(populationInput()))$y),
       xlim = range(density(getDestinationPreference(populationInput()))$x))
  lines(density(getDestinationPreference(populationInput())), lty = "dotted")
})


# Density plots -----------------------------------------------------------

# output$opdensity <- renderPlot({ # Origin quality
#   if (is.null(simulationInput())) return()
#   V <- getNodeCount(simulationInput()$cityA)
#   N <- getNumberOfClasses(simulationInput()$populationA)
#   op <- array(apply(t(getOriginPreference(simulationInput()$populationA)), 2,
#                     function(x) {matrix(rep(x, length.out = V*V), V, V)}), dim = c(V, V, N))
#   weighted.densities(op,
#                      op,
#                      getProbability(simulationInput()$populationA)/getNumberOfClasses(simulationInput()$populationA),
#                      getProbability(simulationInput()$populationB)/getNumberOfClasses(simulationInput()$populationB),
#                      main = " ",
#                      xlab = "Origin quality")
#   lines(density(op), lty = "dotted", col = "grey")
#   legend("topright", 
#          c('Base scenario', 'Do-something scenario', "Original distribution"), 
#          lty = c(1, 1, 3), col = c("red", "blue", "grey"))
# })
# 
# output$dpdensity <- renderPlot({ # Destination quality
#   if (is.null(simulationInput())) return()
#   V <- getNodeCount(simulationInput()$cityA)
#   N <- getNumberOfClasses(simulationInput()$populationA)
#   dp <- array(rep(t(getDestinationPreference(simulationInput()$populationA)), each = V), dim = c(V, V, N)) 
#   weighted.densities(dp,
#                      dp,
#                      getProbability(simulationInput()$populationA)/getNumberOfClasses(simulationInput()$populationA),
#                      getProbability(simulationInput()$populationB)/getNumberOfClasses(simulationInput()$populationB),
#                      main = " ",
#                      xlab = "Destination quality")
#   lines(density(dp), lty = "dotted", col = "grey")
#   legend("topright", 
#          c('Base scenario', 'Do-something scenario', "Original distribution"), 
#          lty = c(1, 1, 3), col = c("red", "blue", "grey"))
# })

# Equity plots ------------------------------------------------------------

# output$equity <- renderPlot({
#   if (is.null(simulationInput())) return()
#   #isolate({
#   cs <- roah4(list(population = simulationInput()$populationA, city = simulationInput()$cityA, price = getVertexPrice(simulationInput()$cityA), comfort = input$a_beta5), 
#               list(population = simulationInput()$populationB, city = simulationInput()$cityB, price = getVertexPrice(simulationInput()$cityB), comfort = input$b_beta5))
#   equityPlot(cs, seq(0, 1, 1/input$probs))
#   #})
# })

# 3D plots ----------------------------------------------------------------

output$residency3dA <- renderPlot({
  if (is.null(simulateEconomyInput())) return(invisible(NULL))
  if (is.null(input$scenario_id_base_plot)) return(invisible(NULL))
  persp3D(simulateEconomyInput(),
          type = "residence",
          sid = as.numeric(input$scenario_id_base_plot),
          linear = ifelse(parameters$type == "grid" || parameters$type == "random", TRUE, FALSE),
          zlab = "Residence density",
          sub = "Base scenario")
}, width = "auto")

output$residency3dB <- renderPlot({
  if (is.null(simulateEconomyInput())) return(invisible(NULL))
  if (is.null(input$scenario_id_alt_plot)) return(invisible(NULL))
  persp3D(simulateEconomyInput(),
          type = "residence",
          sid = as.numeric(input$scenario_id_alt_plot),
          linear = ifelse(parameters$type == "grid" || parameters$type == "random", TRUE, FALSE),
          zlab = "Residence density",
          sub = "Do-something scenario")
}, width = "auto")

output$residency3d_fixed <- renderPlot({
  if (is.null(simulateEconomyFixedInput())) return(invisible(NULL))
  if (is.null(input$scenario_id_fixed_plot)) return(invisible(NULL))
  persp3D(simulateEconomyFixedInput(),
          type = "residence",
          sid = as.numeric(input$scenario_id_fixed_plot),
          linear = ifelse(parameters$type == "grid" || parameters$type == "random", TRUE, FALSE),
          zlab = "Residence density",
          sub = "Fixed land-use")
}, width = "auto")

output$work3dA <- renderPlot({
  if (is.null(simulateEconomyInput())) return(invisible(NULL))
  if (is.null(input$scenario_id_base_plot)) return(invisible(NULL))
  persp3D(simulateEconomyInput(),
          type = "work",
          sid = as.numeric(input$scenario_id_base_plot),
          linear = ifelse(parameters$type == "grid" || parameters$type == "random", TRUE, FALSE),
          zlab = "Worker density",
          sub = "Base scenario")
}, width = "auto")

output$work3dB <- renderPlot({
  if (is.null(simulateEconomyInput())) return(invisible(NULL))
  if (is.null(input$scenario_id_alt_plot)) return(invisible(NULL))
  persp3D(simulateEconomyInput(),
          type = "work",
          sid = as.numeric(input$scenario_id_alt_plot),
          linear = ifelse(parameters$type == "grid" || parameters$type == "random", TRUE, FALSE),
          zlab = "Worker density",
          sub = "Do-something scenario")
}, width = "auto")

output$work3d_fixed <- renderPlot({
  if (is.null(simulateEconomyFixedInput())) return(invisible(NULL))
  if (is.null(input$scenario_id_fixed_plot)) return(invisible(NULL))
  persp3D(simulateEconomyFixedInput(),
          type = "work",
          sid = as.numeric(input$scenario_id_fixed_plot),
          linear = ifelse(parameters$type == "grid" || parameters$type == "random", TRUE, FALSE),
          zlab = "Worker density",
          sub = "Fixed land-use")
}, width = "auto")

output$price3dA <- renderPlot({
  if (is.null(simulateEconomyInput())) return(invisible(NULL))
  if (is.null(input$scenario_id_base_plot)) return(invisible(NULL))
  persp3D(simulateEconomyInput(), 
          type = "landprice",
          sid = as.numeric(input$scenario_id_base_plot),
          linear = ifelse(parameters$type == "grid" || parameters$type == "random", TRUE, FALSE),
          zlab = "Land-price",
          sub = "Base scenario")
}, width = "auto")

output$price3dB <- renderPlot({
  if (is.null(simulateEconomyInput())) return(invisible(NULL))
  if (is.null(input$scenario_id_alt_plot)) return(invisible(NULL))
  persp3D(simulateEconomyInput(),
          type = "landprice",
          sid = as.numeric(input$scenario_id_alt_plot),
          linear = ifelse(parameters$type == "grid" || parameters$type == "random", TRUE, FALSE),
          zlab = "Land-price",
          sub = "Do-something scenario")
}, width = "auto")

output$price3d_fixed <- renderPlot({
  if (is.null(simulateEconomyFixedInput())) return(invisible(NULL))
  if (is.null(input$scenario_id_fixed_plot)) return(invisible(NULL))
  persp3D(simulateEconomyFixedInput(),
          type = "landprice",
          sid = as.numeric(input$scenario_id_fixed_plot),
          linear = ifelse(parameters$type == "grid" || parameters$type == "random", TRUE, FALSE),
          zlab = "Land-price",
          sub = "Fixed land-use")
}, width = "auto")

# Non-fixed land-use
output$argmax1 <- renderPlot({
  if (is.null(simulateEconomyInput())) return(invisible(NULL))
    plot(simulateEconomyInput(), type = "argmax", main = "Time spent at work", xlab = "hours", index = 1)
})

output$argmax2 <- renderPlot({
  if (is.null(simulateEconomyInput())) return(invisible(NULL))
  plot(simulateEconomyInput(), type = "argmax", main = "Consumption per work day", xlab = "SEK", index = 2)
})

output$argmax3 <- renderPlot({
  if (is.null(simulateEconomyInput())) return(invisible(NULL))
  plot(simulateEconomyInput(), type = "argmax", main = "Leisure time", xlab = "hours", index = 3)
})

output$argmax4 <- renderPlot({
  if (is.null(simulateEconomyInput())) return(invisible(NULL))
  plot(simulateEconomyInput(), type = "argmax", main = "Land use", xlab = ~"km"^2, index = 4)
})

output$argmax5 <- renderPlot({
  if (is.null(simulateEconomyInput())) return(invisible(NULL))
  plot(simulateEconomyInput(), type = "argmax", main = "Travel time", xlab = "hours", index = 5)
})

output$wageratedensi <- renderPlot({ # Wage rate
  if (is.null(simulateEconomyInput())) return(invisible(NULL))
  plot(simulateEconomyInput(), type = "wagerate", main = "Wage rate", xlab = "SEK/hour")
})

output$incomedensi <- renderPlot({ # Income
  if (is.null(simulateEconomyInput())) return(invisible(NULL))
  plot(simulateEconomyInput(), type = "income", main = "Income", xlab = "SEK/work day")
})

output$vktdensi <- renderPlot({ # VKT
  if (is.null(simulateEconomyInput())) return(invisible(NULL))
  plot(simulateEconomyInput(), type = "vkt", main = "Vehicle Kilometers Travelled", xlab = "km")
})

# Fixed land-use
output$argmax1_fixed <- renderPlot({
  if (is.null(simulateEconomyFixedInput())) return(invisible(NULL))
  plot(simulateEconomyFixedInput(), type = "argmax", main = "Time spent at work", xlab = "hours", index = 1)
})

output$argmax2_fixed <- renderPlot({
  if (is.null(simulateEconomyFixedInput())) return(invisible(NULL))
  plot(simulateEconomyFixedInput(), type = "argmax", main = "Consumption per work day", xlab = "SEK", index = 2)
})

output$argmax3_fixed <- renderPlot({
  if (is.null(simulateEconomyFixedInput())) return(invisible(NULL))
  plot(simulateEconomyFixedInput(), type = "argmax", main = "Leisure time", xlab = "hours", index = 3)
})

output$argmax4_fixed <- renderPlot({
  if (is.null(simulateEconomyFixedInput())) return(invisible(NULL))
  plot(simulateEconomyFixedInput(), type = "argmax", main = "Land use", xlab = ~"km"^2, index = 4)
})

output$argmax5_fixed <- renderPlot({
  if (is.null(simulateEconomyFixedInput())) return(invisible(NULL))
  plot(simulateEconomyFixedInput(), type = "argmax", main = "Travel time", xlab = "hours", index = 5)
})

output$wageratedensi_fixed <- renderPlot({ # Wage rate
  if (is.null(simulateEconomyFixedInput())) return(invisible(NULL))
  plot(simulateEconomyFixedInput(), type = "wagerate", main = "Wage rate", xlab = "SEK/hour")
})

output$incomedensi_fixed <- renderPlot({ # Income
  if (is.null(simulateEconomyFixedInput())) return(invisible(NULL))
  plot(simulateEconomyFixedInput(), type = "income", main = "Income", xlab = "SEK/work day")
})

output$vktdensi_fixed <- renderPlot({ # VKT
  if (is.null(simulateEconomyFixedInput())) return(invisible(NULL))
  plot(simulateEconomyFixedInput(), type = "vkt", main = "Vehicle Kilometers Travelled", xlab = "km")
})

output$scatter_nonfixed_vs_fixed <- renderPlot({
  if (is.null(simulateEconomyFixedInput())) return(invisible(NULL))
  plot(getROAHBenefitsWithTax(simulateEconomyFixedInput(), parameters$tau)[7, ], getROAHBenefitsWithTax(simulateEconomyInput(), parameters$tau)[7, ], xlab = "Fixed land use", ylab = "Non-Fixed land use")
  abline(0, 1)
})