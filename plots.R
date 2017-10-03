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

output$income3dA <- renderPlot({
  if (is.null(simulateEconomyInput())) return(invisible(NULL))
  if (is.null(input$scenario_id_base_plot)) return(invisible(NULL))
  persp3D(simulateEconomyInput(), 
          type = "income",
          sid = as.numeric(input$scenario_id_base_plot),
          linear = ifelse(parameters$type == "grid" || parameters$type == "random", TRUE, FALSE),
          zlab = "Destination income, pre-tax",
          sub = "Base scenario")
}, width = "auto")

output$income3dB <- renderPlot({
  if (is.null(simulateEconomyInput())) return(invisible(NULL))
  if (is.null(input$scenario_id_alt_plot)) return(invisible(NULL))
  persp3D(simulateEconomyInput(), 
          type = "income",
          sid = as.numeric(input$scenario_id_alt_plot),
          linear = ifelse(parameters$type == "grid" || parameters$type == "random", TRUE, FALSE),
          zlab = "Destination income, pre-tax",
          sub = "Do-something scenario")
}, width = "auto")

output$income3d_fixed <- renderPlot({
  if (is.null(simulateEconomyFixedInput())) return(invisible(NULL))
  if (is.null(input$scenario_id_fixed_plot)) return(invisible(NULL))
  persp3D(simulateEconomyFixedInput(), 
          type = "income",
          sid = as.numeric(input$scenario_id_fixed_plot),
          linear = ifelse(parameters$type == "grid" || parameters$type == "random", TRUE, FALSE),
          zlab = "Destination income, pre-tax",
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
  if (is.null(simulateEconomyFixedInput()) || is.null(simulateEconomyInput())) return(invisible(NULL))
  plot(getROAHBenefitsWithTax(simulateEconomyFixedInput(), parameters$tau)[7, ], getROAHBenefitsWithTax(simulateEconomyInput(), parameters$tau)[7, ], xlab = "Fixed land use", ylab = "Non-Fixed land use")
  abline(0, 1)
})

output$utility_km <- renderPlot({
  if (is.null(simulateEconomyFixedInput()) || is.null(simulateEconomyInput())) return(invisible(NULL))
  if (length(unique(getRanking(simulateEconomyFixedInput(), parameters$tau)[1, ])) < 4) return(invisible(NULL))
  spline <- smooth.spline(getRanking(simulateEconomyInput(), parameters$tau)[1, ], getROAHBenefitsWithTax(simulateEconomyInput(), parameters$tau)[7, ])
  spline_fixed <- smooth.spline(getRanking(simulateEconomyFixedInput(), parameters$tau)[1, ], getROAHBenefitsWithTax(simulateEconomyFixedInput(), parameters$tau)[7, ])
  plot(getRanking(simulateEconomyFixedInput(), parameters$tau)[1, ], getROAHBenefitsWithTax(simulateEconomyFixedInput(), parameters$tau)[7, ], xlab = "Total link length (km)", ylab = "ROAH Benefits", col = "red")
  points(getRanking(simulateEconomyInput(), parameters$tau)[1, ], getROAHBenefitsWithTax(simulateEconomyInput(), parameters$tau)[7, ], col = "blue")
  lines(spline, col = "blue")
  lines(spline_fixed, col = "red")
  legend("topright", 
         legend = c("Fixed", "Non-fixed"), 
         lty = c(1,1), col = c("red", "blue"))
})

output$utility_betweenness <- renderPlot({
  if (is.null(simulateEconomyFixedInput()) || is.null(simulateEconomyInput())) return(invisible(NULL))
  if (length(unique(getRanking(simulateEconomyFixedInput(), parameters$tau)[2, ])) < 4) return(invisible(NULL))
  spline <- smooth.spline(getRanking(simulateEconomyInput(), parameters$tau)[2, ], getROAHBenefitsWithTax(simulateEconomyInput(), parameters$tau)[7, ])
  spline_fixed <- smooth.spline(getRanking(simulateEconomyFixedInput(), parameters$tau)[2, ], getROAHBenefitsWithTax(simulateEconomyFixedInput(), parameters$tau)[7, ])
  plot(getRanking(simulateEconomyFixedInput(), parameters$tau)[2, ], getROAHBenefitsWithTax(simulateEconomyFixedInput(), parameters$tau)[7, ], xlab = "Total link betweenness centrality", ylab = "ROAH Benefits", col = "red")
  points(getRanking(simulateEconomyInput(), parameters$tau)[2, ], getROAHBenefitsWithTax(simulateEconomyInput(), parameters$tau)[7, ], col = "blue")
  lines(spline, col = "blue")
  lines(spline_fixed, col = "red")
  legend("topright", 
         legend = c("Fixed", "Non-fixed"), 
         lty = c(1,1), col = c("red", "blue"))
})

output$utility_per_km_betweenness <- renderPlot({
  if (is.null(simulateEconomyFixedInput()) || is.null(simulateEconomyInput())) return(invisible(NULL))
  if (length(unique(getRanking(simulateEconomyFixedInput(), parameters$tau)[2, ])) < 4) return(invisible(NULL))
  spline <- smooth.spline(getRanking(simulateEconomyInput(), parameters$tau)[2, ], getRanking(simulateEconomyInput(), parameters$tau)[4, ])
  spline_fixed <- smooth.spline(getRanking(simulateEconomyFixedInput(), parameters$tau)[2, ], getRanking(simulateEconomyFixedInput(), parameters$tau)[4, ])
  plot(getRanking(simulateEconomyFixedInput(), parameters$tau)[2, ], getRanking(simulateEconomyFixedInput(), parameters$tau)[4, ], xlab = "Total link betweenness centrality", ylab = "ROAH Benefits per km", col = "red")
  points(getRanking(simulateEconomyInput(), parameters$tau)[2, ], getRanking(simulateEconomyInput(), parameters$tau)[4, ], col = "blue")
  lines(spline, col = "blue")
  lines(spline_fixed, col = "red")
  legend("topright", 
         legend = c("Fixed", "Non-fixed"), 
         lty = c(1,1), col = c("red", "blue"))
})

output$boxplot <- renderPlot({
  if (is.null(simulateEconomyInput())) return(invisible(NULL))
  if (dim(getNetworkWeights(simulateEconomyInput()))[1] < 3) return(invisible(NULL))
  benefits <- getROAHBenefitsWithTax(simulateEconomyInput(), parameters$tau)[c(1,2,4,5,6), -1]
  df <- melt(benefits)
  boxplot(formula = value ~ Var1, data = df)
})

output$boxplot_both <- renderPlot({
  if (is.null(simulateEconomyInput()) || is.null(simulateEconomyFixedInput())) return(invisible(NULL))
  if (dim(getNetworkWeights(simulateEconomyInput()))[1] < 3) return(invisible(NULL))
  if (dim(getNetworkWeights(simulateEconomyFixedInput()))[1] < 3) return(invisible(NULL))
  benefits <- getROAHBenefitsWithTax(simulateEconomyInput(), parameters$tau)[c(1,2,4,5,6), -1]
  benefits_f <- getROAHBenefitsWithTax(simulateEconomyFixedInput(), parameters$tau)[c(1,2,4,5,6), -1]
  colnames(benefits) <- colnames(benefits, do.NULL = FALSE, prefix = "Sim.")
  colnames(benefits_f) <- colnames(benefits_f, do.NULL = FALSE, prefix = "Sim.")
  benefits <- cbind(LU = "NF", benefits)
  benefits_f <- cbind(LU = "F", benefits_f)
  benefits <- cbind(Variable = rownames(benefits), benefits)
  benefits_f <- cbind(Variable = rownames(benefits_f), benefits_f)
  rownames(benefits_f) <- rownames(benefits) <- NULL
  df <- merge(benefits, benefits_f, all = TRUE)
  for (i in 1:(dim(df)[2]-2)) {df[, (i+2)] <- as.numeric(as.character(df[, (i+2)]))}
  df <- melt(df)
  boxplot(formula = value ~ Variable, data = df, subset = LU == "NF",
          boxwex  = 0.25,
          at = 1:5 - 0.2,
          ylim = c(floor(min(df$value))-1, ceiling(max(df$value))+1),
          xaxt = "n",
          frame.plot = TRUE)
  boxplot(formula = value ~ Variable, data = df, subset = LU == "F", 
          col = "grey",
          boxwex = 0.25,
          at = 1:5 + 0.2,
          xaxt = "n",
          add = TRUE)
  axis(side = 1, at =  1:5, labels = c("TC", "LP", "TR", "TR", "WR"))
  legend("topleft", 
         c("Nonfixed", "Fixed"), 
         lty = c(1, 1), col = c("white", "grey"))
})