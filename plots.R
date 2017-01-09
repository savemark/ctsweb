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

output$x1density <- renderPlot({ # Work hours
  if (is.null(simulationInput())) return()
  weighted.densities(getArgMax(simulationInput()$populationA)[ , , , 1],
                     getArgMax(simulationInput()$populationB)[ , , , 1],
                     getProbability(simulationInput()$populationA)/getSize(simulationInput()$populationA),
                     getProbability(simulationInput()$populationB)/getSize(simulationInput()$populationB),
                     main = " ",
                     xlab = "Hours worked",
                     xlim = c(7, 9))
  legend("topright", 
         c('Base scenario', 'Do-something scenario'), 
         lty = c(1,1), col = c("red", "blue"))
})

output$x2density <- renderPlot({ # Consumption
  if (is.null(simulationInput())) return()
  weighted.densities(getArgMax(simulationInput()$populationA)[ , , , 2],
                     getArgMax(simulationInput()$populationB)[ , , , 2],
                     getProbability(simulationInput()$populationA)/getSize(simulationInput()$populationA),
                     getProbability(simulationInput()$populationB)/getSize(simulationInput()$populationB),
                     main = " ",
                     xlab = "Consumption")
  legend("topright", 
         c('Base scenario', 'Do-something scenario'), 
         lty = c(1,1), col = c("red", "blue"))
})

output$x3density <- renderPlot({ # Consumption
  if (is.null(simulationInput())) return()
  weighted.densities(getArgMax(simulationInput()$populationA)[ , , , 3],
                     getArgMax(simulationInput()$populationB)[ , , , 3],
                     getProbability(simulationInput()$populationA)/getSize(simulationInput()$populationA),
                     getProbability(simulationInput()$populationB)/getSize(simulationInput()$populationB),
                     main = " ",
                     xlab = "Leisure",
                     xlim = c(15, 18))
  legend("topright", 
         c('Base scenario', 'Do-something scenario'), 
         lty = c(1,1), col = c("red", "blue"))
})


output$x4density <- renderPlot({ # Land-use
  if (is.null(simulationInput())) return()
  weighted.densities(getArgMax(simulationInput()$populationA)[ , , , 4],
                     getArgMax(simulationInput()$populationB)[ , , , 4],
                     getProbability(simulationInput()$populationA)/getSize(simulationInput()$populationA),
                     getProbability(simulationInput()$populationB)/getSize(simulationInput()$populationB),
                     main = " ",
                     xlab = "Land-use")
  legend("topright", 
         c('Base scenario', 'Do-something scenario'), 
         lty = c(1,1), col = c("red", "blue"))
})

output$x5density <- renderPlot({ # Travel time
  if (is.null(simulationInput())) return()
  weighted.densities(getArgMax(simulationInput()$populationA)[ , , , 5],
                     getArgMax(simulationInput()$populationB)[ , , , 5],
                     getProbability(simulationInput()$populationA)/getSize(simulationInput()$populationA),
                     getProbability(simulationInput()$populationB)/getSize(simulationInput()$populationB),
                     main = " ",
                     xlab = "Travel time")
  legend("topright", 
         c('Base scenario', 'Do-something scenario'), 
         lty = c(1,1), col = c("red", "blue"))
})

output$vktdensity <- renderPlot({ # VKT
  if (is.null(simulationInput())) return()
  weighted.densities(array(getDistance(simulationInput()$cityA), dim = c(nrow(getDistance(simulationInput()$cityA)), 
                                                                         ncol(getDistance(simulationInput()$cityA)), 
                                                                         getSize(simulationInput()$populationA))),
                     array(getDistance(simulationInput()$cityB), dim = c(nrow(getDistance(simulationInput()$cityB)), 
                                                                         ncol(getDistance(simulationInput()$cityB)), 
                                                                         getSize(simulationInput()$populationB))),
                     getProbability(simulationInput()$populationA)/getSize(simulationInput()$populationA),
                     getProbability(simulationInput()$populationB)/getSize(simulationInput()$populationB),
                     main = " ",
                     xlab = "VKT")
  legend("topright", 
         c('Base scenario', 'Do-something scenario'), 
         lty = c(1,1), col = c("red", "blue"))
})

output$opdensity <- renderPlot({ # Origin quality
  if (is.null(simulationInput())) return()
  V <- getNodeCount(simulationInput()$cityA)
  N <- getSize(simulationInput()$populationA)
  op <- array(apply(t(getOriginPreference(simulationInput()$populationA)), 2,
                    function(x) {matrix(rep(x, length.out = V*V), V, V)}), dim = c(V, V, N))
  weighted.densities(op,
                     op,
                     getProbability(simulationInput()$populationA)/getSize(simulationInput()$populationA),
                     getProbability(simulationInput()$populationB)/getSize(simulationInput()$populationB),
                     main = " ",
                     xlab = "Origin quality")
  lines(density(op), lty = "dotted", col = "grey")
  legend("topright", 
         c('Base scenario', 'Do-something scenario', "Original distribution"), 
         lty = c(1, 1, 3), col = c("red", "blue", "grey"))
})

output$dpdensity <- renderPlot({ # Destination quality
  if (is.null(simulationInput())) return()
  V <- getNodeCount(simulationInput()$cityA)
  N <- getSize(simulationInput()$populationA)
  dp <- array(rep(t(getDestinationPreference(simulationInput()$populationA)), each = V), dim = c(V, V, N)) 
  weighted.densities(dp,
                     dp,
                     getProbability(simulationInput()$populationA)/getSize(simulationInput()$populationA),
                     getProbability(simulationInput()$populationB)/getSize(simulationInput()$populationB),
                     main = " ",
                     xlab = "Destination quality")
  lines(density(dp), lty = "dotted", col = "grey")
  legend("topright", 
         c('Base scenario', 'Do-something scenario', "Original distribution"), 
         lty = c(1, 1, 3), col = c("red", "blue", "grey"))
})

output$wageratedensity <- renderPlot({ # Wage rate
  if (is.null(simulationInput())) return()
  isolate({
    wagerate.ref <- exp(log(336000)+sqrt(2*(log(1.12)))*rnorm(10000))/{8*228}
    weighted.densities(getWageRate(simulationInput()$populationA),
                       getWageRate(simulationInput()$populationB),
                       t(apply(getProbability(simulationInput()$populationA), c(2, 3), sum))/getSize(simulationInput()$populationA),
                       t(apply(getProbability(simulationInput()$populationB), c(2, 3), sum))/getSize(simulationInput()$populationB),
                       main = " ",
                       xlab = "Wage rate",
                       ylim = range(c(density(getWageRate(simulationInput()$populationA))$y, 
                                      density(getWageRate(simulationInput()$populationB))$y, 
                                      density(getWageRate(populationInput()))$y,
                                      density(wagerate.ref)$y)))
    lines(density(getWageRate(populationInput())), lty = "dotted", col = "grey")
    lines(density(wagerate.ref))
    legend("topright", 
           c('Base scenario', 'Do-something scenario', 'Wage rate offers', 'Reference distribution'), 
           lty = c(1, 1, 3, 1), col = c("red", "blue", "grey", "black"))
  })
})

output$incomedensity <- renderPlot({ # Income
  if (is.null(simulationInput())) return()
  nodes <- getNodeCount(simulationInput()$cityA)
  N <- getSize(simulationInput()$populationA)
  wagerateA <- array(rep(t(getWageRate(simulationInput()$populationA)), each = nodes), dim = c(nodes, nodes, N))
  wagerateB <- array(rep(t(getWageRate(simulationInput()$populationB)), each = nodes), dim = c(nodes, nodes, N))    
  weighted.densities(getArgMax(simulationInput()$populationA)[ , , , 1]*wagerateA,
                     getArgMax(simulationInput()$populationB)[ , , , 1]*wagerateB,
                     getProbability(simulationInput()$populationA)/getSize(simulationInput()$populationA),
                     getProbability(simulationInput()$populationB)/getSize(simulationInput()$populationB),
                     main = " ",
                     xlab = "Income per day")
  legend("topright", 
         c('Base scenario', 'Do-something scenario'), 
         lty = c(1,1), col = c("red", "blue"))
})


# Equity plots ------------------------------------------------------------

output$equity <- renderPlot({
  if (is.null(simulationInput())) return()
  #isolate({
  cs <- roah4(list(population = simulationInput()$populationA, city = simulationInput()$cityA, price = simulationInput()$solutionA$par, comfort = input$a_beta5), 
              list(population = simulationInput()$populationB, city = simulationInput()$cityB, price = simulationInput()$solutionB$par, comfort = input$b_beta5))
  equityPlot(cs, seq(0, 1, 1/input$probs))
  #})
})

# 3D plots ----------------------------------------------------------------

output$residency3dA <- renderPlot({
  if (is.null(simulationInput())) return()
  dens <- apply(getProbability(simulationInput()$populationA), 1, sum, na.rm = TRUE)/getArea(simulationInput()$cityA)
  surf <- interp(getCoordinate(simulationInput()$cityA)[ , 1], getCoordinate(simulationInput()$cityA)[ , 2], dens)
  persp3D(x = surf$x, 
          y = surf$y,
          z = surf$z,
          col = gg2.col(),
          contour = list(side = "z"),
          lightning = "ambient",
          shade = 0.5,
          alpha = 1,
          ticktype = "detailed",
          cex.axis = 0.5,
          zlim = range(surf$z, na.rm = TRUE),
          zlab = "Residence density",
          sub = "Base scenario")
}, width = "auto")

output$residency3dB <- renderPlot({
  if (is.null(simulationInput())) return()
  dens <- apply(getProbability(simulationInput()$populationB), 1, sum, na.rm = TRUE)/getArea(simulationInput()$cityB)
  surf <- interp(getCoordinate(simulationInput()$cityB)[ , 1], getCoordinate(simulationInput()$cityB)[ , 2], dens)
  persp3D(x = surf$x, 
          y = surf$y,
          z = surf$z,
          col = gg2.col(),
          contour = list(side = "z"),
          lightning = "ambient",
          shade = 0.5,
          alpha = 1,
          ticktype = "detailed",
          cex.axis = 0.5,
          zlim = range(surf$z, na.rm = TRUE),
          zlab = "Residence density",
          sub = "Base scenario")
}, width = "auto")

output$work3dA <- renderPlot({
  if (is.null(simulationInput())) return()
  dens <- apply(getProbability(simulationInput()$populationA), 2, sum, na.rm = TRUE)/getArea(simulationInput()$cityA)
  surf <- interp(getCoordinate(simulationInput()$cityA)[ , 1], getCoordinate(simulationInput()$cityA)[ , 2], dens)
  persp3D(x = surf$x, 
          y = surf$y,
          z = surf$z,
          col = gg2.col(),
          contour = list(side = "z"),
          lightning = "ambient",
          shade = 0.5,
          alpha = 1,
          ticktype = "detailed",
          cex.axis = 0.5,
          zlim = range(surf$z, na.rm = TRUE),
          zlab = "Worker density",
          sub = "Base scenario")
}, width = "auto")

output$work3dB <- renderPlot({
  if (is.null(simulationInput())) return()
  dens <- apply(getProbability(simulationInput()$populationB), 2, sum, na.rm = TRUE)/getArea(simulationInput()$cityB)
  surf <- interp(getCoordinate(simulationInput()$cityB)[ , 1], getCoordinate(simulationInput()$cityB)[ , 2], dens)
  persp3D(x = surf$x, 
          y = surf$y,
          z = surf$z,
          col = gg2.col(),
          contour = list(side = "z"),
          lightning = "ambient",
          shade = 0.5,
          alpha = 1,
          ticktype = "detailed",
          cex.axis = 0.5,
          zlim = range(surf$z, na.rm = TRUE),
          zlab = "Worker density",
          sub = "Do-something scenario")
}, width = "auto")

output$price3dA <- renderPlot({
  if (is.null(simulationInput())) return()
  dens <- simulationInput()$solutionA$par[1:getNodeCount(simulationInput()$cityA)]
  surf <- interp(getCoordinate(simulationInput()$cityA)[ , 1], getCoordinate(simulationInput()$cityA)[ , 2], dens)
  persp3D(x = surf$x, 
          y = surf$y,
          z = surf$z,
          col = gg2.col(),
          contour = list(side = "z"),
          lightning = "ambient",
          shade = 0.5,
          alpha = 1,
          ticktype = "detailed",
          cex.axis = 0.5,
          zlim = range(surf$z, na.rm = TRUE),
          zlab = "Land-price",
          sub = "Base scenario")
}, width = "auto")

output$price3dB <- renderPlot({
  if (is.null(simulationInput())) return()
  dens <- simulationInput()$solutionB$par[1:getNodeCount(simulationInput()$cityB)]
  surf <- interp(getCoordinate(simulationInput()$cityB)[ , 1], getCoordinate(simulationInput()$cityB)[ , 2], dens)
  persp3D(x = surf$x, 
          y = surf$y,
          z = surf$z,
          col = gg2.col(),
          contour = list(side = "z"),
          lightning = "ambient",
          shade = 0.5,
          alpha = 1,
          ticktype = "detailed",
          cex.axis = 0.5,
          zlim = range(surf$z, na.rm = TRUE),
          zlab = "Land-price",
          sub = "Do-something scenario")
}, width = "auto")