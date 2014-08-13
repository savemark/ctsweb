library(shiny)
library(BB)
library(plyr)
library(igraph)
library(tripack)
library(fields)
source("helpers.R")

xy <- matrix(scan("xy.txt"), 33, 2, byrow = TRUE)
v <- nrow(xy) # Number of nodes
adjacency <- matrix(scan("adj.txt"), 33, 33, byrow = TRUE)

shinyServer(
  function(input, output, session) {
    
    # Cities for simulations A and B
    #
    cityInputA <- reactive({
      create.city(
        xy, 
        adjacency, 
        speed = input$a_speed, 
        cost = input$a_travel_cost
      )
    })
    
    cityInputB <- reactive({
      create.city(
        xy, 
        adjacency, 
        speed = input$b_speed, 
        cost = input$b_travel_cost
      )
    })
    
    # Parameters for simulation A and B
    #
    parameterInputA <- reactive({
      return(
        list(
          alpha = input$alpha, 
          beta = input$beta, 
          gamma = input$gamma, 
          theta = input$a_theta, 
          tau = input$tau
        )
      )
    })
    
    parameterInputB <- reactive({
      return(
        list(
          alpha = input$alpha, 
          beta = input$beta, 
          gamma = input$gamma, 
          theta = input$b_theta, 
          tau = input$tau
        )
      )
    })
    
    # Population
    #
    incomeInput <- reactive({
      income <- workerIncomeMatrix(input$n, 
                                   v, 
                                   input$median, 
                                   input$spread,
                                   input$days,
                                   input$hours)
    })
    preferenceInput <- reactive({
      h_preference <- workerPreferenceMatrix(input$n, v, input$h_mean, input$h_sd)
      w_preference <- workerPreferenceMatrix(input$n, v, input$w_mean, input$w_sd)
      list(h_preference = h_preference, w_preference = w_preference)
    })
    
    # Plots
    #
    output$populationIncomeDensity <- renderPlot({
      plot(density(incomeInput()), main = "")
    })
    
    output$populationPreferenceDensity <- renderPlot({
      par(mfrow =c(1, 2))
      plot(density(preferenceInput()$h_preference), main = "")
      plot(density(preferenceInput()$w_preference), main = "")
    })
    
    #output$a_cityPlot <- renderPlot({ 
    #  plotcity(cityInputA())
    #})
    
    #output$b_cityPlot <- renderPlot({ 
    #  plotcity(cityInputB())
    #})
    
    # Simulation
    #
    simulationInputA <- reactive({
      input[["a_recalc"]]
      
      isolate({        
        supply <- V(cityInputA()$graph)$area.supply
        w <- incomeInput()
        h_preference <- preferenceInput()$h_preference
        w_preference <- preferenceInput()$w_preference
        c <- cityInputA()$cost
        t <- cityInputA()$time
        alpha <- parameterInputA()$alpha
        beta <- parameterInputA()$beta
        gamma <- parameterInputA()$gamma
        theta <- parameterInputA()$theta
        tau <- parameterInputA()$tau
        val <- dfsane(
          rep(1000, v), fn = F, method = 2, 
          control = list(M = 75, NM = TRUE, maxit = input$a_maxit), 
          quiet = FALSE, supply = supply, w = w, c = c, t = t, 
          alpha = alpha, beta = beta, gamma = gamma, theta = theta, tau = tau,
          G = 0, H = h_preference, D = w_preference
        )
        #val <- nleqslv(
        #  rep(1000, v), F, jac = NULL, supply = supply, w = w, c = c, t = t,
         # alpha = alpha, beta = beta, gamma = gamma, theta = theta, tau = tau,
         # G = 0, H = h_preference, D = w_preference, method = "Newton", global = "dbldog"
        #)
        
        object <- maxUtility(
          val$par, w, c, t, alpha, beta, gamma, theta, tau,
          G = 0, H = h_preference, D = w_preference
        )
        demand <- nodeLandDemand(object, v)
        Tij <- matrix(0, v, v)
        tab <- count(object$choice)
        for (i in 1:length(tab$freq)) {
          Tij[tab$x.1[i], tab$x.2[i]] <- tab$freq[i]
        }
        return(
          list(
            price = val$par,
            u = object$umax,
            yu = object$yu,
            ltu = object$ltu,
            luu = object$luu,
            tcu = object$tcu,
            income = w, 
            u.monetary = object$u.monetary,
            vot = object$vot,
            Hu = object$Hu,
            Du = object$Du,
            choice = object$choice, 
            ws = object$ws, 
            ld = object$ld,
            demand = demand,
            supply = supply,
            tau = tau,
            Tij = Tij
          )
        )
      })
    })
    
    simulationInputB <- reactive({
      input[["b_recalc"]]
      
      isolate({     
        supply <- V(cityInputB()$graph)$area.supply
        w <- incomeInput()
        h_preference <- preferenceInput()$h_preference
        w_preference <- preferenceInput()$w_preference
        c <- cityInputB()$cost
        t <- cityInputB()$time
        alpha <- parameterInputB()$alpha
        beta <- parameterInputB()$beta
        gamma <- parameterInputB()$gamma
        theta <- parameterInputB()$theta
        tau <- parameterInputB()$tau
        
        val <- dfsane(
          rep(1000, v), fn = F, method = 2, 
          control = list(M = 75, NM = FALSE, maxit = input$b_maxit), 
          quiet = FALSE, supply = supply, w = w, c = c, t = t, 
          alpha = alpha, beta = beta, gamma = gamma, theta = theta, tau = tau,
          G = 0, H = h_preference, D = w_preference
        )
        object <- maxUtility(
          val$par, w, c, t, alpha, beta, gamma, theta, tau,
          G = 0, H = h_preference, D = w_preference
        )
        demand <- nodeLandDemand(object, v)
        Tij <- matrix(0, v, v)
        tab <- count(object$choice)
        for (i in 1:length(tab$freq)) {
          Tij[tab$x.1[i], tab$x.2[i]] <- tab$freq[i]
        }
        return(
          list(
            price = val$par,
            u = object$umax,
            yu = object$yu,
            ltu = object$ltu,
            luu = object$luu,
            tcu = object$tcu,
            income = w,
            u.monetary = object$u.monetary,
            vot = object$vot,
            Hu = object$Hu,
            Du = object$Du,
            choice = object$choice, 
            ws = object$ws, 
            ld = object$ld,
            demand = demand,
            supply = supply,
            tau = tau,
            Tij = Tij
          )
        )
      })
    })
    
    # Summary
    #
    output$simulationPopulationA <- renderTable({
      if (input[["a_recalc"]] == 0) 
        return()
      
      simulationSummary(simulationInputA())$population
    })
    
    output$simulationCityA <- renderTable({
      if (input[["a_recalc"]] == 0)
        return()
      
      simulationSummary(simulationInputA())$city
    })
    
    output$a_cityPlot <- renderPlot({ 
      if (input[["a_recalc"]] == 0)
        return(plotcity(cityInputA()))
      
      plotcity(cityInputA(), simulationInputA()$Tij)
    })
    
    output$simulationPopulationB <- renderTable({
      if (input[["b_recalc"]] == 0) 
        return()
      
      simulationSummary(simulationInputB())$population
    })
    
    output$simulationCityB <- renderTable({
      if (input[["b_recalc"]] == 0)
        return()
      
      simulationSummary(simulationInputB())$city
    })
    
    output$b_cityPlot <- renderPlot({ 
      if (input[["b_recalc"]] == 0)
        return(plotcity(cityInputB()))
      
      plotcity(cityInputB(), simulationInputB()$Tij)
    })
    
    output$WEB <- renderTable({
      if (input[["a_recalc"]] == 0 || input[["b_recalc"]] == 0) 
        return()
      
      widerEconomicBenefits(simulationInputA(), simulationInputB())$WEB
    })
    
    output$WEBP <- renderTable({
      if (input[["a_recalc"]] == 0 || input[["b_recalc"]] == 0) 
        return()
      
      widerEconomicBenefits(simulationInputA(), simulationInputB())$WEBP
    })
    
    output$CS <- renderTable({
      if (input[["a_recalc"]] == 0 || input[["b_recalc"]] == 0) 
        return()
      
      isolate({
        consumer.surplus(simulationInputA(), simulationInputB(), cityInputA(), cityInputB())
      })
    })
    
  }
)

