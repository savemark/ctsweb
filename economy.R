economy <- setClass("Economy",
                    slots = c(
                      city = "City",
                      population = "Population",
                      utility = "list",
                      probability = "list",
                      spillover = "list",
                      sims = "list",
                      network.weights = "array", 
                      in.equilibrium = "logical"
                    )
)

setGeneric("getNetworkWeights", function(object) {standardGeneric("getNetworkWeights")})
setMethod("getNetworkWeights",
          signature = "Economy",
          definition = function(object) {
            object@network.weights
            return(object@network.weights)
          }
)

setGeneric("setNetworkWeights<-", function(object, value) {standardGeneric("setNetworkWeights<-")})
setReplaceMethod("setNetworkWeights",
                 signature = "Economy",
                 definition = function(object, value) {
                   object@network.weights <- value
                   return(object)
                 }
)

setGeneric("simulate", function(object, guess, ...) {standardGeneric("simulate")})
setMethod("simulate",
          signature = "Economy",
          definition = function(object, guess, fixed.landuse = FALSE, rural.utility = NULL) {
            S <- nrow(object@network.weights)
            city <- object@city
            if (!fixed.landuse) {
              for (s in 1:S) {
                city <- setEdgeSpeed(city, value = object@network.weights[s, , 1])
                city <- setEdgeCostFactor(city, value = object@network.weights[s, , 2])
                city <- setEdgeComfort(city, value = object@network.weights[s, , 3])
                sim <- simulation(guess = rep(guess, getNodeCount(city)), 
                                  city = city,
                                  population = object@population,
                                  utility = object@utility,
                                  probability = object@probability,
                                  spillover = object@spillover,
                                  rural.utility = rural.utility)
                object@sims[[s]] <- sim
                if (sim$solution$convergence == 0) {object@in.equilibrium[s] <- TRUE} else {object@in.equilibrium[s] <- FALSE}
              }
            } else {
              city <- setEdgeSpeed(city, value = object@network.weights[1, , 1])
              city <- setEdgeCostFactor(city, value = object@network.weights[1, , 2])
              city <- setEdgeComfort(city, value = object@network.weights[1, , 3])
              sim <- simulation(guess = rep(guess, getNodeCount(city)), 
                                city = city,
                                population = object@population,
                                utility = object@utility,
                                probability = object@probability,
                                spillover = object@spillover,
                                rural.utility = rural.utility)
              if (sim$solution$convergence == 0) {object@in.equilibrium[1] <- TRUE} else {object@in.equilibrium[1] <- FALSE}
              scale <- array(apply(array(rep(colSums(aperm(getProbability(sim$population), c(2, 1, 3))), each = getNodeCount(sim$city)), 
                                         c(getNodeCount(sim$city), getNodeCount(sim$city), getNumberOfClasses(sim$population))), 3, t), 
                             c(getNodeCount(sim$city), getNodeCount(sim$city), getNumberOfClasses(sim$population)))
              object@sims[[1]] <- sim
              for (s in 1:S) {
                city <- setEdgeSpeed(city, value = object@network.weights[s, , 1])
                city <- setEdgeCostFactor(city, value = object@network.weights[s, , 2])
                city <- setEdgeComfort(city, value = object@network.weights[s, , 3])
                sim <- simulation(guess = rep(guess, getNodeCount(city)), 
                                  city = city,
                                  population = object@population,
                                  utility = object@utility,
                                  probability = object@probability,
                                  spillover = object@spillover,
                                  scale = scale,
                                  rural.utility = rural.utility)
                object@sims[[s]] <- sim
                if (sim$solution$convergence == 0) {object@in.equilibrium[s] <- TRUE} else {object@in.equilibrium[s] <- FALSE}
              }
            }
            return(object)
          }
)

setGeneric("getConvergenceMessages", function(object) {standardGeneric("getConvergenceMessages")})
setMethod("getConvergenceMessages", 
          signature = "Economy",
          definition = function (object) {
            for (i in seq_along(object@sims)) {
              cat("Scenario [", i, "]: Land prices and wage rates...", object@sims[[i]]$solution$message, " | ", "(BBSolve) feval: [", object@sims[[i]]$solution$feval, "]", " iter: [", object@sims[[i]]$solution$iter, "] \n")
            }
          }
)

setGeneric("getEquivalentVariation", function(object, sigma) {standardGeneric("getEquivalentVariation")})
setMethod("getEquivalentVariation", 
          signature = "Economy",
          definition = function (object, sigma) {
            res <- matrix(nrow = 1, ncol = (length(seq_along(object@sims))))
            # We need to subtract the maximum of the indrect utilities for each choice set, 
            # otherwise the logsum will not be possible to calculate (because it will be too large)
            maxu.x <- apply(getUtility(object@sims[[1]]$population), 3, max)
            maxu.xa <- array(rep(maxu.x, each = getNodeCount(object@sims[[1]]$city)^2), dim = c(getNodeCount(object@sims[[1]]$city), getNodeCount(object@sims[[1]]$city), getNumberOfClasses(object@sims[[1]]$population)))
            logsum.x <- maxu.x+sigma*log(apply(exp((getUtility(object@sims[[1]]$population)-maxu.xa)/sigma), 3, sum))
            for (i in seq_along(object@sims)) {
              maxu.y <- apply(getUtility(object@sims[[i]]$population), 3, max)
              maxu.ya <- array(rep(maxu.y, each = getNodeCount(object@sims[[i]]$city)^2), dim = c(getNodeCount(object@sims[[i]]$city), getNodeCount(object@sims[[i]]$city), getNumberOfClasses(object@sims[[i]]$population)))
              logsum.y <- maxu.y+sigma*log(apply(exp((getUtility(object@sims[[i]]$population)-maxu.ya)/sigma), 3, sum))
              ev <- (logsum.y-logsum.x)/(apply(getProbability(object@sims[[1]]$population)*getMarginalEffect(object@sims[[1]]$population)[ , , , 1], 3, sum))
              res[ , i] <- sum(ev)
            }
            rownames(res) <- "Difference"
            return(res)
          }
)

setGeneric("getROAHBenefits", function(object) {standardGeneric("getROAHBenefits")})
setMethod("getROAHBenefits", 
          signature = "Economy",
          definition = function (object) {
            res <- matrix(nrow = 6, ncol = (length(seq_along(object@sims))))
            cost.x <- array(getCost(object@sims[[1]]$city), dim = c(getNodeCount(object@sims[[1]]$city), getNodeCount(object@sims[[1]]$city), getNumberOfClasses(object@sims[[1]]$population)))
            time.x <- array(getTime(object@sims[[1]]$city), dim = c(getNodeCount(object@sims[[1]]$city), getNodeCount(object@sims[[1]]$city), getNumberOfClasses(object@sims[[1]]$population)))
            cost.x <- cost.x+mapply(function(i) {t(cost.x[ , , i])}, 1:dim(cost.x)[3], SIMPLIFY = "array")
            time.x <- time.x+mapply(function(i) {t(time.x[ , , i])}, 1:dim(time.x)[3], SIMPLIFY = "array")
            comfort.x <- 1 # getComfort ....
            comfort.y <- 1
            price.x <- array(getVertexPrice(object@sims[[1]]$city), dim = c(getNodeCount(object@sims[[1]]$city), getNodeCount(object@sims[[1]]$city), getNumberOfClasses(object@sims[[1]]$population)))
            wage.x <- array(rep(t(getWageRate(object@sims[[1]]$population)), each = getNodeCount(object@sims[[1]]$city)), dim = c(getNodeCount(object@sims[[1]]$city), getNodeCount(object@sims[[1]]$city), getNumberOfClasses(object@sims[[1]]$population)))
            for (i in seq_along(object@sims)) {
              cost.y <- array(getCost(object@sims[[i]]$city), dim = c(getNodeCount(object@sims[[i]]$city), getNodeCount(object@sims[[i]]$city), getNumberOfClasses(object@sims[[i]]$population)))
              time.y <- array(getTime(object@sims[[i]]$city), dim = c(getNodeCount(object@sims[[i]]$city), getNodeCount(object@sims[[i]]$city), getNumberOfClasses(object@sims[[i]]$population)))
              cost.y <- cost.y+mapply(function(i) {t(cost.y[ , , i])}, 1:dim(cost.y)[3], SIMPLIFY = "array")
              time.y <- time.y+mapply(function(i) {t(time.y[ , , i])}, 1:dim(time.y)[3], SIMPLIFY = "array")
              price.y <- array(getVertexPrice(object@sims[[i]]$city), dim = c(getNodeCount(object@sims[[i]]$city), getNodeCount(object@sims[[i]]$city), getNumberOfClasses(object@sims[[i]]$population)))
              wage.y <- array(rep(t(getWageRate(object@sims[[i]]$population)), each = getNodeCount(object@sims[[i]]$city)), dim = c(getNodeCount(object@sims[[i]]$city), getNodeCount(object@sims[[i]]$city), getNumberOfClasses(object@sims[[i]]$population)))
              roah1 <- sum(0.5*(getProbability(object@sims[[1]]$population)+getProbability(object@sims[[i]]$population))*(-1)*(cost.y-cost.x)) # marginal value of travel cost is -1*marginal value of ex. income
              roah2 <- sum(0.5*(getProbability(object@sims[[1]]$population)+getProbability(object@sims[[i]]$population))*((getMarginalEffect(object@sims[[1]]$population)[ , , , 3]/getMarginalEffect(object@sims[[1]]$population)[ , , , 1])*(time.y-time.x)))
              roah3 <- sum(0.5*(getProbability(object@sims[[1]]$population)+getProbability(object@sims[[i]]$population))*((-time.x/getMarginalEffect(object@sims[[1]]$population)[ , , , 1])*(comfort.y-comfort.x)))
              roah4 <- sum(0.5*(getProbability(object@sims[[1]]$population)+getProbability(object@sims[[i]]$population))*((getMarginalEffect(object@sims[[1]]$population)[ , , , 4]/getMarginalEffect(object@sims[[1]]$population)[ , , , 1])*(price.y-price.x)))
              roah5 <- sum(0.5*(getProbability(object@sims[[1]]$population)+getProbability(object@sims[[i]]$population))*((getMarginalEffect(object@sims[[1]]$population)[ , , , 5]/getMarginalEffect(object@sims[[1]]$population)[ , , , 1])*(wage.y-wage.x)))
              res[ , i] <- c(roah1, roah2, roah3, roah4, roah5, sum(roah1+roah2+roah3+roah4+roah5))
            }
            rownames(res) <- c("Cost", "Time", "Comfort", "Price", "Wage rate", "Sum")
            return(res)
          }
)

setGeneric("getROAHBenefitsWithTax", function(object, tau) {standardGeneric("getROAHBenefitsWithTax")})
setMethod("getROAHBenefitsWithTax", 
          signature = "Economy",
          definition = function (object, tau) {
            dim1 <- c(getNodeCount(object@sims[[1]]$city), getNodeCount(object@sims[[1]]$city), getNumberOfClasses(object@sims[[1]]$population))
            kn1 <- array(rep(getSizeOfEachClass(object@sims[[1]]$population), each = getNodeCount(object@sims[[1]]$city)^2), dim = dim1)
            res <- matrix(nrow = 7, ncol = (length(seq_along(object@sims))))
            cost.x <- array(getCost(object@sims[[1]]$city), dim = dim1)
            time.x <- array(getTime(object@sims[[1]]$city), dim = dim1)
            comfort.x <- array(getComfort(object@sims[[1]]$city), dim = dim1)
            cost.x <- cost.x+mapply(function(i) {t(cost.x[ , , i])}, 1:dim(cost.x)[3], SIMPLIFY = "array")
            time.x <- time.x+mapply(function(i) {t(time.x[ , , i])}, 1:dim(time.x)[3], SIMPLIFY = "array")
            comfort.x <- comfort.x+mapply(function(i) {t(comfort.x[ , , i])}, 1:dim(comfort.x)[3], SIMPLIFY = "array")
            price.x <- array(getVertexPrice(object@sims[[1]]$city), dim = dim1)
            wagerate.x <- array(rep(t(getWageRate(object@sims[[1]]$population)), each = getNodeCount(object@sims[[1]]$city)), dim = dim1)
            tax.x <- sum(tau*wagerate.x*getArgMax(object@sims[[1]]$population)[ , , , 1]*kn1*getProbability(object@sims[[1]]$population))
            for (i in seq_along(object@sims)) {
              dimi <- c(getNodeCount(object@sims[[i]]$city), getNodeCount(object@sims[[i]]$city), getNumberOfClasses(object@sims[[i]]$population))
              kni <- array(rep(getSizeOfEachClass(object@sims[[i]]$population), each = getNodeCount(object@sims[[i]]$city)^2), dim = dimi)
              cost.y <- array(getCost(object@sims[[i]]$city), dim = dimi)
              time.y <- array(getTime(object@sims[[i]]$city), dim = dimi)
              comfort.y <- array(getComfort(object@sims[[i]]$city), dim = dimi)
              cost.y <- cost.y+mapply(function(i) {t(cost.y[ , , i])}, 1:dim(cost.y)[3], SIMPLIFY = "array")
              time.y <- time.y+mapply(function(i) {t(time.y[ , , i])}, 1:dim(time.y)[3], SIMPLIFY = "array")
              comfort.y <- comfort.y+mapply(function(i) {t(comfort.y[ , , i])}, 1:dim(comfort.x)[3], SIMPLIFY = "array")
              price.y <- array(getVertexPrice(object@sims[[i]]$city), dim = dimi)
              wagerate.y <- array(rep(t(getWageRate(object@sims[[i]]$population)), each = getNodeCount(object@sims[[i]]$city)), dim = dimi)
              tax.y <- sum(tau*wagerate.y*getArgMax(object@sims[[i]]$population)[ , , , 1]*kni*getProbability(object@sims[[i]]$population))
              roah1 <- sum(0.5*(kn1*getProbability(object@sims[[1]]$population)+kni*getProbability(object@sims[[i]]$population))*(-1)*(cost.y-cost.x)) # marginal value of travel cost is -1*marginal value of exog. income
              roah2 <- sum(0.5*(kn1*getProbability(object@sims[[1]]$population)+kni*getProbability(object@sims[[i]]$population))*((getMarginalEffect(object@sims[[1]]$population)[ , , , 3]/getMarginalEffect(object@sims[[1]]$population)[ , , , 1])*(time.y-time.x)))
              roah3 <- sum(0.5*(kn1*getProbability(object@sims[[1]]$population)+kni*getProbability(object@sims[[i]]$population))*((-time.x/getMarginalEffect(object@sims[[1]]$population)[ , , , 1])*(comfort.y-comfort.x)))
              roah4 <- sum(0.5*(kn1*getProbability(object@sims[[1]]$population)+kni*getProbability(object@sims[[i]]$population))*((getMarginalEffect(object@sims[[1]]$population)[ , , , 4]/getMarginalEffect(object@sims[[1]]$population)[ , , , 1])*(price.y-price.x)))
              roah5 <- sum(0.5*(kn1*getProbability(object@sims[[1]]$population)*getMarginalEffect(object@sims[[1]]$population)[ , , , 5]/getMarginalEffect(object@sims[[1]]$population)[ , , , 1]+kni*getProbability(object@sims[[i]]$population)*getMarginalEffect(object@sims[[i]]$population)[ , , , 5]/getMarginalEffect(object@sims[[i]]$population)[ , , , 1])*((1/1)*(wagerate.y-wagerate.x)))
              tax.diff <- tax.y-tax.x
              res[ , i] <- c(roah1, roah2, roah3, roah4, roah5, tax.diff, sum(roah1+roah2+roah3+roah4+roah5+tax.diff))
            }
            rownames(res) <- c("Cost", "Time", "Comfort", "Price", "Wage rate", "Tax", "Sum")
            return(res)
          }
)

setGeneric("getTopDownBenefits", function(object, tau) {standardGeneric("getTopDownBenefits")})
setMethod("getTopDownBenefits", 
          signature = "Economy",
          definition = function (object, tau) {
            res <- matrix(nrow = 6, ncol = length(seq_along(object@sims)))
            dim1 <- c(getNodeCount(object@sims[[1]]$city), getNodeCount(object@sims[[1]]$city), getNumberOfClasses(object@sims[[1]]$population))
            kn1 <- array(rep(getSizeOfEachClass(object@sims[[1]]$population), each = getNodeCount(object@sims[[1]]$city)^2), dim = dim1)
            home.x <- apply(mapply(function(j) {getSizeOfEachClass(object@sims[[1]]$population)[j]*getProbability(object@sims[[1]]$population)[ , , j]}, 1:getNumberOfClasses(object@sims[[1]]$population), SIMPLIFY = "array"), 1, sum, na.rm = TRUE)
            lor.x <- sum(getArea(object@sims[[1]]$city)*getVertexPrice(object@sims[[1]]$city)) # /home.x
            wagerate.x <- array(rep(t(getWageRate(object@sims[[1]]$population)), each = getNodeCount(object@sims[[1]]$city)), dim = dim1)
            tax.x <- sum(tau*wagerate.x*getArgMax(object@sims[[1]]$population)[ , , , 1]*kn1*getProbability(object@sims[[1]]$population))
            for (i in seq_along(object@sims)) {
              dimi <- c(getNodeCount(object@sims[[i]]$city), getNodeCount(object@sims[[i]]$city), getNumberOfClasses(object@sims[[i]]$population))
              kni <- array(rep(getSizeOfEachClass(object@sims[[i]]$population), each = getNodeCount(object@sims[[i]]$city)^2), dim = dimi)
              home.y <- apply(mapply(function(j) {getSizeOfEachClass(object@sims[[i]]$population)[j]*getProbability(object@sims[[i]]$population)[ , , j]}, 1:getNumberOfClasses(object@sims[[i]]$population), SIMPLIFY = "array"), 1, sum, na.rm = TRUE)
              lor.y <- sum(getArea(object@sims[[i]]$city)*getVertexPrice(object@sims[[i]]$city)) # /home.y
              wagerate.y <- array(rep(t(getWageRate(object@sims[[i]]$population)), each = getNodeCount(object@sims[[i]]$city)), dim = dimi)
              tax.y <- sum(tau*wagerate.y*getArgMax(object@sims[[i]]$population)[ , , , 1]*kni*getProbability(object@sims[[i]]$population))
              base <- sum((kni*getProbability(object@sims[[i]]$population)*getArgMax(object@sims[[i]]$population)[ , , , 1]-kn1*getProbability(object@sims[[1]]$population)*getArgMax(object@sims[[1]]$population)[ , , , 1])*(1-tau)*wagerate.x)
              scale <- sum((kni*getProbability(object@sims[[i]]$population)*getArgMax(object@sims[[i]]$population)[ , , , 1])*(1-tau)*(wagerate.y-wagerate.x))
              spillovers <- (1-tau)*sum(0.5*(kni*getProbability(object@sims[[i]]$population)*getArgMax(object@sims[[i]]$population)[ , , , 1]+kn1*getProbability(object@sims[[1]]$population)*getArgMax(object@sims[[1]]$population)[ , , , 1])*(wagerate.y-wagerate.x))
              base <- sum((kni*getProbability(object@sims[[i]]$population)*getArgMax(object@sims[[i]]$population)[ , , , 1]-kn1*getProbability(object@sims[[1]]$population)*getArgMax(object@sims[[1]]$population)[ , , , 1])*(1-tau)*wagerate.x)
              scale <- sum((kni*getProbability(object@sims[[i]]$population)*getArgMax(object@sims[[i]]$population)[ , , , 1])*(1-tau)*(wagerate.y-wagerate.x))
              lor.diff <- lor.y-lor.x 
              tax.diff <- tax.y-tax.x
              wage.sum.diff <- sum(base, scale)
              res[ , i] <- c(tax.diff, lor.diff, spillovers, base, scale, wage.sum.diff)
            }
            rownames(res) <- c("Tax revenues", "Land-owner revenues", "Spillovers", "Wages: Matching and Work hours", "Wages: Spillovers", "Total difference in wages")# "Conditional diffrence in wages")
            return(res)
          }
)

setGeneric("getRanking", function(object, tau) {standardGeneric("getRanking")})
setMethod("getRanking", 
          signature = "Economy",
          definition = function (object, tau) {
            res <- matrix(nrow = 7, ncol = length(seq_along(object@sims)))
            for (i in seq_along(object@sims)) {
              tb <- getEdgeTime(object@sims[[1]]$city)!=getEdgeTime(object@sims[[i]]$city) # Time bools
              cb <- getEdgeCost(object@sims[[1]]$city)!=getEdgeCost(object@sims[[i]]$city) # Cost bools
              index <- tb|cb # Either the time or the cost has been changed (or none)
              tlength <- sum(getEdgeLength(object@sims[[1]]$city)[index])
              tbetweenness <- sum(getEdgeBetweenness(object@sims[[1]]$city, normalized = FALSE)[index])
              res[1, i] <- tlength
              res[2, i] <- tbetweenness
            }
            res[3, ] <- getROAHBenefitsWithTax(object, tau)[7, ]
            res[4, ] <- res[3 , ]/res[1 , ] # Benefits/Length
            res[4, is.nan(res[4, ])] <- 0 # Benefits/Length
            res[5, ] <- getROAHBenefitsWithTax(object, tau)[7, ]*res[2, ] # Benefits*Betweenness
            res[5, is.nan(res[5, ])] <- 0 
            res[6, ] <- rank(res[4 , ])
            res[7, ] <- rank(res[5 , ])
            #res <- t(res)
            rownames(res) <- c("Total length", "Total betweenness", "Benefits", "Benefits/length", "Benefits*betweenness", "Rank (length)", "Rank (betweenness)")
            return(res)
           }
)

setGeneric("getTotalBenefitsGivenBudget", function(object, tau, ckm = 1, budget = Inf) {standardGeneric("getTotalBenefitsGivenBudget")})
setMethod("getTotalBenefitsGivenBudget", 
          signature = "Economy",
          definition = function (object, tau, ckm = 1) {
            res <- matrix(nrow = length(seq_along(object@sims)), ncol = 7)
            for (i in seq_along(object@sims)) {
              tb <- getEdgeTime(object@sims[[1]]$city)!=getEdgeTime(object@sims[[i]]$city) # Time bools
              cb <- getEdgeCost(object@sims[[1]]$city)!=getEdgeCost(object@sims[[i]]$city) # Cost bools
              index <- tb|cb # Either the time or the cost has been changed (or none)
              tlength <- sum(getEdgeLength(object@sims[[1]]$city)[index])
              res[i, 5] <- tlength
              res[i, 4] <- tlength*ckm
              res[i, 1] <- i
            }
            res[ , 2] <- getROAHBenefitsWithTax(object, tau)[7, ]/res[ , 4]
            res[ , 3] <- getROAHBenefitsWithTax(object, tau)[7, ]
            colnames(res) <- c("Scenario", "B/C", "B", "C", "Length", "ACC C", "ACC B")
            res <- res[order(-res[ , "B/C"]), ]
            accc <- cumsum(res[ , "C"])
            accb <- cumsum(res[ , "B"])
            res[ , 6] <- accc
            res[ , 7] <- accb
            res <- rbind(c(1, NA, NA, NA, NA, 0, 0), res)
            res <- res[-nrow(res), ]
            return(res)
          }
)

setGeneric("getLogitTransportModel", function(object, sigma) {standardGeneric("getLogitTransportModel")})
setMethod("getLogitTransportModel", 
          signature = "Economy",
          definition = function (object, sigma) {
            res <- matrix(nrow = 1, ncol = length(seq_along(object@sims)))
            for (i in seq_along(object@sims)) {
              vottMean <- apply(getProbability(object@sims[[i]]$population)*getVoT(object@sims[[i]]$population), c(1,2), sum)/apply(getProbability(object@sims[[i]]$population), c(1,2), sum)
              gc <- -getCost(object@sims[[i]]$city)-t(getCost(object@sims[[i]]$city))+vottMean*(getTime(object@sims[[i]]$city)+t(getTime(object@sims[[i]]$city))) # vottMean is negative (as it should)
              Hi <- matrix(apply(getProbability(object@sims[[i]]$population), 1, sum), getNodeCount(object@sims[[i]]$city), getNodeCount(object@sims[[i]]$city))/getNumberOfClasses(object@sims[[i]]$population) # identical rows
              Wk <- t(matrix(apply(getProbability(object@sims[[i]]$population), 2, sum), getNodeCount(object@sims[[i]]$city), getNodeCount(object@sims[[i]]$city)))/getNumberOfClasses(object@sims[[i]]$population) # Number of workers in zone k as matrix with identical cols
              rowsum <- matrix(apply(Wk*exp(sigma*gc), 1, sum), getNodeCount(object@sims[[i]]$city), getNodeCount(object@sims[[i]]$city)) # denominator 
              od <- (Hi*Wk*exp(sigma*gc))/rowsum
              time.avg <- sum(od*(getTime(object@sims[[i]]$city)+t(getTime(object@sims[[i]]$city))))
              res[ , i] <- time.avg
            }
            rownames(res) <- "Mean travel time"
            return(res)
          }
)

setGeneric("getIneq", function(object, tau) {standardGeneric("getIneq")})
setMethod("getIneq", 
          signature = "Economy",
          definition = function (object, tau) {
            res <- matrix(nrow = 1, ncol = length(seq_along(object@sims)))
            for (i in seq_along(object@sims)) {
              wage.x <- array(rep(t(getWageRate(object@sims[[i]]$population)), each = getNodeCount(object@sims[[i]]$city)), 
                              dim = c(getNodeCount(object@sims[[i]]$city), getNodeCount(object@sims[[i]]$city), getNumberOfClasses(object@sims[[i]]$population)))
              gini.x <- ineq(apply((1-tau)*getProbability(object@sims[[i]]$population)*wage.x*getArgMax(object@sims[[i]]$population)[ , , , 1], 3, sum))
              res[ , i] <- gini.x
            }
            rownames(res) <- "Gini"
            return(res)
          }
)

setGeneric("getElasticityOutputAccessibility", function(object, sigma) {standardGeneric("getElasticityOutputAccessibility")})
setMethod("getElasticityOutputAccessibility", 
          signature = "Economy",
          definition = function (object, sigma) {
            res <- matrix(nrow = 1, ncol = length(seq_along(object@sims)))
            wagerate.x <- array(rep(t(getWageRate(object@sims[[1]]$population)), each = getNodeCount(object@sims[[1]]$city)), dim = c(getNodeCount(object@sims[[1]]$city), getNodeCount(object@sims[[1]]$city), getNumberOfClasses(object@sims[[1]]$population)))
            votMean.x <- apply(getProbability(object@sims[[1]]$population)*getVoT(object@sims[[1]]$population), c(1,2), sum)/apply(getProbability(object@sims[[1]]$population), c(1,2), sum)
            Hi.x <- matrix(apply(getProbability(object@sims[[1]]$population), 1, sum), getNodeCount(object@sims[[1]]$city), getNodeCount(object@sims[[1]]$city))/getNumberOfClasses(object@sims[[1]]$population) # Number of residents in zone i as matrix with identical rows
            Wk.x <- t(matrix(apply(getProbability(object@sims[[1]]$population), 2, sum), getNodeCount(object@sims[[1]]$city), getNodeCount(object@sims[[1]]$city)))/getNumberOfClasses(object@sims[[1]]$population) # Number of workers in zone k as matrix with identical cols
            gc.x <- -getCost(object@sims[[1]]$city)-t(getCost(object@sims[[1]]$city))+votMean.x*(getTime(object@sims[[1]]$city)+t(getTime(object@sims[[1]]$city)))
            rowsum.x <- matrix(log(apply(Wk.x*exp(sigma*gc.x), 1, sum)), getNodeCount(object@sims[[1]]$city), getNodeCount(object@sims[[1]]$city))
            accessibility.x <- sum((1/sigma)*Hi.x*rowsum.x)
            output.x <- sum(getArgMax(object@sims[[1]]$population)[ , , , 1]*wagerate.x*getProbability(object@sims[[1]]$population))
            for (i in seq_along(object@sims)) {
              wagerate.y <- array(rep(t(getWageRate(object@sims[[i]]$population)), each = getNodeCount(object@sims[[i]]$city)), dim = c(getNodeCount(object@sims[[i]]$city), getNodeCount(object@sims[[i]]$city), getNumberOfClasses(object@sims[[i]]$population)))
              votMean.y <- apply(getProbability(object@sims[[i]]$population)*getVoT(object@sims[[i]]$population), c(1,2), sum)/apply(getProbability(object@sims[[i]]$population), c(1,2), sum)
              Hi.y <- matrix(apply(getProbability(object@sims[[i]]$population), 1, sum), getNodeCount(object@sims[[i]]$city), getNodeCount(object@sims[[i]]$city))/getNumberOfClasses(object@sims[[i]]$population) # Number of residents in zone i as matrix with identical rows
              Wk.y <- t(matrix(apply(getProbability(object@sims[[i]]$population), 2, sum), getNodeCount(object@sims[[i]]$city), getNodeCount(object@sims[[i]]$city)))/getNumberOfClasses(object@sims[[i]]$population) # Number of workers in zone k as matrix with identical cols
              gc.y <- -getCost(object@sims[[i]]$city)-t(getCost(object@sims[[i]]$city))+votMean.x*(getTime(object@sims[[i]]$city)+t(getTime(object@sims[[i]]$city))) # votMean.x (before the policy)
              rowsum.y <- matrix(log(apply(Wk.y*exp(sigma*gc.y), 1, sum)), getNodeCount(object@sims[[i]]$city), getNodeCount(object@sims[[i]]$city))
              accessibility.y <- sum((1/sigma)*Hi.y*rowsum.y)
              output.y <- sum(getArgMax(object@sims[[i]]$population)[ , , , 1]*wagerate.y*getProbability(object@sims[[i]]$population))
              res[ , i] <- ((output.y-output.x)/output.x)/((accessibility.y-accessibility.x)/abs(accessibility.x)) # Note the absolute value of accessibility
            }
            rownames(res) <- "Accessibility"
            return(res) 
          }
)

setGeneric("getVKTAccessibility", function(object) {standardGeneric("getVKTAccessibility")})
setMethod("getVKTAccessibility", 
          signature = "Economy",
          definition = function (object) {
            res <- matrix(nrow = 2, ncol = length(seq_along(object@sims)))
            for (i in seq_along(object@sims)) {
              log.vkt.ratio <- log(sum(odDemand(getProbability(object@sims[[i]]$population))*getDistance(object@sims[[i]]$city))/sum(odDemand(getProbability(object@sims[[1]]$population))*getDistance(object@sims[[1]]$city)))
              log.time.ratio <- log({mean(getTime(object@sims[[i]]$city))}/{mean(getTime(object@sims[[1]]$city))})
              log.cost.ratio <- log({mean(getCost(object@sims[[i]]$city))}/{mean(getCost(object@sims[[1]]$city))})
              res[1 , i] <- log.vkt.ratio/log.time.ratio
              res[2 , i] <- log.vkt.ratio/log.cost.ratio
            }
            rownames(res) <- c("Travel time", "Travel cost")
            return(res) 
          }
)

setGeneric("getDataFrame", function(object, ...) {standardGeneric("getDataFrame")})
setMethod("getDataFrame", 
          signature = "Economy",
          definition = function (object, id, type = c("city", "population", "path"), ...) {
            # id simulation id
            type <- match.arg(type)
            cityDataFrame <- function(x, id) {
              # x economy
              wagerate <- array(rep(t(getWageRate(x@sims[[id]]$population)), each = getNodeCount(x@sims[[id]]$city)), dim = c(getNodeCount(x@sims[[id]]$city), getNodeCount(x@sims[[id]]$city), getNumberOfClasses(x@sims[[id]]$population)))
              demand <- mapply(function(i) {getProbability(x@sims[[id]]$population)[ , , i]*getArgMax(x@sims[[id]]$population)[ , , i, 4]}, 1:getNumberOfClasses(x@sims[[id]]$population), SIMPLIFY = "array")
              df <- data.frame(
                Node = 1:getNodeCount(x@sims[[id]]$city),
                Supply = getArea(x@sims[[id]]$city),
                Demand = apply(demand, 1, sum, na.rm = TRUE),
                Price = getVertexPrice(x@sims[[id]]$city),
                WorkerShare = apply(getProbability(x@sims[[id]]$population), 2, sum, na.rm = TRUE)/getNumberOfClasses(x@sims[[id]]$population),
                ResidentShare = apply(getProbability(x@sims[[id]]$population), 1, sum, na.rm = TRUE)/getNumberOfClasses(x@sims[[id]]$population),
                "Residential Density" = apply(getProbability(x@sims[[id]]$population), 1, sum, na.rm = TRUE)/getArea(x@sims[[id]]$city),
                Output = apply(getProbability(x@sims[[id]]$population)*wagerate*getArgMax(x@sims[[id]]$population)[ , , , 1], 2, sum, na.rm = TRUE)
              )
              return(df)
            }
            populationDataFrame <- function(x, id) {
              mergeList <- function(x, y){
                df <- merge(x, y, by = intersect(names(x), names(y)))
                return(df)
              }
              df.wmaxj <- array2df(array(rep(apply(getUnderlyingWageRate(x@sims[[id]]$population), 2, max), each = getNodeCount(x@sims[[id]]$city)),
                                      dim = c(getNodeCount(x@sims[[id]]$city), getNodeCount(x@sims[[id]]$city), getNumberOfClasses(x@sims[[id]]$population))), label.x = "Max. Under. Wage Rate")
              df.wagerate.u <- array2df(array(rep(t(getUnderlyingWageRate(x@sims[[id]]$population)), each = getNodeCount(x@sims[[id]]$city)), 
                                              dim = c(getNodeCount(x@sims[[id]]$city), getNodeCount(x@sims[[id]]$city), getNumberOfClasses(x@sims[[id]]$population))), label.x = "Under. Wage Rate")
              df.wagerate <- array2df(array(rep(t(getWageRate(x@sims[[id]]$population)), each = getNodeCount(x@sims[[id]]$city)), 
                                            dim = c(getNodeCount(x@sims[[id]]$city), getNodeCount(x@sims[[id]]$city), getNumberOfClasses(x@sims[[id]]$population))), label.x = "Wage Rate")
              df.vot <- array2df(getVoT(x@sims[[id]]$population), label.x = "VoTT")
              df.prob <- array2df(getProbability(x@sims[[id]]$population), label.x = "Pr")
              df.vou <- array2df(getVoU(x@sims[[id]]$population), label.x = "VoU")
              df.u <- array2df(getUtility(x@sims[[id]]$population), label.x = "Ind utility")
              df.x1 <- array2df(getArgMax(x@sims[[id]]$population)[ , , , 1], label.x = "Work hours")
              df.x2 <- array2df(getArgMax(x@sims[[id]]$population)[ , , , 2], label.x = "Consumption")
              df.x3 <- array2df(getArgMax(x@sims[[id]]$population)[ , , , 3], label.x = "Leisure")
              df.x4 <- array2df(getArgMax(x@sims[[id]]$population)[ , , , 4], label.x = "Land use")
              df.x5 <- array2df(getArgMax(x@sims[[id]]$population)[ , , , 5], label.x = "Travel time")
              df <- Reduce(mergeList, list(df.prob, df.wagerate.u, df.wagerate, df.wmaxj, df.vot, df.vou, df.u, df.x1, df.x2, df.x3, df.x4, df.x5))
              df <- df[ , c("d3", "d1", "d2", "Pr", "Under. Wage Rate", "Wage Rate", "Max. Under. Wage Rate", "VoTT", "VoU", "Ind utility", "Work hours", "Consumption", "Leisure", "Land use", "Travel time")]
              df <- df[order(df$d3, df$d1, df$d2), ]
              names(df)[1:3] <- c("n", "i", "j")
              row.names(df) <- NULL
              return(df)
            }
            pathDataFrame <- function(x, id) {
              # x city
              # returns a path dataframe
              v <- length(V(getGraph(x@sims[[id]]$city)))
              e <- length(E(getGraph(x@sims[[id]]$city)))
              m <- matrix(list(), v^2, 1) # column path matrix
              cost <- matrix(list(), v^2, 1)
              totcost <- matrix(0, v^2, 1)
              time <- matrix(list(), v^2, 1)
              tottime <- matrix(0, v^2, 1)
              edgepathlist <- vec_shortest_paths(1:v, x@sims[[id]]$city) # Store edge ids of the paths
              pathids <- matrix(NA, v^2, 1)
              for (i in 1:v) {
                for (j in 1:v) {
                  m[[as.path.id(x@sims[[id]]$city, from = i, to = j), 1]] <- list(as_ids(edgepathlist[[i]][[j]]))
                  pathids[as.path.id(x@sims[[id]]$city, from = i, to = j), 1] <- as.path.id(x@sims[[id]]$city, from = i, to = j)
                  cost[[as.path.id(x@sims[[id]]$city, from = i, to = j), 1]] <- as.list(
                    (getEdgePath(x@sims[[id]]$city)[, as.path.id(x@sims[[id]]$city, i, j)]*get.edge.attribute(getGraph(x@sims[[id]]$city), "cost"))[getEdgePath(x@sims[[id]]$city)[, as.path.id(x@sims[[id]]$city, i, j)]*get.edge.attribute(getGraph(x@sims[[id]]$city), "cost")>0])
                  totcost[as.path.id(x@sims[[id]]$city, i, j), 1] <- getEdgePath(x@sims[[id]]$city)[, as.path.id(x@sims[[id]]$city, i, j)]%*%get.edge.attribute(getGraph(x@sims[[id]]$city), "cost")+getCost(x@sims[[id]]$city)[i, i]+getCost(x@sims[[id]]$city)[j, j]
                  time[[as.path.id(x@sims[[id]]$city, from = i, to = j), 1]] <- as.list(
                    (getEdgePath(x@sims[[id]]$city)[, as.path.id(x@sims[[id]]$city, i, j)]*get.edge.attribute(getGraph(x@sims[[id]]$city), "time"))[getEdgePath(x@sims[[id]]$city)[, as.path.id(x@sims[[id]]$city, i, j)]*get.edge.attribute(getGraph(x@sims[[id]]$city), "time")>0])
                  tottime[as.path.id(x@sims[[id]]$city, i, j), 1] <- getEdgePath(x@sims[[id]]$city)[, as.path.id(x@sims[[id]]$city, i, j)]%*%get.edge.attribute(getGraph(x@sims[[id]]$city), "time")+getTime(x@sims[[id]]$city)[i, i]+getTime(x@sims[[id]]$city)[j, j]
                }
              }
              df <- data.frame(origin = rep(1:v, each = v), 
                               destination = rep(1:v, times = v), 
                               "path id" = pathids, 
                               link = m, 
                               cost = cost, 
                               "total cost" = totcost,
                               time = time,
                               "total time" = tottime)
              return(df)
            }
            switch(type,
                   city = cityDataFrame(object, id),
                   population = populationDataFrame(object, id),
                   path = pathDataFrame(object, id)
                   )
          }
)

setGeneric("getAvarages", function(object) {standardGeneric("getAvarages")})
setMethod("getAvarages", 
          signature = "Economy",
          definition = function (object) {
            res <- matrix(nrow = 6, ncol = length(seq_along(object@sims)))
            for (i in seq_along(object@sims)) {
              wagerate <- array(rep(t(getWageRate(object@sims[[i]]$population)), each = getNodeCount(object@sims[[i]]$city)), 
                                dim = c(getNodeCount(object@sims[[i]]$city), getNodeCount(object@sims[[i]]$city), getNumberOfClasses(object@sims[[i]]$population)))
              wagerate <- sum(apply(wagerate*getProbability(object@sims[[i]]$population), 3, sum))/getNumberOfClasses(object@sims[[i]]$population)
              vott <- mean(apply(getProbability(object@sims[[i]]$population)*getVoT(object@sims[[i]]$population), c(1,2), sum)/apply(getProbability(object@sims[[i]]$population), c(1,2), sum))
              workingtime <- sum(apply(getArgMax(object@sims[[i]]$population)[ , , , 1]*getProbability(object@sims[[i]]$population), 3, sum))/getNumberOfClasses(object@sims[[i]]$population)
              vkt <- sum(odDemand(getProbability(object@sims[[i]]$population))*(getDistance(object@sims[[i]]$city)+t(getDistance(object@sims[[i]]$city))))/getNumberOfClasses(object@sims[[i]]$population)
              traveltime <- sum(odDemand(getProbability(object@sims[[i]]$population))*(getTime(object@sims[[i]]$city)+t(getTime(object@sims[[i]]$city))))/getNumberOfClasses(object@sims[[i]]$population)
              travelcost <- sum(odDemand(getProbability(object@sims[[i]]$population))*(getCost(object@sims[[i]]$city)+t(getCost(object@sims[[i]]$city))))/getNumberOfClasses(object@sims[[i]]$population)
              res[ , i] <- c(workingtime, wagerate, vkt, traveltime, travelcost, vott) 
            }
            rownames(res) <- c("Working time", "Wage Rate", "VKT", "Travel time", "Travel cost", "Marginal value of Tr. Time")
            return(res)
          }
)

setMethod("initialize",
          signature = "Economy",
          function(.Object, x, y, network.weights, utility = list(), probability = list(), spillover = list(), ...) {
            .Object@city <- x
            .Object@population <- y
            .Object@network.weights <- network.weights
            .Object@utility <- utility
            .Object@probability <- probability
            .Object@spillover <- spillover
            .Object@sims <- vector("list", length = nrow(network.weights))
            .Object@in.equilibrium <- logical(nrow(network.weights))
            return(.Object)
          }
)

setMethod("show",
          signature = "Economy",
          function(object) {
            cat("An object of class Economy which contains...", "\n")
            show(object@city)
            cat("\n")
            show(object@population)
            cat("\n")
            cat("Dimension of network weights array: ", dim(object@network.weights), "\n")
            cat("User-supplied utility functions? ") 
            if (!(length(object@utility) == 0)) cat("TRUE \n") else cat("FALSE \n")
            cat("User-supplied probability functions? ") 
            if (!(length(object@probability) == 0)) cat("TRUE \n") else cat("FALSE \n")
            cat("User-supplied spillover function? ") 
            if (!(length(object@spillover) == 0)) cat("TRUE \n") else cat("FALSE \n")
            cat("In equilibrium? ") 
            cat(object@in.equilibrium)
          }   
)

setMethod("plot",
          signature = c(x = "Economy", y = "missing"),
          function (x, y, type = c("default", "argmax", "wagerate", "income", "quality", "vkt"), index = NULL, ...) {
            type <- match.arg(type)
            weighted.densities <- function(x, type, weight = expression(), index = NULL, main = " ", xlab = NULL, ...) {
              xlim <- matrix(nrow = 2, ncol = length(seq_along(x@sims)))
              ylim <- matrix(nrow = 2, ncol = length(seq_along(x@sims)))
              for (i in seq_along(x@sims)) {
                if (type == "argmax") {den <- density(getArgMax(x@sims[[i]]$population)[ , , , index], weights = eval(weight))}
                if (type == "wagerate") {den <- density(getWageRate(x@sims[[i]]$population), weights = eval(weight))}
                if (type == "income") {den <- density(getArgMax(x@sims[[i]]$population)[ , , , 1]*array(rep(t(getWageRate(x@sims[[i]]$population)), each = getNodeCount(x@sims[[i]]$city)), 
                                                                                                        dim = c(getNodeCount(x@sims[[i]]$city), getNodeCount(x@sims[[i]]$city), getNumberOfClasses(x@sims[[i]]$population))))}
                if (type == "vkt") {den <- density(array(getDistance(x@sims[[i]]$city)+t(getDistance(x@sims[[i]]$city)), dim = c(nrow(getDistance(x@sims[[i]]$city)), 
                                                                                                                                 ncol(getDistance(x@sims[[i]]$city)), 
                                                                                                                                 getNumberOfClasses(x@sims[[i]]$population))))}
                if (type == "wagerate") {
                  den2 <- density(getUnderlyingWageRate(x@sims[[i]]$population))
                  xlim[ , i] <- range(den$x, den2$x)
                  ylim[ , i] <- range(den$y, den2$y)
                } else {
                  xlim[ , i] <- range(den$x)
                  ylim[ , i] <- range(den$y)
                }
              }
              xlim <- range(xlim)
              ylim <- range(ylim)
              plot(0,
                   type = "n",
                   main = main,
                   xlim = xlim,
                   ylim = ylim,
                   xlab = xlab,
                   ylab = "Density",
                   ...)
              if (type == "argmax") {
                for (i in seq_along(x@sims)) {
                  lines(density(getArgMax(x@sims[[i]]$population)[ , , , index], 
                                weights = eval(weight)),
                        col = rainbow(length(x@sims))[i],
                        main = NULL,
                        xlab = xlab
                  )
                }
              } else if (type == "wagerate") {
                for (i in seq_along(x@sims)) {
                  lines(density(getWageRate(x@sims[[i]]$population), 
                                weights = eval(weight)),
                        col = rainbow(length(x@sims))[i],
                        main = NULL,
                        xlab = xlab
                  )
                  lines(density(getUnderlyingWageRate(x@sims[[i]]$population), 
                                weights = NULL),
                                #weights = eval(weight)),
                        col = "black",
                        main = NULL,
                        xlab = xlab
                  )
                  # lines(density(ifelse(getWageRate(x@sims[[i]]$population)>getWageRate(x@sims[[1]]$population),
                  #                      getWageRate(x@sims[[i]]$population),
                  #                      getWageRate(x@sims[[1]]$population)
                  # ), 
                  # weights = eval(weight)),
                  # col = heat.colors(length(x@sims))[i],
                  # main = NULL,
                  # xlab = xlab
                  # )
                  # legend("top", 
                  #        legend = as.character(1:length(x@sims)), 
                  #        lty = c(1,1), col = heat.colors(length(x@sims)))
                }
              } else if (type == "income") {
                for (i in seq_along(x@sims)) {
                  lines(density(getArgMax(x@sims[[i]]$population)[ , , , 1]*array(rep(t(getWageRate(x@sims[[i]]$population)), each = getNodeCount(x@sims[[i]]$city)), 
                                                                                  dim = c(getNodeCount(x@sims[[i]]$city), getNodeCount(x@sims[[i]]$city), getNumberOfClasses(x@sims[[i]]$population))), 
                                weights = eval(weight)),
                        col = rainbow(length(x@sims))[i],
                        main = NULL,
                        xlab = xlab
                  )
                } 
              } else if (type == "vkt") {
                for (i in seq_along(x@sims)) {
                  lines(density(array(getDistance(x@sims[[i]]$city)+t(getDistance(x@sims[[i]]$city)), dim = c(nrow(getDistance(x@sims[[i]]$city)), 
                                                                                                              ncol(getDistance(x@sims[[i]]$city)), 
                                                                                                              getNumberOfClasses(x@sims[[i]]$population))), 
                                weights = eval(weight)),
                        col = rainbow(length(x@sims))[i],
                        main = NULL,
                        xlab = xlab
                  )
                }
              }
              #legend("topright", 
               #      legend = as.character(1:length(x@sims)), 
                #     lty = c(1, 1), col = rainbow(length(x@sims)),
                 #    cex = 0.5, seg.len = 0.5, ncol = (length(x@sims)%/%30+1))
            }
            switch(type,
                   default = plot(x@city, ...),
                   argmax = weighted.densities(x, type, weight = expression(getProbability(x@sims[[i]]$population)/getNumberOfClasses(x@sims[[i]]$population)), index, ...),
                   wagerate = weighted.densities(x, type, weight = expression(t(apply(getProbability(x@sims[[i]]$population), c(2, 3), sum))/getNumberOfClasses(x@sims[[i]]$population)), ...),
                   income = weighted.densities(x, type, weight = expression(getProbability(x@sims[[i]]$population)/getNumberOfClasses(x@sims[[i]]$population)), ...),
                   vkt = weighted.densities(x, type, weight = expression(getProbability(x@sims[[i]]$population)/getNumberOfClasses(x@sims[[i]]$population)), ...)
            )
          }
)

setMethod("persp3D",
          signature = c(x = "Economy"),
          function(x, ...) {
            args <- list(...)
            type <- args$type
            sid <- args$sid
            if (is.null(type)) type <- "landprice"
            if (is.null(sid)) sid <- 1
            landprice <- function(x, sid, ...) {
              persp3D(x@sims[[sid]]$city,
                      getVertexPrice(x@sims[[sid]]$city),
                      ...)
            }
            residence <- function(x, sid, ...) {
              persp3D(x@sims[[sid]]$city,
                      apply(getProbability(x@sims[[sid]]$population), 1, sum, na.rm = TRUE)/getArea(x@sims[[sid]]$city),
                      ...)
            }
            work <- function(x, sid, ...) {
              persp3D(x@sims[[sid]]$city,
                      apply(getProbability(x@sims[[sid]]$population), 2, sum, na.rm = TRUE)/getArea(x@sims[[sid]]$city),
                      ...)
            }
            income <- function(x, sid, ...) {
              # Destination income
              persp3D(x@sims[[sid]]$city,
                      apply(getProbability(x@sims[[sid]]$population)*getArgMax(x@sims[[sid]]$population)[ , , , 1]*array(rep(t(getWageRate(x@sims[[sid]]$population)), each = getNodeCount(x@sims[[sid]]$city)), 
                                                                                                                       dim = c(getNodeCount(x@sims[[sid]]$city), getNodeCount(x@sims[[sid]]$city), getNumberOfClasses(x@sims[[sid]]$population))), 
                            2, sum, na.rm = TRUE)/apply(getProbability(x@sims[[sid]]$population), 2, sum, na.rm = TRUE),
                      ...)
            }
            switch(type,
                   landprice = landprice(x, sid, ...),
                   residence = residence(x, sid, ...),
                   work = work(x, sid, ...),
                   income = income(x, sid, ...)
                   )
          }
)