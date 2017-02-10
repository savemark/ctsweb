economy <- setClass("Economy",
                    slots = c(
                      city = "City",
                      population = "Population",
                      utility = "list",
                      probability = "list",
                      spillover = "list",
                      sims = "list",
                      network.weights = "array"
                    )
)

setGeneric("setNetworkWeights<-", function(object, value) {standardGeneric("setNetworkWeights<-")})
setReplaceMethod("setNetworkWeights",
                 signature = "Economy",
                 definition = function(object, value) {
                   object@network.weights <- value
                   return(object)
                 }
)

setGeneric("simulate", function(object, guess) {standardGeneric("simulate")})
setMethod("simulate",
          signature = "Economy",
          definition = function(object, guess) {
            sim <- simulation(guess = rep(guess, getNodeCount(object@city)), 
                              city = object@city,
                              population = object@population,
                              utility = object@utility,
                              probability = object@probability,
                              spillover = object@spillover)
            object@city <- sim$city
            object@population <- sim$population
            if (sim$solution$convergence == 0) {object@in.equilibrium <- TRUE} else {object@in.equilibrium <- FALSE}
            return(object)
          }
)

setMethod("initialize",
          signature = "Economy",
          function(.Object, x, y, utility = list(), probability = list(), spillover = list(), ...) {
            .Object@city <- x
            .Object@population <- y
            .Object@utility <- utility
            .Object@probability <- probability
            .Object@spillover <- spillover
            .Object@in.equilibrium <- FALSE
            return(.Object)
          }
)

setMethod("show",
          signature = "Economy",
          function(object) {
            cat("An object of class Economy", "\n")
            cat("\n", "Contains...", "\n", "\n")
            show(object@city)
            cat("\n")
            show(object@population)
            cat("\n")
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
          function (x, y, ...) {
            if (x@in.equilibrium) {
              plot(x = x@city, y = x@population, ...) 
            } else { 
              plot(x@city, ...)
            }
          }
)

setMethod("as.data.frame",
          signature(x = "Economy"),
          function (x, row.names = NULL, optional = FALSE, ...) 
          {
            stop("need a definition for the method here")
          }
)