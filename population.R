population <- setClass("Population",
                       slots = c(n = "numeric",
                                 nodes = "numeric",
                                 wagerate = "matrix",
                                 op = "matrix",
                                 dp = "matrix",
                                 utility = "array",
                                 argmax = "array",
                                 lambda = "array",
                                 probability = "array",
                                 od = "matrix"),
                       validity = function(object) {
                         if (!object@n%%1 == 0)
                           return("N needs to be an integer")
                         if (!object@n > 1)
                           return("N must be greater than 1") # Since we are working with arrays of dimension >= 3. Could be fixed to include 1?
                         else {
                           return(TRUE)
                         }
                       }
)

setGeneric("getSize", function(object) {standardGeneric("getSize")})
setMethod("getSize",
          signature = "Population",
          definition = function(object) {
            return(object@n)
          }
)

setGeneric("getWageRate", function(object) {standardGeneric("getWageRate")})
setMethod("getWageRate",
          signature = "Population",
          definition = function(object) {
            return(object@wagerate)
          }
)

setGeneric("getOriginPreference", function(object) {standardGeneric("getOriginPreference")})
setMethod("getOriginPreference",
          signature = "Population",
          definition = function(object) {
            return(object@op)
          }
)

setGeneric("getDestinationPreference", function(object) {standardGeneric("getDestinationPreference")})
setMethod("getDestinationPreference",
          signature = "Population",
          definition = function(object) {
            return(object@dp)
          }
)

setGeneric("getUtility", function(object) {standardGeneric("getUtility")})
setMethod("getUtility",
          signature = "Population",
          definition = function(object) {
            return(object@utility)
          }
)

setGeneric("getArgMax", function(object) {standardGeneric("getArgMax")})
setMethod("getArgMax",
          signature = "Population",
          definition = function(object) {
            return(object@argmax)
          }
)

setGeneric("getVoT", function(object) {standardGeneric("getVoT")})
setMethod("getVoT",
          signature = "Population",
          definition = function(object) {
            return(object@lambda[ , , , 3]/object@lambda[ , , , 1])
          }
)

setGeneric("getVoU", function(object) {standardGeneric("getVoU")})
setMethod("getVoU",
          signature = "Population",
          definition = function(object) {
            return(object@utility/object@lambda[ , , , 1])
          }
)

setGeneric("getProbability", function(object) {standardGeneric("getProbability")})
setMethod("getProbability",
          signature = "Population",
          definition = function(object) {
            return(object@probability)
          }
)

setGeneric("getMarginalEffect", function(object) {standardGeneric("getMarginalEffect")})
setMethod("getMarginalEffect",
          signature = "Population",
          definition = function(object) {
            return(object@lambda)
          }
)

setGeneric("getOriginDestinationMatrix", function(object) {standardGeneric("getOriginDestinationMatrix")})
setMethod("getOriginDestinationMatrix",
          signature = "Population",
          definition = function(object) {
            return(object@od)
          }
)

setGeneric("setOriginDestinationMatrix<-", function(object, value) {standardGeneric("setOriginDestinationMatrix<-")})
setReplaceMethod("setOriginDestinationMatrix",
                 signature = "Population",
                 definition = function(object, value) {
                   object@od <- value
                   return(object)
                 }
)

setGeneric("setWageRate<-", function(object, value) {standardGeneric("setWageRate<-")})
setReplaceMethod("setWageRate", 
                 signature = "Population",
                 definition = function(object, value) {
                   object@wagerate <- value
                   return(object)
                 }
)

setGeneric("setOriginPreference<-", function(object, value) {standardGeneric("setOriginPreference<-")})
setReplaceMethod("setOriginPreference", 
                 signature = "Population",
                 definition = function(object, value) {
                   object@op <- value
                   return(object)
                 }
)

setGeneric("setDestinationPreference<-", function(object, value) {standardGeneric("setDestinationPreference<-")})
setReplaceMethod("setDestinationPreference", 
                 signature = "Population",
                 definition = function(object, value) {
                   object@dp <- value
                   return(object)
                 }
)

setGeneric("setUtility<-", function(object, value) {standardGeneric("setUtility<-")})
setReplaceMethod("setUtility", 
                 signature = "Population",
                 definition = function(object, value) {
                   object@utility <- value
                   return(object)
                 }
)


setGeneric("setProbability<-", function(object, value) {standardGeneric("setProbability<-")})
setReplaceMethod("setProbability", 
                 signature = "Population",
                 definition = function(object, value) {
                   object@probability <- value
                   return(object)
                 }
)

setGeneric("setArgMax<-", function(object, value) {standardGeneric("setArgMax<-")})
setReplaceMethod("setArgMax", 
                 signature = "Population",
                 definition = function(object, value) {
                   object@argmax <- value
                   return(object)
                 }
)

setGeneric("setMarginalEffect<-", function(object, value) {standardGeneric("setMarginalEffect<-")})
setReplaceMethod("setMarginalEffect", 
                 signature = "Population",
                 definition = function(object, value) {
                   object@lambda <- value
                   return(object)
                 }
)

setMethod("initialize",
          signature = "Population",
          function(.Object, x, y, median = 336000, spread = 1.12, days = 228, hours = 8, omean = 0, osd = 0.25, dmean = 0, dsd = 0.25, ...) {
            .Object@n <- x
            .Object@nodes <- y            
            mu <- log(median)
            sigma <- sqrt(2*(log(spread)))
            wagerate <- matrix(exp(mu+sigma*rnorm(y*x))/{hours*days}, x, y)
            setWageRate(.Object) <- wagerate
            op <- matrix(rnorm(x*y, mean = omean, sd = osd), x, y)
            dp <- matrix(rnorm(x*y, mean = dmean, sd = dsd), x, y)
            setOriginPreference(.Object) <- op
            setDestinationPreference(.Object) <- dp            
            .Object@od <- matrix(NA, y, y) # od matrix
            .Object@utility <- array(NA, dim = c(y, y, x, 6))
            .Object@probability <- array(NA, dim = c(y, y, x))
            validObject(.Object)
            return(.Object)            
          }
)

setMethod("show",
          signature = "Population",
          function(object) {
            cat("An object of class Population", "\n", 
                "Number of classes/individuals: ", object@n, "\n",
                "Number of sources of income: ", object@nodes, "\n", "\n",
                "Quantiles", "\n", sep = "")
            tab <- cbind(as.matrix(quantile(object@wagerate, na.rm = TRUE)), 
                         as.matrix(quantile(object@op, na.rm = TRUE)), 
                         as.matrix(quantile(object@dp, na.rm = TRUE)))
            colnames(tab) <- c("Wage Rates", "Origin Pref.", "Destination Pref.")
            print(tab)
            cat("\n",
                "WAGE RATES", "\n",
                "Count of NA(s): ", length(object@wagerate[is.na(object@wagerate)]), "\n",
                "PROBABILITIES", "\n",
                "Count of NA(s): ", length(object@probability[is.na(object@probability)]), sep = "")
          }
)