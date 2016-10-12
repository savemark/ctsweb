population <- setClass("Population",
                       slots = c(n = "numeric",
                                 nodes = "numeric",
                                 wagerate = "matrix",
                                 underlying.wagerate = "matrix",
                                 op = "matrix",
                                 dp = "matrix",
                                 utility = "array",
                                 argmax = "array",
                                 lambda = "array",
                                 probability = "array",
                                 od = "matrix",
                                 sectorid = "numeric"),
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

# Getters -----------------------------------------------------------------

setGeneric("getSize", function(object) {standardGeneric("getSize")})
setMethod("getSize",
          signature = "Population",
          definition = function(object) {
            return(object@n)
          }
)

setGeneric("getNodes", function(object) {standardGeneric("getNodes")})
setMethod("getNodes",
          signature = "Population",
          definition = function(object) {
            return(object@nodes)
          }
)

setGeneric("getWageRate", function(object) {standardGeneric("getWageRate")})
setMethod("getWageRate",
          signature = "Population",
          definition = function(object) {
            return(object@wagerate)
          }
)

setGeneric("getUnderlyingWageRate", function(object) {standardGeneric("getUnderlyingWageRate")})
setMethod("getUnderlyingWageRate",
          signature = "Population",
          definition = function(object) {
            return(object@underlying.wagerate)
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

setGeneric("getSectorID", function(object) {standardGeneric("getSectorID")})
setMethod("getSectorID",
          signature = "Population",
          definition = function(object) {
            return(object@sectorid)
          }
)

# Setters -----------------------------------------------------------------

setGeneric("setSize<-", function(object, value) {standardGeneric("setSize<-")})
setReplaceMethod("setSize", 
                 signature = "Population",
                 definition = function(object, value) {
                   object@n <- value
                   return(object)
                 }
)

setGeneric("setNodes<-", function(object, value) {standardGeneric("setNodes<-")})
setReplaceMethod("setNodes", 
                 signature = "Population",
                 definition = function(object, value) {
                   object@nodes <- value
                   return(object)
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

setGeneric("setUnderlyingWageRate<-", function(object, value) {standardGeneric("setUnderlyingWageRate<-")})
setReplaceMethod("setUnderlyingWageRate", 
                 signature = "Population",
                 definition = function(object, value) {
                   object@underlying.wagerate <- value
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

setGeneric("setSectorID<-", function(object, value) {standardGeneric("setSectorID<-")})
setReplaceMethod("setSectorID", 
                 signature = "Population",
                 definition = function(object, value) {
                   object@sectorid <- value
                   return(object)
                 }
)

setMethod("initialize",
          signature = "Population",
          function(.Object, x, y, median = 336000, spread = 1.12, days = 228, hours = 8, omean = 0, osd = 0.25, dmean = 0, dsd = 0.25, ...) {
            setSize(.Object) <- x
            setNodes(.Object) <- y            
            setWageRate(.Object) <- matrix(exp(log(median)+sqrt(2*(log(spread)))*rnorm(y*x))/{hours*days}, x, y)
            setUnderlyingWageRate(.Object) <- getWageRate(.Object)
            setOriginPreference(.Object) <- matrix(rnorm(x*y, mean = omean, sd = osd), x, y)
            setDestinationPreference(.Object) <- matrix(rnorm(x*y, mean = dmean, sd = dsd), x, y)          
            setOriginDestinationMatrix(.Object) <- matrix(NA, y, y)
            setProbability(.Object) <- array(NA, dim = c(y, y, x))
            validObject(.Object)
            return(.Object)            
          }
)

setMethod("show",
          signature = "Population",
          function(object) {
            cat("\n", "An object of class", class(object), "\n")
            cat(" ", "\n",
                "Sector id: ", object@sectorid, "\n",
                "Number of classes/individuals: ", object@n, "\n",
                "Number of sources of income: ", object@nodes, "\n")
            cat(" ", "\n",
                "WAGE RATES AND PREFERENCES", "\n",
                "Count of NA(s): ", length(object@wagerate[is.na(object@wagerate)]), "\n",
                "Quantiles", "\n")
            tab <- cbind(as.matrix(quantile(object@wagerate, na.rm = TRUE)), 
                         as.matrix(quantile(object@op, na.rm = TRUE)), 
                         as.matrix(quantile(object@dp, na.rm = TRUE)))
            colnames(tab) <- c("Wage Rates", "Origin Pref.", "Destination Pref.")
            print(tab)
            cat("PROBABILITIES", "\n",
                "Count of NA(s): ", length(object@probability[is.na(object@probability)]), "\n")
          }
)