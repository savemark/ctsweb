indirectUtilityClosure <- function(parameter) {
  # parameter[1] = consumption beta 
  # parameter[2] = leisure beta
  # parameter[3] = land-use beta
  # parameter[4] = travel time beta
  # paramater[5] = tau (1-taxation rate)
  # parameter[6] = y (exogenous income)
  # parameter[7] = Time (16 or 24)
  # parameter[8] = sigma (variance parameter)
  # If the function returns NaNs check if cost c and time t are of appropriate sizes
  function(p, w, c, t, op, dp) {
    c <- c+mapply(function(i) {t(c[ , , i])}, 1:dim(c)[3], SIMPLIFY = "array")
    t <- t+mapply(function(i) {t(t[ , , i])}, 1:dim(t)[3], SIMPLIFY = "array")
    v <- -log((parameter[5]*w)^parameter[2]*{p^parameter[3]})+log(parameter[5]*w*(parameter[7]-t)+parameter[6]-c)-parameter[4]*t+op+dp+log({parameter[1]^parameter[1]}*{parameter[2]^parameter[2]}*{parameter[3]^parameter[3]})
    return(v)
  }
}

argmaxUtilityClosure <- function(parameter) {
  function(p, w, c, t) {
    c <- c+mapply(function(i) {t(c[ , , i])}, 1:dim(c)[3], SIMPLIFY = "array")
    t <- t+mapply(function(i) {t(t[ , , i])}, 1:dim(t)[3], SIMPLIFY = "array")
    x <- array(c(parameter[7]-t-{parameter[2]/{parameter[5]*w}}*{parameter[5]*w*(parameter[7]-t)+parameter[6]-c}, # Optimal working hours
                 parameter[1]*{parameter[5]*w*{parameter[7]-t}+parameter[6]-c}, # Optimal consumption
                 {parameter[2]/{parameter[5]*w}}*{parameter[5]*w*{parameter[7]-t}+parameter[6]-c}, # Optimal leisure
                 {parameter[3]/p}*{parameter[5]*w*{parameter[7]-t}+parameter[6]-c}, # Optimal land-use
                 t), # Optimal travel time
               dim = c(dim(t), 5))
    return(x)
  }
}

marginalEffectsClosure <- function(parameter) {
  function(x, p, w, c, t) {
    # x argmax
    # note that marginal utility of income is minus marginal utility of travel cost
    c <- c+mapply(function(i) {t(c[ , , i])}, 1:dim(c)[3], SIMPLIFY = "array")
    t <- t+mapply(function(i) {t(t[ , , i])}, 1:dim(t)[3], SIMPLIFY = "array")
    dvdx <- array(c(parameter[1]/x[ , , , 2], # marginal utility of income
                    parameter[2]/x[ , , , 3], # marginal utility of time
                    -parameter[2]/x[ , , , 3]-parameter[4], # marginal utility of travel time
                    -parameter[3]/p, # marginal utility of land price
                    {(1-parameter[2])*(parameter[7]-t)*parameter[5]*w-parameter[2]*parameter[6]+parameter[2]*c}/{w*((parameter[7]-t)*parameter[5]*w+parameter[6]-c)}), # marginal utility of wage rate
                  dim = dim(x))
    return(dvdx)
  }
}

expectedUtilityClosure <- function(parameter) {
  function(v) {
    maxu <- apply(v, 3, max)
    maxu.arr <- array(rep(maxu, each = nrow(v)^2), dim = dim(v))
    logsum <- maxu+parameter[8]*log(apply(exp((v-maxu.arr)/parameter[8]), 3, sum))
    return(logsum)
  }
}

utilityWrapper <- function(parameter) {
  # This function returns a list of functions
  if(parameter[1]+parameter[2]+parameter[3] != 1)
    stop("The parameters of the utility function does not sum to 1.")
  if(!all(c(parameter[1], parameter[2], parameter[3], parameter[4]) > 0))
    stop("The parameters of the utility functions must be positive.")
  if(!(parameter[5] <= 1 && parameter[5] > 0))
    stop("The tax parameter is not between 0 and 1.")
  return(
    list(indirectUtility = indirectUtilityClosure(parameter),
         argmaxUtility = argmaxUtilityClosure(parameter),
         marginalEffects = marginalEffectsClosure(parameter),
         expectedUtility = expectedUtilityClosure(parameter)
    )
  )
}