indirectUtilityClosure <- function(parameter) {
  # parameter[1] = consumption beta 
  # parameter[2] = leisure beta
  # parameter[3] = land-use beta
  # paramater[4] = tau (taxation rate)
  # parameter[5] = y (exogenous income)
  # parameter[6] = Time (16 or 24)
  # parameter[7] = sigma (variance parameter)
  # If the function returns NaNs check if cost c and time t are of appropriate sizes
  function(p, w, c, t, comfort, op, dp) {
    c <- c+mapply(function(i) {t(c[ , , i])}, 1:dim(c)[3], SIMPLIFY = "array")
    t <- t+mapply(function(i) {t(t[ , , i])}, 1:dim(t)[3], SIMPLIFY = "array")
    v <- -log(((1-parameter[4])*w)^parameter[2]*{p^parameter[3]})+log((1-parameter[4])*w*(parameter[6]-t)+parameter[5]-c)-comfort*t+op+dp+log({parameter[1]^parameter[1]}*{parameter[2]^parameter[2]}*{parameter[3]^parameter[3]})
    return(v)
  }
}

argmaxUtilityClosure <- function(parameter) {
  function(p, w, c, t) {
    c <- c+mapply(function(i) {t(c[ , , i])}, 1:dim(c)[3], SIMPLIFY = "array")
    t <- t+mapply(function(i) {t(t[ , , i])}, 1:dim(t)[3], SIMPLIFY = "array")
    x <- array(c(parameter[6]-t-{parameter[2]/{(1-parameter[4])*w}}*{(1-parameter[4])*w*(parameter[6]-t)+parameter[5]-c}, # Optimal working hours
                 parameter[1]*{(1-parameter[4])*w*{parameter[6]-t}+parameter[5]-c}, # Optimal consumption
                 {parameter[2]/{(1-parameter[4])*w}}*{(1-parameter[4])*w*{parameter[6]-t}+parameter[5]-c}, # Optimal leisure
                 {parameter[3]/p}*{(1-parameter[4])*w*{parameter[6]-t}+parameter[5]-c}, # Optimal land-use
                 t), # Optimal travel time
               dim = c(dim(t), 5))
    return(x)
  }
}

marginalEffectsClosure <- function(parameter) {
  function(x, p, w, c, t, comfort) {
    # x argmax
    # note that marginal utility of income is minus marginal utility of travel cost
    c <- c+mapply(function(i) {t(c[ , , i])}, 1:dim(c)[3], SIMPLIFY = "array")
    t <- t+mapply(function(i) {t(t[ , , i])}, 1:dim(t)[3], SIMPLIFY = "array")
    dvdx <- array(c(parameter[1]/x[ , , , 2], # marginal utility of income
                    parameter[2]/x[ , , , 3], # marginal utility of time
                    -parameter[2]/x[ , , , 3]-comfort, # marginal utility of travel time
                    -parameter[3]/p, # marginal utility of land price
                    {(1-parameter[2])*(parameter[6]-t)*(1-parameter[4])*w-parameter[2]*parameter[5]+parameter[2]*c}/{w*((parameter[6]-t)*(1-parameter[4])*w+parameter[5]-c)}), # marginal utility of wage rate
                  dim = dim(x))
    return(dvdx)
  }
}

expectedUtilityClosure <- function(parameter) {
  function(v) {
    maxu <- apply(v, 3, max)
    maxu.arr <- array(rep(maxu, each = nrow(v)^2), dim = dim(v))
    logsum <- maxu+parameter[7]*log(apply(exp((v-maxu.arr)/parameter[7]), 3, sum))
    return(logsum)
  }
}

utilityWrapper <- function(parameter) {
  # This function returns a list of functions
  if(parameter[1]+parameter[2]+parameter[3] != 1)
    stop("The parameters of the utility function do not sum to 1.")
  if(!all(c(parameter[1], parameter[2], parameter[3]) > 0))
    stop("The parameters of the utility functions must be positive.")
  if(!(parameter[4] < 1 && parameter[4] >= 0))
    stop("The tax parameter is not between 0 and 1.")
  return(
    list(indirectUtility = indirectUtilityClosure(parameter),
         argmaxUtility = argmaxUtilityClosure(parameter),
         marginalEffects = marginalEffectsClosure(parameter),
         expectedUtility = expectedUtilityClosure(parameter)
    )
  )
}