indirectUtilityClosure <- function(parameter) {
  # parameter[1] = consumption beta 
  # parameter[2] = leisure beta
  # parameter[3] = land-use beta
  # parameter[4] = travel time beta
  # paramater[5] = tau
  # parameter[6] = y (exogenous income)
  # parameter[7] = Time (16 or 24)
  function(p, w, c, t, op, dp) {
    v <- log((parameter[5]*w*(parameter[7]-t)+parameter[6]-c)/((parameter[5]*w)^parameter[2]*p^parameter[3]))-parameter[4]*t+op+dp+log({parameter[1]^parameter[1]}*{parameter[2]^parameter[2]}*{parameter[3]^parameter[3]})
    return(v)
  }
}

argmaxUtilityClosure <- function(parameter) {
  function(p, w, c, t) {
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
    dvdx <- array(c(parameter[1]/x[ , , , 2], # marginal utility of income
                    parameter[2]/x[ , , , 3], # marginal utility of leisure time
                    -parameter[2]/x[ , , , 3]-parameter[4], # marginal utility of travel time
                    -parameter[3]/p, # marginal utility of land price
                    {(1-parameter[2])*(parameter[7]-t)*parameter[5]*w-parameter[2]*parameter[6]+parameter[2]*c}/{w*((parameter[7]-t)*parameter[5]*w+parameter[6]-c)}), # marginal utility of wage rate
                  dim = dim(x))
    return(dvdx)
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
         marginalEffects = marginalEffectsClosure(parameter)
    )
  )
}