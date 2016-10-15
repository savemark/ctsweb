utilityOptimClosure <- function(beta2, beta3, beta4, beta5, tau) {
  # This function returns a function (it is a closure)
  # conditions on the parameters
  if(beta2+beta3+beta4 != 1)
    stop("The parameters of the utility function does not sum to 1.")
  stopifnot(all(c(beta2, beta3, beta4, beta5) > 0))
  stopifnot(tau <= 1 && tau > 0)
  
  function(p, city, population, y = 100) {
    # p zone price vector
    # First, calculate the arg max
    # We do this by turning vectors (matrices) into arrays and 
    # then doing caclulations using vectorization
    # so that we don't use too much memory (only temporarily)
    N <- getSize(population)
    V <- getNodeCount(city)   
    p <- array(p, dim = c(V, V, N))
    w <- array(rep(t(getWageRate(population)), each = V), dim = c(V, V, N)) # wage rate
    c <- array(getCost(city), dim = c(V, V, N)) # travel cost 
    t <- array(getTime(city), dim = c(V, V, N)) # travel time 
    op <- array(apply(t(getOriginPreference(population)), 2, # origin-preference
                      function(x) {matrix(rep(x, length.out = V*V), V, V)}), dim = c(V, V, N))
    dp <- array(rep(t(getDestinationPreference(population)), each = V), dim = c(V, V, N)) # destination-preference 
    #if (is.na(y)) {
    #  y <- 100 #max(c-tau*w*(24-t)) # neccessary exogenous income
    #}
    argmaxUtility <- function(p, w, c, t) {      
      x1 <- 24-t-{beta3/{tau*w}}*{tau*w*(24-t)+y-c}     # Optimal working hours
      x2 <- beta2*{tau*w*{24-t}+y-c}                    # Optimal consumption
      x3 <- {beta3/{tau*w}}*{tau*w*{24-t}+y-c}          # Optimal leisure
      x4 <- {beta4/p}*{tau*w*{24-t}+y-c}                # Optimal land-use
      x5 <- t
      argmax <- array(c(x1, x2, x3, x4, x5), dim = c(V, V, N, 5))
      return(argmax)
    }
    marginalEffects <- function(x) {
      # note that marginal utility of income is minus marginal utility of travel cost
      lambda1 <- beta2/x[ , , , 2] # marginal utility of income
      lambda2 <- beta3/x[ , , , 3] # marginal utility of time
      lambda3 <- -(beta5 + lambda2) # marginal utility of travel time
      lambda4 <- -beta4/p # marginal utility of land price
      lambda5 <- {(1-beta3)*(24-t)*tau*w-beta3*y+beta3*c}/{w*((24-t)*tau*w+y-c)} # marginal utility of wage rate
      lambda <- array(c(lambda1, lambda2, lambda3, lambda4, lambda5), dim = c(V, V, N, 5))
      return(lambda)
    }
    indirectUtility <- function(p, w, c, t, op, dp) {
      umax <- log((tau*w*(24-t)+y-c)/((tau*w)^beta3*p^beta4))-beta5*t+op+dp+log({beta2^beta2}*{beta3^beta3}*{beta4^beta4})
      return(umax)
    }
    argmax <- argmaxUtility(p, w, c, t)
    lambda <- marginalEffects(argmax)
    v <- indirectUtility(p, w, c, t, op, dp)
    return(
      list(umax = v,
           argmax = argmax,
           lambda = lambda
      )
    )
  }
}