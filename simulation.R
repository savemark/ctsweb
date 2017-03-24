systemOfEquationsClosure <- function(type = c("nonfixed", "fixed")) {
  # nonfixed: non-fixed land use
  # fixed: fixed land use
  type <- match.arg(type)
  systemOfEquations <- function(pw, c, t, op, dp, utility, wagerate0, area, probability, spillover) {
    p <- array(pw[1:dim(c)[1]], dim = dim(c))
    w <- array(rep(t(pw[-(1:dim(c)[1])]), each = dim(c)[1]), dim = dim(c)) # wage rate
    v <- utility$indirectUtility(p, w, c, t, op, dp) # indirect utility
    pr <- probability$density(v) # choice probabilities
    ld <- utility$argmaxUtility(p, w, c, t)[ , , , 4] # land demand
    diff <- c(apply(pr*ld, 1, sum, na.rm = TRUE)-area, # excess demand
              spillover$f(pr)*wagerate0-pw[-(1:nrow(c))]) # wage rate fixed point
    return(diff)
  }
  systemOfEquations_fixed <- function(pw, c, t, op, dp, utility, wagerate0, area, probability, spillover, scale) {
    p <- array(pw[1:dim(c)[1]], dim = dim(c))
    w <- array(rep(t(pw[-(1:dim(c)[1])]), each = dim(c)[1]), dim = dim(c)) # wage rate
    v <- utility$indirectUtility(p, w, c, t, op, dp) # indirect utility
    pr <- probability$density(v) # choice probabilities
    prj <- array(apply(array(rep(colSums(aperm(pr, c(2, 1, 3))), each = dim(c)[1]), 
                             dim = dim(c)), 3, t), dim = dim(c))
    pr <- pr*{scale/prj} # rescaled probabilities for fixed land-use
    ld <- utility$argmaxUtility(p, w, c, t)[ , , , 4] # land demand
    diff <- c(apply(pr*ld, 1, sum, na.rm = TRUE)-area, # excess demand
              spillover$f(pr)*wagerate0-pw[-(1:nrow(c))]) # wage rate fixed point
    return(diff)
  }
  switch(type,
         nonfixed = systemOfEquations,
         fixed = systemOfEquations_fixed)
}

simulation <- function(guess, city, population, utility, probability, spillover, scale = NULL) {
  if (!all(guess > 0)) # guess = land price guess vector
    stop("The land price guess needs to be larger than 0.")
  wagerate0 <- as.vector(getWageRate(population)) # row vector
  c <- array(getCost(city), dim = c(getNodeCount(city), getNodeCount(city), getNumberOfClasses(population))) # travel cost 
  t <- array(getTime(city), dim = c(getNodeCount(city), getNodeCount(city), getNumberOfClasses(population))) # travel time 
  op <- array(apply(t(getOriginPreference(population)), 2, # origin-preference
                    function(x) {matrix(rep(x, length.out = getNodeCount(city)*getNodeCount(city)), getNodeCount(city), getNodeCount(city))}), dim = c(getNodeCount(city), getNodeCount(city), getNumberOfClasses(population)))
  dp <- array(rep(t(getDestinationPreference(population)), each = getNodeCount(city)), dim = c(getNodeCount(city), getNodeCount(city), getNumberOfClasses(population))) # destination-preference 
  if (is.null(scale)) {
    sol <- BBsolve(par = c(guess, wagerate0), 
                   fn = systemOfEquationsClosure("nonfixed"), 
                   control = list(trace = FALSE, NM = FALSE), 
                   quiet = TRUE,
                   c = c, t = t, op = op, dp = dp, 
                   utility = utility, 
                   wagerate0 = wagerate0, 
                   area = getArea(city), 
                   probability = probability, 
                   spillover = spillover) 
  } else {
    sol <- BBsolve(par = c(guess, wagerate0), 
                   fn = systemOfEquationsClosure("fixed"), 
                   control = list(trace = FALSE, NM = TRUE), 
                   quiet = TRUE,
                   c = c, t = t, op = op, dp = dp, 
                   utility = utility, wagerate0 = wagerate0, 
                   area = getArea(city), probability = probability, 
                   spillover = spillover, scale = scale) 
  }
  price <- sol$par[1:getNodeCount(city)] # land price p*
  wagerate <- sol$par[-(1:getNodeCount(city))] # wage rate w*
  setVertexPrice(city) <- price 
  setWageRate(population) <- matrix(wagerate, getNumberOfClasses(population), getNodeCount(city)) 
  p <- array(price, dim = c(getNodeCount(city), getNodeCount(city), getNumberOfClasses(population)))
  w <- array(rep(t(wagerate), each = getNodeCount(city)), dim = c(getNodeCount(city), getNodeCount(city), getNumberOfClasses(population))) # wage rate
  setUtility(population) <- utility$indirectUtility(p, w, c, t, op, dp)
  if (is.null(scale)) {
    setProbability(population) <- probability$density(utility$indirectUtility(p, w, c, t, op, dp)) # Probabilities with land-price p* and wage rate w*
  } else {
    pr <- probability$density(utility$indirectUtility(p, w, c, t, op, dp))
    prj <- array(apply(array(rep(colSums(aperm(pr, c(2, 1, 3))), each = dim(c)[1]), 
                             dim = dim(c)), 3, t), dim = dim(c))
    pr <- pr*{scale/prj} # rescaled probabilities for fixed land-use
    setProbability(population) <- pr
  }
  setArgMax(population) <- utility$argmaxUtility(p, w, c, t)
  setMarginalEffect(population) <- utility$marginalEffects(utility$argmaxUtility(p, w, c, t), p, w, c, t) 
  setOriginDestinationMatrix(population) <- odDemand(getProbability(population))
  return(list(city = city, population = population, solution = sol))
}