systemOfEquations <- function(pw, c, t, op, dp, utility, wagerate0, area, probability, spillover) {
  p <- array(pw[1:dim(c)[1]], dim = dim(c))
  w <- array(rep(t(pw[-(1:dim(c)[1])]), each = dim(c)[1]), dim = dim(c)) # wage rate
  v <- utility$indirectUtility(p, w, c, t, op, dp) # indirect utility
  pr <- probability$density(v) # choice probabilities
  ld <- utility$argmaxUtility(p, w, c, t)[ , , , 4] # land demand
  diff <- c(apply(pr*ld, 1, sum, na.rm = TRUE)-area, # excess demand
            spillover$f(pr)*wagerate0-pw[-(1:dim(c)[1])]) # wage rate fixed point
  return(diff)
}

simulation <- function(guess, city, population, utility, probability, spillover) {
  if (!all(guess > 0)) # guess = land price guess vector
    stop("The land price guess needs to be larger than 0.")
  setUnderlyingWageRate(population) <- getWageRate(population)
  wagerate0 <- as.vector(getWageRate(population)) # row vector
  c <- array(getCost(city), dim = c(getNodeCount(city), getNodeCount(city), getSize(population))) # travel cost 
  t <- array(getTime(city), dim = c(getNodeCount(city), getNodeCount(city), getSize(population))) # travel time 
  op <- array(apply(t(getOriginPreference(population)), 2, # origin-preference
                    function(x) {matrix(rep(x, length.out = getNodeCount(city)*getNodeCount(city)), getNodeCount(city), getNodeCount(city))}), dim = c(getNodeCount(city), getNodeCount(city), getSize(population)))
  dp <- array(rep(t(getDestinationPreference(population)), each = getNodeCount(city)), dim = c(getNodeCount(city), getNodeCount(city), getSize(population))) # destination-preference 
  sol <- BBsolve(par = c(guess, wagerate0), fn = systemOfEquations, control = list(trace = FALSE, NM = TRUE), quiet = TRUE, 
                 c = c, t = t, op = op, dp = dp, utility = utility, wagerate0 = wagerate0, area = getArea(city), probability = probability, spillover = spillover)
  price <- sol$par[1:getNodeCount(city)] # land price p*
  wagerate <- sol$par[-(1:getNodeCount(city))] # wage rate w*
  setVertexPrice(city) <- price 
  setWageRate(population) <- matrix(wagerate, getSize(population), getNodeCount(city)) 
  p <- array(price, dim = c(getNodeCount(city), getNodeCount(city), getSize(population)))
  w <- array(rep(t(wagerate), each = getNodeCount(city)), dim = c(getNodeCount(city), getNodeCount(city), getSize(population))) # wage rate
  setUtility(population) <- utility$indirectUtility(p, w, c, t, op, dp)
  setProbability(population) <- probability$density(utility$indirectUtility(p, w, c, t, op, dp)) # Probabilities with land-price p* and wage rate w*
  setArgMax(population) <- utility$argmaxUtility(p, w, c, t)
  setMarginalEffect(population) <- utility$marginalEffects(utility$argmaxUtility(p, w, c, t), p, w, c, t) 
  setOriginDestinationMatrix(population) <- odDemand(getProbability(population))
  return(list(city = city, population = population, solution = sol))
}