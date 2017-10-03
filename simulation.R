systemOfEquationsClosure <- function(options = list(landuse = c("nonfixed", "fixed"), 
                                                    population = c("exogenous", "endogenous"))) {
  # nonfixed: non-fixed land use
  # fixed: fixed land use
  landuse <- match.arg(options$landuse, c("nonfixed", "fixed"))
  population <- match.arg(options$population, c("exogenous", "endogenous"))
  nonfixed_exogenous <- function(x, c, t, op, dp, utility, wagerate0, area, probability, spillover) {
    p <- array(x[1:dim(c)[1]], dim = dim(c))
    w <- array(rep(t(x[-(1:dim(c)[1])]), each = dim(c)[1]), dim = dim(c)) # wage rate
    v <- utility$indirectUtility(p, w, c, t, op, dp) # indirect utility
    pr <- probability$density(v) # choice probabilities
    ld <- utility$argmaxUtility(p, w, c, t)[ , , , 4] # land demand
    diff <- c(apply(pr*ld, 1, sum, na.rm = TRUE)-area, # excess demand
              spillover$f(pr)*wagerate0-x[-(1:nrow(c))]) # wage rate fixed point
    return(diff)
  }
  fixed_exogenous <- function(x, c, t, op, dp, utility, wagerate0, area, probability, spillover, scale) {
    p <- array(x[1:dim(c)[1]], dim = dim(c))
    w <- array(rep(t(x[-(1:dim(c)[1])]), each = dim(c)[1]), dim = dim(c)) # wage rate
    v <- utility$indirectUtility(p, w, c, t, op, dp) # indirect utility
    pr <- probability$density(v) # choice probabilities
    prj <- array(apply(array(rep(colSums(aperm(pr, c(2, 1, 3))), each = dim(c)[1]),
                             dim = dim(c)), 3, t), dim = dim(c))
    pr <- pr*{scale/prj} # rescaled probabilities for fixed land-use
    ld <- utility$argmaxUtility(p, w, c, t)[ , , , 4] # land demand
    diff <- c(apply(pr*ld, 1, sum, na.rm = TRUE)-area, # excess demand
              spillover$f(pr)*wagerate0-x[-(1:nrow(c))]) # wage rate fixed point
    return(diff)
  }
  nonfixed_endogenous <- function(x, c, t, op, dp, utility, wagerate0, area, probability, spillover, avg.utility) {
    # x = (p, w, K)
    zoneids <- 1:nrow(c)
    p <- array(x[zoneids], dim = dim(c))
    p <- p*(10^5) # scaling
    area0 <- area/sum(area) 
    w <- x[-c(zoneids)]
    w.arr <- array(rep(t(w), each = dim(c)[1]), dim = dim(c)) # wage rate
    v <- utility$indirectUtility(p, w.arr, c, t, op, dp) # indirect utility
    z_urban <- 0+utility$expectedUtility(v)
    z_rural <- rep(3, length(z_urban))
    pr_C_rural <- exp(z_rural)/(exp(z_rural)+exp(z_urban))
    pr_C_urban <- exp(z_urban)/(exp(z_rural)+exp(z_urban))
    K_urban <- 1000*pr_C_urban
    K_rural <- 1000*pr_C_rural
    cat("K_urban: ", K_urban[1], "\n")
    cat("K_rural: ", K_rural[1], "\n")
    K.arr <- array(rep(K_urban, each = nrow(c)^2), dim = dim(c))
    pr <- probability$density(v) # choice probabilities
    ld <- utility$argmaxUtility(p, w.arr, c, t)[ , , , 4] # land demand
    diff <- c(apply(K.arr*pr*ld, 1, sum, na.rm = TRUE)-area0, # excess demand
              spillover$f(K.arr*pr)*wagerate0-w) # wage rate fixed point
    return(diff)
  }
  switch(paste(landuse, "_", population, sep = ""),
         nonfixed_exogenous = nonfixed_exogenous,
         fixed_exogenous = fixed_exogenous,
         nonfixed_endogenous = nonfixed_endogenous)
}

simulation <- function(guess, city, population, utility, probability, spillover, scale = NULL, avg.utility = NULL) {
  if (!all(guess > 0)) # guess = land price guess vector
    stop("The land price guess needs to be larger than 0.")
  wagerate0 <- as.vector(getWageRate(population)) # row vector
  c <- array(getCost(city), dim = c(getNodeCount(city), getNodeCount(city), getNumberOfClasses(population))) # travel cost 
  t <- array(getTime(city), dim = c(getNodeCount(city), getNodeCount(city), getNumberOfClasses(population))) # travel time 
  op <- array(apply(t(getOriginPreference(population)), 2, # origin-preference
                    function(x) {matrix(rep(x, length.out = getNodeCount(city)*getNodeCount(city)), getNodeCount(city), getNodeCount(city))}), dim = c(getNodeCount(city), getNodeCount(city), getNumberOfClasses(population)))
  dp <- array(rep(t(getDestinationPreference(population)), each = getNodeCount(city)), dim = c(getNodeCount(city), getNodeCount(city), getNumberOfClasses(population))) # destination-preference 
  if (is.null(scale) && is.null(avg.utility)) {
    sol <- BBsolve(par = c(guess, wagerate0), 
                   fn = systemOfEquationsClosure(options = list(landuse = "nonfixed", population = "exogenous")), 
                   control = list(trace = FALSE, NM = TRUE), 
                   quiet = TRUE,
                   c = c, t = t, op = op, dp = dp, 
                   utility = utility, 
                   wagerate0 = wagerate0, 
                   area = getArea(city), 
                   probability = probability, 
                   spillover = spillover) 
    price <- sol$par[1:getNodeCount(city)] # land price p*
    wagerate <- sol$par[-(1:getNodeCount(city))] # wage rate w*
  } else {
    if (!is.null(scale) && is.null(avg.utility)) {
      sol <- BBsolve(par = c(guess, wagerate0), 
                     fn = systemOfEquationsClosure(options = list(landuse = "fixed", population = "exogenous")), 
                     control = list(trace = FALSE, NM = TRUE), 
                     quiet = TRUE,
                     c = c, t = t, op = op, dp = dp, 
                     utility = utility, 
                     wagerate0 = wagerate0, 
                     area = getArea(city), 
                     probability = probability, 
                     spillover = spillover, 
                     scale = scale) 
      price <- sol$par[1:getNodeCount(city)] # land price p*
      wagerate <- sol$par[-(1:getNodeCount(city))] # wage rate w*
    } else {
      if (is.null(scale) && !is.null(avg.utility)) {
        #K0 <- rep(500, dim(c)[3])
        sol <- BBsolve(par = c(guess, wagerate0), 
                       fn = systemOfEquationsClosure(options = list(landuse = "nonfixed", population = "endogenous")), 
                       control = list(trace = TRUE), 
                       quiet = FALSE,
                       c = c, t = t, op = op, dp = dp, 
                       utility = utility, 
                       wagerate0 = wagerate0, 
                       area = getArea(city), 
                       probability = probability, 
                       spillover = spillover,
                       avg.utility = avg.utility) 
        price <- (10^5)*sol$par[1:getNodeCount(city)] # land price p*
        wagerate <- sol$par[-c(1:getNodeCount(city))] # wage rate w*
        w.arr <- array(rep(t(wagerate), each = dim(c)[1]), dim = dim(c))
        v <- utility$indirectUtility(price, w.arr, c, t, op, dp) # indirect utility
        z_urban <- 0+utility$expectedUtility(v)
        z_rural <- rep(3, length(z_urban))
        pr_C_rural <- exp(z_rural)/(exp(z_rural)+exp(z_urban))
        pr_C_urban <- exp(z_urban)/(exp(z_rural)+exp(z_urban))
        K_urban <- 1000*pr_C_urban
        K_rural <- 1000*pr_C_rural
      }
    }
  }
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
  if (!is.null(avg.utility)) {
    setSizeOfEachClass(population) <- K_urban
  }
  setArgMax(population) <- utility$argmaxUtility(p, w, c, t)
  setMarginalEffect(population) <- utility$marginalEffects(utility$argmaxUtility(p, w, c, t), p, w, c, t) 
  setOriginDestinationMatrix(population) <- odDemand(getProbability(population))
  return(list(city = city, population = population, solution = sol))
}