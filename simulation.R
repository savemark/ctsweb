simulation <- function(guess, city, population, utility, delta, spillover.eps = 0) {
  # guess = land price guess vector
  if (!all(guess > 0))
    stop("The land price guess needs to be larger than 0.")
  probability <- function(x, type = "logit", ...) {
    # x indirect utility, 3-dimensional array
    maximumProbability <- function(x, margin = 3) {
      maxIndicator <- function(x) {
        ifelse(x == max(x), 1, 0)
      }
      pr <- apply(x, margin, maxIndicator)
      dim(pr) <- dim(x)
      return(pr)
    }
    logitProbability <- function(x, delta, margin = 3) {
      d <- dim(x)
      y <- exp({x-array(rep(apply(x, 3, max), each = d[1]^2), dim = d)}/delta)
      sfn <- apply(y, margin, sum, na.rm = TRUE)
      sfn <- array(rep(sfn, each = d[1]^2), dim = d)
      pr <- y/sfn
      return(pr)
    }
    switch(type,
           logit = logitProbability(x, ...),
           maximum = maximumProbability(x, ...)
    )
  }
  
  knowledgeSpillover <- function(city, population, epsilon, type = "FromAll") {
    n <- getSize(population)
    knowledgeSpilloverFromAllButOneself <- function(city, population, epsilon) {
      nj <- sapply(1:n, function(m) {apply(getProbability(population)[, , -m], 2, sum)}) # matrix
      nj <- as.vector(t(nj))
      supply <- rep(getArea(city), n) # Need to repeat the supply
      factor <- (1+nj/supply)^epsilon
      return(factor)
    }
    knowledgeSpilloverFromAll <- function(city, population, epsilon) {
      nj <- apply(getProbability(population), 2, sum, na.rm = TRUE)
      factor <- (1+nj/getArea(city))^epsilon
      factor <- rep(factor, each = n)
      return(factor)
    }
    switch(type,
           FromAllButOneself = knowledgeSpilloverFromAllButOneself(city, population, epsilon),
           FromAll = knowledgeSpilloverFromAll(city, population, epsilon)
    )
  }
  
  economyEquilibrium <- function(pw, city, population, utility, wagerate0, delta, epsilon) {
    setWageRate(population) <- matrix(pw[-(1:getNodeCount(city))], getSize(population), getNodeCount(city))
    u <- utility(pw[1:getNodeCount(city)], city, population)
    setProbability(population) <- probability(u$umax, "logit", delta)
    y <- c(apply(getProbability(population)*u$argmax[ , , , 4], 1, sum, na.rm = TRUE)-getArea(city), # excess demand
           knowledgeSpillover(city, population, epsilon)*wagerate0-pw[-(1:getNodeCount(city))]) # wage rate fixed point
    return(y)
  }
  
  wagerate0 <- as.vector(getWageRate(population)) # row vector
  pw0 <- c(guess, wagerate0) # land prices and wage rates guess
  sol <- BBsolve(par = pw0, fn = economyEquilibrium, control = list(trace = FALSE, NM = TRUE), quiet = TRUE, 
                city = city, population = population, utility = utility, wagerate0 = wagerate0, delta = delta, epsilon = spillover.eps)
  price <- sol$par[1:getNodeCount(city)] # land price p*
  wagerate <- sol$par[-(1:getNodeCount(city))] # wage rate w*
  sol$par <- price 
  setWageRate(population) <- matrix(wagerate, getSize(population), getNodeCount(city)) 
  u <- utility(price, city, population)
  setProbability(population) <- probability(u$umax, "logit", delta) # Probabilities with land-price p* and wage rate w*
  setUtility(population) <- u$umax
  setArgMax(population) <- u$argmax
  setMarginalEffect(population) <- u$lambda
  setOriginDestinationMatrix(population) <- odDemand(getProbability(population))
  return(list(population = population, solution = sol))
}