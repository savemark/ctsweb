simulation <- function(guess, city, population, utility, delta, spillover.eps = 0) {
  # guess = land price guess vector
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
  economyEquilibrium.closure <- function(city, population, utility, delta, epsilon) {
    v <- getNodeCount(city)
    supply <- getArea(city)
    n <- getSize(population)
    wagerate0 <- as.vector(getWageRate(population)) # row vector
    function(pw) {
      p <- pw[1:v]
      w <- pw[-(1:v)]
      setWageRate(population) <- matrix(w, n, v)
      u <- utility(p, city, population)
      setProbability(population) <- probability(u$umax, "logit", delta)
      demand <- apply(getProbability(population)*u$argmax[ , , , 4], 1, sum, na.rm = TRUE)
      excess <- demand-supply # excess demand
      nj <- apply(getProbability(population), 2, sum, na.rm = TRUE)
      factor <- (1+nj/supply)^epsilon
      factor <- rep(factor, each = n)
      #factor <- matrix(factor, n, v, byrow = TRUE)
      wageratediff <- factor*wagerate0-w # wage rate fixed point
      return(c(excess, wageratediff))
    }
  }
  economyEquilibrium <- economyEquilibrium.closure(city, population, utility, delta, spillover.eps)
  pw0 <- c(guess, as.vector(getWageRate(population))) # land prices and wage rates guess
  sol <- dfsane(par = pw0, fn = economyEquilibrium, control = list(trace = FALSE, NM = FALSE), quiet = FALSE)
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