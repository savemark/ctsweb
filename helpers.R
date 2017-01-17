odDemand <- function(x) {
  # x probability
  odm <- apply(x, c(1, 2), sum, na.rm = TRUE)
  return(odm)
}

equivalentVariation <- function(x, y, sigma) {
  # x, y list(population, city)
  # We need to subtract the maximum of the indrect utilities for each choice set, 
  # otherwise the logsum will not be possible to calculate (because it will be too large)
  maxu.x <- apply(getUtility(x$population), 3, max)
  maxu.xa <- array(rep(maxu.x, each = getNodeCount(x$city)^2), dim = c(getNodeCount(x$city), getNodeCount(x$city), getSize(x$population)))
  maxu.y <- apply(getUtility(y$population), 3, max)
  maxu.ya <- array(rep(maxu.y, each = getNodeCount(y$city)^2), dim = c(getNodeCount(y$city), getNodeCount(y$city), getSize(y$population)))
  logsum.x <- maxu.x+sigma*log(apply(exp((getUtility(x$population)-maxu.xa)/sigma), 3, sum))
  logsum.y <- maxu.y+sigma*log(apply(exp((getUtility(y$population)-maxu.ya)/sigma), 3, sum))
  ev <- (logsum.y-logsum.x)/(apply(getProbability(x$population)*getMarginalEffect(x$population)[ , , , 1], 3, sum))
  ev <- sum(ev)
  return(ev)
}

roah3 <- function(x, y) {
  # With comfort, land prices, wage rates
  # x, y list(population, city, comfort)
  N <- getSize(x$population)
  V <- getNodeCount(x$city)
  cost.x <- array(getCost(x$city), dim = c(V, V, N))
  cost.y <- array(getCost(y$city), dim = c(V, V, N))
  time.x <- array(getTime(x$city), dim = c(V, V, N))
  time.y <- array(getTime(y$city), dim = c(V, V, N))
  price.x <- array(getVertexPrice(x$city), dim = c(V, V, N))
  price.y <- array(getVertexPrice(y$city), dim = c(V, V, N))
  wage.x <- array(rep(t(getWageRate(x$population)), each = V), dim = c(V, V, N))
  wage.y <- array(rep(t(getWageRate(y$population)), each = V), dim = c(V, V, N))
  beta5.x <- x$comfort
  beta5.y <- y$comfort
  margin1 <- getMarginalEffect(x$population)[ , , , 1] # marginal utility of income
  margin2 <- getMarginalEffect(x$population)[ , , , 2]
  margin3 <- getMarginalEffect(x$population)[ , , , 3]
  margin4 <- getMarginalEffect(x$population)[ , , , 4]
  margin5 <- getMarginalEffect(x$population)[ , , , 5]
  roah1 <- sum(0.5*(getProbability(x$population)+getProbability(y$population))*(-1)*(cost.y-cost.x)) 
  roah2 <- sum(0.5*(getProbability(x$population)+getProbability(y$population))*((margin3/margin1)*(time.y-time.x)))
  roah3 <- sum(0.5*(getProbability(x$population)+getProbability(y$population))*((-time.x/margin1)*(beta5.y-beta5.x)))
  roah4 <- sum(0.5*(getProbability(x$population)+getProbability(y$population))*((margin4/margin1)*(price.y-price.x)))
  roah5 <- sum(0.5*(getProbability(x$population)+getProbability(y$population))*((margin5/margin1)*(wage.y-wage.x)))
  roah <- c(roah1, roah2, roah3, roah4, roah5)
  return(roah)
}

roah4 <- function(x, y) {
  # With comfort, land prices, wage rates
  # x, y list(population, city, comfort)
  # sapply(1:n, function(m) {apply(getProbability(population)[, , -m], 2, sum)})
  N <- getSize(x$population)
  V <- getNodeCount(x$city)
  price.x <- matrix(getVertexPrice(x$city), V, V)
  price.y <- matrix(getVertexPrice(y$city), V, V)
  wage.x <- array(rep(t(getWageRate(x$population)), each = V), dim = c(V, V, N))
  wage.y <- array(rep(t(getWageRate(y$population)), each = V), dim = c(V, V, N))
  beta5.x <- x$comfort
  beta5.y <- y$comfort
  margin1 <- getMarginalEffect(x$population)[ , , , 1] # marginal utility of income
  margin2 <- getMarginalEffect(x$population)[ , , , 2]
  margin3 <- getMarginalEffect(x$population)[ , , , 3]
  margin4 <- getMarginalEffect(x$population)[ , , , 4]
  margin5 <- getMarginalEffect(x$population)[ , , , 5]
  income <- sapply(1:N, function(m) {sum(getProbability(x$population)[ , , m]*wage.x[ , , m]*getArgMax(x$population)[ , , m, 1])})
  roah1 <- sapply(1:N, function(m) {sum(0.5*(getProbability(x$population)[ , , m]+getProbability(y$population)[ , , m])*(-1)*(getCost(y$city)-getCost(x$city)))}) 
  roah2 <- sapply(1:N, function(m) {sum(0.5*(getProbability(x$population)[ , , m]+getProbability(y$population)[ , , m])*((margin3[ , , m]/margin1[ , , m])*(getTime(y$city)-getTime(x$city))))})
  roah3 <- sapply(1:N, function(m) {sum(0.5*(getProbability(x$population)[ , , m]+getProbability(y$population)[ , , m])*((-getTime(x$city)/margin1[ , , m])*(beta5.y-beta5.x)))})
  roah4 <- sapply(1:N, function(m) {sum(0.5*(getProbability(x$population)[ , , m]+getProbability(y$population)[ , , m])*((margin4[ , , m]/margin1[ , , m])*(price.y-price.x)))})
  roah5 <- sapply(1:N, function(m) {sum(0.5*(getProbability(x$population)[ , , m]+getProbability(y$population)[ , , m])*((margin5[ , , m]/margin1[ , , m])*(wage.y[ , , m]-wage.x[ , , m])))})
  roah <- cbind(income, "travel cost" = roah1, "travel time" = roah2, "travel comfort" = roah3, "land price" = roah4, "wage rate" = roah5)
  return(roah)
}

equityPlot <- function(x, probs = seq(0, 1, 0.2)) { # x comes from roah4
  rs <- rowSums(x[ , 2:6])
  tab <- cbind(x, total.utility = rs)
  tab <- as.data.frame(tab)
  range <- cut(tab[ , 1], breaks = quantile(tab[ , 1], probs = probs), include.lowest = T, dig.lab = 5)
  tab <- cbind(tab, range)
  tab.ag <- aggregate(tab[ , 2:7], by = list(income = tab$range), FUN = sum)
  tab.gather <- gather(tab.ag, value = "utility", key = "variable", 2:6)
  g <- ggplot(tab.gather, aes(x = income, y = utility)) + geom_bar(aes(fill = variable), stat = "identity", position = "dodge") # weight = utility, 
  g <- g + ggtitle("Utility in monetary unit per income class and per variable") + theme_bw()
  return(g)
}

tax <- function(city, population, tau) {
  wagerate <- array(rep(t(getWageRate(population)), each = getNodeCount(city)), dim = c(getNodeCount(city), getNodeCount(city), getSize(population)))
  sum(apply(tau*wagerate*getArgMax(population)[ , , , 1]*getProbability(population), 3, sum))
}

wageSum <- function(x, y, tau) {
  # x, y population
  nodes <- ncol(getWageRate(x))
  N <- getSize(x)
  wagerateA <- array(rep(t(getWageRate(x)), each = nodes), dim = c(nodes, nodes, N))
  wagerateB <- array(rep(t(getWageRate(y)), each = nodes), dim = c(nodes, nodes, N))
  base <- sum((getProbability(y)*getArgMax(y)[ , , , 1]-getProbability(x)*getArgMax(x)[ , , , 1])*(1-tau)*wagerateA)
  scale <- sum((getProbability(y)*getArgMax(y)[ , , , 1])*(1-tau)*(wagerateB-wagerateA))
  return(list(Base = base, Scale = scale))
}

avarages <- function(x, y) {
  # x, y list(population, city)
  nodes <- getNodeCount(x$city)
  N <- getSize(x$population)
  wagerateA <- array(rep(t(getWageRate(x$population)), each = nodes), dim = c(nodes, nodes, N))
  wagerateB <- array(rep(t(getWageRate(y$population)), each = nodes), dim = c(nodes, nodes, N))
  votMeanA <- apply(getProbability(x$population)*getVoT(x$population), c(1,2), sum)/apply(getProbability(x$population), c(1,2), sum)
  votMeanB <- apply(getProbability(y$population)*getVoT(y$population), c(1,2), sum)/apply(getProbability(y$population), c(1,2), sum)
  workingTimeAvg.a <- sum(apply(getArgMax(x$population)[ , , , 1]*getProbability(x$population), 3, sum))/N
  workingTimeAvg.b <- sum(apply(getArgMax(y$population)[ , , , 1]*getProbability(y$population), 3, sum))/N
  wagerateAvg.a <- sum(apply(wagerateA*getProbability(x$population), 3, sum))/N
  wagerateAvg.b <- sum(apply(wagerateB*getProbability(y$population), 3, sum))/N
  VKT.a <- sum(odDemand(getProbability(x$population))*getDistance(x$city))/N
  VKT.b <- sum(odDemand(getProbability(y$population))*getDistance(y$city))/N
  travelTimeAvg.a <- sum(odDemand(getProbability(x$population))*getTime(x$city))/N
  travelTimeAvg.b <- sum(odDemand(getProbability(y$population))*getTime(y$city))/N
  travelCostAvg.a <- sum(odDemand(getProbability(x$population))*getCost(x$city))/N
  travelCostAvg.b <- sum(odDemand(getProbability(y$population))*getCost(y$city))/N
  return(rbind("Working time" = c(workingTimeAvg.a, workingTimeAvg.b), 
               "Wage Rate" = c(wagerateAvg.a, wagerateAvg.b), 
               "VKT" = c(VKT.a, VKT.b), 
               "Travel time" = c(travelTimeAvg.a, travelTimeAvg.b), 
               "Travel cost" = c(travelCostAvg.a, travelCostAvg.b),
               "Marginal value of Tr. Time" = c(mean(votMeanA), mean(votMeanB))
               )
  )
}

logitTransportModel <- function(x, mu) {
  N <- getSize(x$population)
  nodes <- getNodeCount(x$city)
  votMean <- apply(getProbability(x$population)*getVoT(x$population), c(1,2), sum)/apply(getProbability(x$population), c(1,2), sum)
  gc <- -getCost(x$city)+votMean*getTime(x$city) # votMean is negative (as it should)
  Hi <- matrix(apply(getProbability(x$population), 1, sum), nodes, nodes)/N # Number of residents in zone i as matrix with identical rows
  Wk <- t(matrix(apply(getProbability(x$population), 2, sum), nodes, nodes))/N # Number of workers in zone k as matrix with identical cols
  rowsum <- matrix(apply(Wk*exp(mu*gc), 1, sum), nodes, nodes) # denominator 
  od <- (Hi*Wk*exp(mu*gc))/rowsum
  time.avg <- sum(od*getTime(x$city))
  return(time.avg)
}

logElasticityProductionAccessibility <- function(x, y, mu) {
  nodes <- getNodeCount(x$city)
  N <- getSize(x$population)
  wagerate.x <- array(rep(t(getWageRate(x$population)), each = nodes), dim = c(nodes, nodes, N))
  wagerate.y <- array(rep(t(getWageRate(y$population)), each = nodes), dim = c(nodes, nodes, N))
  votMean.x <- apply(getProbability(x$population)*getVoT(x$population), c(1,2), sum)/apply(getProbability(x$population), c(1,2), sum)
  votMean.y <- apply(getProbability(y$population)*getVoT(y$population), c(1,2), sum)/apply(getProbability(y$population), c(1,2), sum)
  Hi.x <- matrix(apply(getProbability(x$population), 1, sum), nodes, nodes)/N # Number of residents in zone i as matrix with identical rows
  Hi.y <- matrix(apply(getProbability(y$population), 1, sum), nodes, nodes)/N # Number of residents in zone i as matrix with identical rows
  Wk.x <- t(matrix(apply(getProbability(x$population), 2, sum), nodes, nodes))/N # Number of workers in zone k as matrix with identical cols
  Wk.y <- t(matrix(apply(getProbability(y$population), 2, sum), nodes, nodes))/N # Number of workers in zone k as matrix with identical cols
  gc.x <- -getCost(x$city)+votMean.x*getTime(x$city)
  gc.y <- -getCost(y$city)+votMean.x*getTime(y$city) # votMean.x (before the policy)
  rowsum.x <- matrix(log(apply(Wk.x*exp(mu*gc.x), 1, sum)), nodes, nodes)
  rowsum.y <- matrix(log(apply(Wk.y*exp(mu*gc.y), 1, sum)), nodes, nodes)
  accessibility.x <- sum((1/mu)*Hi.x*rowsum.x)
  accessibility.y <- sum((1/mu)*Hi.y*rowsum.y)
  production.x <- sum(getArgMax(x$population)[ , , , 1]*wagerate.x*getProbability(x$population))
  production.y <- sum(getArgMax(y$population)[ , , , 1]*wagerate.y*getProbability(y$population))
  return(((production.y-production.x)/production.x)/((accessibility.y-accessibility.x)/abs(accessibility.x))) # Note the absolute value of accessibility
}

fixedLandUse <- function(x, y, sigma) {
  # x, y list(city, population)
  # sigma variance parameter
  city <- x$city # Same land prices as Base scenario
  setCostFactor(city) <- getCostFactor(y$city) # Same travel costs as Do-something scenario
  setSpeed(city) <- getSpeed(y$city) # Same travel times as Do-something scenario
  population <- x$population # Or population B? Would imply that probabilities due to travel comfort will be same as scenario B
  rsA <- array(apply(array(rep(apply(getProbability(x$population), c(1, 3), sum), each = getNodeCount(x$city)), 
                           c(getNodeCount(x$city), getNodeCount(x$city), getSize(x$population))), 3, t), 
               c(getNodeCount(x$city), getNodeCount(x$city), getSize(x$population))) # Sum{j} P^0_{ij}
  rsB <- array(apply(array(rep(apply(getProbability(y$population), c(1, 3), sum), each = getNodeCount(y$city)), 
                           c(getNodeCount(y$city), getNodeCount(y$city), getSize(y$population))), 3, t), 
               c(getNodeCount(y$city), getNodeCount(y$city), getSize(y$population))) # Sum{j} P^1_{ij}
  #prBrsB <- aperm(apply(getProbability(y$population), c(1, 3), function(x) x/sum(x)), c(2, 1, 3)) # P^1_{ij}/Sum_{j} P^1_{ij}
  setProbability(population) <- getProbability(y$population)*(rsA/rsB)
  setUtility(population) <- getUtility(y$population)+sigma*log(rsA/rsB) #sigma*
  return(list(city = city, population = population))
}

emptyDataFrame <- function(varnames, obs = 10) {
  n <- length(varnames)
  mat <- matrix(I("-"), obs, n)
  colnames(mat) <- varnames
  as.data.frame(mat)
}

cityDataFrame <- function(city, population) {
  # x simulation
  wagerate <- array(rep(t(getWageRate(population)), each = getNodeCount(city)), dim = c(getNodeCount(city), getNodeCount(city), getSize(population)))
  y <- data.frame(
    Node = 1:getNodeCount(city),
    Supply = getArea(city),
    Demand = apply(getProbability(population)*getArgMax(population)[ , , , 4], 1, sum, na.rm = TRUE),
    Price = getVertexPrice(city),
    WorkerShare = apply(getProbability(population), 2, sum, na.rm = TRUE)/getSize(population),
    ResidentShare = apply(getProbability(population), 1, sum, na.rm = TRUE)/getSize(population),
    "Residential Density" = apply(getProbability(population), 1, sum, na.rm = TRUE)/getArea(city),
    Output = apply(getProbability(population)*wagerate*getArgMax(population)[ , , , 1], 2, sum, na.rm = TRUE)
  )
  return(y)
}

pathDataFrame <- function(x, population = NULL) {
  # x city
  # returns a path dataframe
  v <- length(V(getGraph(x)))
  e <- length(E(getGraph(x)))
  m <- matrix(list(), v^2, 1) # column path matrix
  cost <- matrix(list(), v^2, 1)
  totcost <- matrix(0, v^2, 1)
  time <- matrix(list(), v^2, 1)
  tottime <- matrix(0, v^2, 1)
  edgepathlist <- vec_shortest_paths(1:v, x) # Store edge ids of the paths
  pathids <- matrix(NA, v^2, 1)
  for (i in 1:v) {
    for (j in 1:v) {
      m[[as.path.id(x, from = i, to = j), 1]] <- list(as_ids(edgepathlist[[i]][[j]]))
      pathids[as.path.id(x, from = i, to = j), 1] <- as.path.id(x, from = i, to = j)
      cost[[as.path.id(x, from = i, to = j), 1]] <- as.list(
        (getEdgePath(x)[, as.path.id(x, i, j)]*get.edge.attribute(getGraph(x), "cost"))[getEdgePath(x)[, as.path.id(x, i, j)]*get.edge.attribute(getGraph(x), "cost")>0])
      totcost[as.path.id(x, i, j), 1] <- getEdgePath(x)[, as.path.id(x, i, j)]%*%get.edge.attribute(getGraph(x), "cost")+getCost(x)[i, i]+getCost(x)[j, j]
      time[[as.path.id(x, from = i, to = j), 1]] <- as.list(
        (getEdgePath(x)[, as.path.id(x, i, j)]*get.edge.attribute(getGraph(x), "time"))[getEdgePath(x)[, as.path.id(x, i, j)]*get.edge.attribute(getGraph(x), "time")>0])
      tottime[as.path.id(x, i, j), 1] <- getEdgePath(x)[, as.path.id(x, i, j)]%*%get.edge.attribute(getGraph(x), "time")+getTime(x)[i, i]+getTime(x)[j, j]
    }
  }
  df <- data.frame(origin = rep(1:v, each = v), 
                   destination = rep(1:v, times = v), 
                   "path id" = pathids, 
                   link = m, 
                   cost = cost, 
                   "total cost" = totcost,
                   time = time,
                   "total time" = tottime)
  return(df)
}

populationDataFrame <- function(population, city) {
  if (is.null(population)) return()
  mergeList <- function(x, y){
    df <- merge(x, y, by = intersect(names(x), names(y)))
    return(df)
  }
  df.wagerate.u <- array2df(array(rep(t(getUnderlyingWageRate(population)), each = getNodeCount(city)), dim = c(getNodeCount(city), getNodeCount(city), getSize(population))), label.x = "Under. Wage Rate")
  df.wagerate <- array2df(array(rep(t(getWageRate(population)), each = getNodeCount(city)), dim = c(getNodeCount(city), getNodeCount(city), getSize(population))), label.x = "Wage Rate")
  df.vot <- array2df(getVoT(population), label.x = "VoTT")
  df.prob <- array2df(getProbability(population), label.x = "Pr")
  df.vou <- array2df(getVoU(population), label.x = "VoU")
  df.u <- array2df(getUtility(population), label.x = "Ind utility")
  df.x1 <- array2df(getArgMax(population)[ , , , 1], label.x = "x1")
  df.x2 <- array2df(getArgMax(population)[ , , , 2], label.x = "x2")
  df.x3 <- array2df(getArgMax(population)[ , , , 3], label.x = "x3")
  df.x4 <- array2df(getArgMax(population)[ , , , 4], label.x = "x4")
  df.x5 <- array2df(getArgMax(population)[ , , , 5], label.x = "x5")
  df <- Reduce(mergeList, list(df.prob, df.wagerate.u, df.wagerate, df.vot, df.vou, df.u, df.x1, df.x2, df.x3, df.x4, df.x5))
  df <- df[ , c("d3", "d1", "d2", "Pr", "Under. Wage Rate", "Wage Rate", "VoTT", "VoU", "Ind utility", "x1", "x2", "x3", "x4", "x5")]
  df <- df[order(df$d3, df$d1, df$d2), ]
  names(df)[1:3] <- c("n", "i", "j")
  row.names(df) <- NULL
  return(df)
}

weighted.densities <- function(x, y, wx, wy, main = NULL, xlab = NULL, 
                               ylim = range(c(density(x, weights = wx)$y, density(y, weights = wy)$y)), ...) {
  plot(density(x,
               weights = wx), 
       col = rgb(1, 0, 0, 0.5),
       main = main,
       xlab = xlab,
       ylim = ylim,
       ...)
  lines(density(y,
                weights = wy), 
        col = rgb(0, 0, 1, 0.5),
        main = NULL,
        xlab = xlab)
}

weighted.histograms <- function(x, y, wx, wy, main = NULL, xlab = NULL) {
  histx <- weighted.hist(x, 
                         w = wx,
                         plot = FALSE,
                         freq = FALSE)
  histy <- weighted.hist(y, 
                         w = wy,
                         plot = FALSE,
                         freq = FALSE)
  limit <- range(histx$breaks, histy$breaks)
  delta <- min(histx$breaks[2]-histx$breaks[1], histy$breaks[2]-histy$breaks[1])
  breaks <- c(seq(limit[1], limit[2], by = delta), max(seq(limit[1], limit[2], by = delta)) + delta)
  weighted.hist(x, 
                w = wx, 
                breaks = breaks,
                col = rgb(1, 0, 0, 0.5), 
                border = FALSE,
                freq = FALSE,
                main = main, 
                xlab = xlab)
  weighted.hist(y, 
                w = wy,
                breaks = breaks,
                col = rgb(0, 0, 1, 0.5), 
                border = FALSE,
                freq = FALSE,
                add = TRUE)
}