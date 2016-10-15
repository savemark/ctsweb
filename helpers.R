odDemand <- function(x) {
  # x probability
  odm <- apply(x, c(1, 2), sum, na.rm = TRUE)
  return(odm)
}

roth2 <- function(x, y, comfort) {
  # x, y list(population, city)
  # comfort vector, two values
  votMeanA <- apply(getProbability(x$population)*getMarginalEffect(x$population)[ , , , 3]/getMarginalEffect(x$population)[ , , , 1], c(1,2), 
                    sum, na.rm = TRUE)/apply(getProbability(x$population), c(1,2), sum, na.rm = TRUE)
  marginalUtilityOfIncomeMeanA <- apply(getProbability(x$population)*(1/getMarginalEffect(y$population)[ , , , 1]), c(1,2), 
                                       sum, na.rm = TRUE)/apply(getProbability(x$population), c(1,2), sum, na.rm = TRUE)
  cs <- sum(0.5*(odDemand(getProbability(x$population))+odDemand(getProbability(y$population)))*(-(getCost(y$city)-getCost(x$city))
                                                                                                   +votMeanA*(getTime(y$city)-getTime(x$city))
                                                                                                   +marginalUtilityOfIncomeMeanA*(-getTime(x$city))*(comfort[2]-comfort[1])))
  return(cs)
}

roah3 <- function(x, y) {
  # With comfort, land prices, wage rates
  # x, y list(population, city, price, comfort)
  N <- getSize(x$population)
  V <- getNodeCount(x$city)
  cost.x <- array(getCost(x$city), dim = c(V, V, N))
  cost.y <- array(getCost(y$city), dim = c(V, V, N))
  time.x <- array(getTime(x$city), dim = c(V, V, N))
  time.y <- array(getTime(y$city), dim = c(V, V, N))
  price.x <- array(x$price, dim = c(V, V, N))
  price.y <- array(y$price, dim = c(V, V, N))
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
  # x, y list(population, city, price, comfort)
  # sapply(1:n, function(m) {apply(getProbability(population)[, , -m], 2, sum)})
  N <- getSize(x$population)
  V <- getNodeCount(x$city)
  price.x <- matrix(x$price, V, V)
  price.y <- matrix(y$price, V, V)
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

equityPlot <- function(x) { # x comes from roah4
  rs <- rowSums(x[ , 2:6])
  tab <- cbind(x, total.utility = rs)
  tab <- as.data.frame(tab)
  range <- cut(tab[ , 1], breaks = quantile(tab[ , 1], probs = seq(0, 1, 0.2)), include.lowest = T, dig.lab = 5)
  tab <- cbind(tab, range)
  tab.ag <- aggregate(tab[ , 2:7], by = list(income = tab$range), FUN = sum)
  tab.gather <- gather(tab.ag, value = "utility", key = "variable", 2:6)
  g <- ggplot(tab.gather, aes(x = income, y = utility)) + geom_bar(aes(weight = utility, fill = variable), stat = "identity", position = "dodge")
  g <- g + ggtitle("Utility in monetary unit per income class and per variable") + theme_bw()
  return(g)
}

tax <- function(city, population, tau) {
  nodes <- getNodeCount(city)
  N <- getSize(population)
  wagerate <- array(rep(t(getWageRate(population)), each = nodes), dim = c(nodes, nodes, N))
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
               "Value of Travel Time" = c(mean(votMeanA), mean(votMeanB)))
  )
}

logitTransportModel <- function(x, mu) {
  N <- getSize(x$population)
  nodes <- getNodeCount(x$city)
  votMean <- apply(getProbability(x$population)*getVoT(x$population), c(1,2), sum)/apply(getProbability(x$population), c(1,2), sum)
  gc <- -getCost(x$city)+votMean*getTime(x$city)
  Hi <- matrix(apply(getProbability(x$population), 1, sum), nodes, nodes)/N # Number of residents in zone i as matrix with identical rows
  Wk <- t(matrix(apply(getProbability(x$population), 2, sum), nodes, nodes))/N # Number of workers in zone k as matrix with identical cols
  rowsum <- matrix(apply(Wk*exp(mu*gc), 1, sum), nodes, nodes)
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
  #votMean.y <- apply(getProbability(y$population)*getVoT(y$population), c(1,2), sum)/apply(getProbability(y$population), c(1,2), sum)
  Hi.x <- apply(getProbability(x$population), 1, sum) # Number of residents in zone i 
  Hi.y <- apply(getProbability(y$population), 1, sum) # Number of residents in zone i 
  Wk.x <- t(matrix(apply(getProbability(x$population), 2, sum), nodes, nodes)) # Number of workers in zone k as matrix with identical cols
  Wk.y <- t(matrix(apply(getProbability(y$population), 2, sum), nodes, nodes)) # Number of workers in zone k as matrix with identical cols
  gc.x <- -getCost(x$city)+votMean.x*getTime(x$city)
  gc.y <- -getCost(y$city)+votMean.x*getTime(y$city) # votMean.x (before the policy)
  accessibility.x <- sum((1/mu)*Hi.x*log(apply(Wk.x*exp(mu*gc.x), 1, sum)))
  accessibility.y <- sum((1/mu)*Hi.y*log(apply(Wk.y*exp(mu*gc.y), 1, sum)))
  log.accessibility.ratio <- log(accessibility.y/accessibility.x)
  production.x <- sum(getArgMax(x$population)[ , , , 1]*wagerate.x*getProbability(x$population))
  production.y <- sum(getArgMax(y$population)[ , , , 1]*wagerate.y*getProbability(y$population))
  log.production.ratio <- log(production.y/production.x)
  return(log.production.ratio/log.accessibility.ratio)
}

ev2 <- function(x, y, sigma) {
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

emptyDataFrame <- function(varnames, obs = 10) {
  n <- length(varnames)
  mat <- matrix(I("-"), obs, n)
  colnames(mat) <- varnames
  as.data.frame(mat)
}

cityDataFrame <- function(price, population, city, digits = 4) {
  # x simulation
  nodes <- getNodeCount(city)
  N <- getSize(population)
  supply <- getArea(city)
  demand <- apply(getProbability(population)*getArgMax(population)[ , , , 4], 1, sum, na.rm = TRUE)
  node_productivity <- rep(0, nodes)
  home_node_count <- apply(getProbability(population), 1, sum, na.rm = TRUE)
  home_node_share <- apply(getProbability(population), 1, sum, na.rm = TRUE)/dim(getProbability(population))[3]
  work_node_share <- apply(getProbability(population), 2, sum, na.rm = TRUE)/dim(getProbability(population))[3]
  wagerate <- array(rep(t(getWageRate(population)), each = nodes), dim = c(nodes, nodes, N))
  production <- apply(getProbability(population)*wagerate*getArgMax(population)[ , , , 1], 2, sum, na.rm = TRUE)
  y <- data.frame(
    Node = 1:nodes,
    Supply = round(supply, digits),
    Demand = round(demand, digits),
    Price = round(price, digits),
    WorkerShare = round(work_node_share, digits),
    ResidentShare = round(home_node_share, digits),
    "Residential Density" = round(home_node_count/supply, digits),
    Production = round(production, digits)
    #prod.per.worker = round(prod.per.worker, 0)
    #"Inc/h avg (R)" = round(inc.dist.node.avg.r, 0),
    #"Inc/h avg (W)" = round(inc.dist.node.avg.w, 0),
    #"Daily work supply avg" = round(ws.dist.node.avg, 2)
  )
  return(y)
}

populationDataFrame <- function(population, city, digits = 4) {
  mergeList <- function(x, y){
    df <- merge(x, y, by = intersect(names(x), names(y)))
    return(df)
  }
  nodes <- getNodeCount(city)
  N <- getSize(population)
  df.wagerate <- array2df(array(rep(t(round(getWageRate(population), 0)), each = nodes), dim = c(nodes, nodes, N)), label.x = "Wage Rate")
  df.vot <- array2df(round(getVoT(population), digits), label.x = "VoTT")
  df.prob <- array2df(signif(getProbability(population), digits), label.x = "Pr")
  df.vou <- array2df(round(getVoU(population), digits), label.x = "VoU")
  df.u <- array2df(round(getUtility(population), digits), label.x = "Ind utility")
  df.x1 <- array2df(round(getArgMax(population)[ , , , 1], digits), label.x = "x1")
  df.x2 <- array2df(round(getArgMax(population)[ , , , 2], digits), label.x = "x2")
  df.x3 <- array2df(round(getArgMax(population)[ , , , 3], digits), label.x = "x3")
  df.x4 <- array2df(round(getArgMax(population)[ , , , 4], digits), label.x = "x4")
  df.x5 <- array2df(round(getArgMax(population)[ , , , 5], digits), label.x = "x5")
  df <- Reduce(mergeList, list(df.prob, df.wagerate, df.vot, df.vou, df.u, df.x1, df.x2, df.x3, df.x4, df.x5))
  df <- df[ , c("d3", "d1", "d2", "Pr", "Wage Rate", "VoTT", "VoU", "Ind utility", "x1", "x2", "x3", "x4", "x5")]
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