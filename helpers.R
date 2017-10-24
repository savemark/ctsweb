odDemand <- function(x) {
  # x probability
  odm <- apply(x, c(1, 2), sum, na.rm = TRUE)
  return(odm)
}

roah4 <- function(x, y) {
  # x, y list(population, city)
  # sapply(1:n, function(m) {apply(getProbability(population)[, , -m], 2, sum)})
  price.x <- matrix(getVertexPrice(x$city), getNodeCount(x$city), getNodeCount(x$city))
  price.y <- matrix(getVertexPrice(y$city), getNodeCount(y$city), getNodeCount(y$city))
  wage.x <- array(rep(t(getWageRate(x$population)), each = getNodeCount(x$city)), dim = c(getNodeCount(x$city), getNodeCount(x$city), getNumberOfClasses(x$population)))
  wage.y <- array(rep(t(getWageRate(y$population)), each = getNodeCount(y$city)), dim = c(getNodeCount(y$city), getNodeCount(y$city), getNumberOfClasses(y$population)))
  margin1 <- getMarginalEffect(x$population)[ , , , 1] # marginal utility of income
  margin2 <- getMarginalEffect(x$population)[ , , , 2]
  margin3 <- getMarginalEffect(x$population)[ , , , 3]
  margin4 <- getMarginalEffect(x$population)[ , , , 4]
  margin5 <- getMarginalEffect(x$population)[ , , , 5]
  income <- sapply(1:getNumberOfClasses(x$population), function(m) {sum(getProbability(x$population)[ , , m]*wage.x[ , , m]*getArgMax(x$population)[ , , m, 1])})
  roah1 <- sapply(1:getNumberOfClasses(x$population), function(m) {sum(0.5*(getProbability(x$population)[ , , m]+getProbability(y$population)[ , , m])*(-1)*(getCost(y$city)-getCost(x$city)))}) 
  roah2 <- sapply(1:getNumberOfClasses(x$population), function(m) {sum(0.5*(getProbability(x$population)[ , , m]+getProbability(y$population)[ , , m])*((margin3[ , , m]/margin1[ , , m])*(getTime(y$city)-getTime(x$city))))})
  roah3 <- sapply(1:getNumberOfClasses(x$population), function(m) {sum(0.5*(getProbability(x$population)[ , , m]+getProbability(y$population)[ , , m])*((-getTime(x$city)/margin1[ , , m])*(getComfort(y$city)-getComfort(x$city))))})
  roah4 <- sapply(1:getNumberOfClasses(x$population), function(m) {sum(0.5*(getProbability(x$population)[ , , m]+getProbability(y$population)[ , , m])*((margin4[ , , m]/margin1[ , , m])*(price.y-price.x)))})
  roah5 <- sapply(1:getNumberOfClasses(x$population), function(m) {sum(0.5*(getProbability(x$population)[ , , m]+getProbability(y$population)[ , , m])*((margin5[ , , m]/margin1[ , , m])*(wage.y[ , , m]-wage.x[ , , m])))})
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