workerIncomeMatrix <- function(n, nodes, median, spread, days, h) {
  mean <- log(median)
  sd <- sqrt(2*(log(median*spread)-mean))
  income <- matrix(exp(rnorm(nodes*n, mean = mean, sd = sd))/{h*days}, n, nodes)
  return(income)
}

workerPreferenceMatrix <- function(n, nodes, mean = 0, sd = 1) {
  preference <- matrix(rnorm(nodes*nodes*n, mean, sd), n, nodes)
  return(preference)
}

utilityOptim <- function(p, w, c, t, alpha, beta, gamma, theta, tau, G, H, D) {
  v <- length(p)
  u <- matrix(NA, v, v)
  W <- matrix(NA, v, v)
  L <- matrix(NA, v, v)
  w <- matrix(w, v, v, byrow = TRUE)
  W <- {{alpha+gamma}*{24-t}*tau*w+beta*{c-G}}/{{alpha+beta+gamma}*w*tau}
  L <- {gamma*{{24-t}*w*tau-c+G}}/{p*{alpha+beta+gamma}}
  yu <- alpha*log(ifelse(tau*w*W-p*L-c+G>0, tau*w*W-p*L-c+G, NA))
  ltu <- beta*log(ifelse(24-W-t>0, 24-W-t, NA))
  luu <- gamma*log(ifelse(L>0, L, NA))
  tcu <- theta*t
  pref <- outer(H, D, "+")
  u <- yu + ltu + luu + tcu + pref
  list(u = u, ws = W, ld = L, yu = yu, ltu = ltu, luu = luu, tcu = tcu)
}

maxOfUtilityMatrix <- function(x) {
  umax <- suppressWarnings(max(x$u, na.rm = TRUE))
  #if (umax != -Inf) {
  index <- which(x$u == umax, arr.ind = TRUE)[1, ]
  ws <- x$ws[index[1], index[2]]
  ld <- x$ld[index[1], index[2]]
  yu <- x$yu[index[1], index[2]]
  ltu <- x$ltu[index[1], index[2]]
  luu <- x$luu[index[1], index[2]]
  tcu <- x$tcu[index[1], index[2]]
  #} else {
  #  index <- c(1,1)
  #  ws <- x$ws[index[1], index[2]]
  #  ld <- x$ld[index[1], index[2]]
  #  umax <- 0
  #}
  list(umax = umax,
       yu = yu,
       ltu = ltu,
       luu = luu,
       tcu = tcu,
       ws = ws, 
       ld = ld, 
       choice = index)
}

maxUtility <- function(p, w, c, t, alpha, beta, gamma, theta, tau,
                       G, H, D) {
  n <- nrow(w)
  umax <- vector(mode = "numeric", n)
  ws <- vector(mode = "numeric", n)
  ld <- vector(mode = "numeric", n)
  yu <- vector(mode = "numeric", n)
  ltu <- vector(mode = "numeric", n)
  luu <- vector(mode = "numeric", n)
  tcu <- vector(mode = "numeric", n)
  choice <- matrix(NA, n, 2)
  for (k in 1:n) {
    uo <- utilityOptim(p, w[k, ], c, t, alpha, beta, gamma, theta, tau, 
                       G, H[k, ], D[k, ])
    maxuo <- maxOfUtilityMatrix(uo)
    umax[k] <- maxuo$umax
    yu[k] <- maxuo$yu
    ltu[k] <- maxuo$ltu
    luu[k] <- maxuo$luu
    tcu[k] <- maxuo$tcu
    ws[k] <- maxuo$ws
    ld[k] <- maxuo$ld
    choice[k, ] <- maxuo$choice
  } 
  list(umax = umax, 
       yu = yu,
       ltu = ltu,
       luu = luu,
       tcu = tcu,
       ws = ws, 
       ld = ld, 
       choice = choice)
}

nodeLandDemand <- function(x, nodes) {
  y <- rep(0, nodes)
  obj <- tapply(x$ld, x$choice[,1], sum)
  y[as.numeric(names(obj))] <- as.vector(obj)
  y
}

taxReturn <- function(x, tau) {
  # Tax loop
  old.G <- -Inf
  new.G <- 0
  while (abs(old.tax.refund - new.tax.refund) >= eps) {
    old.G <- new.G
    population <- utility(price, population, city)
    population <- argmax(population)
    population <- tax(population)
    population <- agglomeration(population, city)
    new.tax.refund <- mean(population$tax)      
  }
}

tax <- function(x, tau) {
  tau <- parameter$tau
  function(population) {
    income = population$income[population$choice == 1]
    work.supply = population$work.supply[population$choice == 1]
    tax = {1-tau}*income*work.supply
    population$tax <- tax
    return(population)
  }
}

F <- function(p, supply, ...) {
  v <- length(p)
  obj <- maxUtility(p, ...)
  demand <- nodeLandDemand(obj, v)
  diff <- demand-supply
  return(diff)
}

simulationSummary = function(x, empty = FALSE) {
  n <- nrow(x$income)
  nodes <- length(x$demand)
  u <- x$u
  yu = x$yu
  ltu = x$ltu
  luu = x$luu
  tcu = x$tcu
  income <- x$income
  choice <- x$choice
  ld <- x$ld
  ws <- x$ws
  price <- x$price
  demand <- x$demand
  supply <- x$supply
  
  inc <- matrix(NA, n, 1)
  for (i in 1:n) {
    inc[i] <-  income[i, choice[i, 2]]   
  }
  
  home_node_count <- rep(0, nodes)
  work_node_count <- rep(0, nodes)
  home_node_count[count(choice[ , 1])$x] <- count(choice[ , 1])$freq
  work_node_count[count(choice[ , 2])$x] <- count(choice[ , 2])$freq
  
  #od.utility          <- apply(utility*choice, c(1,2), sum)
  #homeworkcount       <- apply(choice, c(1, 2), sum)    
  #ws.dist.node.avg    <- apply(ws*choice, 1, sum)/homecount
  #inc.dist.node.avg.r <- apply(income*choice, 1, sum)/homecount
  #inc.dist.node.avg.w <- apply(income*choice, 2, sum)/workcount
  
  #du <- deltaNaFill(y, u, n)
  #du.y <- deltaNaFill(y, yu, n)
  #du.lt <- deltaNaFill(y, ltu, n)
  #du.lu <- deltaNaFill(y, luu, n)
  #du.tc <- deltaNaFill(y, tcu, n)
  
  population <- data.frame(
    i = choice[ , 1],
    j = choice[ , 2],
    u = u,
    u.y = yu,
    u.lt = ltu,
    u.lu = luu,
    u.tc = tcu,
    #du = du,
    #du.y = du.y,
    #du.lt = du.lt,
    #du.lu = du.lu,
    #du.tc = du.tc,
    income = inc,
    working.hours = ws,
    area.demand = ld
  )
  
  city <- data.frame(
    price = price,
    land.supply = round(supply, 2), 
    land.demand = demand,
    residential.density = round(home_node_count/supply, 2), 
    worker.density = round(work_node_count/supply, 2)
    #"Inc/h avg (R)" = round(inc.dist.node.avg.r, 0),
    #"Inc/h avg (W)" = round(inc.dist.node.avg.w, 0),
    #"Daily work supply avg" = round(ws.dist.node.avg, 2)
  )
  
  return(
    list(
      population = population,
      city = city
    )
  )
}

widerEconomicBenefits <- function(x, y) {
  if(length(x$u) == length(y$u)) {
    du <- y$u - x$u
    du.y <- y$yu - x$yu
    du.lt <- y$ltu - x$ltu
    du.lu <- y$luu - x$luu
    du.tc <- y$tcu - x$tcu
    
    WEB <- data.frame(
      u = x$u,
      u.y = x$yu,
      u.lt = x$ltu,
      u.lu = x$luu,
      u.tc = x$tcu,
      du = du,
      du.y = du.y,
      du.lt = du.lt,
      du.lu = du.lu,
      du.tc = du.tc
      #income = inc,
      #working.hours = ws,
      #area.demand = ld
    )
    return(WEB)
  } else {
    return()
  }
}
