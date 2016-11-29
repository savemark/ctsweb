spilloverClosure <- function(epsilon, area, type = "FromAll") {
  # x probability
  if (type == "FromAllButOneself") {
    f <- function(x) {
      n <- dim(x)[3]
      nj <- sapply(1:n, function(m) {apply(x[, , -m], 2, sum)}) # matrix
      nj <- as.vector(t(nj))
      supply <- rep(area, n) # Need to repeat the supply
      factor <- (1+nj/supply)^epsilon
      return(factor)
    }
  } else if (type == "FromAll") {
    f <- function(x) {
      n <- dim(x)[3]
      nj <- apply(x, 2, sum, na.rm = TRUE)
      factor <- (1+nj/area)^epsilon
      factor <- rep(factor, each = n)
      return(factor)
    }
  }
  return(list(f = f))
}

# old version:
spillover <- function(city, population, epsilon, type = "FromAll") {
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