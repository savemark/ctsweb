spilloverClosure <- function(epsilon, area, b = 1, type = c("fromAll", "fromAllButOneself", "sameEverywhere")) {
  # epsilon parameter
  type <- match.arg(type)
  fromAll <- function(x) {
    # x probability
    n <- dim(x)[3]
    nj <- apply(x, 2, sum, na.rm = TRUE)
    factor <- b*(1+nj/area)^epsilon
    factor <- rep(factor, each = n)
    return(factor)
  }
  fromAllButOneself <- function(x) {
    # x probability
    # I have checked this function
    n <- dim(x)[3]
    nj <- sapply(1:n, function(m) {apply(x[, , -m], 2, sum)}, simplify = "matrix") # matrix
    nj <- as.vector(t(nj))
    supply <- rep(area, n) # Need to repeat the supply
    factor <- b*(1+nj/supply)^epsilon 
    return(factor)
  }
  sameEverywhere <- function(x) {
    # x probability
    n <- dim(x)[3]
    factor <- b*(1+n/area)^epsilon
    factor <- rep(factor, each = n)
    return(factor)
  }
  switch(type,
         fromAll = list(f = fromAll),
         fromAllButOneself = list(f = fromAllButOneself),
         sameEverywhere = list(f = sameEverywhere)
  )
}

# old version:
# spillover <- function(city, population, epsilon, type = "FromAll") {
#   n <- getSize(population)
#   knowledgeSpilloverFromAllButOneself <- function(city, population, epsilon) {
#     nj <- sapply(1:n, function(m) {apply(getProbability(population)[, , -m], 2, sum)}) # matrix
#     nj <- as.vector(t(nj))
#     supply <- rep(getArea(city), n) # Need to repeat the supply
#     factor <- (1+nj/supply)^epsilon
#     return(factor)
#   }
#   knowledgeSpilloverFromAll <- function(city, population, epsilon) {
#     nj <- apply(getProbability(population), 2, sum, na.rm = TRUE)
#     factor <- (1+nj/getArea(city))^epsilon
#     factor <- rep(factor, each = n)
#     return(factor)
#   }
#   switch(type,
#          FromAllButOneself = knowledgeSpilloverFromAllButOneself(city, population, epsilon),
#          FromAll = knowledgeSpilloverFromAll(city, population, epsilon)
#   )
# }