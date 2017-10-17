spilloverClosure <- function(epsilon, area, distance, b = 1, type = c("fromAll", "fromAllButOneself", "sameEverywhere")) {
  # epsilon parameter
  type <- match.arg(type)
  fromAll <- function(x) {
    # x probability
    n <- dim(x)[3]
    nj <- apply(x, 2, sum, na.rm = TRUE)
    distance[distance == 0] <- Inf
    factor <- (1+nj/area+apply(mapply(function(y) {nj*(distance[y, ])^(-b)*(area)^(-1)}, 1:nrow(x)), 2, sum))^epsilon
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
    factor <- (1+b*nj/supply)^epsilon 
    return(factor)
  }
  sameEverywhere <- function(x) {
    # x probability
    n <- dim(x)[3]
    factor <- (1+b*n/area)^epsilon
    factor <- rep(factor, each = n)
    return(factor)
  }
  switch(type,
         fromAll = list(f = fromAll),
         fromAllButOneself = list(f = fromAllButOneself),
         sameEverywhere = list(f = sameEverywhere)
  )
}

# spilloverClosure <- function(epsilon, area, b = 1, type = c("fromAll", "fromAllButOneself", "sameEverywhere")) {
#   # epsilon parameter
#   # Returns a N*|V| vector
#   type <- match.arg(type)
#   fromAll <- function(x) {
#     # x probability
#     n <- dim(x)[3]
#     njmax <- apply(x, 2, max)
#     njmaxa <- array(rep(njmax, each = dim(x)[1]), dim = dim(x))
#     nj <- apply(exp(x-njmaxa), 2, sum, na.rm = TRUE)
#     factor <- b*(1+(log(nj)+njmax)/sum(log(nj)+njmax))
#     factor <- rep(factor, each = n)
#     return(factor)
#   }
#   fromAllButOneself <- function(x) {
#     # x probability
#     # I have checked this function
#     n <- dim(x)[3]
#     nj <- sapply(1:n, function(m) {apply(x[, , -m], 2, sum)}, simplify = "matrix") # matrix
#     nj <- as.vector(t(nj))
#     supply <- rep(area, n) # Need to repeat the supply
#     factor <- b*(1+nj/supply)^epsilon
#     return(factor)
#   }
#   sameEverywhere <- function(x) {
#     # x probability
#     n <- dim(x)[3]
#     factor <- b*(1+n/area)^epsilon
#     factor <- rep(factor, each = n)
#     return(factor)
#   }
#   switch(type,
#          fromAll = list(f = fromAll),
#          fromAllButOneself = list(f = fromAllButOneself),
#          sameEverywhere = list(f = sameEverywhere)
#   )
# }