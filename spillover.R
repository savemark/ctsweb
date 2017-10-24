spilloverClosure <- function(epsilon, area, distance, b = 1, type = c("fromAll")) {
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
  switch(type,
         fromAll = list(f = fromAll)
         )
}