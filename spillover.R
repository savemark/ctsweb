spilloverClosure <- function(epsilon, b = -1, type = c("fromAll")) {
  # epsilon parameter
  type <- match.arg(type)
  fromAll <- function(x, area, cost, time, vtts = 160) {
    # x probability
    cost[cost == 0] <- Inf
    time[time == 0] <- Inf
    n <- dim(x)[3]
    nj <- apply(x, 2, sum, na.rm = TRUE)
    factor <- (1+nj/area+apply(mapply(function(y) {nj*(cost[y, ]+vtts*time[y, ])^(b)*(area)^(-1)}, 1:nrow(x)), 2, sum))^epsilon
    factor <- rep(factor, each = n)
    return(factor)
  }
  switch(type,
         fromAll = list(f = fromAll)
         )
}