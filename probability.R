probabilityClosure <- function(parameter, type = c("logit", "maximum"), margin = 3) {
  # x indirect utility, 3-dimensional array
  # returns a list with a density function
  type <- match.arg(type)
  logit <- function(x) {
    d <- dim(x)
    y <- exp({x-array(rep(apply(x, 3, max), each = d[1]^2), dim = d)}/parameter[1])
    sfn <- apply(y, margin, sum, na.rm = TRUE)
    sfn <- array(rep(sfn, each = d[1]^2), dim = d)
    pr <- y/sfn
    return(pr)
  }
  maximum <- function(x, margin = 3) {
    maxIndicator <- function(x) {
      ifelse(x == max(x), 1, 0)
    }
    pr <- apply(x, margin, maxIndicator)
    dim(pr) <- dim(x)
    return(pr)
  }
  switch(type,
         logit = list(density = logit),
         maximum = list(density = maximum)
  )
}