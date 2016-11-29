probabilityClosure <- function(parameter, type = "logit", margin = 3) {
  # x indirect utility, 3-dimensional array
  if (type == "logit") {
    density <- function(x) {
      d <- dim(x)
      y <- exp({x-array(rep(apply(x, 3, max), each = d[1]^2), dim = d)}/parameter[1])
      sfn <- apply(y, margin, sum, na.rm = TRUE)
      sfn <- array(rep(sfn, each = d[1]^2), dim = d)
      pr <- y/sfn
      return(pr)
    }
  } else if (type == "maximum") {
    density <- function(x, margin = 3) {
      maxIndicator <- function(x) {
        ifelse(x == max(x), 1, 0)
      }
      pr <- apply(x, margin, maxIndicator)
      dim(pr) <- dim(x)
      return(pr)
    }
  }
  return(list(density = density))
}

# old version:
probability <- function(x, type = "logit", ...) {
  # x indirect utility, 3-dimensional array
  maximum <- function(x, margin = 3) {
    maxIndicator <- function(x) {
      ifelse(x == max(x), 1, 0)
    }
    pr <- apply(x, margin, maxIndicator)
    dim(pr) <- dim(x)
    return(pr)
  }
  logit <- function(x, delta, margin = 3) {
    d <- dim(x)
    y <- exp({x-array(rep(apply(x, 3, max), each = d[1]^2), dim = d)}/delta)
    sfn <- apply(y, margin, sum, na.rm = TRUE)
    sfn <- array(rep(sfn, each = d[1]^2), dim = d)
    pr <- y/sfn
    return(pr)
  }
  switch(type,
         logit = logit(x, ...),
         maximum = maximum(x, ...)
  )
}