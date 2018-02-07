probabilityClosure <- function(parameter, type = c("logit", "maximum"), margin = 3) {
  # x indirect utility, 3-dimensional array
  # returns a list with a density function
  type <- match.arg(type)
  logit <- function(x) {
    x.max <- apply(x, margin, max)
    y <- exp({sweep(x, margin, x.max)}/parameter[1])
    y.sum <- apply(y, margin, sum, na.rm = TRUE)
    pr <- sweep(y, margin, y.sum, FUN = "/")
    return(pr)
  }
  maximum <- function(x) {
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