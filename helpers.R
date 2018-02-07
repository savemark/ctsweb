odDemand <- function(x, na.rm = TRUE) {
  # x probability
  odm <- apply(x, c(1, 2), sum, na.rm = TRUE)
  return(odm)
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