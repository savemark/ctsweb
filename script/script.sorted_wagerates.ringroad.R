library(profvis)

source("data.R")
source("city.R")
source("population.R")
source("utility.R")
source("probability.R")
source("spillover.R")
source("simulation.R")
source("helpers.R")
#profvis({
path <- file.path("c:", "Users", "csave", "Dropbox", "Is it OK to ignore land use", "Default geometry", "Ring road with sorted wage rates")
scale <- 40 # km
delta <- 0.01 # variance param
eps <- 1 # Spillover epsilon
tau <- 0.7 # Tax rate 
n <- 1000 # Number of choice sets
guess <- 100 # Land price guess
param <- c(0.4, 0.48, 0.12, 0.05, tau, 0, 16)
ci <- city(scale*xy, adjacency, mode = "undirected") #ci <- cityA <- cityB <- city.delunay(25*matrix(c(runif(30), runif(30)), 30, 2))
scenario <- "permutation"
linkids <- 1:10 # c(1:10, 12, 14, 16, 18, 20, 22)
alt_speed <- 50
alt_costfactor <- 1.5
base_speed <- matrix(40, 1, getEdgeCount(ci))
base_costfactor <- matrix(1.5, 1, getEdgeCount(ci))
if (scenario == "default") {
  alternative_speed <- matrix(40, 1, getEdgeCount(ci))
  alternative_costfactor <- matrix(1.5, 1, getEdgeCount(ci))
  ids <- linkids
  alternative_speed[1, ids] <- alt_speed
  alternative_costfactor[1, ids] <- alt_costfactor
} else if (scenario == "permutation") {
  alternative_speed <- matrix(40, length(linkids), getEdgeCount(ci))
  alternative_costfactor <- matrix(1.5, length(linkids), getEdgeCount(ci))
  ids <- as.numeric(linkids)
  for (i in seq_along(ids)) {
    alternative_speed[i, ids[i]] <- alt_speed
    alternative_costfactor[i, ids[i]] <- alt_costfactor
  }
}
speeds <- rbind(base_speed, alternative_speed)
costfactors <- rbind(base_costfactor, alternative_costfactor)
weights <- abind(speeds, costfactors, along = 3) # Network weights
weights[1:11, c(25, 26, 28, 30, 32, 34, 36, 38, 41, 42, 44, 46, 48, 50, 52, 54), 1] <- 0
weights[1:11, c(25, 26, 28, 30, 32, 34, 36, 38, 41, 42, 44, 46, 48, 50, 52, 54), 2] <- Inf
weights[2:11, c(25, 26, 28, 30, 32, 34, 36, 38), 1] <- 40
weights[2:11, c(25, 26, 28, 30, 32, 34, 36, 38), 2] <- 1.5
pop <- population(n, getNodeCount(ci), upperbound = 2*10^6)
setWageRate(pop) <- t(apply(getWageRate(pop), 1, sort, decreasing = TRUE))
#setWageRate(pop) <- matrix(sort(getWageRate(pop), decreasing = TRUE), 1000, 33)
setUnderlyingWageRate(pop) <- getWageRate(pop)
utilfunctions <- utilityWrapper(param)
probability <- probabilityClosure(delta, type = "logit")
spillover <- spilloverClosure(eps, getArea(ci), b = 1, type = "fromAll")
ec <- economy(ci, pop, weights, utilfunctions, probability, spillover)
sim <- simulate(ec, guess)
sim_fixed <- simulate(ec, guess, fixed.landuse = TRUE)

plot(sim, type = "argmax", index = 4)
plot(sim, type = "wagerate")

# CSV ##################################################################
write.table(format(getROAHBenefitsWithTax(sim, tau), scientific = FALSE), 
            file = paste(path, "ROAHBenefitsWithTax2.csv", sep = "/"), sep = ",")
write.table(format(getEquivalentVariation(sim, delta), scientific = FALSE), 
            file = paste(path, "EquivalentVariation2.csv", sep = "/"), sep = ",")
write.table(format(getAvarages(sim), scientific = FALSE), 
            file = paste(path, "Averages2.csv", sep = "/"), sep = ",")
write.table(format(getTopDownBenefits(sim, tau), scientific = FALSE), 
            file = paste(path, "TopDownBenefits2.csv", sep = "/"), sep = ",")
write.table(format(getRanking(sim, tau), scientific = FALSE), 
            file = paste(path, "Ranking2.csv", sep = "/"), sep = ",")

write.table(format(getROAHBenefitsWithTax(sim_fixed, tau), scientific = FALSE), 
            file = paste(path, "ROAHBenefitsWithTax_fixed2.csv", sep = "/"), sep = ",")
write.table(format(getEquivalentVariation(sim_fixed, delta), scientific = FALSE), 
            file = paste(path, "EquivalentVariation_fixed2.csv", sep = "/"), sep = ",")
write.table(format(getAvarages(sim_fixed), scientific = FALSE), 
            file = paste(path, "Averages_fixed2.csv", sep = "/"), sep = ",")
write.table(format(getTopDownBenefits(sim_fixed, tau), scientific = FALSE), 
            file = paste(path, "TopDownBenefits_fixed2.csv", sep = "/"), sep = ",")
write.table(format(getRanking(sim_fixed, tau), scientific = FALSE), 
            file = paste(path, "Ranking_fixed2.csv", sep = "/"), sep = ",")

# PLOTS ################################################################
plot(getROAHBenefitsWithTax(sim_fixed, tau)[7, ], 
     getROAHBenefitsWithTax(sim, tau)[7, ], xlab = "Fixed land use", ylab = "Non-Fixed land use")
abline(0, 1)

plot(getTotalBenefitsGivenBudget(sim, tau, 15)[ , "ACC C"], getTotalBenefitsGivenBudget(sim, tau, 15)[ , "ACC B"], type = "l", xlab = "Acc. costs", ylab = "Acc. benefits", col = "blue")
lines(getTotalBenefitsGivenBudget(sim_fixed, tau, 15)[ , "ACC C"], getTotalBenefitsGivenBudget(sim_fixed, tau, 15)[ , "ACC B"], col = "red")
legend("topleft", 
       c("NF", "F"), 
       lty = c(0, 0), col = c("blue", "red"), pch = 15)

# BOXPLOT ##############################################################
benefits <- getROAHBenefitsWithTax(sim, tau)[c(1,2,4,5,6), -1]
benefits_f <- getROAHBenefitsWithTax(sim_fixed, tau)[c(1,2,4,5,6), -1]
colnames(benefits) <- colnames(benefits, do.NULL = FALSE, prefix = "Sim.")
colnames(benefits_f) <- colnames(benefits_f, do.NULL = FALSE, prefix = "Sim.")
benefits <- cbind(LU = "NF", benefits)
benefits_f <- cbind(LU = "F", benefits_f)
benefits <- cbind(Variable = rownames(benefits), benefits)
benefits_f <- cbind(Variable = rownames(benefits_f), benefits_f)
rownames(benefits_f) <- rownames(benefits) <- NULL
df <- merge(benefits, benefits_f, all = TRUE)
for (i in 1:(dim(df)[2]-2)) {df[, (i+2)] <- as.numeric(as.character(df[, (i+2)]))}
df <- melt(df)
boxplot(formula = value ~ Variable, data = df, subset = LU == "NF",
        boxwex  = 0.25,
        at = 1:5 - 0.2,
        ylim = c(floor(min(df$value))-1, ceiling(max(df$value))+1),
        xaxt = "n",
        frame.plot = TRUE)
boxplot(formula = value ~ Variable, data = df, subset = LU == "F", 
        col = "grey",
        boxwex = 0.25,
        at = 1:5 + 0.2,
        xaxt = "n",
        add = TRUE)
axis(side = 1, at =  1:5, labels = c("TC", "LP", "TR", "TT", "WR"))
legend("topleft", 
       c("NF", "F"), 
       lty = c(0, 0), col = c("white", "grey"), pch = 15)

# SPLINE ###############################################################
spline <- smooth.spline(getRanking(sim, tau)[2, ], getRanking(sim, tau)[4, ])
spline_fixed <- smooth.spline(getRanking(sim_fixed, tau)[2, ], getRanking(sim_fixed, tau)[4, ])
plot(getRanking(sim_fixed, tau)[2, ], getRanking(sim_fixed, tau)[4, ], xlab = "Total link betweenness centrality", ylab = "ROAH Benefits per km", col = "red")
points(getRanking(sim, tau)[2, ], getRanking(sim, tau)[4, ], col = "blue")
lines(spline, col = "blue")
lines(spline_fixed, col = "red")
legend("topleft", 
       legend = c("F", "NF"), 
       lty = c(1,1), col = c("red", "blue"))

########################################################################