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
eps <- 0.03 # Spillover epsilon
tau <- 0.3 # Tax rate
n <- 500 # Number of choice sets
guess <- 1000 # Land price guess
param <- c(0.4, 0.48, 0.12, tau, 0, 16, delta)
ci <- city(scale*xy, adjacency, mode = "undirected")
base_speed_const <- 40
base_costfactor_const <- 1.5
base_comfort_const <- 0.05
alt_speed_const <- 40
alt_costfactor_const <- 1.5
alt_comfort_const <- 0.04
base_speed <- matrix(base_speed_const, 1, getEdgeCount(ci))
base_costfactor <- matrix(base_costfactor_const, 1, getEdgeCount(ci))
base_comfort <-  matrix(base_comfort_const, 1, getEdgeCount(ci))
alt_speed <- matrix(alt_speed_const, 1, getEdgeCount(ci))
alt_costfactor <- matrix(alt_costfactor_const, 1, getEdgeCount(ci))
alt_comfort <- matrix(alt_comfort_const, 1, getEdgeCount(ci))
# No Ring roads
base_speed[c(9, 10, 12, 14, 16, 18, 20, 22, 25, 26, 28, 30, 32, 34, 36, 38, 41, 42, 44, 46, 48, 50, 52, 54, 57, 58, 59, 60, 61, 62, 63, 64)] <- base_speed_const
base_costfactor[c(9, 10, 12, 14, 16, 18, 20, 22, 25, 26, 28, 30, 32, 34, 36, 38, 41, 42, 44, 46, 48, 50, 52, 54, 57, 58, 59, 60, 61, 62, 63, 64)] <- base_costfactor_const
base_comfort[c(9, 10, 12, 14, 16, 18, 20, 22, 25, 26, 28, 30, 32, 34, 36, 38, 41, 42, 44, 46, 48, 50, 52, 54, 57, 58, 59, 60, 61, 62, 63, 64)] <- base_comfort_const
# 1st ring road
base_speed_ringroad1 <- base_speed
base_costfactor_ringroad1 <- base_costfactor
base_comfort_ringroad1 <- base_comfort
base_speed_ringroad1[c(9, 10, 12, 14, 16, 18, 20, 22)] <- alt_speed_const
base_costfactor_ringroad1[c(9, 10, 12, 14, 16, 18, 20, 22)] <- alt_costfactor_const
base_comfort_ringroad1[c(9, 10, 12, 14, 16, 18, 20, 22)] <- alt_comfort_const
# 2nd ring road
base_speed_ringroad2 <- base_speed #_ringroad1
base_costfactor_ringroad2 <- base_costfactor #_ringroad1
base_comfort_ringroad2 <- base_comfort
base_speed_ringroad2[c(25, 26, 28, 30, 32, 34, 36, 38)] <- alt_speed_const
base_costfactor_ringroad2[c(25, 26, 28, 30, 32, 34, 36, 38)] <- alt_costfactor_const
base_comfort_ringroad2[c(25, 26, 28, 30, 32, 34, 36, 38)] <- alt_comfort_const
# 3rd ring road
base_speed_ringroad3 <- base_speed #_ringroad2
base_costfactor_ringroad3 <- base_costfactor #_ringroad2
base_comfort_ringroad3 <- base_comfort
base_speed_ringroad3[c(41, 42, 44, 46, 48, 50, 52, 54)] <- alt_speed_const
base_costfactor_ringroad3[c(41, 42, 44, 46, 48, 50, 52, 54)] <- alt_costfactor_const
base_comfort_ringroad3[c(41, 42, 44, 46, 48, 50, 52, 54)] <- alt_comfort_const
# 4th ring road
base_speed_ringroad4 <- base_speed #_ringroad3
base_costfactor_ringroad4 <- base_costfactor #_ringroad3
base_comfort_ringroad4 <- base_comfort
base_speed_ringroad4[c(57, 58, 59, 60, 61, 62, 63, 64)] <- alt_speed_const
base_costfactor_ringroad4[c(57, 58, 59, 60, 61, 62, 63, 64)] <- alt_costfactor_const
base_comfort_ringroad4[c(57, 58, 59, 60, 61, 62, 63, 64)] <- alt_comfort_const
# Weights
speeds <- rbind(base_speed, base_speed_ringroad1, base_speed_ringroad2, base_speed_ringroad3, base_speed_ringroad4)
costfactors <- rbind(base_costfactor, base_costfactor_ringroad1, base_costfactor_ringroad2, base_costfactor_ringroad3, base_costfactor_ringroad4)
comforts <- rbind(base_comfort, base_comfort_ringroad1, base_comfort_ringroad2, base_comfort_ringroad3, base_comfort_ringroad4)
weights <- abind(speeds, costfactors, comforts, along = 3)
pop <- population(n, getNodeCount(ci), nk = 4000, lowerbound = 2.5*10^5, upperbound = 10^6)
utilfunctions <- utilityWrapper(param)
probability <- probabilityClosure(delta, type = "logit")
spillover <- spilloverClosure(eps, getArea(ci), getDistance(ci), type = "fromAll")
ec <- economy(ci, pop, weights, utilfunctions, probability, spillover)

#sim0 <- simulate(ec, guess)
sim1 <- simulate(ec, guess)
sim2 <- simulate(ec, guess)

dim1 <- c(getNodeCount(sim1@sims[[1]]$city), getNodeCount(sim1@sims[[1]]$city), getNumberOfClasses(sim1@sims[[1]]$population))
kn5 <- array(rep(getSizeOfEachClass(sim1@sims[[5]]$population), each = 33), dim = dim1)
kn1 <- array(rep(getSizeOfEachClass(sim1@sims[[1]]$population), each = 33), dim = dim1)
sum(kn5*getProbability(sim1@sims[[5]]$population)*array(rep(t(getWageRate(sim1@sims[[5]]$population)), each = 33), dim = dim1)-kn1*getProbability(sim1@sims[[1]]$population)*array(rep(t(getWageRate(sim1@sims[[1]]$population)), each = 33), dim = dim1))
sim2 <- simulate(ec, guess, avg.utility = 2)
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

budget <- 2000
ckm <- 100
index <- match(getTotalBenefitsGivenBudget(sim_fixed, tau, ckm)[ , "Scenario"], getTotalBenefitsGivenBudget(sim, tau, ckm))
wwg <- getTotalBenefitsGivenBudget(sim, tau, ckm)[index, ]
accc <- cumsum(wwg[-1 , "C"])
accb <- cumsum(wwg[-1 , "B"])
wwg[-1 , 6] <- accc
wwg[-1 , 7] <- accb
nf <- getTotalBenefitsGivenBudget(sim, tau, ckm)
plot(getTotalBenefitsGivenBudget(sim, tau, ckm)[ , "ACC C"], getTotalBenefitsGivenBudget(sim, tau, ckm)[ , "ACC B"], type = "b", xlab = "Acc. costs", ylab = "Acc. benefits", col = "blue")
lines(getTotalBenefitsGivenBudget(sim_fixed, tau, ckm)[ , "ACC C"], getTotalBenefitsGivenBudget(sim_fixed, tau, ckm)[ , "ACC B"], type = "b", col = "red")
lines(wwg[ , "ACC C"], wwg[ , "ACC B"], type = "b", col = "black")
abline(v = budget, lty = 2, col = "gray60")
text(budget, 0, "Budget constraint", col = "gray60", adj = c(-0.1, 0))
legend("topleft", 
       c("NF", "F", "WWG"), 
       lty = c(0, 0), col = c("blue", "red", "black"), pch = 15)

# Benefit loss:
wwg[wwg[, "ACC C"] < budget, ]
nf[nf[, "ACC C"] < budget, ]
nf[nf[, "ACC C"] < budget, "ACC B"]-wwg[wwg[, "ACC C"] < budget, "ACC B"]
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