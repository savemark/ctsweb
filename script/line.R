library(profvis)
source("data.R", local = TRUE)
source("city.R")
source("population.R")
source("utility.R")
source("probability.R")
source("spillover.R")
source("simulation.R")
source("helpers.R")
#profvis({
  scale <- 40 # km
  delta <- 1 # variance param
  eps <- 0 # Spillover epsilon
  tau <- 0.3 # Tax rate 
  n <- 2 # Number of choice sets
  guess <- 100 # Land price guess
  param <- c(0.4, 0.48, 0.12, 0.05, tau, 0, 16, delta)
ci <- city(scale*xy, adjacency, mode = "undirected")
#ci <- cityA <- cityB <- city.delunay(25*matrix(c(runif(30), runif(30)), 30, 2))
guess <- rep(50, getNodeCount(ci))
setEdgeSpeed(ci) <- 50
setEdgeCostFactor(ci) <- 1.5
pop <- population(n, getNodeCount(ci), lowerbound = 2.5*10^5, upperbound = 2*10^6)
setUnderlyingWageRate(pop) <- getWageRate(pop)
utilfunctions <- utilityWrapper(param)
probability <- probabilityClosure(delta, type = "logit")
spillover <- spilloverClosure(eps, getArea(ci), b = 1, type = "fromAll")
# (guess, city, population, utility, probability, spillover, scale = NULL, avg.utility = NULL)"
simA <- simulation(guess,
                   ci,
                   pop,
                   utilfunctions,
                   probability = probability,
                   spillover = spillover)
simB <- simulation(guess,
                   ci,
                   pop,
                   utilfunctions,
                   probability = probability,
                   spillover = spillover,
                   avg.utility = 3)
simC <- simulation(rep(100000, getNodeCount(ci)),
                   ci,
                   pop,
                   utilfunctions,
                   probability = probability,
                   spillover = spillover,
                   avg.utility = 3.5)
popB <- simB$population
populationC <- simB$population
rsA <- array(apply(array(rep(apply(getProbability(simA$population), c(1, 3), sum), 
                             each = getNodeCount(cityA)), 
                         c(getNodeCount(cityA), getNodeCount(cityA), getSize(simA$population))), 3, t), 
             c(getNodeCount(cityA), getNodeCount(cityA), getSize(simA$population))) # Sum{j} P^0_{ij}
rsB <- array(apply(array(rep(apply(getProbability(populationC), c(1, 3), sum), 
                             each = getNodeCount(cityB)), 
                         c(getNodeCount(cityB), getNodeCount(cityB), getSize(populationC))), 3, t), 
             c(getNodeCount(cityB), getNodeCount(cityB), getSize(populationC))) # Sum{j} P^1_{ij}
#prBrsB <- aperm(apply(getProbability(simB$population), c(1, 3), function(x) x/sum(x)), c(2, 1, 3)) # P^1_{ij}/Sum_{j} P^1_{ij}
setProbability(populationC) <- getProbability(populationC)*(rsA/rsB)
setUtility(populationC) <- getUtility(populationC)+log(rsA/rsB)
cba2 <- roah3(list(population = popA, city = cityA, price = simA$solution$par, comfort = 0.05), 
              list(population = popB, city = cityB, price = simB$solution$par, comfort = 0.05))
cba3 <- roah4(list(population = popA, city = cityA, price = simA$solution$par, comfort = 0.05), 
              list(population = popB, city = cityB, price = simB$solution$par, comfort = 0.05))
equityPlot(cba3)

LTM <- logitTransportModel(list(city = cityA, population = popA), 0.004)
LEPA <- logElasticityProductionAccessibility(list(city = cityA, population = popA), list(city = cityB, population = popB), 0.004)
exact <-equivalentVariation(list(city = cityA, population = popA), 
            list(city = cityB, population = popB), 
            sigma = delta)
exact2 <-equivalentVariation(list(city = cityA, population = popA), 
            list(city = cityB, population = populationC), 
            sigma = delta)
deltalor <- sum(getArea(cityB)*simB$solution$par[1:getNodeCount(cityB)])-sum(getArea(cityA)*simA$solution$par[1:getNodeCount(cityA)])   

w1 <- t(apply(getProbability(popA), c(2, 3), sum))/sum(getProbability(popA))
wr1 <- getWageRate(popA)
w2 <- t(apply(getProbability(popB), c(2, 3), sum))/sum(getProbability(popB))
wr2 <- getWageRate(popB)
plot(density(getWageRate(pop)), lty = 3)
lines(density(wr1, weights = w1), col = "red")
lines(density(wr2, weights = w2), col = "blue")
legend("topright", "Original wage rate offers", lty = 3, title = "topright, inset = .02",
       inset = .02)

# work hours
plot(density(getArgMax(popA)[ , , , 1], weights = getProbability(popA)/sum(getProbability(popA))))
lines(density(getArgMax(popB)[ , , , 1], weights = getProbability(popB)/sum(getProbability(popB))), lty = 3)
avarages(list(city = cityA, population = popA), 
         list(city = cityB, population = popB))

})