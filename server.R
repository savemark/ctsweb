packages <- c("shiny", "BB", "igraph", "plyr", "deldir", "truncdist", "lattice", "fields", "mgcv", "arrayhelpers", 
              "plotrix", "akima", "plot3D", "tidyr", "ggplot2", "ineq", "shinythemes", "RColorBrewer", "reshape2", "stringi", 
              "gtools", "rgl", "abind")

for (package in packages) {
  if (!require(package, character.only = TRUE, quietly = FALSE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

source("data.R")
source("city.R")
source("population.R")
source("utility.R")
source("probability.R")
source("spillover.R")
source("simulation.R")
source("economy.R")
source("helpers.R")

shinyServer(
  function(input, output, session) {
    source("reactives.R", local = TRUE)
    source("plots.R", local = TRUE)
    source("prints.R", local = TRUE)
    source("tables.R", local = TRUE)
  }
)