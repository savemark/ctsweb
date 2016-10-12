library(shiny)
library(BB)
library(igraph)
library(plyr)
library(tripack)
library(lattice)
library(fields)
library(mgcv)
library(arrayhelpers)
library(plotrix)
library(akima)
library(plot3D)

source("data.R")
source("city.R")
source("population.R")
source("utilityOptimClosure.R")
source("simulation.R")
source("helpers.R")

shinyServer(
  function(input, output, session) {
    source("reactives.R", local = T)
    source("plots.R", local = T)
    source("prints.R", local = T)
    source("tables.R", local = T)
  }
)