setClass("igraph")
setClass("voronoi")

city <- setClass("City",
                 slots = c(coordinate = "matrix",
                           nodecount = "numeric",
                           adjacency = "matrix",
                           speed = "numeric",
                           costfactor = "numeric",
                           network = "igraph",
                           cells = "voronoi",
                           distance = "matrix",
                           edgepath = "matrix", # Edge-path matrix
                           time = "matrix",
                           cost = "matrix"),
                 validity = function(object) {
                   if (nrow(object@coordinate) != nrow(object@adjacency))
                     paste("The number of rows of x (", nrow(object@coordinate), ") and y (", nrow(object@adjacency), ") should have been equal", sep = "")
                   else
                     return(TRUE)
                 }
)

# Getters -----------------------------------------------------------------

setGeneric("getNodeCount", function(object) {standardGeneric("getNodeCount")})
setMethod("getNodeCount",
          signature = "City",
          definition = function(object) {
            return(object@nodecount)
          }
)

setGeneric("getCoordinate", function(object) {standardGeneric("getCoordinate")})
setMethod("getCoordinate",
          signature = "City",
          definition = function(object) {
            return(object@coordinate)
          }
)

setGeneric("getAdjacency", function(object) {standardGeneric("getAdjacency")})
setMethod("getAdjacency",
          signature = "City",
          definition = function(object) {
            return(object@adjacency)
          }
)

setGeneric("getGraph", function(object) {standardGeneric("getGraph")})
setMethod("getGraph",
          signature = "City",
          definition = function(object) {
            return(object@network)
          }
)

setGeneric("getArea", function(object) {standardGeneric("getArea")})
setMethod("getArea",
          signature = "City",
          definition = function(object) {
            return(V(object@network)$area)
          }
)

setGeneric("getShortestPaths", function(object, from, ...) {standardGeneric("getShortestPaths")})
setMethod("getShortestPaths",
          signature = "City",
          definition = function(object, from, ...) {
            return(shortest_paths(object@network, from = from, ...))
          }
)

setGeneric("getTime", function(object) {standardGeneric("getTime")})
setMethod("getTime",
          signature = "City",
          definition = function(object) {
            return(object@time)
          }
)

setGeneric("getSpeed", function(object) {standardGeneric("getSpeed")})
setMethod("getSpeed",
          signature = "City",
          definition = function(object) {
            return(object@speed)
          }
)

setGeneric("getCost", function(object) {standardGeneric("getCost")})
setMethod("getCost",
          signature = "City",
          definition = function(object) {
            return(object@cost)
          }
)

setGeneric("getCostFactor", function(object) {standardGeneric("getCostFactor")})
setMethod("getCostFactor",
          signature = "City",
          definition = function(object) {
            return(object@costfactor)
          }
)

setGeneric("getVertexPrice", function(object, index = V(object@network)) {standardGeneric("getVertexPrice")})
setMethod("getVertexPrice",
          signature = "City",
          definition = function(object, index = V(object@network)) {
            return(vertex_attr(object@network, "price", index))
          }
)

setGeneric("getEdgeCost", function(object, index = E(object@network)) {standardGeneric("getEdgeCost")})
setMethod("getEdgeCost",
          signature = "City",
          definition = function(object, index = E(object@network)) {
            return(edge_attr(object@network, "cost", index))
          }
)

setGeneric("getEdgeTime", function(object, index = E(object@network)) {standardGeneric("getEdgeTime")})
setMethod("getEdgeTime",
          signature = "City",
          definition = function(object, index = E(object@network)) {
            return(edge_attr(object@network, "time", index))
          }
)

setGeneric("getEdgeSpeed", function(object, index = E(object@network)) {standardGeneric("getEdgeSpeed")})
setMethod("getEdgeSpeed",
          signature = "City",
          definition = function(object, index = E(object@network)) {
            return(edge_attr(object@network, "speed", index))
          }
)

setGeneric("getDistance", function(object) {standardGeneric("getDistance")})
setMethod("getDistance",
          signature = "City",
          definition = function(object) {
            return(object@distance)
          }
)

setGeneric("getEdgePath", function(object) {standardGeneric("getEdgePath")})
setMethod("getEdgePath",
          signature = "City",
          definition = function(object) {
            return(object@edgepath)
          }
)

setGeneric("getCentrality", function(object) {standardGeneric("getCentrality")})
setMethod("getCentrality",
          signature = "City",
          definition = function(object) {
            return(V(object@network)$centrality)
          }
)

# Setters -----------------------------------------------------------------

setGeneric("setSpeed<-", function(object, value) {standardGeneric("setSpeed<-")})
setReplaceMethod("setSpeed",
                 signature = "City",
                 definition = function(object, value) {
                   object <- internalSetSpeed(object, value)
                   return(object)
                 }
)

setGeneric("setCostFactor<-", function(object, value) {standardGeneric("setCostFactor<-")})
setReplaceMethod("setCostFactor",
                 signature = "City",
                 definition = function(object, value) {
                   object <- internalSetCostFactor(object, value)
                   return(object)
                 }
)

setGeneric("setVertexPrice", function(object, index = V(object@network), value) {standardGeneric("setVertexPrice")})
setMethod("setVertexPrice",
          signature = "City",
          definition = function(object, index = V(object@network), value) {
            object@network <- set_vertex_attr(object@network, "price", index, value)
            return(object)
          }
)

setGeneric("setVertexPrice<-", function(object, index = V(object@network), value) {standardGeneric("setVertexPrice<-")})
setReplaceMethod("setVertexPrice",
                 signature = "City",
                 definition = function(object, index = V(object@network), value) {
                   object@network <- set_vertex_attr(object@network, "price", index, value)
                   return(object)
                 }
)

setGeneric("setEdgeCost", function(object, index = E(object@network), value) {standardGeneric("setEdgeCost")})
setMethod("setEdgeCost",
          signature = "City",
          definition = function(object, index = E(object@network), value) {
            object@network <- set_edge_attr(object@network, "cost", index, value)
            object@cost <- shortest.paths(object@network, V(object@network), weights = E(object@network)$cost) #+diagonal.col+diagonal.row
            return(object)
          }
)

setGeneric("setEdgeTime", function(object, index = E(object@network), value) {standardGeneric("setEdgeTime")})
setMethod("setEdgeTime",
          signature = "City",
          definition = function(object, index = E(object@network), value) {
            object@network <- set_edge_attr(object@network, "time", index, value)
            object@network <- set_edge_attr(object@network, "speed", index, value = edge_attr(object@network, "length", index)/edge_attr(object@network, "time", index))
            # Within zones:
            # diagonal.row <- (1/object@speed)*matrix(diag(object@distance), nrow = nrow(object@distance), ncol = ncol(object@distance)) # Origin zone
            # diagonal.col <- (1/object@speed)*matrix(diag(object@distance), nrow = nrow(object@distance), ncol = ncol(object@distance), byrow = TRUE) # Destination zone
            # diag(diagonal.col) <- 0            
            # Total time
            object@time <- shortest.paths(object@network, V(object@network), weights = E(object@network)$time) #+diagonal.col+diagonal.row
            return(object)
          }
)

setGeneric("setEdgeSpeed", function(object, index = E(object@network), value) {standardGeneric("setEdgeSpeed")})
setMethod("setEdgeSpeed",
          signature = "City",
          definition = function(object, index = E(object@network), value) {
            object@network <- set_edge_attr(object@network, "speed", index, value)
            object@network <- set_edge_attr(object@network, "time", index, value = edge_attr(object@network, "length", index)/edge_attr(object@network, "speed", index))
            # Within zones:
            # diagonal.row <- (1/object@speed)*matrix(diag(object@distance), nrow = nrow(object@distance), ncol = ncol(object@distance)) # Origin zone
            # diagonal.col <- (1/object@speed)*matrix(diag(object@distance), nrow = nrow(object@distance), ncol = ncol(object@distance), byrow = TRUE) # Destination zone
            # diag(diagonal.col) <- 0            
            # Total time
            object@time <- shortest.paths(object@network, V(object@network), weights = E(object@network)$time) #+diagonal.col+diagonal.row
            return(object)
          }
)

setGeneric("internalSetSpeed", function(object, value) {standardGeneric("internalSetSpeed")})
setMethod("internalSetSpeed",
          signature = "City",
          definition = function(object, value) {
            object@speed <- value
            # Between zones:
            E(object@network)$time <- E(object@network)$length/object@speed 
            # Within zones:
            # diagonal.row <- (1/object@speed)*matrix(diag(object@distance), nrow = nrow(object@distance), ncol = ncol(object@distance)) # Origin zone
            # diagonal.col <- (1/object@speed)*matrix(diag(object@distance), nrow = nrow(object@distance), ncol = ncol(object@distance), byrow = TRUE) # Destination zone
            # diag(diagonal.col) <- 0            
            # Total time
            object@time <- shortest.paths(object@network, V(object@network), weights = E(object@network)$time) #+diagonal.col+diagonal.row
            return(object)
          }
)

setGeneric("internalSetCostFactor", function(object, value) {standardGeneric("internalSetCostFactor")})
setMethod("internalSetCostFactor",
          signature = "City",
          definition = function(object, value) {
            object@costfactor <- value
            # Between zones:
            E(object@network)$cost <- E(object@network)$length*object@costfactor 
            # Within zones:
            # diagonal.row <- object@costfactor*matrix(diag(object@distance), nrow = nrow(object@distance), ncol = ncol(object@distance)) # Origin zone
            # diagonal.col <- object@costfactor*matrix(diag(object@distance), nrow = nrow(object@distance), ncol = ncol(object@distance), byrow = TRUE) # Destination zone
            # diag(diagonal.col) <- 0
            # Total cost
            object@cost <- shortest.paths(object@network, V(object@network), weights = E(object@network)$cost) #+diagonal.col+diagonal.row
            return(object)
          }
)

vec_shortest_paths <- Vectorize(function(i, x) {
  shortest_paths(getGraph(x), i, weights = E(getGraph(x))$length, mode = "out", output = "epath")$epath
}, vectorize.args = c("i"), SIMPLIFY = FALSE) # Edge ids of the path

edgePathMatrix <- function(x) {
  # x city
  # returns an edge-path matrix
  v <- length(V(getGraph(x)))
  e <- length(E(getGraph(x)))
  m <- matrix(0, e, v^2) # edge-path matrix
  colindex <- 1
  edgepathlist <- vec_shortest_paths(1:v, x) # Store edge ids of the paths
  for (i in 1:v) {
    for(j in 1:v) {
      cm <- matrix(0, e, 1) # column matrix
      cm[as_ids(edgepathlist[[i]][[j]])] <- 1 # a 1 if edge is part of the path
      m[ , colindex] <- cm
      colindex <- colindex + 1
    }
  }
  return(m)
}

as.path.id <- function(x, from, to) {
  # x city
  # from (origin node id)
  # to (destination node id)
  # returns the path index in the path-edge matrix for an origin i and destination j
  v <- getNodeCount(x)
  pathid <- (from-1)*v+to
  return(pathid)
}

setMethod("initialize",
          signature = "City",
          function(.Object, x, y, speed = 1, costfactor = 1) {
            .Object@coordinate <- x
            .Object@nodecount <- nrow(x)
            .Object@adjacency <- y
            .Object@network <- graph_from_adjacency_matrix(y, mode = "directed")
            tri <- tri.mesh(x[, 1], x[, 2]) # zone area
            # tri <- add.constraint(tri, 
            #                       c(min(x[ , 1])-0.1*range(x[ , 1]), min(x[ , 1])-0.1*range(x[ , 1]), max(x[ , 1])+0.1*range(x[ , 1]), max(x[ , 1])+0.1*range(x[ , 1])), 
            #                       c(min(x[ , 2])-0.1*range(x[ , 2]), max(x[ , 2])+0.1*range(x[ , 2]), max(x[ , 2])+0.1*range(x[ , 2]), min(x[ , 2])-0.1*range(x[ , 2])), 
            #                       reverse = FALSE)
            .Object@cells <- voronoi.mosaic(tri)
            node.area <- voronoi.area(.Object@cells)
            #node.area <- rev(node.area)[-(1:4)]
            #node.area <- rev(node.area)
            node.area[is.na(node.area)] <- max(node.area, na.rm = TRUE) # For simplicity
            V(.Object@network)$area <- node.area
            # Vertex size
            #deg <- degree(.Object@network, mode = "all")
            #V(.Object@network)$size <- deg
            V(.Object@network)$x <- x[ , 1] # Node coordinates
            V(.Object@network)$y <- x[ , 2] # Node coordinates
            edge.length <- y*rdist(x) # link length, speed, time, cost
            edge.length <- t(edge.length)[t(edge.length)>0]
            E(.Object@network)$length <- edge.length
            .Object@distance <- distances(.Object@network, weights = E(.Object@network)$length)
            # Set the distance within a zone to half the distance to nearest zone
            diag(.Object@distance) <- NA
            diagonal <- apply(.Object@distance, 2, min, na.rm = TRUE)
            diag(.Object@distance) <- diagonal/2
            # Edge-Path Matrix
            .Object@edgepath <- edgePathMatrix(.Object)
            #
            .Object <- internalSetSpeed(.Object, speed)
            .Object <- internalSetCostFactor(.Object, costfactor)
            # Vertex betweenness centrality (could use GC as weight?)
            V(.Object@network)$centrality <- betweenness(.Object@network, V(.Object@network), weights = E(.Object@network)$length)
            validObject(.Object)
            return(.Object)            
          }
)

setMethod("show",
          signature = "City",
          function(object) {
            cat("An object of class", class(object), "\n")
            cat(" ", "\n",
                "ROAD NETWORK", "\n",
                "Number of network nodes: ", length(V(object@network)), "\n",
                "Number of network links: ", length(E(object@network)), "\n",
                "Longest link", "\n",
                "Id(s):", which(E(object@network)$length == max(E(object@network)$length), arr.ind = TRUE), "\n",
                "Length: ", max(E(object@network)$length), "\n",
                "Shortest link", "\n",
                "Id(s):", which(E(object@network)$length == min(E(object@network)$length), arr.ind = TRUE), "\n",
                "Length: ", min(E(object@network)$length), "\n")
            cat(" ", "\n",
                "ZONES", "\n",
                "Number of zones: ", length(V(object@network)), "\n",
                "Largest cell area: ", max(V(object@network)$area), "\n",
                "Id(s):", which(V(object@network)$area == max(V(object@network)$area), arr.ind = TRUE), "\n",
                "Smallest cell area: ", min(V(object@network)$area), "\n",
                "Id(s):", which(V(object@network)$area == min(V(object@network)$area), arr.ind = TRUE), "\n")
            cat(" ", "\n",
                "PATHS", "\n",
                "Number of paths: ", dim(getEdgePath(object))[2], "\n",
                "Longest distance (shortest path) between an OD-pair: ", max(shortest.paths(object@network, V(object@network), weights = E(object@network)$length)), "\n",
                "Largest travel time (shortest path) between an OD-pair: ", max(shortest.paths(object@network, V(object@network), weights = E(object@network)$time)), "\n",
                "Largest travel cost (shortest path) between an OD-pair: ", max(shortest.paths(object@network, V(object@network), weights = E(object@network)$cost)), "\n")
            cat(" ", "\n",
                "SPEED and COSTFACTOR", "\n",
                "Speed: ", object@speed, "\n",
                "Costfactor: ", object@costfactor, "\n")
            invisible(NULL)
          }
)

setMethod("plot",
          signature = c(x = "City", y = "missing"),
          function(x, y, ...) {
            # Settings
            v <- length(V(x@network))
            e <- length(E(x@network))
            edge.flow <- rep(0, e)
            # Edge color
            #palf <- brewer.pal(8, "Blues")
            palf <- colorRampPalette(c("LightSlateGray", "LightSkyBlue"))
            eb <- edge.betweenness(x@network, weights = E(x@network)$length)
            neb <- (eb-min(eb))/(max(eb)-min(eb)) # Normalized edge betweenness
            E(x@network)$edge.color <- palf(8)[as.numeric(cut(eb, breaks = 8))]
            # Labels
            edge.labels <- FALSE
            if (edge.labels) {
              edge.label <- paste(round(E(x@network)$length, 2))#, "\n", 
                                  #round(E(x@network)$cost, 2), "\n", 
                                  #round(E(x@network)$time, 2))
            } else {
              edge.label <- NA
            }
            # Plots
            xlim <- c(min(x@coordinate[ , 1]), max(x@coordinate[ , 1]))
            ylim <- c(min(x@coordinate[ , 2]), max(x@coordinate[ , 2]))
            par(pty = "s",
                mar = c(0, 0, 0, 0) + 2, 
                bg = "#ffffff"
            )
            plot(0, 0, type = "n", axes = TRUE, xlim = xlim, ylim = ylim, xlab = NA, ylab = NA)
            plot(x@network, 
                 rescale = FALSE, 
                 add = TRUE,
                 vertex.size = 0.1,
                 vertex.label = NA,
                 vertex.color = "red",
                 vertex.frame.color = "#ffffff",
                 edge.color = E(x@network)$edge.color,
                 edge.arrow.size = 0.4,
                 edge.curved = 0.1,
                 edge.label = edge.label,
                 edge.label.cex = 0.7,
                 edge.label.color = "black")
            plot(x@cells, 
                 add = TRUE, 
                 col = "black", 
                 do.points = FALSE,
                 #pch = NA, 
                 main = "", 
                 sub = "")
          }
)

setMethod("plot",
          signature = c(x = "City", y = "Population"),
          function(x, y, ...) {
            # Settings
            v <- length(V(x@network))
            e <- length(E(x@network))
            vertex.color <- rep("black", v)
            vertex.frame.color <- rep("NA", v)
            #vertex.size <- rep(0.1, v)
            edge.color <- rep("LightSkyBlue", e)
            edge.flow <- rep(0, e)
            edge.width <- rep(1, e)
            # Labels
            edge.labels <- FALSE
            if (edge.labels) {
              edge.label <- paste(round(E(x@network)$length, 2), "\n",
                                  round(E(x@network)$cost, 2), "\n",
                                  round(E(x@network)$time, 2))
            } else {
              edge.label <- NA
            }
            # Origin-Destination (pair-by-pair)
            for (i in 1:v) {
              for (j in 1:v) {
                if(i != j) {
                  edgelist <- as_ids(get.shortest.paths(x@network, i, j, output = "epath")$epath[[1]]) # Convert the edges to ids
                  edge.color[edgelist] <- "LightSkyBlue"
                  edge.flow[edgelist] <- edge.flow[edgelist] + y@od[i, j] 
                  edge.width[edgelist] <- edge.width[edgelist] + y@od[i, j]
                }
              }
            }
            x@network <- set.edge.attribute(x@network, "color", index = E(x@network), edge.color)
            x@network <- set.vertex.attribute(x@network, "frame.color", index = V(x@network), vertex.frame.color)
            vertex.size <- {colSums(y@od)-min(colSums(y@od))+1}*{10/(max(colSums(y@od))-min(colSums(y@od)))}
            edge.width <- {edge.width-min(edge.width)+1}*{10/(max(edge.width)-min(edge.width))}
            # Plots
            par(pty = "s", mar = c(0, 0, 0, 0)+0.1)
            plot(x@cells, col = "black", pch = NA, main = "", sub = "")
            plot(x@network, rescale = FALSE, add = TRUE,
                 vertex.label = NA,
                 vertex.label.font = 1,
                 vertex.label.cex = 0.7,
                 vertex.label.degree = pi/2,
                 vertex.label.color = "black",
                 vertex.color = vertex.color,
                 vertex.frame.color = vertex.frame.color,
                 vertex.label.dist = 0,
                 vertex.size = vertex.size,
                 edge.color = edge.color,
                 edge.width = edge.width,
                 edge.label = edge.label,
                 edge.label.cex = 0.7,
                 edge.label.color = "black")
          }
)

city.delunay <- function(x, ...) {
  A <- matrix(0, nrow(x), nrow(x))
  tri <- tri.mesh(x[, 1], x[, 2])
  for(i in 1:nrow(x)) {A[i, tripack::neighbours(tri)[[i]]] <- 1}
  return(new(Class = "City", x = x, y = A, ...))
}

city.manhattan <- function(n, ...) {
  x <- seq(from = 0, to = 1, by = 1/n)
  x <- as.matrix(expand.grid(x, x))
  A <- matrix(0, nrow(x), nrow(x))
  tri <- tri.mesh(x[ , 1], x[ , 2], duplicate = "remove")
  for(i in 1:nrow(x)) {A[i, tripack::neighbours(tri)[[i]]] <- 1}
  return(new(Class = "City", x = x, y = A, ...))
}