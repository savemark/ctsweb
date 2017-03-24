setClass("igraph")
setClass("voronoi")

city <- setClass("City",
                 slots = c(coordinate = "matrix",
                           adjacency = "matrix",
                           network = "igraph",
                           cells = "deldir", # Voronoi cells
                           distance = "matrix", # Distance matrix
                           edgepath = "matrix", # Edge-path matrix
                           time = "matrix", # Travel time matrix
                           cost = "matrix"), # Travel cost matrix
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
            return(length(V(object@network)))
          }
)

setGeneric("getEdgeCount", function(object) {standardGeneric("getEdgeCount")})
setMethod("getEdgeCount",
          signature = "City",
          definition = function(object) {
            return(length(E(object@network)))
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

setGeneric("getCost", function(object) {standardGeneric("getCost")})
setMethod("getCost",
          signature = "City",
          definition = function(object) {
            return(object@cost)
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

setGeneric("getEdgeCostFactor", function(object, index = E(object@network)) {standardGeneric("getEdgeCostFactor")})
setMethod("getEdgeCostFactor",
          signature = "City",
          definition = function(object, index = E(object@network)) {
            return(edge_attr(object@network, "costfactor", index))
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

setGeneric("getEdgeLength", function(object, index = E(object@network)) {standardGeneric("getEdgeLength")})
setMethod("getEdgeLength",
          signature = "City",
          definition = function(object, index = E(object@network)) {
            return(edge_attr(object@network, "length", index))
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

setGeneric("getVertexBetweenness", function(object, normalized = FALSE) {standardGeneric("getVertexBetweenness")})
setMethod("getVertexBetweenness",
          signature = "City",
          definition = function(object, normalized = FALSE) {
            if (!normalized) {
              return(V(object@network)$betweenness)
            } else {
              vb <- V(object@network)$betweenness
              nvb <- (vb-min(vb))/(max(vb)-min(vb))
              return(nvb)
            }
          }
)

setGeneric("getEdgeBetweenness", function(object, normalized = FALSE) {standardGeneric("getEdgeBetweenness")})
setMethod("getEdgeBetweenness",
          signature = "City",
          definition = function(object, normalized = FALSE) {
            if (!normalized) {
              return(E(object@network)$betweenness)
            } else {
              eb <- E(object@network)$betweenness
              neb <- (eb-min(eb))/(max(eb)-min(eb))
              return(neb)
            }
          }
)

# Setters -----------------------------------------------------------------

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

setGeneric("setEdgeCostFactor", function(object, index = E(object@network), value) {standardGeneric("setEdgeCostFactor")})
setMethod("setEdgeCostFactor",
          signature = "City",
          definition = function(object, index = E(object@network), value) {
            object@network <- set_edge_attr(object@network, "costfactor", index, value = value)
            object@network <- set_edge_attr(object@network, "cost", index, value = edge_attr(object@network, "length", index)*edge_attr(object@network, "costfactor", index))
            # Within zones:
            # diagonal.row <- object@costfactor*matrix(diag(object@distance), nrow = nrow(object@distance), ncol = ncol(object@distance)) # Origin zone
            # diagonal.col <- object@costfactor*matrix(diag(object@distance), nrow = nrow(object@distance), ncol = ncol(object@distance), byrow = TRUE) # Destination zone
            # diag(diagonal.col) <- 0
            # Total cost
            object@cost <- distances(object@network, mode = "out", weights = E(object@network)$cost) #+diagonal.col+diagonal.row
            return(object)
          }
)

setGeneric("setEdgeCostFactor<-", function(object, index = E(object@network), value) {standardGeneric("setEdgeCostFactor<-")})
setReplaceMethod("setEdgeCostFactor",
                 signature = "City",
                 definition = function(object, index = E(object@network), value) {
                   object@network <- set_edge_attr(object@network, "costfactor", index, value = value)
                   object@network <- set_edge_attr(object@network, "cost", index, value = edge_attr(object@network, "length", index)*edge_attr(object@network, "costfactor", index))
                   # Within zones:
                   # diagonal.row <- object@costfactor*matrix(diag(object@distance), nrow = nrow(object@distance), ncol = ncol(object@distance)) # Origin zone
                   # diagonal.col <- object@costfactor*matrix(diag(object@distance), nrow = nrow(object@distance), ncol = ncol(object@distance), byrow = TRUE) # Destination zone
                   # diag(diagonal.col) <- 0
                   # Total cost
                   object@cost <- distances(object@network, mode = "out", weights = E(object@network)$cost) #+diagonal.col+diagonal.row
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
            object@time <- distances(object@network, mode = "out", weights = E(object@network)$time) #+diagonal.col+diagonal.row
            return(object)
          }
)

setGeneric("setEdgeSpeed<-", function(object, index = E(object@network), value) {standardGeneric("setEdgeSpeed<-")})
setReplaceMethod("setEdgeSpeed",
                 signature = "City",
                 definition = function(object, index = E(object@network), value) {
                   object@network <- set_edge_attr(object@network, "speed", index, value)
                   object@network <- set_edge_attr(object@network, "time", index, value = edge_attr(object@network, "length", index)/edge_attr(object@network, "speed", index))
                   # Within zones:
                   # diagonal.row <- (1/object@speed)*matrix(diag(object@distance), nrow = nrow(object@distance), ncol = ncol(object@distance)) # Origin zone
                   # diagonal.col <- (1/object@speed)*matrix(diag(object@distance), nrow = nrow(object@distance), ncol = ncol(object@distance), byrow = TRUE) # Destination zone
                   # diag(diagonal.col) <- 0            
                   # Total time
                   object@time <- distances(object@network, mode = "out", weights = E(object@network)$time) #+diagonal.col+diagonal.row
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
          function(.Object, x, y, speed = 1, costfactor = 1, mode = "directed") {
            .Object@coordinate <- x
            .Object@adjacency <- y
            edge.length <- y*rdist(x) # link lengths
            .Object@network <- graph_from_adjacency_matrix(edge.length, mode = mode, weighted = TRUE)
            tri <- deldir(x[ , 1], x[ , 2], dpl = list(ndx = 2, ndy = 2))
            V(.Object@network)$area <- tri$summary$dir.area[1:length(x[ , 1])]
            .Object@cells <- tri
            # Vertex size
            #deg <- degree(.Object@network, mode = "all")
            #V(.Object@network)$size <- deg
            V(.Object@network)$x <- x[ , 1] # Node coordinates
            V(.Object@network)$y <- x[ , 2] # Node coordinates
            E(.Object@network)$length <- E(.Object@network)$weight
            .Object@distance <- distances(.Object@network, mode = "out", weights = E(.Object@network)$length)
            # Set the distance within a zone to half the distance to nearest zone
            #diag(.Object@distance) <- NA
            #diagonal <- apply(.Object@distance, 2, min, na.rm = TRUE)
            #diag(.Object@distance) <- diagonal/2
            # Edge-Path Matrix
            .Object@edgepath <- edgePathMatrix(.Object)
            #
            .Object <- setEdgeSpeed(.Object, value = speed)
            .Object <- setEdgeCostFactor(.Object, value = costfactor)
            # Vertex betweenness centrality (could use GC as weight?)
            V(.Object@network)$betweenness <- betweenness(.Object@network, V(.Object@network), weights = NULL)
            E(.Object@network)$betweenness <- edge_betweenness(.Object@network, E(.Object@network), weights = NULL)
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
                "Longest distance (shortest path) between an OD-pair: ", max(distances(object@network, mode = "out", weights = E(object@network)$length)), "\n",
                "Largest travel time (shortest path) between an OD-pair: ", max(distances(object@network, mode = "out", weights = E(object@network)$time)), "\n",
                "Largest travel cost (shortest path) between an OD-pair: ", max(distances(object@network, mode = "out", weights = E(object@network)$cost)), "\n")
            cat(" ", "\n",
                "SPEEDS and COSTS on links", "\n",
                "Speed (max): ", max(getEdgeSpeed(object)), "\n",
                "Speed (min): ", min(getEdgeSpeed(object)), "\n",
                "Cost (max): ", max(getEdgeCost(object)), "\n",
                "Cost (min): ", min(getEdgeCost(object)), "\n")
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
            # edge.labels <- FALSE
            # if (edge.labels) {
            #   edge.label <- paste(round(E(x@network)$length, 2))#, "\n", 
            #   #round(E(x@network)$cost, 2), "\n", 
            #   #round(E(x@network)$time, 2))
            # } else {
            #   edge.label <- NA
            # }
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
                 vertex.color = NA,
                 vertex.frame.color = NA,
                 edge.color = E(x@network)$edge.color,
                 edge.arrow.size = 0.4,
                 edge.curved = 0.1,
                 edge.label = as_ids(E(x@network)),
                 edge.label.cex = 0.8,
                 edge.label.color = "black")
            plot(x@cells, 
                 wlines = "tess",
                 add = TRUE, 
                 col = c(1, 1, 2, 3, 4), 
                 main = "", 
                 sub = "")
          }
)

setMethod("persp3D",
          signature = c(x = "City", y = "numeric"),
          function(x, y, ...) {
            args <- list(...)
            datalen <- getNodeCount(x)
            if (is.null(args$linear) | datalen < 4) args$linear <- TRUE else if (args$linear) args$linear <- TRUE else args$linear <- FALSE
            if ((is.null(args$extrap) & args$linear) | datalen < 4) args$extrap <- FALSE else args$extrap <- TRUE
            dens <- y
            surf <- interp(x = getCoordinate(x)[ , 1], # bicubic() from package akima could be an alternative?
                           y = getCoordinate(x)[ , 2], 
                           z = dens, 
                           linear = args$linear, 
                           extrap = args$extrap)
            persp3D(x = surf$x, 
                    y = surf$y,
                    z = surf$z,
                    col = gg2.col(),
                    contour = list(side = "z"),
                    lightning = "ambient",
                    resfac = 2,
                    shade = 0.2,
                    alpha = 1,
                    ticktype = "detailed",
                    cex.axis = 0.75,
                    colkey = list(cex.axis = 0.75),
                    zlim = range(surf$z, na.rm = TRUE),
                    zlab = args$zlab,
                    sub = args$sub)
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
            plot(x@cells, wlines = "tess", col = "black", main = "", sub = "")
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

city.random <- function(x, ...) {
  A <- matrix(0, nrow(x), nrow(x))
  tri <- deldir(x = x[ , 1], y = x[ , 2])
  for (i in 1:length(tri$delsgs$ind1)) {A[tri$delsgs$ind1[i], tri$delsgs$ind2[i]] <- 1; A[tri$delsgs$ind2[i], tri$delsgs$ind1[i]] <- 1}
  return(new(Class = "City", x = x, y = A, ...))
}

city.grid <- function(n, scale = 1, ...) {
  x <- scale*seq(from = 0, to = 1, by = 1/(n-1))
  x <- as.matrix(expand.grid(x, x))
  A <- matrix(0, nrow(x), nrow(x))
  tri <- deldir(x = x[ , 1], y = x[ , 2])
  for (i in 1:length(tri$delsgs$ind1)) {A[tri$delsgs$ind1[i], tri$delsgs$ind2[i]] <- 1; A[tri$delsgs$ind2[i], tri$delsgs$ind1[i]] <- 1}
  return(new(Class = "City", x = x, y = A, ...))
}