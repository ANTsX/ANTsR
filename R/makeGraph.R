#' Simple function to create and measure a graph from a square input matrix.
#'
#' Creates an igraph object from a square input correlation matrix - only
#' positive correlations are used.  Based on the graph.adjacency function of
#' igraph.  gplot is helpful for visualization.
#'
#'
#' @param mat input matrix
#' @param graphdensity fraction of edges to keep
#' @return a named list is output including the graph object, adjacency matrix
#' and several graph metrics
#' @author Avants BB
#' @examples
#'
#' \dontrun{
#' mat <- matrix(c(rep(1,100)),ncol=10)
#' gobj<-makeGraph( mat )
#' mat <- matrix(  c( 1, 0.5, 0.2, -0.1, 1, 0.3, -0.2, 0.6, 1 ) , ncol= 3 )
#' gobj<-makeGraph( mat , 0.5 )
#' gplot( dd$adjacencyMatrix )
#' }
#'
#' @export makeGraph
makeGraph <- function(myrsfnetworkcorrsin, graphdensity = 1, getEfficiency = FALSE) {
  usePkg("igraph")
  myrsfnetworkcorrs <- myrsfnetworkcorrsin
  if (typeof(myrsfnetworkcorrs) == "list") {
    myrsfnetworkcorrs <- data.matrix(myrsfnetworkcorrsin)
  }
  diag(myrsfnetworkcorrs) <- 0
  correlationThreshold <- 1e-06

  numberOfNeighbors <- nrow(myrsfnetworkcorrs)
  if (numberOfNeighbors == 0) {
    return(0)
  }
  myrsfnetworkcorrs[myrsfnetworkcorrs < correlationThreshold] <- 0
  adjmat <- 1/myrsfnetworkcorrs
  npossibleedges <- nrow(adjmat) * (nrow(adjmat) - 1)
  ndesirededges <- npossibleedges * graphdensity
  if (graphdensity < 1) {
    myord <- rev(order(adjmat))
    whichnodestoZero <- round((1 - graphdensity) * length(adjmat))
    adjmat[myord[1:whichnodestoZero]] <- 0
  }
  adjmat[adjmat == Inf] <- 0
  adjmat[adjmat > 0] <- adjmat[adjmat > 0] - 1
  adjacencyMatrix <- as.matrix(adjmat, nrow = numberOfNeighbors, ncol = nnumberOfNeighbors)
  g1 <- graph.adjacency(adjacencyMatrix, mode = c("undirected"), weighted = TRUE)
  #
  edgeWeights <- E(g1)$weight
  # compute local efficiency
  if (getEfficiency) {
    mysps <- igraph::shortest.paths(g1)
    mysps[mysps == Inf] <- 2 * max(adjacencyMatrix)
    myspsa <- apply(mysps, FUN = mean, MARGIN = 2, na.rm = T)
  } else myspsa <- NA
  gmetric0 <- igraph::evcent(g1)$vector
  gmetric1 <- igraph::closeness(g1, normalized = T, weights = edgeWeights)
  gmetric2 <- igraph::page.rank(g1)$vector  #
  gmetric3 <- igraph::degree(g1)
  gmetric4 <- igraph::betweenness(g1, normalized = F, weights = edgeWeights)  #
  gmetric5 <- igraph::transitivity(g1, isolates = c("zero"), type = c("barrat"))
  gmetric6 <- igraph::graph.strength(g1)
  gmetric7 <- igraph::centralization.degree(g1)$res
  gmetric8 <- myspsa
  mycommunity <- multilevel.community(g1)
  walktrapcomm <- walktrap.community(g1)
  return(list(mygraph = g1, centrality = gmetric0, closeness = gmetric1, pagerank = gmetric2,
    degree = gmetric3, betweeness = gmetric4, localtransitivity = gmetric5, strength = gmetric6,
    degcent = gmetric7, effinv = myspsa, community = mycommunity, walktrapcomm = walktrapcomm,
    adjacencyMatrix = adjacencyMatrix))
}



clique.community <- function(graph, k) {
  clq <- cliques(graph, min = k, max = k)
  edges <- c()
  for (i in seq_along(clq)) {
    for (j in seq_along(clq)) {
      if (length(unique(c(clq[[i]], clq[[j]]))) == k + 1) {
        edges <- c(edges, c(i, j) - 1)
      }
    }
  }
  clq.graph <- simplify(graph(edges))
  V(clq.graph)$name <- seq_len(vcount(clq.graph))
  comps <- decompose.graph(clq.graph)

  lapply(comps, function(x) {
    unique(unlist(clq[V(x)$name]))
  })
}


largeScaleCommunity <- function(g, mode = "all") {
  cat("Assigning initial communities...\n")
  V(g)$group <- V(g)$name
  ## random order in which vertices will be processed
  cat("Generating random order...\n")
  order <- sample(vcount(g), vcount(g))
  t <- 0
  done <- FALSE

  while (!done) {
    t <- t + 1
    cat("round: ", t, "\n")
    ## change to FALSE whenever a node changes groups
    done <- TRUE

    for (i in order) {
      ## get the neighbor group frequencies:
      group.freq <- table(V(g)[neighbors(g, i, mode = mode)]$group)
      ## pick one of the most frequent:
      new.group <- sample(names(group.freq)[group.freq == max(group.freq)],
        1)
      if (done) {
        ## we are only done if new group is the same as old group
        done <- (new.group == V(g)[i]$group)
      }
      V(g)[i]$group <- new.group
    }
  }

  cat("Creating community-object...\n")
  comms <- list(membership = as.numeric(V(g)$group), vcount = vcount(g), algorithm = "LPA",
    names = V(g)$name)
  class(comms) <- "communities"
  return(comms)
}
