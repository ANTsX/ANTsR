

diffmat <- function(mat) {
  # return( cor( mat ) )
  if (missing(mat)) {
    print(args(diffmat))
    return(1)
  }
  mydiffs <- matrix(rep(NA, ncol(mat) * ncol(mat)), ncol = ncol(mat))
  for (x in 1:ncol(mat)) {
    for (y in 1:ncol(mat)) {
      mydiffs[x, y] <- mean(mat[, x] - mat[, y])
    }
  }
  return(mydiffs)
}


makeDiffGraph <- function(myrsfnetworkcorrs, graphdensity = 1, correlationThreshold = c(-0.001, 
  10)) {
  pckg <- try(require(igraph))
  if (!pckg) {
    getPckg("igraph")
  }
  library(igraph)
  
  numberOfNeighbors <- nrow(myrsfnetworkcorrs)
  if (numberOfNeighbors == 0) {
    return(0)
  }
  
  myrsfnetworkcorrs[myrsfnetworkcorrs < correlationThreshold[1]] <- Inf
  myrsfnetworkcorrs[myrsfnetworkcorrs > correlationThreshold[2]] <- Inf
  adjmat <- myrsfnetworkcorrs
  diag(adjmat) <- Inf
  npossibleedges <- nrow(adjmat) * (nrow(adjmat) - 1)
  ndesirededges <- npossibleedges * graphdensity
  if (graphdensity < 1) {
    myord <- rev(order(adjmat))
    whichnodestoZero <- round((1 - graphdensity) * length(adjmat))
    adjmat[myord[1:whichnodestoZero]] <- 0
    # print( paste( ' 0'd ',whichnodestoZero,' of ', length( adjmat ) ) )
  }
  adjmat[adjmat == Inf] <- 0
  adjacencyMatrix <- as.matrix(adjmat, nrow = numberOfNeighbors, ncol = nnumberOfNeighbors)
  g1 <- graph.adjacency(adjacencyMatrix, mode = c("undirected"), weighted = TRUE)
  # 
  edgeWeights <- E(g1)$weight
  # print( paste( 'Graph-Density:',graph.density( g1 ) ) )
  gmetric0 <- evcent(g1, scale = TRUE)$vector
  gmetric1 <- closeness(g1, normalized = T, weights = edgeWeights)
  gmetric2 <- page.rank(g1, weights = edgeWeights)$vector  #
  gmetric3 <- degree(g1)
  gmetric4 <- betweenness(g1, normalized = F, weights = edgeWeights)
  gmetric5 <- transitivity(g1, isolates = c("zero"), type = c("local"))
  mycommunity <- fastgreedy.community(g1)
  walktrapcomm <- walktrap.community(g1)
  return(list(mygraph = g1, closeness = gmetric1, pagerank = gmetric2, degree = gmetric3, 
    betweeness = gmetric4, localtransitivity = gmetric5, community = mycommunity, 
    walktrapcomm = walktrapcomm, adjacencyMatrix = adjacencyMatrix, centrality = gmetric0, 
    evcent = evcent(g1)))
} 
