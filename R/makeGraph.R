#' Simple function to create and measure a graph from a square input matrix.
#'
#' Creates an igraph object from a square input correlation matrix - only
#' positive correlations are used.  Based on the graph.adjacency function of
#' igraph.  gplot is helpful for visualization.
#'
#'
#' @param mat input matrix
#' @param graphdensity fraction of edges to keep
#' @param communityMethod see igraph's community detection
#' @param getEfficiency boolean, this is slow to compute
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
#' # gplot( gobj$adjacencyMatrix ) # need sna library for this
#' }
#'
#' @export makeGraph
makeGraph <- function( mat, graphdensity = 1,
  communityMethod=NA, getEfficiency = FALSE) {
  if ( !usePkg("igraph") ) { print("Need igraph package"); return(NULL) }
  myrsfnetworkcorrs <- mat
  if (typeof(myrsfnetworkcorrs) == "list") {
    myrsfnetworkcorrs <- data.matrix(mat)
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
    mysps <- shortest.paths(g1)
    mysps[mysps == Inf] <- 2 * max(adjacencyMatrix)
    myspsa <- apply(mysps, FUN = mean, MARGIN = 2, na.rm = T)
  } else myspsa <- NA
  gmetric0 <- evcent(g1)$vector
  gmetric1 <- closeness(g1, normalized = T, weights = edgeWeights)
  gmetric2 <- page.rank(g1)$vector  #
  gmetric3 <- degree(g1)
  gmetric4 <- betweenness(g1, normalized = F, weights = edgeWeights)  #
  gmetric5 <- transitivity(g1, isolates = c("zero"), type = c("barrat"))
  gmetric6 <- graph.strength(g1)
  gmetric7 <- centralization.degree(g1)$res
  gmetric8 <- myspsa
  walktrapcomm <- walktrap.community(g1)
  if (  !is.na(communityMethod) )
    {
    if ( communityMethod == 'spinglass' )
      mycommunity <- spinglass.community(g1)
    else if ( communityMethod == 'optimal' )
      mycommunity <- optimal.community(g1)
    else if ( communityMethod == 'walktrap' )
      mycommunity <- walktrap.community(g1)
    else if ( communityMethod == 'multilevel' )
      mycommunity <- multilevel.community(g1)
    else if ( communityMethod == 'leading.eigenvector' )
      mycommunity <- leading.eigenvector.community(g1)
    else if ( communityMethod == 'label.propagation' )
      mycommunity <- label.propagation.community(g1)
    else if ( communityMethod == 'edge.betweenness' )
      mycommunity <- edge.betweenness.community(g1)
    else # ( communityMethod == 'fastgreedy.propagation' )
      mycommunity <- fastgreedy.community(g1)
    } else mycommunity<-walktrapcomm
  #########################################################
  return( list(
    mygraph = g1,
    centrality = gmetric0,
    closeness = gmetric1,
    pagerank = gmetric2,
    degree = gmetric3,
    betweeness = gmetric4,
    localtransitivity = gmetric5,
    strength = gmetric6,
    degcent = gmetric7,
    effinv = myspsa,
    community = mycommunity,
    walktrapcomm = walktrapcomm,
    adjacencyMatrix = adjacencyMatrix) )
}




.largeScaleCommunity <- function(g, mode = "all") {
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
