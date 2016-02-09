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
#' @param inverseCorrsAsWeights if TRUE, high correlations produce small
#' edge weights.
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
  communityMethod=NA, getEfficiency = FALSE,
  inverseCorrsAsWeights = FALSE ) {
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
  if (  inverseCorrsAsWeights ) adjmat <- 1/myrsfnetworkcorrs
  if ( !inverseCorrsAsWeights ) adjmat <- myrsfnetworkcorrs
  npossibleedges <- nrow(adjmat) * (nrow(adjmat) - 1)
  ndesirededges <- npossibleedges * graphdensity
  if (graphdensity < 1) {
    if (  inverseCorrsAsWeights ) myord = rev( order( adjmat ) )
    if ( !inverseCorrsAsWeights ) myord = order( adjmat )
    whichnodestoZero <- round((1 - graphdensity) * length(adjmat))
    adjmat[myord[1:whichnodestoZero]] <- 0
  }
  adjmat[ adjmat == Inf ] <- 0
  adjacencyMatrix <- as.matrix(adjmat, nrow = numberOfNeighbors, ncol = numberOfNeighbors)
  g1 <- igraph::graph_from_adjacency_matrix( adjacencyMatrix,
    mode = c("undirected"), weighted = TRUE, diag = FALSE )
  edgeWeights <- igraph::E(g1)$weight
  if ( !inverseCorrsAsWeights )
    {
    edgeWeights <- psych::fisherz( igraph::E(g1)$weight )
    E(g1)$weight = edgeWeights
    }
  # compute local efficiency
  if (getEfficiency) {
    mysps <- igraph::shortest.paths( g1 )
    mysps[mysps == Inf] <- 2 * max( adjacencyMatrix )
    myspsa <- apply(mysps, FUN = mean, MARGIN = 2, na.rm = T)
  } else myspsa <- NA
  gmetric0 <- igraph::evcent( g1 )$vector
  gmetric1 <- igraph::closeness( g1, normalized = T, weights = edgeWeights)
  gmetric2 <- igraph::page.rank( g1 )$vector  #
  gmetric3 <- igraph::degree( g1 )
  gmetric4 <- igraph::betweenness( g1, normalized = T, weights = edgeWeights )
#  gmetric5 <- igraph::transitivity(g1, isolates = c("zero"), type = c("barrat")
  gmetric5 <- igraph::transitivity(g1, isolates = c("zero"), type = c("barrat"),
    weights = edgeWeights )
  gmetric6 <- igraph::graph.strength(g1)
  gmetric7 <- igraph::centralization.degree(g1)$res
  gmetric8 <- myspsa
  gmetric9 <- igraph::hub_score(g1)$vector
  walktrapcomm <- igraph::walktrap.community(g1)
  if (  !is.na(communityMethod) )
    {
    if ( communityMethod == 'spinglass' )
      mycommunity <- igraph::spinglass.community(g1)
    else if ( communityMethod == 'optimal' )
      mycommunity <- igraph::optimal.community(g1)
    else if ( communityMethod == 'walktrap' )
      mycommunity <- igraph::walktrap.community(g1)
    else if ( communityMethod == 'multilevel' )
      mycommunity <- igraph::multilevel.community(g1)
    else if ( communityMethod == 'leading.eigenvector' )
      mycommunity <- igraph::leading.eigenvector.community(g1)
    else if ( communityMethod == 'label.propagation' )
      mycommunity <- igraph::label.propagation.community(g1)
    else if ( communityMethod == 'edge.betweenness' )
      mycommunity <- igraph::edge.betweenness.community(g1)
    else # ( communityMethod == 'fastgreedy.propagation' )
      mycommunity <- igraph::fastgreedy.community(g1)
    } else mycommunity<-walktrapcomm
  #########################################################

  return(
    list(
    mygraph = g1,
    centrality = gmetric0,
    closeness = gmetric1,
    pagerank = gmetric2,
    degree = gmetric3,
    betweeness = gmetric4,
    localtransitivity = gmetric5,
    globalTransitivity = igraph::transitivity(g1),
    strength = gmetric6,
    degcent = gmetric7,
    hubScore = gmetric9,
    effinv = myspsa,
    community = mycommunity,
    walktrapcomm = walktrapcomm,
    adjacencyMatrix = adjacencyMatrix )
    )
}
