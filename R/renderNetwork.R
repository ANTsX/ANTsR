# renderNetwork






#' 3D surface-based rendering of image ROI-based networks
#'
#' Will use rgl to render a spatial graph
#'
#'
#' @param network NxN array or matrix of network connectivity values.
#' @param locations Nx3 array or matrix of node locations
#' @param scaling min and max values to use for coloring of edge values
#' @param lwd width of edge lines
#' @param edgecolors pre-defined colors for graph edges
#' @param radius radius of nodes
#' @param nodecolor  color of node
#' @param nodetype type of glyph for nodes
#' @return 0 -- Success\cr 1 -- Failure
#' @author Duda, J
#' @examples
#'
#' \dontrun{
#' renderNetwork(network, locations)
#' renderNetwork(network, locations, lwd=3, edgecolors=heat.colors(100) )
#' }
#'
#' @export renderNetwork
renderNetwork <- function(network, locations, scaling = c(0, 0), lwd = 2, radius = 3,
  edgecolors = 0, nodecolors = "blue", nodetype = "s") {

  nLabels <- dim(locations$vertices)[1]
  network <- as.array(network)

  if (length(dim(network)) != 2) {
    stop("network must have exactly 2 dimensions")
  }
  if ((dim(network)[1] != dim(locations$vertices)[1]) || (dim(network)[2] != dim(locations$vertices)[1])) {
    stop("network and centroids must have matching sizes")
  }

  labelVerts <- c(1:nLabels)
  spheres3d(locations$vertices[labelVerts, ], color = nodecolors, type = nodetype,
    radius = radius)

  edgelocations <- c()
  edgeweights <- c()

  for (i in c(1:nLabels)) {
    for (j in c(i:nLabels)) {
      if (network[i, j] != 0) {
        edgelocations <- c(edgelocations, c(i, j))
        edgeweights <- c(edgeweights, network[i, j])
      }
    }
  }

  if ((length(edgecolors) == 1) && (edgecolors[1] == 0)) {
    if ((scaling[1] == scaling[2])) {
      scaling[1] <- min(edgeweights)
      scaling[2] <- max(edgeweights) - min(edgeweights)
    }

    edgeweights <- edgeweights - scaling[1]
    edgeweights <- edgeweights/scaling[2]
    edgeweights <- edgeweights * 0.75  # prevent 'wrapping' of colors

    edgeweights <- 1 + (edgeweights * 511)
    # colormap <- topo.colors(512)
    colormap <- rainbow(512)
    edgecolors <- edgeweights
    for (i in c(1:length(edgeweights))) {
      edgecolors[i] <- colormap[floor(edgeweights[i])]
    }
  }

  segments3d(locations$vertices[edgelocations, ], col = rep(edgecolors, each = 2),
    lwd = lwd)
}
