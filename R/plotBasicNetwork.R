#' Simple plotBasicNetwork function.
#'
#' takes an object output from renderSurfaceFunction and a list of centroids
#' and plots the centroid network over the rendering object
#'
#' @param centroids input matrix of size number of 3D points ( in rows ) by 3 (
#' in columns )
#' @param brain input rendering object which is output of renderSurfaceFunction
#' or a function derived from renderSurfaceFunction
#' @param weights edge weights
#' @param edgecolors a color(map) for edges
#' @param nodecolors a color(map) for nodes
#' @param nodetype sphere or other node type
#' @param scaling controls functional range
#' @param lwd line width
#' @param radius for nodes
#' @param showOnlyConnectedNodes boolean
#' @return None
#' @author Avants BB and Duda JT
#' @examples
#'
#' \dontrun{
#' # more complete example
#'   mnit<-getANTsRData("mni")
#'   mnit<-antsImageRead(mnit)
#'   mnia<-getANTsRData("mnia")
#'   mnia<-antsImageRead(mnia)
#'   mnit<-thresholdImage( mnit, 1, max(mnit) )
#'   mnit<-iMath(mnit,"FillHoles")
#'   cnt<-getCentroids( mnia, clustparam = 50 )
#'   aalcnt<-cnt[1:90,1:3]
#'   brain<-renderSurfaceFunction( surfimg =list( mnit ) , alphasurf=0.1 ,smoothsval = 1.5 )
#'   testweights<-matrix( rep( 0, 90*90 ) ,nrow=90)
#'   testweights[31,37]<-1  # ant cingulate to hipp
#'   testweights[31,36]<-2  # ant cingulate to post cingulate
#'   testweights[11,65]<-3  # broca to angular
#'   plotBasicNetwork( centroids = aalcnt , brain , weights=testweights )
#'   id<-rgl::par3d('userMatrix')
#'   rid<-rotate3d( id , -pi/2, 1, 0, 0 )
#'   rid2<-rotate3d( id , pi/2, 0, 0, 1 )
#'   rid3<-rotate3d( id , -pi/2, 0, 0, 1 )
#'   rgl::par3d(userMatrix = id )
#'   dd<-make3ViewPNG(  rid, id, rid2,  paste('network1',sep='') )
#'   rgl::par3d(userMatrix = id )
#' }
#'
#' @export plotBasicNetwork
plotBasicNetwork <- function(
  centroids,
  brain,
  weights = NA,
  edgecolors = 0,
  nodecolors = "blue",
  nodetype = "s",
  scaling = c(0, 0),
  lwd = 2,
  radius = 3,
  showOnlyConnectedNodes = TRUE ) {
  if (missing(centroids) | missing(brain)) {
    print(args(plotBasicNetwork))
    return(1)
  }
  if(!usePkg('rgl')){
    print("rgl is necessary for this function.")
    return(NULL)
  }
  nLabels <- nrow(centroids)
  rgl::rgl.bg(color = "white")
  rgl::par3d(windowRect = c(100, 100, 600, 600))
  mesh <- .getvertices(brain[[1]])
  nSurfaceVerts <- dim(mesh$vertices)[1]
  mesh$vertices <- rbind(mesh$vertices, as.matrix(centroids))
  labelVerts <- c(1:nrow(centroids)) + nSurfaceVerts
  if (!is.na(weights) & showOnlyConnectedNodes) {
    radiusw <- rep(0, nrow(centroids))
    gg <- which(apply(weights, FUN = mean, MARGIN = 1, na.rm = T) > 0 | apply(weights,
      FUN = mean, MARGIN = 2, na.rm = T) > 0)
    radiusscale <- as.numeric(apply(weights, FUN = sum, MARGIN = 1, na.rm = T) +
      apply(weights, FUN = sum, MARGIN = 2, na.rm = T))
    radiusscale <- (radiusscale/max(radiusscale))
    radiusw <- (radius * radiusscale)
    radius <- radiusw
  }
  rgl::spheres3d(mesh$vertices[labelVerts, ], color = nodecolors, type = nodetype, radius = radius)
  edgelocations <- c()
  edgeweights <- c()
  for (i in c(1:nLabels)) {
    for (j in c(1:nLabels)) {
      if (is.na(weights))
        edgelocations <- c(edgelocations, nSurfaceVerts + c(i, j)) else if (weights[i, j] > 0 & weights[i, j] < Inf) {
        edgelocations <- c(edgelocations, nSurfaceVerts + c(i, j))
        edgeweights <- c(edgeweights, weights[i, j])
      }
    }
  }
  if (is.na(weights)) {
    rgl::segments3d(mesh$vertices[edgelocations, ], col = "red", lwd = 2)
    return(1)
  }

  if ((length(edgecolors) == 1) && (edgecolors[1] == 0)) {
    if ((scaling[1] == scaling[2])) {
      scaling[1] <- min(edgeweights)
      scaling[2] <- max(edgeweights) - min(edgeweights)
      if (scaling[2] == 0)
        scaling[2] <- 1
    }

    edgeweights <- edgeweights - scaling[1] + 1
    edgeweights <- edgeweights/scaling[2]
    edgeweights <- edgeweights * 0.75  # prevent 'wrapping' of colors
    edgeweights <- (edgeweights * 400)
    # colormap <- topo.colors(512)
    colormap <- rainbow(512)
    colormap <- heat.colors(512, alpha = 1)
    edgecolors <- edgeweights
    for (i in c(1:length(edgeweights))) {
      colind <- floor(edgeweights[i])
      if (colind < 1)
        colind <- 1
      edgecolors[i] <- colormap[colind]
    }
  }
  rgl::segments3d(mesh$vertices[edgelocations, ], col = rep(edgecolors, each = 2), lwd = lwd)
  return(1)
}
