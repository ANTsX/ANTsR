# getNetwork
plotNetwork <- function(network, mask, centroids, N = 100, scaling = c(0, 0)) {
  
  nLabels <- dim(centroids$centroids)[1]
  network <- as.array(network)
  
  if (length(dim(network)) != 2) {
    stop("network must have exactly 2 dimensions")
  }
  if ((dim(network)[1] != dim(centroids$centroids)[1]) || (dim(network)[2] != dim(centroids$centroids)[1])) {
    stop("network and centroids must have matching sizes")
  }
  
  
  open3d()
  rgl.bg(color = "black")
  
  # brain<-renderSurfaceFunction( mask, surfval=0.25 )
  surf <- as.array(mask)
  brain <- contour3d(surf, level = c(0.5), alpha = 0.3, draw = FALSE, smooth = 1, 
    material = "metal", depth = 0.6, color = "white")
  
  # convert to physical space
  brain$v1 <- antsTransformIndexToPhysicalPoint(mask, brain$v1)
  brain$v2 <- antsTransformIndexToPhysicalPoint(mask, brain$v2)
  brain$v3 <- antsTransformIndexToPhysicalPoint(mask, brain$v3)
  
  drawScene.rgl(list(brain))
  par3d(windowRect = c(100, 100, 600, 600))
  
  mesh <- .getvertices(brain)
  nSurfaceVerts <- dim(mesh$vertices)[1]
  
  # print( 'get centroids' )
  
  # Get centroids of labels d <- dim(labels) xcoords <- rep(c(1:d[1]), d[2]*d[3] )
  # ycoords <- rep(c(1:d[2]), each=d[1], d[3] ) zcoords <- rep(c(1:d[3]),
  # each=(d[1]*d[2]) )
  
  # labels <- as.array(labels) nLabels = max(labels) labelVerts <- rep(0,nLabels)
  # xc <- rep(0,nLabels) yc <- rep(0,nLabels) zc <- rep(0,nLabels)
  
  # for ( i in c(1:nLabels) ) { idx <- (labels == i) xc[i] <- mean( subset(xcoords,
  # idx) ) yc[i] <- mean( subset(ycoords, idx) ) zc[i] <- mean( subset(zcoords,
  # idx) ) }
  
  # xc <- dim(labels)[1] - xc + 1 centroids <- cbind(xc,yc,zc)
  
  
  mesh$vertices <- rbind(mesh$vertices, centroids$centroids)
  labelVerts <- c(1:nLabels) + nSurfaceVerts
  
  spheres3d(mesh$vertices[labelVerts, ], col = "blue", type = "s", radius = 3)
  # dd<-triangles3d( mesh$vertices[1:nSurfaceVerts,], col='white',alpha=0.4)
  
  up <- upper.tri(network)
  thresh <- sort(network[up], decreasing = TRUE)[N + 1]
  edgemat <- (up * (network > thresh))
  edges <- which((up * (network > thresh)) == 1)
  
  print("extracted edges")
  
  
  edgelocations <- c()
  edgeweights <- c()
  print(dim(edgemat))
  
  for (i in c(1:nLabels)) {
    for (j in c(i:nLabels)) {
      if (edgemat[i, j] == 1) {
        edgelocations <- c(edgelocations, nSurfaceVerts + c(i, j))
        edgeweights <- c(edgeweights, network[i, j])
      }
    }
  }
  print("finding edge values")
  if ((scaling[1] == scaling[2])) {
    scaling[1] <- min(edgeweights)
    scaling[2] <- max(edgeweights)
  }
  
  edgeweights <- edgeweights - scaling[1]
  edgeweights <- edgeweights/scaling[2]
  
  
  edgeweights <- 1 + (edgeweights * N)
  heat <- heat.colors(N + 1)
  colors <- heat
  for (i in c(1:length(edgeweights))) {
    colors[i] <- heat[floor(edgeweights[i])]
  }
  print("edge colors determined")
  
  segments3d(mesh$vertices[edgelocations, ], col = rep(colors, each = 2), lwd = 7)
  mat <- diag(4)
  mat[4, 4] <- 0.75
  par3d(userMatrix = mat)
  
  
  # view all average time series plot in a single window labelID <- rep(
  # 0:max(as.array(labels)), each=dim(tmat)[1] ) times <- rep(
  # timeStep*c(0:(dim(regions)[1]-1)), dim(regions)[2] ) dat <- data.frame(
  # signal=as.vector(regions), time=times, id=factor(as.vector(labelID)) ) ggplot(
  # data=dat, aes(x=time, y=signal, group=id, colour=id) ) + geom_line() +
  # ylab('regional cbf') + xlab( 'time (ms)' )
  
} 
