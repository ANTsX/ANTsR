plotBasicNetwork <- function(centroids, brain, weights = NA, edgecolors = 0, nodecolors = "blue", 
  nodetype = "s" , scaling = c(0, 0), lwd = 2, radius = 3 ) {
  if (missing(centroids) | missing(brain)) {
    print(args(plotBasicNetwork))
    return(1)
  }
  nLabels <- nrow(centroids)
  rgl.bg(color = "white")
  par3d(windowRect = c(100, 100, 600, 600))
  mesh <- getvertices(brain[[1]])
  nSurfaceVerts <- dim(mesh$vertices)[1]
  mesh$vertices <- rbind(mesh$vertices, as.matrix(centroids))
  labelVerts <- c(1:nrow(centroids)) + nSurfaceVerts
  spheres3d(mesh$vertices[labelVerts, ], color = nodecolors, type = nodetype, radius = radius)  
  edgelocations <- c()
  edgeweights <- c()
  for (i in c(1:nLabels)) {
    for (j in c(1:nLabels)) {
      if ( is.na( weights ) ) edgelocations <- c(edgelocations, nSurfaceVerts + c(i, j))
       else if ( weights[i,j] > 0 &  weights[i,j] < Inf ) {
           edgelocations <- c(edgelocations, nSurfaceVerts + c(i, j))
           edgeweights <- c(edgeweights,  weights[i,j])
       }
    }
  }
  if ( is.na(weights) ) {
      segments3d(mesh$vertices[edgelocations, ], col = "red", lwd = 2)
      return(1)
  }

  if ((length(edgecolors) == 1) && (edgecolors[1] == 0)) {
    if ((scaling[1] == scaling[2])) {
      scaling[1] <- min(edgeweights) 
      scaling[2] <- max(edgeweights) - min(edgeweights)
      if ( scaling[2] == 0 ) scaling[2]<-1
    }
    print( scaling )
    
    edgeweights <- edgeweights - scaling[1] + 1
    edgeweights <- edgeweights/scaling[2]
    edgeweights <- edgeweights * 0.75  # prevent 'wrapping' of colors
    
    edgeweights <- (edgeweights * 511)
    # colormap <- topo.colors(512)
    colormap <- rainbow(512)
    edgecolors <- edgeweights
    for (i in c(1:length(edgeweights))) {
      edgecolors[i] <- colormap[floor(edgeweights[i])]
    }
  }
  segments3d(mesh$vertices[edgelocations, ], col = rep(edgecolors, each = 2), lwd = lwd)
  return(1)
} 
