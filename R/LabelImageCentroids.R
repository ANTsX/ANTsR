LabelImageCentroids <- function( img, Physical=FALSE )
{  

  # Get centroids of labels
  d <- dim(img)
  xcoords <- rep(c(1:d[1]), d[2]*d[3] )
  ycoords <- rep(c(1:d[2]), each=d[1], d[3] )
  zcoords <- rep(c(1:d[3]), each=(d[1]*d[2]) )
  
  labels <- as.array(img)
  nLabels = max(labels)
  labelVerts <- rep(0,nLabels)
  xc <- rep(0.0,nLabels)
  yc <- rep(0.0,nLabels)
  zc <- rep(0.0,nLabels)

  for ( i in c(1:nLabels) )
    {
    idx <- (labels == i)
    xc[i] <- mean( subset(xcoords, idx) )
    yc[i] <- mean( subset(ycoords, idx) )
    zc[i] <- mean( subset(zcoords, idx) )
    }
  centroids <- cbind(xc,yc,zc)  

  if ( Physical == TRUE )
    {
    for ( i in c(1:nLabels) )
      {
      centroids[i,] <- antsTransformIndexToPhysicalPoint(img, centroids[i,])
      }
    }

  return (centroids)

}
