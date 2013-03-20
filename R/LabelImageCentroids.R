LabelImageCentroids <- function( img, Physical=FALSE )
{  

  # Get centroids of labels
  d <- dim(img)
  if ( length(d) != 3 )
    {
    print("Currently only works for 3D")
    return(1)
    }
  xcoords <- rep(c(1:d[1]), d[2]*d[3] )
  ycoords <- rep(c(1:d[2]), each=d[1], d[3] )
  zcoords <- rep(c(1:d[3]), each=(d[1]*d[2]) )
  
  labels <- as.array(img)
  nLabels = max(labels)
  labelVerts <- rep(0,nLabels)
  xc <- rep(0.0,nLabels)
  yc <- rep(0.0,nLabels)
  zc <- rep(0.0,nLabels)
  mylabels<-unique( labels[ labels > 0 ] )
  progress <- txtProgressBar( min = 0, max = length(mylabels), style = 3 )
  for ( i in mylabels )
    {
    idx <- (labels == i)
    xc[i] <- mean( subset(xcoords, idx) )
    yc[i] <- mean( subset(ycoords, idx) )
    zc[i] <- mean( subset(zcoords, idx) )
    if( i %% 50 == 0 )
      {
      setTxtProgressBar( progress, i )
      }
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
