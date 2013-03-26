# LabelImageCentroids.R
# Inputs
#  img - ants image of labels (ints)
#  physical - return centroid in physical space instead of voxel space
#  convex - if TRUE, return centroid, if FALSE return point
#           with min average distance to other points with same label
# Returns a list with
#  labels <- array of label values
#  centroids <- coordinates of label centroids

LabelImageCentroids <- function( img, physical=FALSE, convex=TRUE )
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
  mylabels <- sort(unique( labels[ labels > 0 ] ))
  nLabels <- length(mylabels)
  labelVerts <- rep(0,nLabels)
  xc <- rep(0.0,nLabels)
  yc <- rep(0.0,nLabels)
  zc <- rep(0.0,nLabels)
  progress <- txtProgressBar( min = 0, max = length(mylabels), style = 3 )

  if ( convex == TRUE )
    {
      for ( i in mylabels )
        {
          #print( i )
          idx <- (labels == i)
          xc[i] <- mean( subset(xcoords, idx) )
          yc[i] <- mean( subset(ycoords, idx) )
          zc[i] <- mean( subset(zcoords, idx) )
          if( i %% 20 == 0 )
            {
              setTxtProgressBar( progress, i )
            }
        }
    }
  else
    {
      for ( i in mylabels )
        {
          idx <- (labels == i)
          xci <- subset(xcoords, idx)
          yci <- subset(ycoords, idx)
          zci <- subset(zcoords, idx)
          dist <- rep( 0, length(xci) )
          
          for ( j in c(1:length(xci)) )
            {
            dist[j] <- mean( sqrt( (xci[j]-xci)^2 + (yci[j]-yci)^2 + (zci[j]-zci)^2 ) )
            }
          
          mid <- which( dist == min(dist), arr.ind=TRUE )
          xc[i] <- xci[mid]
          yc[i] <- yci[mid]
          zc[i] <- zci[mid]
          if( i %% 20 == 0 )
            {
              setTxtProgressBar( progress, i )
            }
          
        }
      
    }
  setTxtProgressBar( progress, nLabels )
  close(progress)
  centroids <- cbind(xc,yc,zc)  
      
  if ( physical == TRUE )
    {
    for ( i in c(1:nLabels) )
      {
      centroids[i,] <- antsTransformIndexToPhysicalPoint(img, centroids[i,])
      }
    }

  return (list(labels=mylabels,centroids=centroids))

}
