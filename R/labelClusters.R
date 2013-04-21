labelClusters <- function( x , minClusterSize = 50 , minThresh = 1.e-6 , maxThresh = 1 )
  {
  if ( nargs() == 0 )
    {
    print("Usage: clusers<-labelClusters( x , minClusterSize = 50 ) ")
    print(" x is an antsImage ")
    return(1)
    }
  dim<-x@dimension
  clust<-antsImageClone( x )
  ThresholdImage(dim,x,clust, minThresh, maxThresh)
  LabelClustersUniquely(dim,clust,clust,minClusterSize)
  labs<-unique( clust[ clust > 0 ] )
  for ( i in 1:length(labs) )
    {
    clust[ clust == labs[i] ]<-i 
    }
  return(clust)
  }


