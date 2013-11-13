image2ClusterImages <- function(x, minClusterSize = 50, minThresh = 1e-06, maxThresh = 1) {
  if (nargs() == 0) {
    print("Usage: cluserimages<-image2ClusterImages( x , minClusterSize = 50 ) ")
    print(" x is an antsImage ")
    return(1)
  }
  dim <- x@dimension
  clust <- labelClusters(x, minClusterSize, minThresh, maxThresh)
  labs <- unique(clust[clust > 0])
  clustlist <- list()
  for (i in 1:length(labs)) {
    labimg <- antsImageClone(x)
    labimg[clust != labs[i]] <- 0
    clustlist <- lappend(clustlist, labimg)
  }
  return(clustlist)
} 
