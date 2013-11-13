labelClusters <- function(imagein, minClusterSize = 50, minThresh = 1e-06, maxThresh = 1) {
  if (nargs() == 0) {
    print("Usage: clusers<-labelClusters( imagein , minClusterSize = 50 ) ")
    print(" imagein is an antsImage ")
    return(1)
  }
  dim <- imagein@dimension
  clust <- antsImageClone(imagein)
  ThresholdImage(dim, imagein, clust, minThresh, maxThresh)
  LabelClustersUniquely(dim, clust, clust, minClusterSize)
  return(clust)
} 
