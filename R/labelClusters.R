#' Simple labelClustering function.
#'
#' This will give a unique ID to each connected component 1 through N of size >
#' minClusterSize
#'
#'
#' @param imagein input antsImage e.g. a statistical map
#' @param minClusterSize throw away clusters smaller than this value
#' @param minThresh threshold to a statistical map
#' @param maxThresh threshold to a statistical map
#' @return labeled cluster image is output
#' @author Avants BB
#' @examples
#'
#' \dontrun{
#' img<-antsImageRead( getANTsRData("mnib"), 3 )
#' outimage<-labelClusters( img )
#' }
#'
#' @export labelClusters
labelClusters <- function(imagein, minClusterSize = 50,
   minThresh = 1e-06, maxThresh = 1) {
  if (nargs() == 0) {
    print("Usage: clusers<-labelClusters( imagein , minClusterSize = 50 ) ")
    print(" imagein is an antsImage ")
    return(1)
  }
  dim <- imagein@dimension
  clust<-thresholdImage( imagein, minThresh, maxThresh )
  .LabelClustersUniquely(dim, clust, clust, minClusterSize)
  return(clust)
}

.LabelClustersUniquely <- function(...) {
pp<-.Call("LabelClustersUniquely", .int_antsProcessArguments(c(...)), PACKAGE = "ANTsR")
}
