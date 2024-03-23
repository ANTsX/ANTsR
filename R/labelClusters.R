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
#' @param fullyConnected boolean sets neighborhood connectivity pattern
#' @return labeled cluster image is output
#' @author Avants BB
#' @examples
#'
#' img <- antsImageRead(getANTsRData("r16"))
#' timgFully <- labelClusters(img, 10, 128, 150, TRUE)
#' timgFace <- labelClusters(img, 10, 128, 150, FALSE)
#' \dontrun{
#' plot(img, timgFace)
#' plot(img, timgFully)
#' }
#'
#' @export labelClusters
labelClusters <- function(imagein, minClusterSize = 50,
                          minThresh = 1e-06, maxThresh = 1, fullyConnected = FALSE) {
  imagein <- check_ants(imagein)
  dim <- imagein@dimension
  clust <- thresholdImage(imagein, minThresh, maxThresh)
  temp <- as.numeric(fullyConnected)
  .LabelClustersUniquely(dim, clust, clust, minClusterSize, temp)
  return(clust)
}

.LabelClustersUniquely <- function(...) {
  pp <- ANTsRCore::LabelClustersUniquely(.int_antsProcessArguments(c(...)))
}
