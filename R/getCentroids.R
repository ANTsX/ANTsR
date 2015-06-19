#' Convert an image to the geometric centroids of its signal
#'
#' Reduces a variate/statistical/network image to a set of centroids describing
#' the center of each stand-alone non-zero component in the image.
#'
#' @param img the image to reduce to centroids - presumably some kind of
#' statistical or network map
#' @param clustparam look at regions greater than or equal to this size
#' @return the centroids are output in matrix of size npoints by 3
#' @author Avants BB
#' @examples
#' img<-antsImageRead( getANTsRData( "r16" ) )
#' img<-thresholdImage( img, 90, 120 )
#' img<-labelClusters( img, 10 )
#' cents<-getCentroids( img  )
#'
#' @export getCentroids
getCentroids <- function(img, clustparam = 250 ) {
  if (nargs() == 0 | missing(img)) {
    print(args(getCentroids))
    return(1)
  }
  if (class(img)[[1]] != "antsImage") {
    print("  class(img)[[1]] != antsImage ")
  }
  imagedim <- img@dimension
  mypoints <- labelClusters( img, clustparam, maxThresh=Inf )
  mypoints = data.frame( labelStats( mypoints, mypoints ) )
  mypoints = mypoints[-1,] # remove 0 label
  x = mypoints$x
  y = mypoints$y
  if ( imagedim ==  3 ) z = mypoints$z else z=rep(0,nrow(mypoints))
  if ( imagedim ==  4 ) t = mypoints$t else t=rep(0,nrow(mypoints))
  centroids <- as.matrix(
    data.frame(
      x = x,
      y = y,
      z = z,
      t = t )
      )
  return( centroids )
}
