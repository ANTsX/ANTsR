#' Convert an image to the geometric centroids of its signal
#'
#' Reduces a variate/statistical/network image to a set of centroids describing
#' the center of each stand-alone non-zero component in the image.
#'
#' @param img the image to reduce to centroids - presumably some kind of
#' statistical or network map
#' @param clustparam look at regions greater than or equal to this size
#' @param outprefix prefix if you want to output to a file
#' @return the centroids are output in matrix of size npoints by 3
#' @author Avants BB
#' @examples
#' img<-antsImageRead( getANTsRData( "r16" ) )
#' img<-thresholdImage( img, 90, 120 )
#' img<-labelClusters( img, 10 )
#' cents<-getCentroids( img  )
#'
#' @export getCentroids
getCentroids <- function(img, clustparam = 250, outprefix = NA) {
  if (nargs() == 0 | missing(img)) {
    print(args(getCentroids))
    return(1)
  }
  if (class(img)[[1]] != "antsImage") {
    print("  class(img)[[1]] != antsImage ")
  }
  imagedim <- img@dimension
  if (is.na(outprefix)) {
    outprefix <- paste(tempdir(), "/Z", sep = "")
  }
  pointfile <- paste(outprefix, "coords.csv", sep = "")
  imageMath(imagedim, pointfile, "LabelStats", img, img, clustparam)
  mypoints <- read.csv(pointfile)
  centroids <- as.matrix(data.frame(x = mypoints$x, y = mypoints$y, z = mypoints$z, t=mypoints$t ))
  return( centroids )
}
