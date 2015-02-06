#' Convert an image to the geometric centroids of its signal
#'
#' Reduces a variate/statistical/network image to a set of centroids describing
#' the center of each stand-alone non-zero component in the image.
#'
#'
#' @param img the image to reduce to centroids - presumably some kind of
#' statistical or network map
#' @param threshparam thresholds the input image at mean( img[ img > 0 ] ) -
#' threshparam * sdev( img[ img > 0 ] )
#' @param clustparam ignore post-threshold clusters smaller than this value
#' @param outprefix prefix if you want to output to a file
#' @return the centroids are output in matrix of size npoints by 3
#' @author Avants BB
#' @examples
#'
#' \dontrun{
#'  getCentroids( f  , clustparam = 250, threshparam = 0 )
#' }
#'
#' @export getCentroids
getCentroids <- function(img, clustparam = 250, threshparam = NA, outprefix = NA) {
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
  clust <- NA
  if (!is.na(threshparam)) {
    threshimg <- antsImageClone(img)
    ImageMath(threshimg@dimension, threshimg, "abs", threshimg)
    meanval <- mean(threshimg[threshimg > (.Machine$double.eps * 2)])
    sdval <- sd(threshimg[threshimg > (.Machine$double.eps * 2)])
    threshval <- (meanval - sdval * threshparam)
    if (threshval < (.Machine$double.eps * 2))
      threshval <- (.Machine$double.eps * 2)
    threshimg[threshimg > threshval] <- 1
    threshimg[threshimg <= threshval] <- 0
    clust <- labelClusters(threshimg, clustparam)
    ImageMath(imagedim, pointfile, "LabelStats", clust, clust, 1)
  } else ImageMath(imagedim, pointfile, "LabelStats", img, img, clustparam)
  mypoints <- read.csv(pointfile)
  scl <- 10
  mypoints$x <- round(mypoints$x * scl)/scl
  mypoints$y <- round(mypoints$y * scl)/scl
  mypoints$z <- round(mypoints$z * scl)/scl
  centroids <- as.matrix(data.frame(x = mypoints$x, y = mypoints$y, z = mypoints$z))
  return(list(centroids = centroids, clustimg = clust))
  ##########################
}
