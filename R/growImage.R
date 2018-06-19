#' @name confidenceConnected
#' @title Grow image using confidence connected filter
#' @description Grow image using confidence connected filter
#'
#' @param inimg Image to grow
#' @param radius the radius of the neighborhood over which the statistics are evaluated
#' @param multiplier multiplier to define the confidence interval. 
#' Multiplier can be anything greater than zero. A typical value is 2.5
#' @param iterations the number of iterations to repeat the process
#' @param seed xyz components to seed the image
#'
#' @note 
#' After this initial segmentation is calculated, the mean and variance are 
#' re-calculated. All the pixels in the previous segmentation are 
#' used to calculate the mean the standard deviation (as opposed to using 
#' the pixels in the neighborhood of the seed point). 
#' The segmentation is then recalculated using these refined estimates 
#' for the mean and variance of the pixel values. This process is 
#' repeated for the specified number of iterations. Setting the 
#' `iterations` to zero stops the algorithm after the initial 
#' segmentation from the seed point.
#' 
#' @return antsImage region grow image
#' @examples
#' library(ANTsR)
#' img <- makeImage(c(20, 20, 20), 0)
#' img[1:5, 1:5, 1:2] = 2
#' img[8:10, 9:14, 3:5] = 5
#' seed = xyz(as.array(img) == 2)
#' simg <- confidenceConnected(img, seed = seed, iterations = 20L)
#' @export confidenceConnected
confidenceConnected <- function(inimg, seed,
                      radius = 3,
                      multiplier = 1.5,
                      iterations = 0L) {
  if ( inimg@components == 1 )
    return( .confidenceConnectedHelper(   inimg, seed,
                                radius, multiplier, iterations))
}


.confidenceConnectedHelper <- function(inimg, seed,
                             radius, multiplier, iterations) {
  iterations = as.integer(iterations)
  outimg<-antsImageClone(inimg)
  seed <- as.vector(seed)
  radius <- as.vector(radius)
  multiplier <- as.vector(multiplier)
  if ( (length(seed) != length(dim(inimg))) ) {
    stop(paste("Length of seed must be the",
               "dimensionality of input image."))
  }
  inimg.float <- antsImageClone(inimg, "float")
  outimg <- antsImageClone(inimg.float)
  
  outimg <- .Call("confidenceConnectedImage",
                  inimg.float, outimg, seed,
                  radius, multiplier, iterations,
                  PACKAGE = "ANTsR")
  return(antsImageClone(outimg, inimg@pixeltype))
}
