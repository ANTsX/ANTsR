#' @name connectedThreshold
#' @title Grow image using connected threshold filter
#' @description Grow image using connected threshold  filter
#'
#' @param inimg Image to grow
#' @param seed xyz components to seed the image
#' @param upper upper threshold value
#' @param lower lower threshold value
#'
#' 
#' @return antsImage region grow image
#' @examples
#' library(ANTsR)
#' img <- makeImage(c(20, 20, 20), rnorm(20^3))
#' img[1:5, 1:5, 1:2] = 2
#' img[8:10, 9:14, 3:5] = 5
#' seed = xyz(as.array(img) == 2)
#' simg <- connectedThreshold(img, seed = seed)
#' @export connectedThreshold
connectedThreshold <- function(inimg, seed,
                               upper = 1, lower = 0) {
  if ( inimg@components == 1 )
    return( .connectedThresholdHelper(   inimg, seed,
                                        upper, lower))
}


.connectedThresholdHelper <- function(inimg, seed,
                                       upper, lower) {
  outimg<-antsImageClone(inimg)
  seed <- as.vector(seed)
  upper <- as.vector(upper)
  lower <- as.vector(lower)
  if ( (length(seed) != length(dim(inimg))) ) {
    stop(paste("Length of seed must be the",
               "dimensionality of input image."))
  }
  inimg.float <- antsImageClone(inimg, "float")
  outimg <- antsImageClone(inimg.float)
  
  outimg <- .Call("connectedThresholdImage",
                  inimg.float, outimg, seed,
                  upper, lower,
                  PACKAGE = "ANTsR")
  return(antsImageClone(outimg, inimg@pixeltype))
}
