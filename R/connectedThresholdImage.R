#' @name connectedThresholdImage
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
#' img[2:5, 3:5, 4:8] = 2
#' seed = floor(colMeans(which(as.array(img) == 2, arr.ind = TRUE)))
#' simg <- connectedThresholdImage(img, seed = seed,
#' upper = 2.5, lower = 1.5)
#' # ortho2(simg, xyz=xyz(as.array(simg) ==1))
#' @export connectedThresholdImage
connectedThresholdImage <- function(inimg, seed,
                               upper = 1, lower = 0) {
  if (is.character(inimg)) {
    inimg = antsImageRead(inimg)
  }
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
