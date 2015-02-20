#' @name smoothImage
#' @title Smooth image
#' @description Perform smoothing on the given image with a given sigma, defined in physical space units
#' @usage smoothImage( inimg, sigma )
#' @param inimg Input image to operate on
#' @param sigma Smoothing factor.  Either scalar, or vector of length \code{dim} for \code{dim}-dimensional image.
#' @return antsImage smoothed
#' @author Shrinidhi KL, Avants BB
#' @examples
#' img<-makeImage(c(5,5),rnorm(25))
#' simg<-smoothImage( img ,c(1.2,1.5) )
#' @export smoothImage
smoothImage <- function(inimg,sigma) {
  smoothingparams<-sigma
  dim<-inimg@dimension
  outimg<-antsImageClone(inimg)
  if (typeof(sigma)=='double') smoothingparams<-paste(sigma,collapse='x')
  args <- list(dim,outimg,"G",inimg,smoothingparams)
  pp<-.Call("imageMath", .int_antsProcessArguments(c(args)),
    PACKAGE = "ANTsR")
  return(outimg)
}
