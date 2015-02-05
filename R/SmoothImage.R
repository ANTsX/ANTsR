#' @name SmoothImage 
#' @title Smooth image 
#' @description Perform smoothing on the given image with a given sigma, defined in physical space units
#' @usage SmoothImage(dim, inimg, sigma, outimg)
#' @param dim Number of dimensions of the input image
#' @param inimg Input image to operate on
#' @param sigma Smoothing factor.  Either scalar, or vector of length \code{dim} for \code{dim}-dimensional image. 
#' @param outimg Result image, placeholder
#' @return 0 -- Success\cr 1 -- Failure
#' @author Shrinidhi KL, Avants BB
#' @examples
#' img<-makeImage(c(5,5),rnorm(25))
#' SmoothImage( 2 , img ,c(1.2,1.5), img)
#' @export SmoothImage
SmoothImage <- function(dim,inimg,sigma,outimg) {
  numargs <- nargs()
  if (numargs < 4) {
    print("Usage: SmoothImage Dimensionality InImage SmoothingFactor OutImage ")
    return(0)
  }
  smoothingparams<-sigma
  if (typeof(sigma)=='double') smoothingparams<-paste(sigma,collapse='x')
  args <- list(dim,outimg,"G",inimg,smoothingparams)
  pp<-.Call("ImageMath", .int_antsProcessArguments(c(args)),
    PACKAGE = "ANTsR")
}
