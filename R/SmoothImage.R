#' Smooth an image
#'
#' Perform smoothing on the given image with a given sigma, defined in physical
#' space units
#'
#'
#' @param dim Number of dimensions of the input image
#' @param inimg Input image to operate on
#' @param sigma Smoothing factor in format 1.1x1.5x0.5 for a 3D image
#' @param outimg Result image, placeholder
#' @return 0 -- Success\cr 1 -- Failure
#' @author Shrinidhi KL, Avants BB
#' @examples
#'
#' # different ways you might smooth images of different dimensionality
#' img<-makeImage(c(5,5),rnorm(25))
#' SmoothImage( 2 , img ,'1.2x1.5', img)
#' SmoothImage( 2 , img ,c(1.2,1.5), img)
#'
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
