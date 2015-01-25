#' Smooth an image
#' 
#' Perform smoothing on the given image with a given sigma, defined in physical
#' space units
#' 
#' 
#' @param imageDimension2|3|4 Number of dimensions of the input image
#' @param inputImage Input image to operate on
#' @param Sigma Smoothing factor in format 1.1x1.5x0.5 for a 3D image
#' @param outputImage Result image
#' @return 0 -- Success\cr 1 -- Failure
#' @author Shrinidhi KL, Avants BB
#' @examples
#' 
#' \dontrun{
#' # different ways you might smooth images of different dimensionality
#' SmoothImage( 2 , img ,"1.2x1.5", img)
#' SmoothImage( 3 , img ,"1x0x1", img)
#' SmoothImage( 4 , img ,"1x2x1x0", img)
#' }
#' 
#' @export SmoothImage
SmoothImage <- function(...) {
  numargs <- nargs()
  if (numargs < 4) {
    print("Usage: SmoothImage Dimensionality InImage SmoothingFactor OutImage ")
    return(0)
  }
  args <- list(...)
  newargs <- list(args[[1]], args[[4]], "G", args[[2]], args[[3]])
  .Call("ImageMath", int_antsProcessArguments(c(newargs)), PACKAGE = "ANTsR")
} 
