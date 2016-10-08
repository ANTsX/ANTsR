#' @name smoothImage
#' @title Smooth image
#' @description Smooth image or multi-channel image
#' @param inimg Image to smooth
#' @param sigma Smoothing factor.  Can be scalar, in which case the same sigma
#' is applied to each dimension, or a vector of length \code{dim(inimg)} to
#' specify a unique smoothness for each dimension.
#' @param sigmaInPhysicalCoordinates If true, the smoothing factor is in
#' millimeters; if false, it is in pixels.
#' @param FWHM If true, sigma is interpreted as the full-width-half-max (FWHM)
#' of the filter, not the sigma of a Gaussian kernel.
#' @param max_kernel_width Maximum kernel width
#' @return antsImage smoothed image
#' @author Kandel BM, Avants BB
#' @examples
#' img <- makeImage(c(5,5), rnorm(25))
#' simg <- smoothImage(img, c(1.2,1.5))
#' @export smoothImage
smoothImage <- function(inimg, sigma, sigmaInPhysicalCoordinates=TRUE,
    FWHM=FALSE, max_kernel_width = 70) {
  if ( inimg@components == 1 )
    return( .smoothImageHelper(   inimg, sigma, sigmaInPhysicalCoordinates,
      FWHM, max_kernel_width ) ) else {
        iList = splitChannels( inimg )
        return( mergeChannels( lapply( iList, function(x) {
          .smoothImageHelper(  x, sigma, sigmaInPhysicalCoordinates,
            FWHM, max_kernel_width )
           } )
           )
        )
      }
}


.smoothImageHelper <- function(inimg, sigma, sigmaInPhysicalCoordinates=TRUE,
    FWHM=FALSE, max_kernel_width = 70) {
outimg<-antsImageClone(inimg)
sigma <- as.vector(sigma)
if ( (length(sigma) != 1) & (length(sigma) != length(dim(inimg))) ) {
  stop(paste("Length of sigma must be either 1 or the",
             "dimensionality of input image."))
}
inimg.float <- antsImageClone(inimg, "float")
outimg <- antsImageClone(inimg.float)
if (FWHM) {
  sigma <- sigma / 2.355
}
max_kernel_width = as.integer(ceiling(max_kernel_width))
outimg <- .Call("smoothImage",
  inimg.float, outimg, sigma, sigmaInPhysicalCoordinates,
  max_kernel_width,
  PACKAGE = "ANTsR")
return(antsImageClone(outimg, inimg@pixeltype))
}
