#' convolve one image with another
#'
#' convolves images together
#'
#' @param image antsImage to convolve
#' @param kernelImage antsImage acting as kernel
#' @param crop boolean automatically crops kernelImage
#' @return convimage
#' @author Brian B. Avants
#' @keywords convolve, convolution
#' @examples
#'
#' fi<-antsImageRead( getANTsRData("r16") ,2)
#' convimg<-makeImage( c(5,5) , rnorm(25) )
#' convout<-convolveImage( fi, convimg )
#'
#' @export convolveImage
convolveImage <- function( image, kernelImage, crop=TRUE ) {
  if ( image@pixeltype != "float" | kernelImage@pixeltype != "float" ) {
    print(args(convolveImage))
    print("input images must have float pixeltype")
    return(NA)
  }
  if ( crop )
    {
    kernelImageMask<-getMask( kernelImage )
    kernelImage<-cropImage( kernelImage , kernelImageMask )
    }
  .Call("convolveImage",
    image, kernelImage, PACKAGE = "ANTsR")
}
