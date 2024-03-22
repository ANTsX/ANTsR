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
#' convimg<-makeImage( c(3,3) , c(1,0,1,0,-4,0,1,0,1) )
#' convout<-convolveImage( fi, convimg )
#' convimg2<-makeImage( c(3,3) , c(0,1,0,1,0,-1,0,-1,0) )
#' convout2<-convolveImage( fi, convimg2 )
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
    kernelImageMask<-cropImage( kernelImageMask , kernelImageMask )
    kernelImage[ kernelImageMask == 0 ]<-
      mean( kernelImage[ kernelImageMask == 1 ] )
    }
  outimg = ANTsRCore::itkConvolveImage(image, kernelImage)
  outimg@components = as.integer( image@components )
  return( outimg )
}
