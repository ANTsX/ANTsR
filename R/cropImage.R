#' crop a sub-image via a mask
#'
#' uses a label image to crop a smaller image from within a larger image
#'
#' @param image antsImage to crop
#' @param labelImage antsImage with label values
#' @param label the label value to use
#' @return subimage
#' @author Brian B. Avants, Nicholas J. Tustison
#' @keywords crop, extract sub-image
#' @examples
#'
#' fi<-antsImageRead( getANTsRData("r16") ,2)
#' mask<-getMask( fi )
#' cropped<-cropImage( fi, mask, 1 )
#' cropped<-cropImage( fi, fi, 250 )
#'
#' @export cropImage
cropImage <- function( image, labelImage, label=1 ) {
  if ( image@pixeltype != "float" | labelImage@pixeltype != "float" ) {
    print(args(cropImage))
    print("input images must have float pixeltype")
    return(NA)
  }
  .Call("cropImage",
    image, labelImage, label, PACKAGE = "ANTsR")
}
