#' Mask input image by mask image.
#' 
#' Mask an input image by a mask image.  If the mask image has multiple labels,
#' it is possible to specify which label(s) to mask at.
#' 
#' 
#' @param img.in Input image.
#' @param img.mask Mask or label image.
#' @param level Level at which to mask image.
#' @param binarize binarize the output image
#' @return An object of type antsImage.
#' @author Kandel BM and Avants B.
#' @examples
#' 
#'   myimg <- antsImageRead(getANTsRData('ch2'), 3)
#'   mylab <- antsImageRead(getANTsRData('ch2a'), 3)
#'   myimg.mask <- maskImage(myimg, mylab, 3)
#'   plot(myimg.mask, axis=3, slices='90x90x90')
#'   myimg.mask <- maskImage(myimg, mylab, list( 1,  3, 9 ) )
#' 
#' @export maskImage
maskImage <- function(img.in, img.mask, level = 1, binarize = FALSE) {
  if (class(level) == "numeric") {
    img.out <- antsImageClone(img.in)
    img.out[img.mask != level] <- 0
    return(img.out)
  }
  if (class(level) == "list") {
    img.out <- antsImageClone(img.in)
    img.out[img.out > 0] <- 0
    for (mylevel in level) {
      myval <- as.numeric(mylevel)
      if (binarize) 
        img.out[img.mask == myval] <- 1
      if (!binarize) 
        img.out[img.mask == myval] <- img.in[img.mask == myval]
    }
    return(img.out)
  }
  
} 
