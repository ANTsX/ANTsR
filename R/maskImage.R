#' Mask input image by mask image.
#'
#' Mask an input image by a mask image.  If the mask image has multiple labels,
#' it is possible to specify which label(s) to mask at.
#'
#'
#' @param img.in Input image.
#' @param img.mask Mask or label image.
#' @param level Level(s) at which to mask image.  If vector or list
#' of values, output image is non-zero at all locations where label image
#' matches any of the levels specified.
#' @param binarize binarize the output image?
#' @return An object of type antsImage.
#' @author Kandel BM and Avants B.
#' @examples
#'
#' myimg <- antsImageRead(getANTsRData("r16"))
#' mask <- getMask(myimg)
#' myimg.mask <- maskImage(myimg, mask, 3)
#' myimg.mask <- maskImage(myimg, mask)
#' seg <- kmeansSegmentation(myimg, 3)
#' myimg.mask <- maskImage(myimg, seg$segmentation, c(1, 3))
#'
#' @export maskImage
maskImage <- function(img.in, img.mask, level = 1, binarize = FALSE) {
  level <- as.numeric(level)
  if (is.numeric(level) & length(level) == 1) {
    img.in <- check_ants(img.in)
    img.out <- antsImageClone(img.in)
    img.mask <- check_ants(img.mask)
    img.out[img.mask != level] <- 0
    return(img.out)
  }
  if ((is.list(level)) |
    (is.numeric(level) &
      length(level) > 1)) {
    img.in <- check_ants(img.in)
    img.out <- antsImageClone(img.in) * 0
    img.mask <- check_ants(img.mask)
    for (mylevel in level) {
      myval <- as.numeric(mylevel)
      if (binarize) {
        img.out[img.mask == myval] <- 1
      }
      if (!binarize) {
        img.out[img.mask == myval] <- img.in[img.mask == myval]
      }
    }
    return(img.out)
  }
}
