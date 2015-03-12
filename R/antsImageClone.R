#' Clone an \code{antsImage}.
#' 
#' Clone an image object of S4 class \code{antsImage}. N.B.: You cannot use 
#' \code{a <- img} because the R assignment operator does not deal with the underlying
#' C++ pointers. 
#' 
#' @param in_image image object of S4 class \code{antsImage} to be cloned.
#' @param out_pixeltype C++ datatype to be used to represent the pixels in the
#' output image. Allowed values: \code{double}, \code{float}, 
#' \code{unsigned int}, \code{unsigned char}.
#' @return object of class \code{antsImage} 
#' @author Shrinidhi KL
#' @examples
#' 
#' img <- antsImageRead(getANTsRData("r16"), 2) 
#' img2 <- antsImageClone(img) 
#' img.int <- antsImageCone(img , "unsigned int")
#' 
#' @export antsImageClone
antsImageClone <- function(in_image, out_pixeltype = in_image@pixeltype) {
  if (length(dim(in_image)) == 1) 
    if (dim(in_image)[1] == 1) 
      return(NULL)
  .Call("antsImageClone", in_image, out_pixeltype, PACKAGE = "ANTsR")
} 
