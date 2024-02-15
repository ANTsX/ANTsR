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
#' img.int <- antsImageClone(img , "unsigned int")
#' multi = antsImageRead(getANTsRData("multi_component_image"))
#' img <- antsImageRead(getANTsRData("r16"), 2)
#' m2 = antsImageClone(multi)
#'
#' @export antsImageClone
antsImageClone <- function(in_image, out_pixeltype = in_image@pixeltype) {
  # this condition I believe is impossible
  # if in_image = NULL, then first condition is FALSE (it's 0, not 1)
  # if in_image is vector, still condition false
  if (length(dim(in_image)) == 1)
    if (dim(in_image)[1] == 1)
      return(NULL)
  
  if (in_image@components > 1) 
  {
    mychanns <- splitChannels( in_image )
    for ( k in 1:length(mychanns) )
    {
      img.clone <- .Call("antsImageClone", mychanns[[k]], out_pixeltype, PACKAGE = "ANTsRCore")
      mychanns[[k]] <- img.clone
    }
    return( mergeChannels( mychanns ) )
  }
  
  .Call("antsImageClone", in_image, out_pixeltype, PACKAGE = "ANTsRCore")
}
