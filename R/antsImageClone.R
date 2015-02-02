#' Image Clone
#' 
#' Clone an image object of S4 class 'antsImage'.
#' 
#' 
#' @param in_image Image object of S4 class 'antsImage' to be cloned.
#' @param out_pixeltype C++ datatype to be used to represent the pixels in the
#' output image. Allowed values: 'double', 'float', 'unsigned int', 'unsigned
#' char'.
#' @return S4 object of Class 'antsImage' -- Success\cr 1 -- Failure
#' @author Shrinidhi KL
#' @examples
#' 
#' \dontrun{
#' # clone an image 'img' of class 'antsImage' to an image of pixeltype 'double'
#' antsImageCone( img , 'double' )
#' }
#' 
#' @export antsImageClone
antsImageClone <- function(in_image, out_pixeltype = in_image@pixeltype) {
  if (length(dim(in_image)) == 1) 
    if (dim(in_image)[1] == 1) 
      return(NULL)
  .Call("antsImageClone", in_image, out_pixeltype, PACKAGE = "ANTsR")
} 
