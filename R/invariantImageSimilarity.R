#' @name invariantImageSimilarity
#' @title mutual information between two images as a function of geometry
#' @description  compute mutual information between two images as image is rotated about its center w/or w/o optimization
#' @usage  mival<-invariantImageSimilarity( i1, i2, thetas=0:360 )
#' @param img antsImage
#' @param img antsImage
#' @param thetas numeric vector
#' @return vector of similarity values
#' @author Brian B. Avants
#' @keywords image similarity
#' @examples
#' fi<-antsImageRead( getANTsRData('r16') ,2)
#' mi<-antsImageRead( getANTsRData('r64') ,2)
#' mival<-invariantImageSimilarity( fi, mi, c(0,10,20) )
invariantImageSimilarity <- function(in_image1, in_image2, thetas ) {
  if (length(dim(in_image1)) == 1)
    if (dim(in_image1)[1] == 1)
      return(NULL)
  if ( in_image1@pixeltype != "float" |
       in_image2@pixeltype != "float"   )
       {
       print(args(invariantImageSimilarity))
       print("input images must have float pixeltype")
       return(NA)
       }
  .Call("invariantImageSimilarity", in_image1, in_image2,
    thetas, PACKAGE = "ANTsR")
}
