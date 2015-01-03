#' @name antsImageMutualInformation
#' @title mutual information between two images
#' @description  compute mutual information between two images
#' @usage  mival<-antsImageMutualInformation( i1, i2 )
#' @param img antsImage
#' @param img antsImage
#' @return mutual information value
#' @author Brian B. Avants
#' @keywords image similarity, mutual information
#' @examples
#' fi<-antsImageRead( getANTsRData('r16') ,2)
#' mi<-antsImageRead( getANTsRData('r64') ,2)
#' mival<-antsImageMutualInformation(fi,mi)
antsImageMutualInformation <- function(in_image1, in_image2) {
  if (length(dim(in_image1)) == 1)
    if (dim(in_image1)[1] == 1)
      return(NULL)
  if ( in_image1@pixeltype != "float" |
       in_image2@pixeltype != "float"   )
       {
       print(args(antsImageMutualInformation))
       print("input images must have float pixeltype")
       return(NA)
       }
  .Call("antsImageMutualInformation", in_image1, in_image2, PACKAGE = "ANTsR")
}
