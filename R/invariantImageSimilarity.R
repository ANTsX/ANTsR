#' @name invariantImageSimilarity
#' @title mutual information between two images as a function of geometry
#' @description  compute mutual information between two images as image is rotated about its center w/or w/o optimization
#' @usage  mival<-invariantImageSimilarity( i1, i2, thetas=0:360, 0 )
#' @param img antsImage
#' @param img antsImage
#' @param thetas numeric vector
#' @param localSearchIterations integer controlling local search in multistart
#' @param metric which metric MI or GC (string)
#' @param scaleImage global scale
#' @return vector of similarity values
#' @author Brian B. Avants
#' @keywords image similarity
#' @examples
#' fi<-antsImageRead( getANTsRData('r16') ,2)
#' mi<-antsImageRead( getANTsRData('r64') ,2)
#' mival<-invariantImageSimilarity( fi, mi, c(0,10,20) )
invariantImageSimilarity <- function(in_image1, in_image2, thetas,
  localSearchIterations=0, metric="MI", scaleImage=1 ) {
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
  # convert to radians if necessary
  thetain<-thetas
  if ( max(abs(thetas)) > 2 )
    thetain <- (thetas * pi)/180.0
  ImageMath(in_image1@dimension,in_image1,"Normalize",in_image1)
  ImageMath(in_image2@dimension,in_image2,"Normalize",in_image2)
  if ( class(localSearchIterations) != "numeric") {
    print("wrong input: localSearchIterations is not numeric")
    return(NA)
  }
  if ( class(metric) != "character") {
    print("wrong input: metric is not numeric")
    return(NA)
  }
  .Call("invariantImageSimilarity", in_image1, in_image2,
    thetain, localSearchIterations, metric, scaleImage,
    PACKAGE = "ANTsR")
}
