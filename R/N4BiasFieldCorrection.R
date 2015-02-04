#' Bias Field Correction
#'
#' Perform Bias Field Correction on the given image
#'
#' @param img input antsImage
#' @param mask input mask, if one is not passed one will be made
#' @return outimg
#' @author BB Avants
#' @examples
#'
#'  img<-makeImage( c(50,50), rnorm(2500) )
#'  ImageMath(2,img,PadImage,img,5)
#'  n4img<-N4BiasFieldCorrection(img,mask)
#'
#' @export N4BiasFieldCorrection
N4BiasFieldCorrection<-function( img , mask=NA )
{
if ( is.na(mask) ) mask<-getMask(img)
N4_CONVERGENCE_1 <- "[50x50x50x50,0.0000001]"
N4_CONVERGENCE_2 <- "[20x20x20x20,0.0000001]"
N4_SHRINK_FACTOR_1 <- "4"
N4_SHRINK_FACTOR_2 <- "2"
N4_BSPLINE_PARAMS <- "[200]"
outimg<-antsImageClone(img)
.helpN4BiasFieldCorrection(
  list(d = outimg@dimension,
       i = img,
       s = N4_SHRINK_FACTOR_1,
       c = N4_CONVERGENCE_1,
       b = N4_BSPLINE_PARAMS,
       x = mask,
       o = outimg)
       )
return(outimg)
}
.helpN4BiasFieldCorrection <- function(...) {
  .Call("N4BiasFieldCorrection", .int_antsProcessArguments(c(...)), PACKAGE = "ANTsR")
}
