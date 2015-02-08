#' Bias Field Correction
#'
#' Perform Bias Field Correction on the given image
#'
#' @param img antsImage to correct
#' @param downsampleFactor integer e.g. 4 downsample by a factor of 4
#' @return antsImage
#' @author Shrinidhi KL
#' @examples
#'
#' img<-makeImage(c(10,10),rnorm(100))
#' n3img<-n3BiasFieldCorrection(  img, 1 )
#'
#' @export n3BiasFieldCorrection
n3BiasFieldCorrection <- function( img, downsampleFactor ) {
  outimg<-antsImageClone(img)
  args<-list(img@dimension,img,outimg,downsampleFactor)
  pp<-.Call("N3BiasFieldCorrection", .int_antsProcessArguments(args), PACKAGE = "ANTsR")
  return(outimg)
}
