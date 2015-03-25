#' a multi-start optimizer for affine registration
#'
#' Searches over the sphere to find a good initialization for further
#' registration refinement, if needed.  This is a arapper for the ANTs
#' function antsAffineInitializer.
#'
#' @param fixedImage the fixed reference image
#' @param movingImage the moving image to be mapped to the fixed space
#' @param searchFactor degree of increments on the sphere to search
#' @param radianFraction between zero and one, defines the arc to search over
#' @param usePrincipalAxis boolean to initialize by principal axis
#' @param localSearchIterations gradient descent iterations
#' @param mask optional mask to restrict registration
#' @param txfn filename for the transformation
#' @return transformationMatrix
#' @author Avants BB
#' @examples
#'
#' fi <- antsImageRead(getANTsRData("r16"))
#' mi <- antsImageRead(getANTsRData("r27"))
#' tx <- affineInitializer( fi, mi )
#'
#'
#' @export affineInitializer
affineInitializer <- function( fixedImage, movingImage, searchFactor=20,
  radianFraction=0.1, usePrincipalAxis=FALSE, localSearchIterations=10,
  mask, txfn )
  {
  if ( missing( txfn )) txfn <- tempfile( fileext='.mat' )
  veccer<-list( fixedImage@dimension, fixedImage, movingImage, txfn,
    searchFactor, radianFraction, as.numeric(usePrincipalAxis),
    localSearchIterations )
  if ( ! missing( mask ) ) veccer<-lappend( veccer, mask )
  xxx <- .int_antsProcessArguments( veccer )
  temp<-.Call("antsAffineInitializer", xxx,  PACKAGE = "ANTsR")
  return( txfn )
}
