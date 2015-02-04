#' Bias Field Correction
#'
#' Perform Bias Field Correction on the given image
#'
#'
#' @param ... See N3BiasFieldCorrection in ANTs
#' @return NULL
#' @author Shrinidhi KL
#' @examples
#'
#' img<-makeImage(c(5,5),rnorm(25))
#' N3BiasFieldCorrection( 2 , img, img, 2 )
#'
#' @export N3BiasFieldCorrection
N3BiasFieldCorrection <- function(...) {
  pp<-.Call("N3BiasFieldCorrection", .int_antsProcessArguments(c(...)), PACKAGE = "ANTsR")
}
