#' Motion Correction
#'
#' This program is a user-level registration application meant to utilize
#' ITKv4-only classes. The user can specify any number of \emph{stages} where a
#' \emph{stage} consists of -- a transform, an image metric, number of
#' iterations, shrink factors, and smoothing sigmas for each level. Specialized
#' for 4D time series data: fixed image is 3D, moving image should be the 4D
#' time series. Fixed image is a reference space or time slice.
#'
#' @param ... antsMotionCorr parameters, as in ANTs. this is a direct call to
#' the low level \code{c++} and as such has non-standard parameters.
#' @return 0 -- Success\cr 1 -- Failure
#' @author Shrinidhi KL
#' @examples
#'
#' # boldfn <- getANTsRData( "pcasl" )
#' # bold <- antsImageRead( boldfn , 4 )
#' bold <- makeImage( c(10,10,10,20) , rnorm( 10*10*10*20)+1 )
#' aimg <- new("antsImage", "float", 3)
#' aimg <- new("antsImage", "float", 3)
#' mocoImg <- new("antsImage", "float", 4)
#' mocoParams <- new("antsMatrix", "double")
#' antsMotionCorr( list( d = 3 , a = bold , o = aimg ) )
#' antsMotionCorr( list( d = 3 ,
#'   o = list( mocoParams , mocoImg , aimg ) ,
#'   m = list( name = "MI" , aimg , bold , 1 , 32 , "Regular", 0.1 ) ,
#'   t = "Rigid[0.01]" , i = 25 ,
#'   u = 1 , e = 1 , s = 0 , f = 1 , n = 25 ) )
#' motiondf <- as.data.frame( mocoParams )
#'
#' @export antsMotionCorr
antsMotionCorr <- function(...) {
  .Call("antsMotionCorr", .int_antsProcessArguments(c(...)), PACKAGE = "ANTsR")
}
