#' Motion Correction
#' 
#' This program is a user-level registration application meant to utilize
#' ITKv4-only classes. The user can specify any number of \emph{stages} where a
#' \emph{stage} consists of -- a transform, an image metric, number of
#' iterations, shrink factors, and smoothing sigmas for each level. Specialized
#' for 4D time series data: fixed image is 3D, moving image should be the 4D
#' time series. Fixed image is a reference space or time slice.
#' 
#' 
#' @param d-or-dimensionality=<value> This option forces the image to be
#' treated as a specified-dimensional image. If not specified, N4 tries to
#' infer the dimensionality from the input image. Allowed values: 2, 3.
#' @return 0 -- Success\cr 1 -- Failure
#' @author Shrinidhi KL
#' @examples
#' 
#' \dontrun{
#' antsMotionCorr( list( d = 3 , a = img , o = avg_img ) )
#' antsMotionCorr( list( d = 3 ,
#'   o = list( moco_params , moco_img , avg_img ) ,
#'   m = list( name = "MI" , avg_img , img , 1 , 32 , 50 ) ,
#'   t = "Rigid[0.01]" , i = 25 ,
#'   u = 1 , e = 1 , s = 0 , f = 1 , n = 25 ) )
#' }
#' 
#' @export antsMotionCorr
antsMotionCorr <- function(...) {
  .Call("antsMotionCorr", int_antsProcessArguments(c(...)), PACKAGE = "ANTsR")
  gc()  # trigger garbage collection
} 
