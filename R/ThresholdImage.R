#' @name ThresholdImage 
#' @title Threshold Image 
#' @usage ThresholdImage(dim, inim, outimg, lothresh, hithresh, inval=0, outval=1)
#' @param dim Dimension of the input image
#' @param inim Input image to operate on
#' @param outimg Result image
#' @param lothresh Lower edge of threshold window
#' @param hithresh Higher edge of threshold window
#' @param inval Output value for image voxels in between \code{lothresh} and \code{hithresh} 
#' @param outval Output value for image voxels lower than \code{lothresh} or higher than \code{hithresh} 
#' @return 0 -- Success\cr 1 -- Failure
#' @author Shrinidhi KL
#' @examples
#' img <- makeImage(c(5,5), rnorm(25)) 
#' ThresholdImage(2, img, img, 0.5, 1e3)
#' @export ThresholdImage
ThresholdImage <- function(...) {
  .Call("ThresholdImage", .int_antsProcessArguments(c(...)), PACKAGE = "ANTsR")
} 
