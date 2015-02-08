#' @name thresholdImage
#' @title Threshold Image
#' @usage thresholdImage(inimg, lothresh, hithresh, inval=1, outval=0)
#' @param inimg Input image to operate on
#' @param lothresh Lower edge of threshold window
#' @param hithresh Higher edge of threshold window
#' @param inval Output value for image voxels in between \code{lothresh} and \code{hithresh}
#' @param outval Output value for image voxels lower than \code{lothresh} or higher than \code{hithresh}
#' @return antsImage
#' @author Shrinidhi KL
#' @examples
#' img <- makeImage(c(5,5), rnorm(25)+0.5)
#' imgt<-thresholdImage( img, 0.5, Inf )
#' @export thresholdImage
thresholdImage <- function(inimg,
  lothresh, hithresh, inval=1, outval=0) {
  dim<-inimg@dimension
  outimg<-antsImageClone( inimg )
  args <- list(dim, inimg, outimg, lothresh, hithresh, inval, outval)
  temp<-.Call("ThresholdImage", .int_antsProcessArguments(args),
    PACKAGE = "ANTsR")
  return(outimg)
}
