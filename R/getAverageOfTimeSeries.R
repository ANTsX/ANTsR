#' @title getAverageOfTimeSeries
#' @description  Returns a 3D average of a 4D time series.
#' @usage getAverageOfTimeSeries(img)
#' @param img input 4D image
#' @return 3D ants image is output
#' @author Avants BB
#' @examples
#' img<-as.antsImage( array(data = rep(0,10^4), dim = c(10,10,10,10) ) )
#' avg<-getAverageOfTimeSeries( img )
#' @export getAverageOfTimeSeries
getAverageOfTimeSeries <- function(img) {
  if (nargs() == 0) {
    return(1)
  }
  if (img@dimension != 4) {
    stop("FAILED--your input image is not 4D")
  }
  averageImage <- new("antsImage", img@pixeltype, 3)
  antsMotionCorr(list(d = 3, a = img, o = averageImage))
  return(averageImage)
} 
