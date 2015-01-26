#' Returns a 3D average of a 4D time series.
#'
#' Averages the 3D sub-volumes of a 4D input ants image.
#'
#'
#' @param img input 4D image
#' @return 3D ants image is output
#' @author Avants BB
#' @examples
#'
#' img<-as.antsImage( array(data = rep(0,10^4), dim = c(10,10,10,10) ) )
#' avg<-getAverageOfTimeSeries( img )
#' print(dim(avg))
#' avg<-getAverageOfTimeSeries( avg ) 
#'
#' @export getAverageOfTimeSeries
getAverageOfTimeSeries <- function(timeseriesimage) {
  if (nargs() == 0) {
    return(1)
  }
  if (timeseriesimage@dimension != 4) {
    print("FAILED--your input image is not 4D")
    return(NA)
  }
  averageImage <- new("antsImage", timeseriesimage@pixeltype, 3)
  antsMotionCorr(list(d = 3, a = timeseriesimage, o = averageImage))
  return(averageImage)
}
