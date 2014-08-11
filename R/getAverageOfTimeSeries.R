getAverageOfTimeSeries <- function( timeseriesimage ) {
  if (nargs() == 0) {
    return(1)
  }
  if ( timeseriesimage@dimension != 4 ) {
    print("FAILED--your input image is not 4D")
    return(NA)
  }
  averageImage<-new("antsImage", timeseriesimage@pixeltype, 3)
  antsMotionCorr(list(d = 3, a = timeseriesimage, o = averageImage))
  return( averageImage ) 
} 
