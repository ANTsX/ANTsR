#' histogramEqualizeImage
#'
#' Histogram equalize image
#'
#' @param image image to undergo intensity transformation.
#' @param numberOfHistogramBins number of histogram levels.
#' @return equalized image.
#'
#' @author NJ Tustison
#'
#' @examples
#' image <- antsImageRead( getANTsRData( "r16" ), 2 )
#' eqImage <- histogramEqualizeImage( image )
#' 
#' @export histogramEqualizeImage
histogramEqualizeImage <- function(
  image,
  numberOfHistogramBins = 256
  ) {

  imageArray <- as.array( image )
  imageVector <- as.vector( imageArray )
  imageHistogram <- hist( imageVector, n = numberOfHistogramBins,  plot = FALSE )
  cdf <- cumsum( imageHistogram$density )
  imageArrayEqualizedFlat <- approx( imageHistogram$breaks[1:( numberOfHistogramBins - 2 )], cdf, imageVector, method = "linear" )
  imageArrayEqualized = array( data = imageArrayEqualizedFlat$y, dim = dim( imageArray ) )    
  imageEqualized <- as.antsImage( imageArrayEqualized, reference = image )

  return( imageEqualized )
  }

