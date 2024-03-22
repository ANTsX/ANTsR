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
  breakVector <- seq( min( imageArray ), max( imageArray ), length.out = numberOfHistogramBins )
  imageHistogram <- hist( imageVector, breaks = breakVector,  plot = FALSE )
  cdf <- cumsum( imageHistogram$density )
  cdf <- cdf / tail( cdf, n = 1 )
  imageArrayEqualizedFlat <- approx( imageHistogram$breaks[1:( numberOfHistogramBins - 1 )], cdf, imageVector, method = "linear", rule = 2 )
  imageArrayEqualized <- array( data = imageArrayEqualizedFlat$y, dim = dim( imageArray ) )
  imageArrayEqualized <- ( imageArrayEqualized - min( imageArrayEqualized ) ) / ( max( imageArrayEqualized ) - min( imageArrayEqualized ) )
  imageArrayEqualized <- imageArrayEqualized * ( max( imageArray ) - min( imageArray ) ) + min( imageArray )

  return( as.antsImage( imageArrayEqualized, reference = image ) )
  }
