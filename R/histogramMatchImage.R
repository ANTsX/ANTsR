#' histogramMatchImage
#'
#' Match intensity profile with a reference image.  
#'
#' @param sourceImage image to undergo intensity transformation.
#' @param referenceImage image providing reference intensity profile.
#' @param numberOfHistogramBins number of histogram levels.
#' @param numberOfMatchPoints number of histogram match points.
#' @param useThresholdAtMeanIntensity use a simple background exclusion criterion.
#' @return source image intensity matched to reference image.
#'
#' @author NJ Tustison
#'
#' @examples
#' sourceImage <- antsImageRead( getANTsRData( "r16" ), 2 )
#' referenceImage <- antsImageRead( getANTsRData( "r64" ), 2 )
#' matchedImage <- histogramMatchImage( sourceImage, referenceImage )
#' 
#' @export histogramMatchImage

histogramMatchImage <- function(
  sourceImage,
  referenceImage,
  numberOfHistogramBins = 255,
  numberOfMatchPoints = 64,
  useThresholdAtMeanIntensity = FALSE
  ) {

  outputImage <- .Call( "histogramMatchImageR",
    antsImageClone( sourceImage ), 
    antsImageClone( referenceImage ),
    as.numeric( numberOfHistogramBins ), 
    as.numeric( numberOfMatchPoints ),
    as.numeric( useThresholdAtMeanIntensity ),
    PACKAGE = "ANTsR")
  return( outputImage )
}
