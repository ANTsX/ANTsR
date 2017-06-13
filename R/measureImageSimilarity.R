#' Wrapper for the ANTs function measureImageSimilarity
#'
#' Calculate the similarity between a pair of images using metrics available
#' via ITK and optional fixed and/or moving image masks.
#'
#' @param fixedImage Fixed image.
#' @param movingImage Moving image.
#' @param fixedMask Optional mask limiting the voxels considered by the metric.
#' @param movingMask Optional mask limiting the voxels considered by the metric.
#' @param metric Possible options include: ANTs neighborhoo cross-correlation
#'        ('cc'), joint mutual information ('mi'), Mattes mutual information ('mattes'),
#'        mean squares ('meansquares'), demons ('demons'), global cross-correlation
#'        ('gc').
#' @param radiusOrNumberOfBins Parameter which depends on the metric.  For 'cc',
#'        this value determines the radius of the correlation window.  For either of
#'        the mutual information metrics, this determines the number of bins.
#' @param samplingStrategy Sampling strategy employed by the metric.  Either
#'        'none', 'regular', or 'random'.
#' @param samplingPercentage Metric sampling percentage (in the range [0,1]).
#' @return scalar value
#' @author Tustison NJ, Avants BB
#' @examples
#'
#' fixedImage <- antsImageRead( getANTsRData( "r16" ), 2 )
#' movingImage <- antsImageRead( getANTsRData( "r64" ), 2 )
#' value <- measureImageSimilarity( fixedImage, movingImage )
#'
#' @export measureImageSimilarity
measureImageSimilarity <- function( fixedImage, movingImage,
  fixedMask = NA,
  movingMask = NA,
  metric = 'cc',
  radiusOrNumberOfBins = 4,
  samplingStrategy = 'none',
  samplingPercentage = 1.0 ) {

  simArgs <- list( '-d' = as.character( fixedImage@dimension ) )

  metricOption <- list( '-m', paste0( metric, '[',
                          antsrGetPointerName( fixedImage ),
                          antsrGetPointerName( movingImage ),
                          1, radiusOrNumberOfBins,
                          samplingStrategy, samplingPercentage,
                          ']' ) )
  simArgs <- lappend( simArgs, metricOption )

  maskOption <- list( '-x', '[NA, NA]' )
  if( ! is.na( fixedMask ) && ! is.na( movingMask ) )
    {
    maskOption <- list( '-x', paste0( '[',
                        antsrGetPointerName( fixedMask ),
                        antsrGetPointerName( movingMask ),
                        ']' ) )
    }
  if( ! is.na( fixedMask ) && is.na( movingMask ) )
    {
    maskOption <- list( '-x', antsrGetPointerName( fixedMask ) )
    }
  simArgs <- lappend( simArgs, maskOption )

  metricValue <- .Call( "MeasureImageSimilarity",
    .int_antsProcessArguments( simArgs ), PACKAGE = "ANTsR" )
  return( metricValue )
}
