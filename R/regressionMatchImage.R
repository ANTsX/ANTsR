#' Image intensity normalization using linear regression.
#'
#' Image intensity normalization by regressing the image
#' intensities of the reference image with the source image.
#'
#' @param sourceImage image whose intensities we will match to the
#'                    \code{referenceImage} intensities.
#' @param referenceImage defines the reference intensity function.
#' @param mask Defines voxels for regression modeling.
#' @param polyOrder of polynomial fit.  Default is 1 (linear fit).
#' @param truncate boolean which turns on/off the clipping of intensities.
#' @return the \code{sourceImage} matched to the \code{referenceImage}.
#' @author Avants BB
#'
#' @examples
#' library(ANTsRCore)
#' sourceImage <- antsImageRead( getANTsRData( "r16" ) )
#' referenceImage <- antsImageRead( getANTsRData( "r64" ) )
#' matchedImage <- regressionMatchImage( sourceImage, referenceImage )
#' bad_source = sourceImage[1:200, 1:200]
#' testthat::expect_error(regressionMatchImage( bad_source, referenceImage ))
#' @export
regressionMatchImage <- function( sourceImage, referenceImage,
  mask = NULL, polyOrder = 1, truncate = TRUE )
  {
  if( any( dim( sourceImage ) != dim( referenceImage ) ) )
    {
    stop( "Images do not have the same dimension." )
    }

  sourceIntensities <- c()
  referenceIntensities <- c()
  if( ! is.null( mask ) )
    {
    sourceIntensities <- as.numeric( sourceImage[mask != 0] )
    referenceIntensities <- as.numeric( referenceImage[mask != 0] )
    } else {
    sourceIntensities <- as.numeric( sourceImage )
    referenceIntensities <- as.numeric( referenceImage )
    }
  sourceIntensitiesPoly <- stats::poly( sourceIntensities, polyOrder )
  model <- lm( referenceIntensities ~ sourceIntensitiesPoly )
  if( ! is.null( mask ) )
    {
    sourceIntensitiesPoly <- stats::poly( as.numeric( sourceImage ), polyOrder )
    }
  matchedSourceIntensities <- predict( model, sourceIntensitiesPoly )

  if( truncate )
    {
    minReferenceValue <-  min( referenceIntensities )
    maxReferenceValue <-  max( referenceIntensities )
    matchedSourceIntensities[matchedSourceIntensities < minReferenceValue] <-
      minReferenceValue
    matchedSourceIntensities[matchedSourceIntensities > maxReferenceValue] <-
      maxReferenceValue
    }

  matchedSourceImage <- makeImage( dim( sourceImage ), matchedSourceIntensities )
  matchedSourceImage <- antsCopyImageInfo( sourceImage,  matchedSourceImage )

  return( matchedSourceImage )
  }
