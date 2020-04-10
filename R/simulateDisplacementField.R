#' simulateDisplacementField
#'
#' Simulate a displacement field using random points with B-spline
#' smoothing
#'
#' @param domainImage image to define the domain of the field.
#' @param fieldType either "bspline" or "exponential".
#' @param numberOfRandomPoints  number of displacement points.
#' Default = 1000.
#' @param sdNoise standard deviation of the displacement field
#' noise (in mm).  Default = 10.0.
#' @param enforceStationaryBoundary boolean determining fixed boundary
#' conditions.  Default = TRUE.
#' @param numberOfFittingLevels (bspline only) number of fitting levels.
#' Default = 4.
#' @param meshSize (bspline only) scalar or n-D vector determining fitting
#' resolution.  Default = 1.
#' @param sdSmoothing (exponential only) standard deviation of the
#' Gaussian smoothing in mm.  Default = 4.0.
#' @return ANTsR displacement field.
#'
#' @author NJ Tustison
#'
#' @examples
#' domainImage <- antsImageRead( getANTsRData( "r16" ), 2 )
#' expField <- simulateDisplacementField( domainImage, fieldType = "exponential" )
#' bsplineField <- simulateDisplacementField( domainImage, fieldType = "bspline" )
#'
#' @export simulateDisplacementField

simulateDisplacementField <- function(
  domainImage,
  fieldType = c( "bspline", "exponential" ),
  numberOfRandomPoints = 1000,
  sdNoise = 10.0,
  enforceStationaryBoundary = TRUE,
  numberOfFittingLevels = 4,
  meshSize = 1,
  sdSmoothing = 4.0
  ) {

  if( fieldType == 'bspline' )
    {

    imageDimension <- domainImage@dimension
    if( length( meshSize ) != 1 && length( meshSize ) != imageDimension )
      {
      stop( "Error:  incorrect specification for meshSize.")
      }

    splineOrder <- 3
    numberOfControlPoints <- meshSize + splineOrder

    if( length( numberOfControlPoints ) == 1 )
      {
      numberOfControlPoints <- rep( numberOfControlPoints, imageDimension )
      }

    outputField <- .Call( "simulateBSplineDisplacementFieldR",
      antsImageClone( domainImage ),
      as.numeric( numberOfRandomPoints ),
      as.numeric( sdNoise ),
      as.numeric( enforceStationaryBoundary ),
      as.numeric( numberOfFittingLevels ),
      as.numeric( numberOfControlPoints ),
      PACKAGE = "ANTsR" )
    return( outputField )

    } else if( fieldType == 'exponential' ) {

    outputField <- .Call( "simulateExponentialDisplacementFieldR",
      antsImageClone( domainImage ),
      as.numeric( numberOfRandomPoints ),
      as.numeric( sdNoise ),
      as.numeric( enforceStationaryBoundary ),
      as.numeric( sdSmoothing ),
      PACKAGE = "ANTsR" )
    return( outputField )

    } else {
    stop( "Error:  unrecognized field type.")
    }
}