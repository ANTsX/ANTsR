#' fitBsplineDisplacementField
#'
#' Fit a b-spline object to a dense displacement field image and/or a set of points
#' with associated displacements and smooths them using B-splines.  The inverse
#' can also be estimated..  This is basically a wrapper
#' for the ITK filter \url{https://itk.org/Doxygen/html/classitk_1_1DisplacementFieldToBSplineImageFilter.html}
#' which, in turn is a wrapper for the ITK filter used for the function
#' \code{ANTsR::fitBsplineObjectToScatteredData}
#'
#' @param displacementField input displacement field.  Either this and/or the points
#' must be specified.
#' @param displacementWeightImage input image defining weighting of the voxelwise displacements
#' in the \code{displacementField}.  if \code{NULL}, defaults to identity weighting for
#' each displacement.  Default = NULL.
#' @param displacementOrigins matrix (\code{numberOfPoints x dimension}) defining the
#' origins of the input displacement points.  Default = NULL.
#' @param displacements matrix (\code{numberOfPoints x dimension}) defining the
#' displacements of the input displacement points.  Default = NULL.
#' @param displacementWeights vector defining the individual weighting of the corresponding
#' scattered data value.  Default = NULL meaning all values are weighted the same.
#' @param origin vector defining the physical origin of the B-spline
#' displacement field.  Must be specified if \code{displacementField} is not specified.
#' Otherwise, the \code{displacementField} is used to define the domain parameters.
#' @param spacing vector defining the physical spacing of the B-spline
#' object.  Defines the sampling rate in the parametric domain.  Must be specified if
#' \code{displacementField} is not specified. Otherwise, the
#' \code{displacementField} is used to define the domain parameters.
#' @param size vector defining the size (length) of the
#' B-spline object.  Note that the length of the B-spline object in dimension \code{d}
#' is defined as \code{parametricDomainSpacing[d] * (parametricDomainSize[d]-1)}.
#' Must be specified if \code{displacementField} is not specified.  Otherwise, the
#' \code{displacementField} is used to define the domain parameters.
#' @param direction matrix defining the orientation of the sampled
#' B-spline object.  Must be specified if \code{displacementField} is not specified.
#' Otherwise, the \code{displacementField} is used to define the domain parameters.
#' @param numberOfFittingLevels integer specifying the number of fitting levels.
#' @param meshSize vector defining the mesh size at the initial fitting level.
#' @param splineOrder spline order of the B-spline object.  Default = 3.
#' @param enforceStationaryBoundary ensure no displacements on the image boundary.
#' Default = TRUE.
#' @param estimateInverse estimate the inverse displacement field.  Default = FALSE.
#' @param rasterizePoints distribute the displacements (and weights) to the image domain
#' grid for speed-up.  Is only compatible for points only.
#' @return ANTsR image.
#'
#' @author NJ Tustison
#'
#' @examples
#'
#' # Perform 2-D fitting
#'
#' points <- matrix( data = c( -50, -50 ), nrow = 1, byrow = TRUE )
#' deltas <- matrix( data = c( 10, 10 ), nrow = 1, byrow = TRUE )
#'
#' bsplineField <- fitBsplineDisplacementField(
#'   displacementOrigins = points, displacements = deltas,
#'   origin = c( 0.0, 0.0 ), spacing = c( 1.0, 1.0 ), size = c( 100, 100 ),
#'   direction = matrix( data = c( -1, 0, 0, -1 ), nrow = 2, byrow = TRUE ),
#'   numberOfFittingLevels = 4, meshSize = c( 1, 1 ) )
#'
#' @export fitBsplineDisplacementField

fitBsplineDisplacementField <- function(
  displacementField = NULL,
  displacementWeightImage = NULL,
  displacementOrigins = NULL,
  displacements = NULL,
  displacementWeights = NULL,
  origin = NULL,
  spacing = NULL,
  size = NULL,
  direction = NULL,
  numberOfFittingLevels = 4,
  meshSize = 1,
  splineOrder = 3,
  enforceStationaryBoundary = TRUE,
  estimateInverse = FALSE,
  rasterizePoints = FALSE
  ) {

  if( is.null( displacementField ) && ( is.null( displacementOrigins ) || is.null( displacements ) ) )
    {
    stop( "Error: missing input.  Either a displacement field or input point set (origins + displacements) needs to be specified." )
    }

  if( is.null( displacementField ) && ( is.null( origin ) || is.null( spacing ) || is.null( size ) || is.null( direction ) ) )
    {
    stop( "Error: if the displacement field is not specified, one must fully specify the input physical domain." )
    }

  if( ! is.null( displacementField ) && is.null( displacementWeightImage ) )
    {
    displacementWeightImage <- makeImage( dim( displacementField ), voxval = 1,
      spacing = antsGetSpacing( displacementField ), origin = antsGetOrigin( displacementField ),
      direction = antsGetDirection( displacementField ), components = FALSE )
    }

  if( ! is.null( displacementField ) && is.null( displacementWeightImage ) )
    {
    if( is.null( origin ) )
      {
      origin <- antsGetOrigin( displacementField )
      }
    if( is.null( spacing ) )
      {
      spacing <- antsGetSpacing( displacementField )
      }
    if( is.null( direction ) )
      {
      direction <- antsGetDirection( displacementField )
      }
    if( is.null( size ) )
      {
      size <- dim( displacementField )
      }
    }

  dimensionality <- NULL
  if( ! is.null( displacementField ) )
    {
    dimensionality <- displacementField@dimension
    } else {
    dimensionality <- ncol( displacementOrigins )
    if( ncol( displacements ) != dimensionality )
      {
      stop( "Error:  Dimensionality between origins and displacements does not match." )
      }
    }

  if( ! is.null( displacementOrigins ) )
    {
    if( ! is.null( displacementWeights ) && ( length( displacementWeights ) != nrow( displacementOrigins ) ) )
      {
      stop( "Error:  length of displacement weights must match the number of displacement points." )
      } else {
      displacementWeights <- rep( 1.0, nrow( displacementOrigins ) )
      }
    }

  if( length( meshSize ) != 1 && length( meshSize ) != dimensionality )
    {
    stop( "Error:  incorrect specification for meshSize.")
    }
  if( ! is.null( origin ) && length( origin ) != dimensionality )
    {
    stop( "Error:  origin is not of length dimensionality." )
    }
  if( ! is.null( spacing ) && length( spacing ) != dimensionality )
    {
    stop( "Error:  spacing is not of length dimensionality." )
    }
  if( ! is.null( size ) && length( size ) != dimensionality )
    {
    stop( "Error:  size is not of length dimensionality." )
    }
  if( ! is.null( direction ) &&
    ( dim( direction )[1] != dimensionality || dim( direction )[2] != dimensionality ) )
    {
    stop( "Error:  direction is not of size dimensionality x dimensionality." )
    }

  numberOfControlPoints <- meshSize + splineOrder

  if( length( numberOfControlPoints ) == 1 )
    {
    numberOfControlPoints <- rep( numberOfControlPoints, dimensionality )
    }

  bsplineField <- .Call( "fitBsplineDisplacementField",
    dimensionality,
    displacementField, displacementWeightImage,
    displacementOrigins, displacements, displacementWeights,
    origin, spacing, size, direction,
    numberOfFittingLevels, numberOfControlPoints, splineOrder,
    enforceStationaryBoundary, estimateInverse, rasterizePoints,
    PACKAGE = "ANTsR" )
  return( bsplineField )
}
