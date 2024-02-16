#' fitThinPlateSplineDisplacementField
#'
#' Fit a thin-plate spline object to a set of source points with associated displacements.  
#' This is basically a wrapper
#' for the ITK class \url{https://itk.org/Doxygen/html/itkThinPlateSplineKernelTransform_8h.html}.
#' Returns a displacement field.
#'
#' @param displacementOrigins matrix (\code{numberOfPoints x dimension}) defining the
#' origins of the input displacement points.  Default = NULL.
#' @param displacements matrix (\code{numberOfPoints x dimension}) defining the
#' displacements of the input displacement points.  Default = NULL.
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
#' tpsField <- fitThinPlateSplineDisplacementField(
#'   displacementOrigins = points, displacements = deltas,
#'   origin = c( 0.0, 0.0 ), spacing = c( 1.0, 1.0 ), size = c( 100, 100 ),
#'   direction = matrix( data = c( -1, 0, 0, -1 ), nrow = 2, byrow = TRUE ) )
#'
#' @export fitThinPlateSplineDisplacementField

fitThinPlateSplineDisplacementField <- function(
  displacementOrigins = NULL,
  displacements = NULL,
  origin = NULL,
  spacing = NULL,
  size = NULL,
  direction = NULL
  ) {

  if( is.null( displacementOrigins ) || is.null( displacements ) )
    {
    stop( "Error: missing input.  Input point set (origins + displacements) needs to be specified." )
    }

  if( is.null( origin ) || is.null( spacing ) || is.null( size ) || is.null( direction ) )
    {
    stop( "Error: one must fully specify the input physical domain." )
    }

  dimensionality <- ncol( displacementOrigins )
  if( ncol( displacements ) != dimensionality )
    {
    stop( "Error:  Dimensionality between origins and displacements does not match." )
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

  tpsField <- ANTsRCore::fitThinPlateSplineDisplacementField(
    dimensionality,
    displacementOrigins, displacements,
    origin, spacing, size, direction)
  return( tpsField )
}
