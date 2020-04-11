#' fitBsplineObjectToScatteredData
#'
#' Fit a b-spline object to scattered data.  This is basically a wrapper
#' for the ITK filter \url{https://itk.org/Doxygen/html/classitk_1_1BSplineScatteredDataPointSetToImageFilter.html}.
#' This filter is flexible in the possible objects that can be approximated.
#' Possibilities include:
#'
#'     * 1/2/3/4-D curve
#'     * 2-D surface in 3-D space
#'     * 1/2/3/4-D scalar field
#'     * 2/3-D displacement field
#'
#' In order to understand the input parameters, it is important to understand
#' the difference between the parametric and data dimensions.  A curve as one
#' parametric dimension but the data dimension can be 1-D, 2-D, 3-D, or 4-D.
#' In contrast, a 3-D displacement field has a parametric and data dimension
#' of 3.  The scattered data is what's approximated by the B-spline object and
#' the parametric point is the location of scattered data within the domain of
#' the B-spline object.
#'
#' @param scatteredData matrix defining the scattered data input to be approximated.
#' Data is organized by row --> data v, column ---> data dimension.
#' @param parametricData matrix defining the parametric location of the scattered
#' data.  Data is organized by row --> parametric point, column --> parametric
#' dimension.  Note that each row corresponds to the same row in the
#' \code{scatteredData}.
#' @param dataWeights vector defining the individual weighting of the corresponding
#' scattered data value.  Default = NULL meaning all values are weighted the same.
#' @param parametricDomainOrigin vector defining the parametric origin of the B-spline
#' object.
#' @param parametricDomainSpacing vector defining the parametric spacing of the B-spline
#' object.  Defines the sampling rate in the parametric domain.
#' @param parametricDomainSize vector defining the size (length) of the
#' B-spline object.  Note that the length of the B-spline object in dimension \code{d}
#' is defined as \code{parametricDomainSpacing[d] * (parametricDomainSize[d]-1)}.
#' @param isParametricDimensionClosed vector of bools defining whether or not the
#' corresponding parametric dimension is closed (e.g., closed loop).  Default = FALSE.
#' @param numberOfFittingLevels integer specifying the number of fitting levels.
#' @param meshSize vector defining the mesh size at the initial fitting level.
#' @param splineOrder spline order of the B-spline object.  Default = 3.
#' @param centerize subtract the mean data value before fitting.  Default = TRUE.
#' @return Matrix for B-spline curve.  Otherwise, returns ANTsR image.
#'
#' @author NJ Tustison
#'
#' @examples
#'
#' x <- seq( from = -4, to = 4, by = 0.1 )
#' y <- exp( -(x * x) ) + runif( length( x ), min = -0.1, max = 0.1 )
#' u <- seq( from = 0.0, to = 1.0, length.out = length( x ) )
#' scatteredData <- cbind( x, y )
#' parametricData <- as.matrix( u, ncol = 1 )
#' numberOfSamplePoints <- 100
#' spacing <- 1/(numberOfSamplePoints-1) * 1.0;
#'
#' bsplineCurve <- fitBsplineObjectToScatteredData( scatteredData, parametricData,
#'   parametricDomainOrigin = c( 0.0 ), parametricDomainSpacing = c( spacing ),
#'   parametricDomainSize = c( numberOfSamplePoints ), isParametricDimensionClosed = c( FALSE ),
#'   numberOfFittingLevels = 5, meshSize = 1 )
#'
#' plot( x, y, "p", col = "red" )
#' points( scatteredData[,1], scatteredData[,2], col = "green" )
#' lines( bsplineCurve[,1], bsplineCurve[,2], col = "blue" )
#'
#' @export fitBsplineObjectToScatteredData

fitBsplineObjectToScatteredData <- function(
  scatteredData,
  parametricData,
  parametricDomainOrigin,
  parametricDomainSpacing,
  parametricDomainSize,
  isParametricDimensionClosed = NULL,
  dataWeights = NULL,
  numberOfFittingLevels = 4,
  meshSize = 1,
  splineOrder = 3,
  centerize = TRUE
  ) {

  if( missing( scatteredData ) )
    {
    stop( "Error: missing scatteredData." )
    }

  if( missing( parametricData ) )
    {
    stop( "Error: missing parametricData." )
    }

  if( is.null( isParametricDimensionClosed ) )
    {
    isParametricDimensionClosed <- rep( TRUE, parametricDimension )
    }

  parametricDimension <- ncol( parametricData )
  if( length( meshSize ) != 1 && length( meshSize ) != imageDimension )
    {
    stop( "Error:  incorrect specification for meshSize.")
    }
  if( length( parametricDomainOrigin ) != parametricDimension )
    {
    stop( "Error:  origin is not of length parametricDimension." )
    }
  if( length( parametricDomainSpacing ) != parametricDimension )
    {
    stop( "Error:  spacing is not of length parametricDimension." )
    }
  if( length( parametricDomainSize ) != parametricDimension )
    {
    stop( "Error:  size is not of length parametricDimension." )
    }
  if( length( isParametricDimensionClosed ) != parametricDimension )
    {
    stop( "Error:  closed is not of length parametricDimension." )
    }

  splineOrder <- 3
  numberOfControlPoints <- meshSize + splineOrder

  if( length( numberOfControlPoints ) == 1 )
    {
    numberOfControlPoints <- rep( numberOfControlPoints, parametricDimension )
    }

  if( nrow( parametricData ) != nrow( scatteredData ) )
    {
    stop( "Error:  the number of points is not equal to the number of scattered data values." )
    }

  if( is.null( dataWeights ) )
    {
    dataWeights <- rep( 1.0, nrow( parametricData ) )
    }
  if( length( dataWeights ) != nrow( parametricData ) )
    {
    stop( "Error:  number of weights is not the same as the number of points." )
    }

  if( centerize == TRUE )
    {
    centerizeValues <- colMeans( scatteredData )

    centeredData <- scatteredData
    for( j in seq_len( length( centerizeValues ) ) )
      {
      centeredData[,j] <- scatteredData[,j] - centerizeValues[j]
      }

    bsplineObject <- .Call( "fitBsplineObjectToScatteredData",
      centeredData, parametricData, dataWeights,
      parametricDomainOrigin, parametricDomainSpacing,
      parametricDomainSize, isParametricDimensionClosed,
      numberOfFittingLevels, numberOfControlPoints,
      splineOrder,
      PACKAGE = "ANTsR" )

    for( j in seq_len( length( centerizeValues ) ) )
      {
      bsplineObject[,j] <- bsplineObject[,j] + centerizeValues[j]
      }
    return( bsplineObject )  
    } else {
    bsplineObject <- .Call( "fitBsplineObjectToScatteredData",
      scatteredData, parametricData, dataWeights,
      parametricDomainOrigin, parametricDomainSpacing,
      parametricDomainSize, isParametricDimensionClosed,
      numberOfFittingLevels, numberOfControlPoints,
      splineOrder,
      PACKAGE = "ANTsR" )
    return( bsplineObject )  
    }
}