#' Transform image intensities based on histogram mapping.
#'
#' Apply B-spline 1-D maps to an input image for intensity warping.
#'
#' @param image input image.
#' @param breakPoints parametric points at which the intensity transform
#' displacements are specified between [0, 1].  Alternatively, a single
#' number can be given and the sequence is linearly spaced in [0, 1].
#' @param displacements displacements to define intensity warping.  Length
#' must be equal to the \code{breakPoints}.  Alternatively, if \code{NULL}
#' random displacements are chosen (random normal:  mean = 0, sd = \code{sdDisplacements}).
#' @param sdDisplacements characterize the randomness of the intensity displacement.
#' @param clampEndPoints specify non-zero intensity change at the ends of the histogram.
#' @param transformDomainSize Defines the sampling resolution of the B-spline warping.
#' @return warped intensity image
#' @author Tustison NJ
#' @importFrom stats rnorm
#' @examples
#'
#' library( ANTsR )
#' image <- antsImageRead( getANTsRData( "r16" ) )
#' transformedImage <- histogramWarpImageIntensities( image, transformDomainSize = 10 )
#' rm(image); gc()
#' rm(transformedImage); gc()
#' @export histogramWarpImageIntensities
histogramWarpImageIntensities <- function( image,
  breakPoints = c( 0.25, 0.5, 0.75 ),
  displacements = NULL, clampEndPoints = c( FALSE, FALSE ),
  sdDisplacements = 0.05, transformDomainSize = 20 )
  {

  if( length( clampEndPoints ) != 2 )
    {
    stop( "clampEndPoints must be a boolean vector of length 2." )
    }

  if( length( breakPoints ) > 1 )
    {
    if( any( breakPoints < 0.0 ) || any( breakPoints > 1.0 ) )
      {
      stop( "If specifying breakPoints as a vector, values must be in the range [0, 1]." )
      }
    }

  parametricPoints <- NULL
  numberOfNonZeroDisplacements <- 1
  if( length( breakPoints ) > 1 )
    {
    parametricPoints <- breakPoints
    numberOfNonZeroDisplacements <- length( breakPoints )
    if( clampEndPoints[1] == TRUE )
      {
      parametricPoints <- c( 0, parametricPoints )
      }
    if( clampEndPoints[2] == TRUE )
      {
      parametricPoints <- c( parametricPoints, 1 )
      }
    } else {
    parametricPoints <- seq( 0, 1, length.out = breakPoints + length( which( clampEndPoints == TRUE ) ) )
    numberOfNonZeroDisplacements <- breakPoints
    }

  if( is.null( displacements ) )
    {
    displacements <- rnorm( numberOfNonZeroDisplacements, 0, sdDisplacements )
    }

  weights <- rep( 1, length( displacements ) )
  if( clampEndPoints[1] == TRUE )
    {
    displacements <- c( 0, displacements )
    weights <- c( 1000, weights )
    }
  if( clampEndPoints[2] == TRUE )
    {
    displacements <- c( displacements, 0 )
    weights <- c( weights, 1000 )
    }

  if( length( displacements ) != length( parametricPoints ) )
    {
    cat( "displacements = ", displacements, "\n" )
    cat( "break points = ", parametricPoints, "\n" )
    stop( "Length of displacements does not match the length of the break points." )
    }

  scatteredData <- matrix( displacements )
  parametricData <- matrix( parametricPoints )

  transformDomainOrigin <- 0
  transformDomainSpacing <- ( 1.0 - transformDomainOrigin ) / ( transformDomainSize - 1 )

  bsplineHistogramTransform <- fitBsplineObjectToScatteredData( scatteredData, parametricData,
    c( transformDomainOrigin ), c( transformDomainSpacing ), c( transformDomainSize ),
    dataWeights = weights, isParametricDimensionClosed = NULL, numberOfFittingLevels = 4,
    meshSize = 1, splineOrder = 3 )

  transformDomain <- seq( 0, 1, length.out = transformDomainSize )

  normalizedImage <- antsImageClone( image ) %>% iMath( "Normalize" )
  transformedArray <- as.array( normalizedImage )
  normalizedArray <- as.array( normalizedImage )

  for( i in seq.int( length( transformDomain ) - 1 ) )
    {
    indices <- which( normalizedArray >= transformDomain[i] & normalizedArray < transformDomain[i+1] )
    intensities <- normalizedArray[indices]

    alpha <- ( intensities - transformDomain[i] ) / ( transformDomain[i+1] - transformDomain[i] )
    xfrm <- alpha * ( bsplineHistogramTransform[i+1, 1] - bsplineHistogramTransform[i, 1] ) + bsplineHistogramTransform[i, 1]
    transformedArray[indices] <- intensities + xfrm
    }
  transformedImage <- as.antsImage( transformedArray, reference = image ) *
    ( ANTsRCore::max( image ) - ANTsRCore::min( image ) ) + ANTsRCore::min( image )

  return( transformedImage )
  }
