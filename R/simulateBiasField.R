#' Simulate random bias field
#'
#' Low frequency, spatial varying simulated random bias field using
#' random points and B-spline fitting.
#'
#' @param domainImage image to define the spatial domain of the bias field.
#' @param numberOfPoints number of randomly defined points to define the bias
#' field (default = 10).
#' @param sdBiasField characterize the standard deviation of the amplitude
#' (default = 1).
#' @param numberOfFittingLevels B-spline fitting parameter.
#' @param meshSize B-spline fitting parameter (scalar or vector of size image
#' dimension).
#' @return simulated bias field
#' @author Tustison NJ
#' @importFrom stats rnorm
#' @examples
#'
#' library( ANTsR )
#' image <- antsImageRead( getANTsRData( "r16" ) )
#' logField <- simulateBiasField(image, numberOfPoints = 10, sdBiasField = 1.0,
#'    numberOfFittingLevels = 2, meshSize = 10 ) %>% iMath( "Normalize" )
#' logField <- ( exp( logField ) )^4
#' image <- image * logField
#' rm(image); gc()
#' rm(logField); gc()
#' @export simulateBiasField
simulateBiasField <- function( domainImage, numberOfPoints = 10,
  sdBiasField = 1.0, numberOfFittingLevels = 4, meshSize = 1 )
  {

  dimension <- domainImage@dimension
  origin <- antsGetOrigin( domainImage )
  spacing <- antsGetSpacing( domainImage )
  direction <- antsGetDirection( domainImage )
  shape <- dim( domainImage )

  minSpatialDomain <- origin
  maxSpatialDomain <- origin + ( shape - 1.0 ) * spacing

  scatteredData <- array( 0, dim = c( numberOfPoints, 1 ) )
  parametricData <- array( data = 0, dim = c( numberOfPoints, dimension ) )

  scatteredData[,1] <- rnorm( numberOfPoints, mean = 0, sd = sdBiasField )
  for( d in seq.int( dimension ) )
    {
    parametricData[, d] <- runif( numberOfPoints, min = minSpatialDomain[d], max = maxSpatialDomain[d] )
    }

  if( length( meshSize ) == 1 )
    {
    meshSize <- rep( meshSize, dimension )
    }

  biasField <- fitBsplineObjectToScatteredData( scatteredData, parametricData,
    parametricDomainOrigin = origin, parametricDomainSpacing = spacing,
    parametricDomainSize = shape, numberOfFittingLevels = numberOfFittingLevels,
    meshSize = meshSize )

  antsSetDirection( biasField, direction )

  return( biasField )
  }
