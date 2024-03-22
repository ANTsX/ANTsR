#' integrateVelocityField
#'
#' Integrate velocity field
#'
#' @param velocityField time-varying displacement field
#' @param lowerIntegrationBound Lower time bound for integration in [0, 1]
#' @param upperIntegrationBound Upper time bound for integration in [0, 1]
#' @param numberOfIntegrationSteps Number of integration steps used in the Runge-Kutta solution
#' @return integrated field
#'
#' @author NJ Tustison
#'
#' @examples
#' fi <- antsImageRead( getANTsRData( "r16" ) )
#' mi <- antsImageRead( getANTsRData( "r27" ) )
#' reg <- antsRegistration( fi, mi, "TV[2]" )
#' velocityField <- antsImageRead( reg$velocityfield )
#' field <- integrateVelocityField( velocityField, 0.0, 1.0, 10 )
#'
#' @export integrateVelocityField

integrateVelocityField <- function(
  velocityField,
  lowerIntegrationBound = 0.0,
  upperIntegrationBound = 1.0,
  numberOfIntegrationSteps = 10
  ) {

  dimensionality <- velocityField@dimension - 1

  integratedField <- ANTsRCore::integrateVelocityField(
    dimensionality,
    velocityField, 
    as.numeric( lowerIntegrationBound ), 
    as.numeric( upperIntegrationBound ),
    as.numeric( numberOfIntegrationSteps ))
  return( integratedField )
}
