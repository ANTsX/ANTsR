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
#' 
#' @export integrateVelocityField

integrateVelocityField <- function(
  velocityField,
  lowerIntegrationBound = 0.0,
  upperIntegrationBound = 1.0,
  numberOfIntegrationSteps = 10
  ) {

  dimensionality <- velocityField@dimension - 1

  integratedField <- .Call( "integrateVelocityField",
    dimensionality,
    velocityField, 
    as.numeric( lowerIntegrationBound ), 
    as.numeric( upperIntegrationBound ),
    as.numeric( numberOfIntegrationSteps ),
    PACKAGE = "ANTsR")
  return( integratedField )
}
