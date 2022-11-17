#' invertDisplacementField
#'
#' Invert displacement field.
#'
#' @param displacementField displacement field.
#' @param inverse_field_initial_estimate initial guess.
#' @param maximum_number_of_iterations number of iterations.
#' @param meanErrorToleranceThreshold mean error tolerance threshold.
#' @param maxErrorToleranceThreshold max error tolerance threshold.
#' @param enforceBoundaryCondition enforce stationary boundary condition.
#' @return inverse displacement field
#'
#' @author NJ Tustison
#'
#' @examples
#' 
#' @export invertDisplacementField

invertDisplacementField <- function(
  displacementField,
  inverseFieldInitialEstimate,
  maximumNumberOfIterations = 20,
  meanErrorToleranceThreshold = 0.001,
  maxErrorToleranceThreshold = 0.1,
  enforceBoundaryCondition = True
  ) {

  dimensionality <- displacementField@dimension

  inverseField <- .Call( "invertDisplacementFieldR",
    dimensionality,
    displacementField, 
    inverseFieldInitialEstimate,
    as.numeric( maximumNumberOfIterations ), 
    as.numeric( meanErrorToleranceThreshold ),
    as.numeric( maxErrorToleranceThreshold ),
    as.numeric( enforceBoundaryCondition ),
    PACKAGE = "ANTsR")
  return( inverseField )
}
