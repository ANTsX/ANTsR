#' invertDisplacementField
#'
#' Invert displacement field.
#'
#' @param displacementField displacement field.
#' @param inverseFieldInitialEstimate initial guess.
#' @param maximumNumberOfIterations number of iterations.
#' @param meanErrorToleranceThreshold mean error tolerance threshold.
#' @param maxErrorToleranceThreshold max error tolerance threshold.
#' @param enforceBoundaryCondition enforce stationary boundary condition.
#' @return inverse displacement field
#'
#' @author NJ Tustison
#'
#' @export invertDisplacementField

invertDisplacementField <- function(
    displacementField,
    inverseFieldInitialEstimate,
    maximumNumberOfIterations = 20,
    meanErrorToleranceThreshold = 0.001,
    maxErrorToleranceThreshold = 0.1,
    enforceBoundaryCondition = TRUE) {
  dimensionality <- displacementField@dimension

  inverseField <- ANTsRCore::invertDisplacementFieldR(
    dimensionality,
    displacementField,
    inverseFieldInitialEstimate,
    as.numeric(maximumNumberOfIterations),
    as.numeric(meanErrorToleranceThreshold),
    as.numeric(maxErrorToleranceThreshold),
    as.numeric(enforceBoundaryCondition)
  )
  return(inverseField)
}
