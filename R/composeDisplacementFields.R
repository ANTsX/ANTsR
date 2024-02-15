#' composeDisplacementFields
#'
#' Compose displacement fields.
#'
#' @param displacementField displacement field.
#' @param warpingField warping field.
#' @return composite displacement field
#'
#' @author NJ Tustison
#'
#' @export
composeDisplacementFields <- function(
  displacementField,
  warpingField
  ) {

  dimensionality <- displacementField@dimension

  compField <- .Call( "composeDisplacementFields",
    dimensionality,
    displacementField,
    warpingField,
    PACKAGE = "ANTsRCore" )
  return( compField )
}
