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
#' @export composeDisplacementFields
composeDisplacementFields <- function(
    displacementField,
    warpingField
) {
  
  dimensionality <- displacementField@dimension
  
  compField <- ANTsRCore::composeDisplacementFields(
                      dimensionality,
                      displacementField,
                      warpingField)
  return( compField )
}
