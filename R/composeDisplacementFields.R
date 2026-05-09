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
  outimg <- tempfile(fileext = ".nii.gz")
  antsApplyTransforms(fixed = displacementField,
                      moving = displacementField,
                      transformlist = list(displacementField, warpingField),
                      compose = outimg)
  compField <- antsImageRead(outimg)
  return( compField )
}
