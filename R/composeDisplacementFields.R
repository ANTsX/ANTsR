#' composeDisplacementFields
#'
#' Compose displacement fields.
#'
#' As in ANTsPy (\code{itk::ComposeDisplacementFieldsImageFilter}), the
#' result is \eqn{u(x) = w(x) + d(x + w(x))}, where \eqn{d} is
#' \code{displacementField} and \eqn{w} is \code{warpingField}: the warping
#' field is applied first.  The result is defined on the grid of
#' \code{displacementField}.
#'
#' @param displacementField displacement field.
#' @param warpingField warping field.
#' @return composite displacement field
#'
#' @author NJ Tustison
#' @examples
#' d <- as.antsImage(array(0.5, c(2, 8, 8)), components = TRUE)
#' w <- as.antsImage(array(1, c(2, 8, 8)), components = TRUE)
#' u <- composeDisplacementFields(d, w)
#'
#' @export composeDisplacementFields
composeDisplacementFields <- function(
    displacementField,
    warpingField
) {
  # composing in the order (warping, displacement) gives
  # u(x) = w(x) + d(x + w(x)), as checked in test-composeDisplacementFields.R
  composed <- antsApplyTransforms(
    fixed = displacementField,
    moving = displacementField,
    transformlist = list(warpingField, displacementField),
    whichtoinvert = c(FALSE, FALSE),
    compose = tempfile()
  )
  if (is.na(composed)) {
    stop("Displacement field composition failed.")
  }
  on.exit(unlink(composed), add = TRUE)
  antsImageRead(composed)
}
