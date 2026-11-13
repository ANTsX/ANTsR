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
  
  # antsApplyTransforms needs the fields as files.  As in ANTsPy
  # (itk::ComposeDisplacementFieldsImageFilter) the result is
  # u(x) = w(x) + d(x + w(x)), which the composite transform built by
  # antsApplyTransforms gives for the order (warping, displacement).
  fieldFiles <- c(tempfile(fileext = ".nii.gz"), tempfile(fileext = ".nii.gz"))
  antsImageWrite(warpingField, fieldFiles[1])
  antsImageWrite(displacementField, fieldFiles[2])
  domainImage <- splitChannels(displacementField)[[1]]
  outimg <- antsApplyTransforms(fixed = domainImage,
                      moving = domainImage,
                      transformlist = fieldFiles,
                      whichtoinvert = c(FALSE, FALSE),
                      compose = tempfile())
  if (is.na(outimg)) {
    stop("Displacement field composition failed.")
  }
  compField <- antsImageRead(outimg)
  unlink(c(fieldFiles, outimg))
  return( compField )
}
