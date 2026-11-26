#' Perform partial volume correction for ASL images.
#'
#' This function performs a partial volume correction for ASL images by
#' dividing the observed CBF by the gray matter and white matter probabilities,
#' as described in Johnson et al., Radiology 2005: CBF_corrected = CBF_observed
#' / (GM_prob + 0.4 * WM_prob)
#'
#' @param img Low-resolution image to be corrected.  All input images can be
#' either of type antsImage or numeric vectors (if numeric vectors, the mask is
#' ignored).
#' @param img.gm Gray matter probability image for partial volume correction.
#' @param img.wm White matter probability image for partial volume
#' correction.
#' @param mask Brain mask for image.
#' @param proportion Ratio of activity for white matter to gray matter.
#' Assumed to be 0.4.  Must be a single finite non-negative number.
#' @return Returns partial volume corrected antsImage (or numeric vector for
#' numeric input).
#' @details
#' The correction assumes a fixed WM:GM flow ratio (\code{proportion}) in
#' every voxel and returns the implied GM flow,
#' \code{CBF / (GM + proportion * WM)}.  Voxels with little tissue
#' (\code{GM + WM < 0.25}) are returned uncorrected for numerical
#' stability.  Where the denominator itself is (numerically) zero, e.g. a
#' pure WM voxel with \code{proportion = 0}, the model cannot recover GM
#' flow; those voxels are returned as \code{NA} with a warning.  Small but
#' nonzero denominators still amplify noise.  This fixed-ratio model differs
#' from methods that estimate tissue-specific perfusion, such as FSL BASIL.
#' @author Kandel BM %% ~~who you are~~
#' @references Method: Pattern of cerebral hypoperfusion in Alzheimer disease
#' and mild cognitive impairment measured with arterial spin-labeling MR
#' imaging: initial experience.  Johnson NA, Jahng GH, Weiner MW, Miller BL,
#' Chui HC, Jagust WJ, Gorno-Tempini ML, Schuff N. Radiology 2005.
#'
#' Ratio of GM to WM activity: Quantitative magnetic resonance imaging of human
#' brain perfusion at 1.5 T using steady-state inversion of arterial water.
#' Roberts DA, Detre JA, Bolinger L, Insko EK, Leigh JS Jr. PNAS 1994.
#'
#' @examples
#'
#' activity.gm <- 10
#' activity.wm <- activity.gm * 0.4
#' percent.gm <- matrix(seq(0.1, 1, by = 0.1), nrow = 2)
#' percent.wm <- -percent.gm + 1
#' activity.obs <- percent.gm * rnorm(n = length(percent.gm), mean = activity.gm, sd = 5) +
#'   rnorm(n = length(percent.wm), mean = activity.wm, sd = 5)
#' activity.corrected <- partialVolumeCorrection(activity.obs, percent.gm, percent.wm)
#'
#' @export partialVolumeCorrection
partialVolumeCorrection <- function(img, img.gm, img.wm, mask = NULL, proportion = 0.4) {
  if (!is.numeric(proportion) || length(proportion) != 1 ||
    !is.finite(proportion) || proportion < 0) {
    stop("proportion must be a single finite non-negative number")
  }
  if (is.antsImage(img)) {
    if (is.null(mask)) {
      mask <- antsImageClone(img)
      mask[img != 0] <- 1
    } else {
      mask <- check_ants(mask)
    }
    values.img <- img[mask > 0]
    values.gm <- img.gm[mask > 0]
    values.wm <- img.wm[mask > 0]
  } else if (is.numeric(img)) {
    values.img <- img
    values.gm <- img.gm
    values.wm <- img.wm
  } else {
    stop("Input image must be either antsImage or numeric.")
  }

  denominator <- values.gm + proportion * values.wm
  values.corrected <- values.img / denominator
  # little tissue in the voxel: return it uncorrected (numerical stability)
  # (which() so missing GM/WM values stay NA instead of erroring)
  lowtissue <- which((values.gm + values.wm) < 0.25)
  values.corrected[lowtissue] <- values.img[lowtissue]
  # the fixed-ratio model cannot recover GM flow when its denominator is zero
  unrecoverable <- setdiff(
    which(abs(denominator) < sqrt(.Machine$double.eps)), lowtissue
  )
  if (length(unrecoverable) > 0) {
    warning(
      length(unrecoverable), " voxel(s) have GM + proportion * WM == 0, so GM ",
      "flow cannot be recovered; returning NA for them"
    )
    values.corrected[unrecoverable] <- NA
  }
  if (is.numeric(img)) {
    return(values.corrected)
  } else {
    # floating point so that NA and fractional corrected values survive
    img.corrected <- antsImageClone(
      img, if (img@pixeltype == "double") "double" else "float"
    )
    img.corrected[mask > 0] <- values.corrected
    img.corrected[mask == 0] <- 0
    return(img.corrected)
  }
}
