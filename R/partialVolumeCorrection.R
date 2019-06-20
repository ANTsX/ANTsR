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
#' Assumed to be 0.4.
#' @return Returns partial volume corrected antsImage.
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
#' percent.gm <- matrix(seq(0.1, 1, by=0.1), nrow=2)
#' percent.wm <- -percent.gm + 1
#' activity.obs <- percent.gm * rnorm(n=length(percent.gm), mean=activity.gm, sd=5) +
#'                   rnorm(n=length(percent.wm), mean=activity.wm, sd=5)
#' activity.corrected <- partialVolumeCorrection(activity.obs, percent.gm, percent.wm)
#'
#' @export partialVolumeCorrection
partialVolumeCorrection <- function(img, img.gm, img.wm, mask = NULL, proportion = 0.4) {
  if ( is.antsImage(img)) {
    if (is.null(mask)) {
      mask <- antsImageClone(img)
      mask[img != 0] <- 1
    } else {
      mask = check_ants(mask)
    }
    values.img <- img[mask > 0]
    values.gm <- img.gm[mask > 0]
    values.wm <- img.wm[mask > 0]
  } else if (is.numeric(img)) {
    values.img <- img
    values.gm <- img.gm
    values.wm <- img.wm
  } else stop("Input image must be either antsImage or numeric.")

  values.corrected <- values.img/(values.gm + 0.4 * values.wm)
  values.corrected[(values.gm + values.wm) < 0.25] <- values.img[(values.gm + values.wm) <
    0.25]  # numerical stability
  if (is.numeric(img)) {
    return(values.corrected)
  } else {
    img.corrected <- antsImageClone(img)
    img.corrected[mask > 0] <- values.corrected
    img.corrected[mask == 0] <- 0
    return(img.corrected)
  }
}
