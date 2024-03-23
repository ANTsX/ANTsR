#' spatial prior-based image segmentation.
#'
#' markov random field regularized, prior-based image segmentation that is a
#' wrapper around atropos (see ANTs and related publications).
#'
#' @param img input image or image list for multivariate segmentation
#' @param priors list of priors that cover the number of classes
#' @param mask segment inside this mask
#' @param priorWeight usually 0 (priors used for initialization only), 0.25 or 0.5.
#' @param mrf regularization, higher is smoother, a numerical value in range 0.0 to 0.2
#' @param iterations maximum number of iterations.  could be a large value eg 25.
#' @param verbose maximum number of iterations.  could be a large value eg 25.
#' @return segmentation and probability images
#' @author Brian B. Avants
#' @examples
#'
#' fi <- antsImageRead(getANTsRData("r16"))
#' seg <- kmeansSegmentation(fi, 3)
#' msk <- thresholdImage(seg$segmentation, 1, Inf)
#' pseg <- priorBasedSegmentation(fi, seg$probabilityimages, msk, 0.25, 0.1, 3)
#'
#' @export priorBasedSegmentation
priorBasedSegmentation <- function(
    img,
    priors,
    mask,
    priorWeight = 0.25,
    mrf = 0.1,
    iterations = 25,
    verbose = FALSE) {
  if (class(img)[1] == "antsImage") dim <- img@dimension
  if (class(img)[1] == "list") dim <- img[[1]]@dimension
  nhood <- paste(rep(1, dim), collapse = "x")
  mrf <- paste("[", mrf, ",", nhood, "]")
  conv <- paste("[", iterations, ",", 0, "]")
  pseg <- atropos(
    a = img, m = mrf,
    c = conv, i = priors, x = mask,
    priorweight = priorWeight, verbose = verbose
  )
  return(pseg)
}
