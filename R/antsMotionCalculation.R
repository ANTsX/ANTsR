#'  antsMotionCalculation
#'
#'  Correct 4D time-series data for motion.
#'
#' @param img antsImage, usually 4D.
#' @param mask mask for image (3D).  If not provided, estimated from data.
#' @param fixed Fixed image to register all timepoints to.
#' If not provided, mean image is used.
#' @param moreaccurate strategy desired for motion correction.  One of 0 (test)
#' 1 (high-res only), 2 (multi-level inter-subject), 3 (FIXME), or a special
#' method for intraSubjectBOLD.
#' @param txtype Type of transform.  One of \code{"Affine"},
#' \code{"Rigid"}, or
#' \code{"SyN"}.
#' @param framewise Calculate framewise displacement?
#' @param verbose enables verbose output.
#' @param num_threads will execute
#' \code{Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = num_threads)} before
#' running to attempt a more reproducible result.  See
#' \url{https://github.com/ANTsX/ANTs/wiki/antsRegistration-reproducibility-issues}
#' for discussion.  If \code{NULL}, will not set anything.
#' @param seed will execute
#' \code{Sys.setenv(ANTS_RANDOM_SEED = seed)} before
#' running to attempt a more reproducible result.  See
#' \url{https://github.com/ANTsX/ANTs/wiki/antsRegistration-reproducibility-issues}
#' for discussion.  If \code{NULL}, will not set anything.
#' @param ... additional argument to \code{\link{.motion_correction}}
#'
#' @return List containing:
#' \itemize{
#'  \item{moco_img}{ Motion corrected time-series image.}
#'  \item{moco_params}{ Data frame of translation parameters.}
#'  \item{moco_avg_img}{ Average motion-corrected image.}
#'  \item{moco_mask}{ Mask used to calculate framewise displacement.}
#'  \item{tsDisplacement}{ Time-series displacement image.}
#'  \item{fd}{ Time-series mean and max displacements.}
#'  \item{dvars}{ DVARS, derivative of frame-wise intensity changes.}
#' }
#' @author Benjamin M. Kandel
#' @examples
#' \dontrun{
#' set.seed(120)
#' simimg <- makeImage(rep(5, 4), rnorm(5^4))
#' # for real data, use simimg <- antsImageRead(getANTsRData('pcasl'), 4)
#' antsMotionCalculation(simimg, moreaccurate = 0)
#' }
#' @export antsMotionCalculation
antsMotionCalculation <- function(
    img, mask = NULL, fixed = NULL, moreaccurate = 1,
    txtype = "Affine", framewise = 1, verbose = FALSE,
    num_threads = 1,
    seed = NULL,
    ...) {
  if (is.null(fixed)) {
    fixed <- getAverageOfTimeSeries(img)
  }
  moco <- .motion_correction(
    img,
    fixed = fixed,
    moreaccurate = moreaccurate, txtype = txtype, verbose = verbose,
    num_threads = num_threads,
    seed = seed,
    ...
  )
  #  moco <- .motion_correction(img, fixed=moco$moco_avg_img,
  #    moreaccurate = moreaccurate, txtype=txtype, verbose=verbose )
  mocoparams <- moco$moco_params
  if (is.null(mask)) {
    mask <- getMask(moco$moco_avg_img, mean(moco$moco_avg_img),
      Inf,
      cleanup = 2
    )
  }
  tsimg <- antsImageClone(img, "double")
  mocostats <- .antsMotionCorrStats(tsimg, mask, mocoparams,
    num_threads = num_threads,
    seed = seed
  )
  fd <- as.data.frame(mocostats$Displacements)
  names(fd) <- c("MeanDisplacement", "MaxDisplacement")
  aslmat <- timeseries2matrix(img, mask)
  dvars <- computeDVARS(aslmat)
  list(
    moco_img = antsImageClone(moco$moco_img),
    moco_params = mocoparams,
    moco_avg_img = antsImageClone(moco$moco_avg_img),
    moco_mask = antsImageClone(mask),
    tsDisplacement = mocostats$TimeSeriesDisplacement,
    fd = fd,
    dvars = dvars
  )
}
