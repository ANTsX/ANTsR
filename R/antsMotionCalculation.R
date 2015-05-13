#' @name antsMotionCalculation
#' @title Correct 4D time-series data for motion.
#' @usage antsMotionCalculation(img, mask=NA, fixed=NA, moreaccurate=1, framewise=1)
#' @param img antsImage, usually 4D.
#' @param mask mask for image (3D).  If not provided, estimated from data.
#' @param fixed Fixed image to register all timepoints to.  If not provided, mean image is used.
#' @param moreaccurate Level of accuracy desired for motion correction.  Higher is more accurate.
#' @param framewise Calculate framewise displacement?
#' @return List containing:
#' \itemize{
#'  \item{moco_img}{ Motion corrected time-series image.}
#'  \item{moco_params}{ Data frame of translation parameters.}
#'  \item{moco_avg_img}{ Average motion-corrected image.}
#'  \item{moco_mask}{ Mask used to calculate framewise displacement.}
#'  \item{tsDisplacement}{ Time-series displacement image.}
#'  \item{dvars}{ DVARS, derivative of frame-wise intensity changes.}
#' }
#' @author Benjamin M. Kandel
#' @examples
#' set.seed(120)
#' simimg<-makeImage(rep(5,4), rnorm(5^4))
#' # for real data, use simimg <- antsImageRead(getANTsRData('pcasl'), 4)
#' antsMotionCalculation(simimg,moreaccurate=0)
#' @export antsMotionCalculation
antsMotionCalculation <- function(img, mask = NA, fixed = NA, moreaccurate = 1, framewise = 1) {
  if ( is.na( fixed )  )
  {
  fixed <- getAverageOfTimeSeries( img )
  }
  moco <- .motion_correction( img, fixed = fixed, moreaccurate = moreaccurate)
  tmpdir <- tempdir()
  file.mocoparam <- tempfile( fileext="moco.csv" )
  file.out <- tempfile( fileext="out.csv" )
  tsout<-sub("out.csv", "out.nii.gz", file.out ) # should replace text here
  write.csv(moco$moco_params, file.mocoparam, row.names = F)
  if (is.na(mask)) {
    mask <- getMask(moco$moco_avg_img, mean(moco$moco_avg_img),
      Inf, cleanup = 2)
  }
  tsimg <- antsImageClone( img, "double" )
 #.antsMotionCorrStats( list( x = mask, d = tsimg, o = file.out,
 #   f = framewise,
 #   m = file.mocoparam) )
 # tsDisplacement <- antsImageRead( tsout , 4, "double" )
  aslmat <- timeseries2matrix( img, mask)
  dvars <- computeDVARS(aslmat)
  list(
    moco_img = antsImageClone(moco$moco_img),
    moco_params = moco$moco_params,
    moco_avg_img = antsImageClone(moco$moco_avg_img),
    moco_mask = antsImageClone(mask),
 #   tsDisplacement = antsImageClone(tsDisplacement),
    dvars = dvars )
}
