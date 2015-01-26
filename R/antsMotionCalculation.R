#' @name antsMotionCalculation
#' @title Correct ASL data for motion.
#' @usage antsMotionCalculation(img, mask=NA, fixed=NA, moreaccurate=1, framewise=1)
#' @param img Name of image file, e.g. 'img.nii.gz'.
#' @param mask Name of mask file, e.g. 'mask.nii.gz'.  If not provided, estimated from data. 
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
#' antsMotionCalculation(getANTsRData('pcasl'))

antsMotionCalculation <- function(img, mask = NA, fixed = NA, moreaccurate = 1, framewise = 1){
  moco <- motion_correction(img, fixed=fixed, moreaccurate=moreaccurate)
  tmpdir <- tempdir()
  file.mocoparam <- paste(tmpdir, 'moco.csv',sep='')
  file.mask <- paste(tmpdir, 'mask.nii.gz',sep='')
  file.out <- paste(tmpdir, 'out.csv',sep='')
  write.csv(moco$moco_params, file.mocoparam, row.names=F)
  if(is.na(mask)){
    mask <- getMask(moco$moco_avg_img, 500, Inf, cleanup=T)
  }
  antsImageWrite(mask, file.mask) 
  antsMotionCorrStats(list(x=file.mask, d=img, o=file.out,
    f=framewise, m=file.mocoparam))
  tsDisplacement <- antsImageRead(paste(tmpdir, 'out.nii.gz', sep=''), 4)
  aslmat <- timeseries2matrix(antsImageRead(img, 4), mask)
  dvars <- computeDVARS(aslmat) 
  list(moco_img=antsImageClone(moco$moco_img),
    moco_params=moco$moco_params, 
    moco_avg_img=antsImageClone(moco$moco_avg_img), 
    moco_mask=antsImageClone(mask), 
    tsDisplacement=antsImageClone(tsDisplacement), 
    dvars=dvars)
}
