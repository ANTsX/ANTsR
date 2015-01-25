#' Compute mean value in each ROI label.
#' 
#' Computes the mean value in each label of an ROI after masking with MaskImage
#' 
#' 
#' @param valueImage input image with values to average
#' @param roiImage ROI lable image
#' @param valueImage overall maskImage
#' @return A list with components: \code{roiValues}, the (number) labels of
#' each ROI; \code{roiMeans}, the mean value of each ROI; \code{roiVolumes},
#' the volume (in voxels) of each ROI; and \code{roiSDs}, the standard
#' deviation of each ROI.
#' @author Avants BB
#' @examples
#' 
#' \dontrun{
#' data("aal", package = "ANTsR")
#' vals<-getROIValues( image, aal, mask  ) 
#' }
#' 
#' @export getROIValues
getROIValues <- function(valueImage, roiImage, maskImage) {
  if (nargs() == 0) {
    print(args(getROIValues))
    return(1)
  }
  uvals <- sort(unique(roiImage[maskImage == 1]))
  roivals <- rep(NA, length(uvals))
  roivolumes <- rep(NA, length(uvals))
  roisds <- rep(NA, length(uvals))
  pb <- txtProgressBar(min = 0, max = length(uvals), style = 3)
  for (x in 1:length(uvals)) {
    setTxtProgressBar(pb, x)
    inds <- ((roiImage == uvals[x]) & (maskImage == 1))
    if (sum(inds) > 0) {
      roivals[x] <- mean(valueImage[inds])
      roivolumes[x] <- length(inds[inds == T])
      roisds[x] <- sd(valueImage[inds])
    }
  }
  return(list(roiValues = uvals, roiMeans = roivals, roiVolumes = roivolumes, roiSDs = roisds))
} 
