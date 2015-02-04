#' Simple matrix2timeseries function.
#'
#' matrix2timeseriess converts a matrix to a 4D image.
#'
#' @param referenceImage reference 4D image
#' @param maskImage mask image defining voxels of interest
#' @param timeSeriesMatrix matrix to convert to image
#' @return antsImage in 4D is output
#' @author Avants BB
#'
#' @export matrix2timeseries
matrix2timeseries <- function(referenceImage, maskImage, timeSeriesMatrix) {
  if (length(dim(referenceImage)) != 4) {
    cat("This function is for 4D images.  Returning reference image.\n")
    return(referenceImage)
  }
  indexMask <- (maskImage == 1)
  newImageArray <- as.array(referenceImage)

  # set everything to zero so that only non-zero mask elements are non-zero
  newImageArray <- newImageArray * 0

  for (i in 1:nrow(timeSeriesMatrix)) {
    newImageArray[, , , i][indexMask] <- timeSeriesMatrix[i, ]
  }
  newImageArray <- newImageArray[, , , 1:nrow(timeSeriesMatrix)]
  newImage <- as.antsImage(newImageArray)
  antsCopyImageInfo(referenceImage, newImage)
  newImage <- antsImageClone(newImage, out_pixeltype = referenceImage@pixeltype)
  return(newImage)
}
