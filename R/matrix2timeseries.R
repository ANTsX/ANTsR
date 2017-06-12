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
  temp = matrixToImages( timeSeriesMatrix, maskImage )
  newImage = mergeListToNDImage( referenceImage, temp )
  antsCopyImageInfo(referenceImage, newImage)
  newImage<-antsImageClone( newImage,
    out_pixeltype = referenceImage@pixeltype)
  return(newImage)
}
