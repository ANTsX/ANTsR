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

  newImage <- as.antsImage(newImageArray)
  antsCopyImageInfo(referenceImage, newImage)

  return(newImage)
}
