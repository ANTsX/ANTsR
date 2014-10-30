imageListToMatrix <- function(imageList, mask) {
  # imageList is a list containing images.  Mask is a mask image Returns matrix of
  # dimension (numImages, numVoxelsInMask)
  
  numImages <- length(imageList)
  
  numVoxels <- length(which(mask > 0))
  
  dataMatrix <- matrix(nrow = numImages, ncol = numVoxels)
  
  for (i in 1:numImages) {
    image <- imageList[[i]]
    
    if ((sum(dim(image) - dim(mask)) != 0)) {
      stop(paste("Dimensions of image", image, "do not match mask"))
    }
    
    # Have to convert mask to a boolean because as.numeric in antsImage won't accept
    # an antsImage as a mask
    dataMatrix[i, ] <- as.numeric(image, mask > 0)
  }
  
  return(dataMatrix)
  
} 
