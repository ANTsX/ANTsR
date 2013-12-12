matrixToImages <- function(dataMatrix, mask) {
  # Writes rows of a matrix to 3D images.  mask should be an antsImage of the correct dimensions and physical
  # space
  if (!is(mask, "antsImage")) {
    stop("Mask must be an antsImage")
  }
  
  numImages <- dim(dataMatrix)[1]
  
  numVoxelsInMatrix <- dim(dataMatrix)[2]
  
  numVoxelsInMask <- length(which(mask > 0))
  
  if (numVoxelsInMatrix != numVoxelsInMask) {
    stop(paste("Number of masked voxels",numVoxelsInMask," do not match data",numVoxelsInMatrix))
  }
  
  imagelist <- list()
  for (i in 1:numImages) {
    
    img <- antsImageClone(mask)
    vec <- dataMatrix[i, ]
    img[mask <= 0] <- 0
    img[mask > 0] <- vec
    imagelist <- lappend(imagelist, img)
    # antsImageWrite( img, paste(outputRoot, sprintf('%03d.nii.gz', i), sep = '') )
  }
  return(imagelist)
} 
