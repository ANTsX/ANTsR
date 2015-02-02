#' Read Images into a Matrix
#' 
#' Read images into rows of a matrix.
#' 
#' 
#' @param imageList A character vector containing a list of image files to
#' read, in order - these are image objects, not file names.
#' @param mask An \code{antsImage} containing a binary mask, voxels in the mask
#' are placed in the matrix.
#' @return A matrix containing the masked data, the result of calling
#' \code{as.numeric(image, mask)} on each input image.
#' @author Cook PA, Avants B
#' @seealso \code{\link{matrixToImages}, \link{getMask}}
#' @examples
#' 
#' \dontrun{
#' 
#'   imageFNList <- list.files('./', pattern = glob2rx('testView*.nii.gz'), full.names = TRUE)
#'   
#'   imageList<-list()
#'   for ( fn in imageFNList ) 
#'     imageList<-lappend( imageList, antsImageRead( fn, 3 ) )
#' 
#'   mask <- antsImageRead('/mnt/data/masks/brainmask.nii.gz',3)
#' 
#'   mat <- imageListToMatrix(imageList, mask)
#' 
#' }
#' 
#' 
#' @export imageListToMatrix
imageListToMatrix <- function(imageList, mask) {
  # imageList is a list containing images.  Mask is a mask image Returns matrix of
  # dimension (numImages, numVoxelsInMask)
  
  numImages <- length(imageList)
  
  numVoxels <- length(which(mask > 0))
  
  dataMatrix <- matrix(nrow = numImages, ncol = numVoxels)
  
  for (i in 1:numImages) {
    image <- imageList[[i]]
    
    if ((sum(dim(image) - dim(mask)) != 0)) {
      stop(paste("Dimensions of image do not match mask"))
    }
    
    # Have to convert mask to a boolean because as.numeric in antsImage won't accept
    # an antsImage as a mask
    dataMatrix[i, ] <- as.numeric(image, mask > 0)
  }
  
  return(dataMatrix)
  
} 
