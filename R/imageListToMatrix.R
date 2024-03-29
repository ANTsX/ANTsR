#' Read Images into a Matrix
#'
#' Read images into rows of a matrix.
#'
#'
#' @param imageList A character vector containing a list of image files to
#' read, in order - these are image objects, not file names.
#' @param mask An \code{antsImage} containing a binary mask, voxels in the mask
#' are placed in the matrix. If not provided, estimated from first image in list.
#' If the mask is a different size than the image, the images will be downsampled
#' and smoothed to the size of the mask.
#' @param sigma smoothing operation in physical space.  See \code{smoothImage}.
#' @param epsilon threshold value determining what is included in the mask
#' @return A matrix containing the masked data, the result of calling
#' \code{as.numeric(image, mask)} on each input image.
#' @author Cook PA, Avants B, Kandel BM
#' @seealso \code{\link{matrixToImages}, \link{getMask}}
#' @examples
#' img <- ri(1) %>% resampleImage(c(32, 32))
#' imglist <- list()
#' nsubj <- 3
#' for (ii in 1:nsubj) {
#'   imglist[[ii]] <- img
#' }
#' mask <- getMask(img)
#' imgmat <- imageListToMatrix(imglist, mask)
#'
#' @export imageListToMatrix
imageListToMatrix <- function(imageList, mask, sigma = NA, epsilon = 0) {
  # imageList is a list containing images.  Mask is a mask image Returns matrix of
  # dimension (numImages, numVoxelsInMask)
  if (missing(mask)) {
    mask <- getMask(imageList[[1]])
  }

  numImages <- length(imageList)
  mask_arr <- as.array(mask) > epsilon
  numVoxels <- length(which(mask_arr))

  listfunc <- function(x) {
    if ((sum(dim(x) - dim(mask)) != 0)) {
      x <- resampleImageToTarget(x, mask, 2) # gaussian interpolation
    }
    as.numeric(x, mask = mask_arr)
  }
  dataMatrix <- matrix(nrow = numImages, ncol = numVoxels)
  doSmooth <- !any(is.na(sigma))
  for (i in 1:length(imageList))
  {
    if (doSmooth) {
      dataMatrix[i, ] <- listfunc(
        smoothImage(imageList[[i]], sigma,
          sigmaInPhysicalCoordinates = TRUE
        )
      )
    } else {
      dataMatrix[i, ] <- listfunc(imageList[[i]])
    }
  }
  return(dataMatrix)
}
