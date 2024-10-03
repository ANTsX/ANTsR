#' Read Images into a Matrix
#'
#' Read images into rows of a matrix.
#'
#'
#' @param imageList A character vector containing a list of image files to
#' read, in order - these are image objects, not file names.  these are 
#' assumed to be in a normalized space i.e. they are all in registration.
#' @param mask An \code{antsImage} containing a binary mask, voxels in the mask
#' are placed in the matrix. If not provided, estimated from first image in list.
#' If the mask is a different size than the image, the images will be downsampled
#' and smoothed to the size of the mask.
#' @param sigma smoothing operation in physical space.  See \code{smoothImage}.
#' @param epsilon threshold value determining what is included in the mask
#' @param asymmetryTx a reflection transform
#' @param asymmetryMask a mask defining left and right side of the image.  this 
#' should be defined in the space of the images within the imageList.
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
imageListToMatrix <- function(imageList, mask, sigma = NA, epsilon = 0, asymmetryTx=NULL, asymmetryMask=NULL ) {
  # imageList is a list containing images. Mask is a mask image. Returns matrix of
  # dimension (numImages, numVoxelsInMask)
  if (missing(mask)) {
    mask <- getMask(imageList[[1]])
  }

  if ( ! is.null( asymmetryMask ) ) {
    asymmaskmod = resampleImageToTarget( asymmetryMask, mask, 'nearestNeighbor') # gaussian interpolation
  }

  numImages <- length(imageList)
  mask_arr <- as.array(mask) > epsilon
  numVoxels <- length(which(mask_arr))

  listfunc <- function(x) {
    if ((sum(dim(x) - dim(mask)) != 0)) {
      x <- resampleImageToTarget(x, mask, 'linear') # gaussian interpolation
    }
    as.numeric(x, mask = mask_arr)
  }
  dataMatrix <- matrix(nrow = numImages, ncol = numVoxels)
  doSmooth <- !any(is.na(sigma))

  pb <- txtProgressBar(min = 0, max = numImages, style = 3)


  # Add progress bar using pblapply from pbapply package
  for (i in seq_along(imageList)) {
    temp = imageList[[i]]
    if (doSmooth) {
      temp = smoothImage( temp, sigma,
          sigmaInPhysicalCoordinates = TRUE
        )
    } 
    if ( !is.null(asymmetryTx) & !is.null(asymmetryMask) ) {
      temp = resampleImageToTarget( temp , mask, 'linear') # gaussian interpolation
      temp_reflected = antsApplyTransforms( temp, temp, asymmetryTx$fwdtransforms )
      temp_new = temp * 0.0
      temp_new[ asymmaskmod == 1 ] = 0.5 * ( temp[ asymmaskmod == 1 ] + temp_reflected[ asymmaskmod == 1 ] )
      temp_new[ asymmaskmod == 2 ] = abs( temp[ asymmaskmod == 2 ] - temp_reflected[ asymmaskmod == 2 ] )
      temp = temp_new
    }
    vec = listfunc( temp )
    dataMatrix[i, ] <- vec
     # Update the progress bar
    setTxtProgressBar(pb, i)
  }
  close(pb)
  return(dataMatrix)
}
