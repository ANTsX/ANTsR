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
#' @return A matrix containing the masked data, the result of calling
#' \code{as.numeric(image, mask)} on each input image.
#' @author Cook PA, Avants B, Kandel BM
#' @seealso \code{\link{matrixToImages}, \link{getMask}}
#' @examples
#'  img <- antsImageRead(getANTsRData('r16') )
#'  imglist <- list()
#'  nvox <- dim(img)[1] * dim(img)[2]
#'  nsubj <- 50
#'  for(ii in 1:nsubj){
#'    imglist[ii] <- img + rnorm(nvox, sd=mean(img[img!=0]))
#'  }
#'  mask <- getMask(img) %>% resampleImage( c( 2,2 ) )
#'  imgmat <- imageListToMatrix(imglist, mask)
#'
#' @export imageListToMatrix
imageListToMatrix <- function(imageList, mask) {
  # imageList is a list containing images.  Mask is a mask image Returns matrix of
  # dimension (numImages, numVoxelsInMask)
  if(missing(mask))
    mask <- getMask(imageList[[1]])

  numImages <- length(imageList)

  numVoxels <- length(which(mask > 0))

  listfunc <- function(x) {
    if ((sum(dim(x) - dim(mask)) != 0)) {
      x = resampleImageToTarget( x, mask, 2 ) # gaussian interpolation
    }
    as.numeric(x, mask > 0)
  }

  dataMatrix <- t( matrix(
    unlist(lapply(imageList, listfunc)),
    ncol = numImages, nrow = numVoxels) )

  return(dataMatrix)
}
