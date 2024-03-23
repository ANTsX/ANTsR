#' Read Images into a Matrix
#'
#' Read images into rows of a matrix, given a mask - much faster for large
#' datasets as it is based on C++ implementations.
#'
#'
#' @param imageList A character vector containing a list of image files to
#' read, in order.
#' @param mask An \code{antsImage} containing a binary mask, voxels in the mask
#' are placed in the matrix.
#' @return A matrix containing the masked data, the result of calling
#' \code{as.numeric(image, mask)} on each input image.
#' @author Cook PA, Avants BB (C++ version)
#' @seealso \code{\link{matrixToImages}, \link{getMask}}
#' @examples
#'
#'
#' # make some simulated images and convert them to a matrix
#'
#' n <- 2
#' tdir <- tempdir()
#' for (i in 1:n) {
#'   simimg <- as.antsImage(replicate(64, rnorm(64)))
#'   antsImageWrite(simimg, tempfile(fileext = ".mha"))
#' }
#' imageList <- list.files(tdir, pattern = ".mha", full.names = TRUE)
#' mask <- getMask(antsImageRead(imageList[1]))
#' mat <- imagesToMatrix(imageList, mask)
#' print(dim(mat))
#'
#' @export imagesToMatrix
imagesToMatrix <- function(imageList, mask) {
  n <- length(imageList)
  if (n < 1) {
    print(" length of input list must be >= 1 ")
    return(NA)
  }
  if (class(imageList) != "character") {
    print("Must pass a list of filenames")
    return(NA)
  }
  return(ANTsRCore::imagesToMatrix(imageList, mask, n))
}
