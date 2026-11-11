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
#' tdir <- file.path(tempdir(), "imagesToMatrix")
#' dir.create(tdir, showWarnings = FALSE)
#' for (i in 1:n) {
#'   simimg <- as.antsImage(replicate(64, rnorm(64)))
#'   antsImageWrite(simimg, tempfile(tmpdir = tdir, fileext = ".mha"))
#' }
#' imageList <- list.files(tdir, pattern = "[.]mha$", full.names = TRUE)
#' mask <- getMask(antsImageRead(imageList[1]))
#' mat <- imagesToMatrix(imageList, mask)
#' print(dim(mat))
#' unlink(tdir, recursive = TRUE)
#'
#' @export imagesToMatrix
imagesToMatrix <- function(imageList, mask) {
  n <- length(imageList)
  if (n < 1) {
    print(" length of input list must be >= 1 ")
    return(NA)
  }
  if (!is.character(imageList)) {
    print("Must pass a list of filenames")
    return(NA)
  }
  # the C++ reader indexes every image with the mask's dimensions, so a
  # mismatched image reads out of bounds (a crash, not an R error)
  for (f in imageList) {
    if (!file.exists(f)) {
      stop("image file does not exist: ", f)
    }
    hdr <- antsImageHeaderInfo(f)
    if (!identical(as.numeric(hdr$dimensions), as.numeric(dim(mask)))) {
      stop(
        "image ", f, " has dimensions ", paste(hdr$dimensions, collapse = "x"),
        " but the mask has ", paste(dim(mask), collapse = "x")
      )
    }
  }
  return(ANTsRCore::imagesToMatrixR(imageList, mask, n))
}
