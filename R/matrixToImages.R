#' Convert rows of a matrix to images
#'
#' Unmasks rows of a matrix and writes as images.
#'
#'
#' @param dataMatrix A matrix such as that created by \code{imagesToMatrix}.
#' @param mask An \code{antsImage} containing a binary mask.
#' Rows of the matrix
#' are unmasked and written as images. The mask defines the output image space.
#' @author Cook PA
#' @seealso \code{\link{imagesToMatrix}, \link{getMask}}
#' @examples
#'
#' \dontrun{
#'   # mat = matrixToImages( aMat, mask )
#' }
#'
#'
#' @export matrixToImages
matrixToImages <- function(dataMatrix, mask) {
  # Writes rows of a matrix to 3D images.  mask should be an antsImage of the
  # correct dimensions and physical space
  if (!is(mask, "antsImage")) {
    stop("Mask must be an antsImage")
  }

  numImages <- dim(dataMatrix)[1]

  numVoxelsInMatrix <- dim(dataMatrix)[2]

  numVoxelsInMask <- length(which(mask > 0))

  if (numVoxelsInMatrix != numVoxelsInMask) {
    stop(paste("Number of masked voxels", numVoxelsInMask, " do not match data",
      numVoxelsInMatrix))
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
