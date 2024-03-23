#' Read file info from image header
#'
#' Read file info from image header
#'
#'
#' @param filename name of image file to scan for info

#' @author Duda JT
#' @examples
#' antsImageHeaderInfo(getANTsRData("r16"))
#'
#' @return outputs a list containing:
#' \itemize{
#'   \item{pixelclass: }{Type of pixel (scalar, vector, etc).}
#'   \item{pixeltype: }{Type of pixel values (int, float, etc).}
#'   \item{nDimensions: }{Number of image dimensions.}
#'   \item{nComponents: }{Number of pixel dimensions.}
#'   \item{dimensions: }{Size of image dimensions.}
#'   \item{spacing: }{Pixel resolution.}
#'   \item{origin: }{Spatial origin of image}
#'   \item{pixelclass: }{Spatial directions of image axes.}
#' }
#' @export antsImageHeaderInfo
#' @examples
#' img <- antsImageRead(getANTsRData("r16"))
#' antsImageHeaderInfo(img)
#' antsImageHeaderInfo(getANTsRData("r16"))
#' testthat::expect_error(antsImageHeaderInfo(""))
antsImageHeaderInfo <- function(filename) {
  if (is.antsImage(filename)) {
    tfile <- tempfile(fileext = ".nii.gz")
    antsImageWrite(filename, tfile)
    on.exit(unlink(tfile))
    filename <- tfile
  } else {
    filename <- path.expand(filename)
  }
  if (!file.exists(filename)) {
    stop("File does not exist")
  }

  ret <- ANTsRCore::antsImageHeaderInfo(filename)
  return(ret)
}
