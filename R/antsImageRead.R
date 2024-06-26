#' Image Read
#'
#' Read an image file into an S4 object of class 'antsImage'.
#'
#' @param filename Name of the file to read the image from.
#' @param dimension Number of dimensions of the image read. This need not be
#' the same as the dimensions of the image in the file. Allowed values: 2, 3,
#' 4. If not provided, the dimension is obtained from the image file
#' @param pixeltype C++ datatype to be used to represent the pixels read. This
#' datatype need not be the same as the datatype used in the file. Allowed
#' values: 'double', 'float' , 'unsigned int' , 'unsigned char'.
#' @return S4 object of Class 'antsImage' -- Success\cr 1 -- Failure
#' @author Shrinidhi KL
#' @seealso \code{\link{antsImageWrite}}
#' @examples
#'
#' fn <- getANTsRData("r16")
#' fi <- antsImageRead(fn)
#' img <- antsImageRead(fn, dimension = 2)
#' img <- antsImageRead(fn, dimension = 2, "double")
#'
#' @export antsImageRead
antsImageRead <- function(filename, dimension = NULL, pixeltype = "float") {
  components <- 1

  if (!is.character(filename) || length(filename) != 1) {
    stop("'filename' argument must be of class 'character' and have length 1")
  }
  filename <- path.expand(filename)
  if (!file.exists(filename)) stop("file does not exist")
  if (!is.character(pixeltype) || length(pixeltype) != 1) {
    stop("'pixeltype' argument must be of class 'character' and have length 1")
  }
  if (!is.null(dimension)) {
    if (((!is.numeric(dimension)) && (!is.integer(dimension))) || length(dimension) !=
      1) {
      stop("'dimension' argument must be of class 'numeric' and have length 1")
    }
  } else {
    if (file.access(filename, 4) == -1) {
      stop(paste("image is not readable, check permissions to", filename))
    }
    imageInfo <- tryCatch(
      antsImageHeaderInfo(filename),
      error = function(e) {
        return(NA)
      }
    )
    if (any(is.na(imageInfo))) {
      stop(paste("image may not be readable, check permissions to", filename))
    }
    dimension <- imageInfo$nDimensions
    components <- imageInfo$nComponents
  }

  if ((dimension < 2) || (dimension > 4)) {
    stop("only images of dimensions 2,3,4 are supported")
  }

  rval <- try(
    ANTsRCore::AntsImageRead(
      filename, pixeltype,
      dimension, components
    )
  )
  if (inherits(rval, "try-error")) {
    rval <- NA
  } else {
    slot(rval, "filename") <- filename
  }

  return(rval)
}
