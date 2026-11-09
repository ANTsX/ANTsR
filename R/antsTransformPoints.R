#' Get Spatial Point from Index
#'
#' Get spatial point from index of an \code{antsImage}.
#'
#'
#' @param x image object of S4 class \code{antsImage} to get values from.
#' @param index image index (1-based), either a vector of length
#' \code{x@@dimension} or an N x \code{x@@dimension} matrix with one
#' index per row.
#' @return physical point(s): a vector for a vector \code{index}, otherwise
#' an N x \code{x@@dimension} matrix.
#' @examples
#' img <- makeImage(c(10, 10), rnorm(100))
#' pt <- antsTransformIndexToPhysicalPoint(img, c(2, 2))
#'
#' @export
antsTransformIndexToPhysicalPoint <- function(x, index) {
  x <- check_ants(x)
  if (!is.antsImage(x)) {
    stop("Input must be of class 'antsImage'")
  }
  if ((!is.numeric(index)) && (!inherits(index, "matrix"))) {
    stop("index must be of class 'numeric' or 'matrix'")
  }

  isVector <- is.null(dim(index))
  if (isVector) {
    index <- t(as.matrix(index))
  }

  imgdim <- length(dim(x))
  if (dim(index)[2] != imgdim) {
    stop(paste("Index matrix must be of size N x", imgdim))
  }

  ret <- ANTsRCore::antsImage_TransformIndexToPhysicalPoint(x, index)
  # as in ANTsPy, a single point (vector) gives a vector
  if (isVector) {
    ret <- as.numeric(ret)
  }
  return(ret)
}



#' Get Index from Spatial Point
#'
#' Get index from spatial point of an 'antsImage'.
#'
#'
#' @param x Image object of S4 class 'antsImage' to get values from.
#' @param point image physical point, either a vector of length
#' \code{x@@dimension} or an N x \code{x@@dimension} matrix with one
#' point per row.
#' @return continuous (1-based) index / indices: a vector for a vector
#' \code{point}, otherwise an N x \code{x@@dimension} matrix.
#' @examples
#' img <- makeImage(c(10, 10), rnorm(100))
#' pt <- antsTransformPhysicalPointToIndex(img, c(2, 2))
#' arr <- as.array(img)
#' testthat::expect_error(
#'   antsTransformPhysicalPointToIndex(arr, c(2, 2)), "antsImage"
#' )
#' testthat::expect_error(
#'   antsTransformPhysicalPointToIndex(img, c("2", 2)), "point must be"
#' )
#' testthat::expect_error(
#'   antsTransformPhysicalPointToIndex(img, c(2, 2, 2)), "matrix must be of"
#' )
#'
#' @export
antsTransformPhysicalPointToIndex <- function(x, point) {
  x <- check_ants(x)
  if (!is.antsImage(x)) {
    stop("Input must be of class 'antsImage'")
  }
  if ((!is.numeric(point)) && (!inherits(point, "matrix"))) {
    stop("point must be of class 'numeric' or 'matrix'")
  }

  isVector <- is.null(dim(point))
  if (isVector) {
    point <- t(as.matrix(point))
  }

  imgdim <- length(dim(x))
  if (dim(point)[2] != imgdim) {
    stop(paste("Point matrix must be of size N x", imgdim))
  }

  ret <- ANTsRCore::antsImage_TransformPhysicalPointToIndex(x, point)
  # as in ANTsPy, a single point (vector) gives a vector
  if (isVector) {
    ret <- as.numeric(ret)
  }
  return(ret)
}
