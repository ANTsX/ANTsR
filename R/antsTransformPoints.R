#' Get Spatial Point from Index
#'
#' Get spatial point from index of an \code{antsImage}.
#'
#'
#' @param x image object of S4 class \code{antsImage} to get values from.
#' @param index image index
#' @return array of pixel values
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

  if (is.numeric(index)) {
    index <- t(as.matrix(index))
  }

  imgdim <- length(dim(x))
  if (dim(index)[2] != imgdim) {
    stop(paste("Index matrix must be of size N x", imgdim))
  }

  return(ANTsRCore::antsImage_TransformIndexToPhysicalPoint(x, index))
}



#' Get Index from Spatial Point
#'
#' Get index from spatial point of an 'antsImage'.
#'
#'
#' @param x Image object of S4 class 'antsImage' to get values from.
#' @param point image physical point
#' @return array of pixel values
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

  if (is.numeric(point)) {
    point <- t(as.matrix(point))
  }

  imgdim <- length(dim(x))
  if (dim(point)[2] != imgdim) {
    stop(paste("Point matrix must be of size N x", imgdim))
  }

  return(ANTsRCore::antsImage_TransformPhysicalPointToIndex(x, point))
}
