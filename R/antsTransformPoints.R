#' Get Spatial Point from Index
#'
#' Get spatial point from index of an \code{antsImage}.
#'
#'
#' @param x image object of S4 class \code{antsImage} to get values from.
#' @param index image index
#' @return array of pixel values
#' @examples
#' img <- makeImage(c(10,10),rnorm(100))
#' pt <- antsTransformIndexToPhysicalPoint(img, c(2,2))
#' arr = as.array(img)
#' testthat::expect_error(
#' antsTransformIndexToPhysicalPoint(arr,c(2,2)), "antsImage")
#' testthat::expect_error(
#' antsTransformIndexToPhysicalPoint(img,c("2",2)), "index must be")
#' testthat::expect_error(
#' antsTransformIndexToPhysicalPoint(img,c(2,2,2)), "matrix must be of")
#'
#' @export
antsTransformIndexToPhysicalPoint <- function(x, index) {
  x = check_ants(x)
  if (!is.antsImage(x)) {
    stop("Input must be of class 'antsImage'")
  }
  if ((class(index)[1] != "numeric") && (class(index)[1] != "matrix")) {
    stop("index must be of class 'numeric' or 'matrix'")
  }

  if (class(index)[1] == "numeric") {
    index <- t(as.matrix(index))
  }

  imgdim <- length(dim(x))
  if (dim(index)[2] != imgdim) {
    stop(paste("Index matrix must be of size N x", imgdim))
  }

  return(.Call("antsImage_TransformIndexToPhysicalPoint", x, index, PACKAGE = "ANTsRCore"))
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
#' img<-makeImage(c(10,10),rnorm(100))
#' pt<-antsTransformPhysicalPointToIndex(img,c(2,2))
#' arr = as.array(img)
#' testthat::expect_error(
#' antsTransformPhysicalPointToIndex(arr,c(2,2)), "antsImage")
#' testthat::expect_error(
#' antsTransformPhysicalPointToIndex(img,c("2",2)), "point must be")
#' testthat::expect_error(
#' antsTransformPhysicalPointToIndex(img,c(2,2,2)), "matrix must be of")
#'
#' @export
antsTransformPhysicalPointToIndex <- function(x, point) {
  x = check_ants(x)
  if (!is.antsImage(x)) {
    stop("Input must be of class 'antsImage'")
  }
  if ((class(point) != "numeric") && (class(point) != "matrix")) {
    stop("point must be of class 'numeric' or 'matrix'")
  }

  if (class(point) == "numeric") {
    point <- t(as.matrix(point))
  }

  imgdim <- length(dim(x))
  if (dim(point)[2] != imgdim) {
    stop(paste("Point matrix must be of size N x", imgdim))
  }

  return(.Call("antsImage_TransformPhysicalPointToIndex", x, point, PACKAGE = "ANTsRCore"))
}
