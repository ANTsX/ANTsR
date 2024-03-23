#' Copy header info
#'
#' Copy origin, direction, and spacing from one \code{antsImage} to another
#'
#'
#' @param reference image object of S4 class \code{antsImage} to get values from.
#' @param target image object of S4 class \code{antsImage} to copy values to.
#' @return Target image with reference header information.
#' @examples
#'
#' img <- makeImage(c(10, 10), rnorm(100))
#' img2 <- makeImage(c(10, 10), rnorm(100))
#' img2 <- antsCopyImageInfo(img, img2)
#' testthat::expect_error(antsCopyImageInfo(img, 1))
#'
#' @export
antsCopyImageInfo <- function(reference, target) {
  reference <- check_ants(reference)
  target <- check_ants(target)
  if (!(class(reference) == "antsImage") || !(class(target) == "antsImage")) {
    stop("Both inputs must be of class 'antsImage'")
  }
  antsSetOrigin(target, as.numeric(antsGetOrigin(reference)))
  antsSetDirection(target, antsGetDirection(reference))
  antsSetSpacing(target, as.numeric(antsGetSpacing(reference)))
  return(target)
}


#' Copy header info with different input order
#'
#' Copy origin, direction, and spacing from one \code{antsImage} to another.
#' Opposite order of input relative to \code{antsCopyImageInfo}
#'
#'
#' @param target image object of S4 class \code{antsImage} to copy values to.
#' @param reference image object of S4 class \code{antsImage} to get values from.
#' @return Target image with reference header information.
#' @examples
#'
#' img <- makeImage(c(10, 10), rnorm(100))
#' img2 <- makeImage(c(10, 10), rnorm(100))
#' img2 <- antsCopyImageInfo(img2, img)
#' testthat::expect_error(antsCopyImageInfo(img, 1))
#'
#' @export
antsCopyImageInfo2 <- function(target, reference) {
  reference <- check_ants(reference)
  target <- check_ants(target)
  if (!(class(reference) == "antsImage") || !(class(target) == "antsImage")) {
    stop("Both inputs must be of class 'antsImage'")
  }
  antsSetOrigin(target, as.numeric(antsGetOrigin(reference)))
  antsSetDirection(target, antsGetDirection(reference))
  antsSetSpacing(target, as.numeric(antsGetSpacing(reference)))
  return(target)
}
