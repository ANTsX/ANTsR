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
#' img <- makeImage(c(10,10),rnorm(100))
#' img2 <- makeImage(c(10,10), rnorm(100))
#' img2 <- antsCopyImageInfo(img, img2)
#'
#' @export
antsCopyImageInfo <- function(reference, target) {
  if (!(class(reference) == "antsImage") || !(class(target) == "antsImage")) {
    stop("Both inputs must be of class 'antsImage'")
  }
  antsSetOrigin(target, as.numeric(antsGetOrigin(reference)))
  antsSetDirection(target, antsGetDirection(reference))
  antsSetSpacing(target, as.numeric(antsGetSpacing(reference)))
  return(target)
}
