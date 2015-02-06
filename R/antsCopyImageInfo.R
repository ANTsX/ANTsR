#' Copy header info
#'
#' Copy header info from one 'antsImage' to another
#'
#'
#' @param reference Image object of S4 class 'antsImage' to get values from.
#' @param target Image object of S4 class 'antsImage' to copy values to.
#' @return Matrix of image indices
#' @examples
#'
#' img<-makeImage(c(10,10),rnorm(100))
#` img2<-makeImage(c(10,10), rnorm(100))
#' img2<-antsCopyImageInfo(img, img2)
#'
#'
#' @export antsCopyImageInfo
antsCopyImageInfo <- function(reference, target) {
  if (!(class(target) == "antsImage") || !(class(target) == "antsImage")) {
    stop("Both inputs must be of class 'antsImage'")
  }

  antsSetOrigin(target, as.numeric(antsGetOrigin(reference)))
  antsSetDirection(target, antsGetDirection(reference))
  antsSetSpacing(target, as.numeric(antsGetSpacing(reference)))
  return(target)
}
