#' split a multichannel antsImage
#'
#' split a multichannel antsImage into a list of scalar antsImages
#'
#' @param image a multichannel antsImage to split
#' @return list of scalar antsImages
#' @author Duda, JT
#' @examples
#' r <- floor(seq(1:(64 * 64)) / (64 * 64) * 255)
#' dim(r) <- c(64, 64)
#' r <- as.antsImage(r)
#' g <- r * 0
#' b <- r * 0
#' testthat::expect_error(splitChannels(r))
#' rgbImage <- mergeChannels(list(r, g, b))
#' imgList <- splitChannels(rgbImage)
#' testthat::expect_length(imgList, 3)
#' sapply(imgList, function(x) {
#'   testthat::expect_s4_class(x, class = "antsImage")
#' })
#' testthat::expect_error(splitChannels("hey"))
#'
#' @export splitChannels
splitChannels <- function(image) {
  image <- check_ants(image)

  if (!is.antsImage(image)) {
    stop("input must be an 'antsImage'")
  }
  if (!(image@components > 1)) {
    stop("input must have more than 1 components")
  }

  ANTsRCore::splitChannels(image)
  return(img)
}
