#' merge images into a multiChannel antsImage
#'
#' merge images into a multiChannel antsImage
#'
#' @param imageList a list of antsImage objects to merge
#' @return A multiChannel antsImage object
#' @author Duda, JT
#' @examples
#' dims <- c(30, 30)
#' n <- prod(dims)
#' r <- floor(seq(n) / (n) * 255)
#' dim(r) <- dims
#' arr <- r
#' r <- as.antsImage(r)
#' g <- r * 0
#' b <- r * 0
#' rgbImage <- mergeChannels(list(r, g, b))
#' testthat::expect_error(mergeChannels(list(arr, arr)))
#'
#' @export mergeChannels
mergeChannels <- function(imageList) {
  nImages <- length(imageList)
  for (i in c(1:nImages))
  {
    if (!is.antsImage(imageList[[i]])) {
      stop("list may only contain 'antsImage' objects")
    }
    if (length(imageList[[i]]@components) == 0) {
      imageList[[i]]@components <- as.integer(1)
    }
  }

  img <- ANTsRCore::mergeChannels(imageList)
  return(img)
}
