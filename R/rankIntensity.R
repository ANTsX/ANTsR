#' rank intensity transformation for an image
#'
#' Rank transform the intensity of an image to reduce the impact of outliers;
#' useful when using correlation-based metrics for registration and possibly
#' for segmentation tasks. best used with a mask.
#'
#' @param x input image
#' @param mask optional mask
#' @param getMask boolean only used if mask is not present
#' @param method optional method passed to \code{ties.method} in \code{rank}
#' @return image is output
#' @author Avants BB
#' @examples
#'
#' fi <- makeImage(c(10, 10, 10), rnorm(1000))
#' rfi <- rankIntensity(fi)
#'
#' @export rankIntensity
rankIntensity <- function(x, mask, getMask = TRUE, method = "max") {
  rx <- antsImageClone(x)
  if (missing(mask) & getMask) mask <- getMask(x)
  if (missing(mask) & !getMask) mask <- x * 0 + 1
  mat <- rank(x[mask == 1], ties.method = method)
  mat <- mat - min(mat)
  rx[mask == 1] <- mat
  rx <- iMath(rx, "Normalize") * mask
  return(rx)
}
