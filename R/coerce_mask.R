#' @title Simple Mask Maker
#' @description Creates a logical array from an antsImage object
#'
#' @param x antsImage object
#' @param error should the function error if the mask as not binary?
#'
#' @return Array, logical if all values within 0, 1, \code{NA}, and \code{NaN}
#' @export
coerce_mask = function(x, error = TRUE) {
  x = as.array(x)
  ux = unique(x)
  if (all(ux %in% c(0, 1, NaN, NA))) {
    x = x != 0
  } else {
    if (error) {
      stop("Mask is not binary!")
    }
  }
  return(x)
}
