#' @title Simple Mask Maker
#' @description Creates a logical array from an antsImage object
#'
#' @param x antsImage object
#'
#' @return Array, logical if all values within 0, 1, \code{NA}, and \code{NaN}
coerce_mask = function(x) {
  x = as.array(x)
  ux = unique(x)
  if (all(ux %in% c(0, 1, NaN, NA))) {
    x = x != 0
  }
  return(x)
}
