#' @title Simple Mask Maker
#' @description Creates a logical array from an antsImage object
#'
#' @param x antsImage object
#' @param error should the function error if the mask as not binary?
#'
#' @return Array, logical if all values within 0, 1, \code{NA}, and \code{NaN}
#' @examples
#' img = makeImage(c(10,10),rnorm(100))
#' mask = img > 0
#' coerce_mask(mask, error = TRUE)
#' coerce_mask(mask + 1, error = FALSE)
#' testthat::expect_error(coerce_mask(mask + 1, error = TRUE))
#' coerce_mask(as.array(mask) > 0, error = FALSE)
#' @export
coerce_mask = function(x, error = TRUE) {
  if (typeof(x) == "logical") {
    return(x)
  }
  x = as.array(x)
  # checking if zeroes and ones
  # we can change this when | and || are implemented in C++
  one = x == 1
  check = all(x == 0 | one)
  if (check) {
    return(one)
  } else {
    if (error) {
      stop("Mask is not binary!")
    }
  }
  return(x)
}
