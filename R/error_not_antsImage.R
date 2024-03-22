#' Stop if not an antsImage object
#'
#' @param x object to test
#' @param argname Name of argument to say was not an \code{antsImage}
#'
#' @return Invisible \code{NULL}
#' @export
#'
#' @examples
#' img = antsImageRead(getANTsRData('r16'), 2)
#' error_not_antsImage(img) 
#' testthat::expect_error(error_not_antsImage(as.array(img)))
error_not_antsImage = function(x, argname = "") {
  if (!is.antsImage(x)) {
    stop(paste("Object", argname, "is not an antsImage object!"))
  }
  return(invisible(NULL))
}
