which.antsImage <- function(x, arr.ind = FALSE, useNames = TRUE) {
  x <- coerce_mask(x, error = TRUE)
  which(x, arr.ind = arr.ind, useNames = useNames)
}

#' @aliases which,antsImage-method
#' @export
#' @param x a logical vector or array.
#' @param arr.ind logical; should array indices be returned when x is an array?
#' @param useNames logical indicating if the value of arrayInd()
#' should have (non-null) dimnames at all.
#'
#' @examples
#' img <- makeImage(c(10, 10), rnorm(100))
#' mask <- img > 0
#' which(mask)
#' which(mask, arr.ind = TRUE)
#' @rdname antsImageops
setMethod("which", "antsImage", which.antsImage)
