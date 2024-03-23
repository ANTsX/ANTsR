#' Simple robustMatrixTransform function.
#'
#' Transform a matrix such that each column has rank-ordered entries where rank #' is determined by the column values.
#'
#' @param mat input matrix
#' @return matrix is output
#' @author Avants BB
#' @examples
#' mat <- replicate(100, rnorm(20))
#' rmat <- robustMatrixTransform(mat)
#'
#' @export robustMatrixTransform
robustMatrixTransform <- function(mat) {
  ANTsRCore::robustMatrixTransform(data.matrix(mat))
}
