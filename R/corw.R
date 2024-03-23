#' produces a correlation matrix via weighted correlation.
#'
#' Uses weighted regression to compute pairwise correlation matrix on input
#' matrix - by columns.
#'
#'
#' @param mat input matrix
#' @param weights input weights, size of nrow of matrix
#' @return matrix is output
#' @author Avants BB
#' @examples
#'
#' mat <- matrix(rnorm(100), nrow = 10)
#' wcmat <- corw(mat, weights = abs(nrorm(nrow(mat))))
#'
#' @export corw
corw <- function(mat, weights) {
  if (missing(mat) | missing(weights)) {
    print(args(corw))
    return(1)
  }
  cormat <- matrix(rep(NA, ncol(mat) * ncol(mat)), ncol = ncol(mat))
  for (x in 1:ncol(mat)) {
    for (y in 1:ncol(mat)) {
      if (x != y) {
        cormat[x, y] <- sqrt(summary(lm(mat[, x] ~ mat[, y]), weights = weights / sum(weights))$r.squared)
      }
    }
  }
  return(cormat)
}
