#' Simple autocorrelation-based temporal whitening function.
#'
#' Temporally whitens each column of the input matrix with an autoregressive
#' model and returns the whitened series.
#'
#' For each column \eqn{x} with mean \eqn{m}, an AR model of order up to
#' \code{myord} is fitted with \code{\link[stats]{ar}} and the whitening
#' operator is applied:
#' \deqn{w_t = m + (x_t - m) - \sum_k a_k (x_{t-k} - m),}
#' i.e. the AR prediction is subtracted from the input, leaving the
#' (approximately white) innovations around the column mean.  Lags before the
#' start of the series are taken to be at the mean.  \code{ar} may select
#' fewer lags than requested (or none, in which case the column is returned
#' unchanged); all selected coefficients are used.
#'
#' @param mat input matrix (time by columns)
#' @param myord integer, maximum AR order
#' @return matrix of the same size as \code{mat} with whitened columns
#' @author Avants BB
#' @examples
#'
#' mat <- replicate(100, rnorm(20))
#' wmat <- temporalwhiten(mat)
#'
#' @export temporalwhiten
temporalwhiten <- function(mat, myord = 2) {
  if (nargs() == 0) {
    print("Usage:  x_whitened<-whiten( x ) ")
    return(1)
  }
  mat <- as.matrix(mat)
  n <- nrow(mat)
  omat <- mat
  for (i in seq_len(ncol(mat))) {
    gsig <- mat[, i]
    arval <- numeric(0)
    try(arval <- ar(gsig, FALSE, myord)$ar, silent = TRUE)
    arval <- as.numeric(arval)
    m <- mean(gsig)
    dev <- gsig - m
    whitened <- dev
    for (k in seq_along(arval)) {
      if (k >= n) break
      lagged <- c(rep(0, k), dev[seq_len(n - k)])
      whitened <- whitened - arval[k] * lagged
    }
    omat[, i] <- m + whitened
  }
  return(omat)
}
