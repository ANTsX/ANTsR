#' Simple icawhitening function.
#'
#' Icawhitens the input matrix using SVD and returns the result.
#'
#'
#' @param Xin input matrix
#' @param n.comp number of components on which to project
#' @param verbose bool
#' @return matrix is output
#' @author Avants BB, fastICA package (see CRAN)
#' @examples
#'
#' mat <- matrix(c(rep(1, 100), rep(0, 200)), ncol = 50)
#' wmat <- icawhiten(mat, 2)
#'
#' @export icawhiten
icawhiten <- function(Xin, n.comp, verbose = FALSE) {
  # from the fastICA library
  X <- t(Xin)
  alpha <- 1
  dd <- dim(X)
  d <- dd[dd != 1L]
  if (length(d) != 2L) {
    stop("data must be matrix-conformal")
  }
  X <- if (length(d) != length(dd)) {
    matrix(X, d[1L], d[2L])
  } else {
    as.matrix(X)
  }
  if (alpha < 1 || alpha > 2) {
    stop("alpha must be in range [1,2]")
  }
  n <- nrow(X)
  p <- ncol(X)
  X <- scale(X, scale = FALSE)
  X <- t(X)
  if (verbose) {
    message("Whitening")
  }
  V <- X %*% t(X) / n
  s <- La.svd(V)
  D <- diag(c(1 / sqrt(s$d)))
  K <- D %*% t(s$u)
  K <- matrix(K[1:n.comp, ], n.comp, p)
  X1 <- K %*% X
  return(X1)
}
