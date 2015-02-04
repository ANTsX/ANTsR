#' Simple whitening function.
#'
#' Whitens the input matrix using SVD and returns the result.
#'
#' @param x input matrix
#' @param k rank to use
#' @param reducex reduce the input matrix to k-size subspace
#' @return matrix is output
#' @author Avants BB
#' @examples
#'
#' mat <- matrix(rnorm(300),ncol=50)
#' wmat<-whiten( mat )
#' wmat2<-whiten( mat, 2, TRUE )
#'
#' @export whiten
whiten <- function(x, k = NA, reducex = FALSE) {
  if (nargs() == 0) {
    print("Usage:  x_whitened<-whiten( x ) ")
    return(1)
  }
  if (is.na(k)) {
    svdx <- svd(scale(x %*% t(x)))
    dd <- (svdx$d)^(-1/2)
    xw <- ((svdx$u %*% diag(dd)) %*% t(svdx$v)) %*% x
  } else {
    n <- nrow(x)
    p <- ncol(x)
    svdx <- svd(scale(x %*% t(x)), nu = min(n, p, k), nv = min(n, p, k))
    dd <- diag(((svdx$d)^(-1/2))[1:k])
    xw <- (svdx$u %*% dd) %*% t(svdx$v)
    xw <- (xw) %*% x
  }
  if (reducex)
    xw <- .lowrankRowMatrix(xw, k)
  return(xw)
}
