#' Simple autocorrelation-based temporal whitening function.
#'
#' Temporally whitens the input matrix using autoregression and returns the
#' result.
#'
#'
#' @param mat input matrix
#' @param myord integer order value
#' @return matrix is output
#' @author Avants BB
#' @examples
#'
#' mat <- matrix(c(rep(1,100),rep(0,200)),ncol=50)
#' wmat<-temporalwhiten( mat )
#'
#' @export temporalwhiten
temporalwhiten <- function(mat, myord = 2) {
  if (nargs() == 0) {
    print("Usage:  x_whitened<-whiten( x ) ")
    return(1)
  }
  if ( !usePkg("magic") ) { print("Need magic package"); return(NULL) }
  omat <- mat
  for (i in 1:ncol(mat)) {
    gsig <- mat[, i]
    arval <- rep(0, myord)
    try(arval <- ar(gsig, FALSE, myord)$ar)
    omat[, i] <- shift(gsig, 1) * arval[1] + shift(gsig, 2) * arval[2]
  }
  return(omat)
}
