#' Simple fastwhitening function.
#'
#' whitens the input matrix using a fast SVD and returns the result.  can
#' select number of principal components to use.  might want to scale the
#' matrix before input.
#'
#'
#' @param x input matrix
#' @param mynu number of u vectors
#' @return matrix is output
#' @author Avants BB
#' @examples
#'
#' \dontrun{
#' mat<-replicate(400, rnorm(600))
#' print(mean(abs(cor(mat))))
#' wmat<-fastwhiten( scale( mat ) )
#' print(mean(abs(cor(wmat))))
#' wmat<-fastwhiten( scale( mat ), 20 )
#' print(mean(abs(cor(wmat))))
#' }
#'
#' @export fastwhiten
fastwhiten <- function(x, mynu = NA) {
  if (nargs() == 0) {
    print("Usage:  x_whitened<-whiten( x ) ")
    return(1)
  }
  if ( !usePkg("irlba") ) { print("Need irlba package"); return(NULL) }
  if (is.na(mynu))
    svdx <- irlba(x) else svdx <- irlba(x, nu = mynu, nv = 0)
  dd <- (svdx$d)^(-1/2)
  xw <- ((svdx$u %*% diag(dd)) %*% t(svdx$u)) %*% x
  return(xw)
}
