#' Simple interleaveMatrixWithItself function.
#' 
#' Replicate columns of input matrix n times as neighbors of columns in
#' original matrix.
#' 
#' 
#' @param mat input matrix
#' @return matrix is output
#' @author Avants BB
#' @examples
#' 
#' mat<-replicate(100, rnorm(20)) 
#' wmat<-interleaveMatrixWithItself( mat , 5 ) 
#' 
#' @export interleaveMatrixWithItself
interleaveMatrixWithItself <- function(x, n = 1) {
  if (nargs() == 0) {
    print(args(interleaveMatrixWithItself))
    return(1)
  }
  nc <- ncol(x)
  nr <- nrow(x)
  xi <- matrix(rep(0, nr * nc * n), nrow = nr)
  colnames(xi) <- paste("V", 1:ncol(xi))
  colnamesxi <- colnames(xi)
  colnamesx <- colnames(x)
  j <- 1
  for (i in 1:ncol(x)) {
    xi[, j:(j + n - 1)] <- x[, i]
    colnamesxi[j:(j + n - 1)] <- paste(colnamesx[i], 1:n, sep = "")
    j <- j + n
  }
  colnames(xi) <- colnamesxi
  return(xi)
} 
