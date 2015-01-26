#' produces a correlation matrix via weighted correlation.
#' 
#' Uses weighted regression to compute pairwise correlation matrix on input
#' matrix - by columns.
#' 
#' 
#' @param mat input matrix
#' @return matrix is output
#' @author Avants BB
#' @examples
#' 
#' mat <- matrix(c(rep(1,100),rep(0,20)),ncol=10)
#' wcmat<-corw( mat , weights = rep(1,ncol(mat) ) ) 
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
      cormat[x, y] <- sqrt(summary(lm(mat[, x] ~ mat[, y]), weights = weights/sum(weights))$r.squared)
      # cormat[x,y]<-corr( cbind(mat[,x], mat[,y]), w = weights/sum(weights) )
    }
  }
  return(cormat)
} 
