whiten <- function( x )
  {
  svdx<-svd( x %*% t(x) )
  dd<-(svdx$d)^(-1/2)
  xw<-( (svdx$u %*% diag(dd) ) %*% t(svdx$v) ) %*% x
  }


