whiten2 <- function( x )
  {
  if ( nargs() == 0 )
    {
    print("Usage:  x_whitened<-whiten( x ) ")
    return(1)
    }
  svdx<-svd( x )
  dd<-(svdx$d)^(-1/2)
  xw<-( (svdx$u %*% diag(dd) ) %*% t(svdx$u) ) %*% x
  return( xw )
  }

