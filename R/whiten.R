whiten <- function( x , k = NA, reducex = FALSE ) {
  if (nargs() == 0) {
    print("Usage:  x_whitened<-whiten( x ) ")
    return(1)
  }
  if ( is.na( k ) )
    {
    svdx <- svd( scale( x %*% t(x) ) )
    dd <- (svdx$d)^(-1/2)
    xw <- ((svdx$u %*% diag(dd)) %*% t(svdx$v)) %*% x
    }
  else
    {
    n<-nrow( x )
    p<-ncol( x ) 
    svdx <- svd( scale( x %*% t(x) ) , nu = min( n , p , k ), nv = min( n , p , k ) )
    dd <-diag( ( (svdx$d)^(-1/2) )[1:k] )
    xw <- (svdx$u %*% dd ) %*% t(svdx$v)
    xw <- ( xw ) %*% x 
    }
  if ( reducex )  xw <- lowrankRowMatrix( xw , k )
  return( xw ) 
} 
