fastwhiten <- function( x , mynu=NA )
  {
  if ( nargs() == 0 )
    {
    print("Usage:  x_whitened<-whiten( x ) ")
    return(1)
    }
  library(irlba)
  if ( is.na(mynu) ) svdx<-irlba( x %*% t(x) ) else  svdx<-irlba( x %*% t(x) , nu=mynu , nv=mynu )
  dd<-(svdx$d)^(-1/2)
  xw<-( (svdx$u %*% diag(dd) ) %*% t(svdx$v) ) %*% x
  return(xw) 
  }
