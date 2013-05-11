corw <- function( mat, weights )
  {
  if ( missing( mat ) | missing( weights ) )
  {
  print( args(corw) )
  return( 1 )
  }
  cormat<-matrix(rep(NA,ncol(mat)*ncol(mat)),ncol=ncol(mat))
  for ( x in 1:ncol(mat) )
    {
  for ( y in 1:ncol(mat) )
    {
      cormat[x,y]<-corr( cbind(mat[,x], mat[,y]), w = weights ) 
    }
    }
  return( cormat )
  }



