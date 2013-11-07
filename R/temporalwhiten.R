temporalwhiten <- function( mat, myord=2 )
  {
  if ( nargs() == 0 )
    {
    print("Usage:  x_whitened<-whiten( x ) ")
    return(1)
    }
  omat<-mat
  for ( i in 1:ncol(mat) )
    {
    gsig<-mat[,i]
    arval<-rep(0,myord)
    try ( arval<-ar(gsig,FALSE,myord)$ar )
    omat[,i]<-shift(gsig,1)*arval[1]+shift(gsig,2)*arval[2]
    }
  return( omat )
  }


