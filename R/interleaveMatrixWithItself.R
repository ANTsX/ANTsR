interleaveMatrixWithItself <- function(x,n=1) {
  if (nargs() == 0) {
    print( args( interleaveMatrixWithItself ) )
    return(1)
  }
  nc<-ncol(x)
  nr<-nrow(x)
  xi<-matrix( rep(0,nr*nc*n), nrow=nr )
  j<-1
  for ( i in 1:ncol(x) )
    {
    xi[,j:(j+n-1)]<-x[,i]
    }
  return(xi)
} 
