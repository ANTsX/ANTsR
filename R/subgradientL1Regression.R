subgradientL1Regression <- function( y, x, s=0.01, percentvals=0.1 , nits=100, betas=NA , sparval = NA ) {
  if (nargs() == 0) {
    print("Usage:  betas<-subgradientL1Regression( y ,  x) ")
    print("Needs to be checked more carefully")
    return(1)
  }
  if ( percentvals > 1 | percentvals < 0 ) percentvals<-1
  if ( is.na(betas) ) betas<-rep(0,ncol(x))
  nvals<-round(percentvals*ncol(x))
  if ( nvals > ncol(x) ) nvals<-ncol(x)
  selectvals<-1:nvals
  deltmag<-rep(0,nits)
  for ( i in 1:nits ) {
    mysamp<-sort( sample( 1:ncol(x) , size=nvals ) )
    delt<-( c(x[,mysamp] %*% betas[mysamp]) - y )
    deltmag[i]<-sqrt(sum( delt * delt ) )
    delt<-sign(  delt )
    subgrad<-rep(0,ncol(x))
    subgrad[mysamp]<-t(x[,mysamp]) %*% as.matrix( delt )
    tk<-s/sqrt(sum( subgrad * subgrad ) )
    betas<-betas-tk*subgrad
    if ( !is.na(sparval) ) betas<-sparsify( betas , sparval )
    resultcorr<-cor.test( y,  x %*% betas )$est
    }
  resultcorr<-cor.test( y,  x %*% betas )$est
  return( list( betas=betas,  deltmag=deltmag, resultcorr=resultcorr  ) )
}

