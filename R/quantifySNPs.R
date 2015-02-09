#' Simple quantifySNPs function.
#'
#' quantifySNPs converts trinary snps to frequency data
#'
#' @param snps input matrix
#' @param freqthresh remove snps below this frequency
#' @param shiftit shift the snps to smooth the estimate
#' @param replaceWithF replaces snps with frequency values
#' @param traitvecin map snps to trait vector
#' @param trainvec defines training data
#' @return matrix is output
#' @author Avants BB
#' @examples
#'
#' mat <- matrix(c(0,1,2,0,0,1,2,2,2),ncol=3)
#' wmat<-quantifySNPs( mat , freqthresh=0)
#'
#' @export quantifySNPs
quantifySNPs <- function(snps, freqthresh = 0.1 ,
  shiftit = FALSE, replaceWithF=T, traitvecin=NA, trainvec=NA ) {
  if (nargs() == 0) {
    print("Usage:  x_b<-quantifySNPs( x ) ")
    return(1)
  }
  qsnps<-snps
  okrow<-rep(FALSE,ncol(qsnps))
  traitvec<-as.numeric( traitvecin )
  progress <- txtProgressBar(min = 0, max = ncol(qsnps), style = 3)
  for ( y in 1:ncol(qsnps) ) # or dd for ADNI_SNPS
    {
    temp<-snps[,y]
    if ( sum( is.na( as.numeric(temp) ) ) == 0 ) okrow[y]<-TRUE
    if ( okrow[y] ) {
      t1<-which( temp == 0 )
      t2<-which( temp == 1 )
      t3<-which( temp == 2 )
      f1<-length( t1  ) / length( temp )
      f2<-length( t2  ) / length( temp )
      f3<-length( t3  ) / length( temp )
      if ( f1 < freqthresh ) okrow[y]<-FALSE
      if ( f2 < freqthresh ) okrow[y]<-FALSE
      if ( f3 < freqthresh ) okrow[y]<-FALSE
      if ( replaceWithF & is.na( traitvec ) ) {
          qsnps[t1,y]<-f1
          qsnps[t2,y]<-f2
          qsnps[t3,y]<-f3
      }
      if ( replaceWithF & !is.na( traitvec ) & is.na( trainvec ) ) {
          qsnps[t1,y]<-mean( traitvec[t1] )
          qsnps[t2,y]<-mean( traitvec[t2] )
          qsnps[t3,y]<-mean( traitvec[t3] )
      }
      if ( replaceWithF & !is.na( traitvec ) & !is.na( trainvec ) ) {
          t1tr<-which( temp == 0 & trainvec )
          t2tr<-which( temp == 1 & trainvec )
          t3tr<-which( temp == 2 & trainvec )
          qsnps[t1,y]<-mean( traitvec[t1tr] )
          qsnps[t2,y]<-mean( traitvec[t2tr] )
          qsnps[t3,y]<-mean( traitvec[t3tr] )
      }
    }
    if (y%%100 == 0) {
      setTxtProgressBar(progress, y)
    }
  }
  qsnps<-qsnps[,okrow]
  if ( shiftit & usePkg("magic") ) {
    qsnps<-qsnps+magic::ashift(qsnps,c(0,1))+magic::ashift(qsnps,c(0,-1))
    qsnps<-qsnps+magic::ashift(qsnps,c(0,1))+magic::ashift(qsnps,c(0,-1))
  }
  return( qsnps )
}
