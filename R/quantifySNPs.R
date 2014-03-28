quantifySNPs <- function(snps, freqthresh = 0.1 , shiftit = FALSE, replaceWithF=T ) {
  if (nargs() == 0) {
    print("Usage:  x_b<-quantifySNPs( x ) ")
    return(1)
  }
  qsnps<-snps
  okrow<-rep(FALSE,ncol(qsnps))
  for ( y in 2:ncol(qsnps) ) # or dd for ADNI_SNPS
    {
    temp<-snps[,y]
    if ( sum( is.na( as.numeric(temp) ) ) == 0 ) okrow[y]<-TRUE
    t1<-which( temp == 0 )
    t2<-which( temp == 1 )
    t3<-which( temp == 2 )
    f1<-length( t1  ) / length( temp )
    f2<-length( t2  ) / length( temp )
    f3<-length( t3  ) / length( temp )
    if ( f1 < freqthresh ) okrow[y]<-FALSE
    if ( f2 < freqthresh ) okrow[y]<-FALSE
    if ( f3 < freqthresh ) okrow[y]<-FALSE
    if ( replaceWithF ) {
      qsnps[t1,y]<-f1
      qsnps[t2,y]<-f2
      qsnps[t3,y]<-f3
      }
    }
  qsnps<-qsnps[,okrow]
  if ( shiftit ) {
    qsnps<-qsnps+ashift(qsnps,c(0,1))+ashift(qsnps,c(0,-1))
    qsnps<-qsnps+ashift(qsnps,c(0,1))+ashift(qsnps,c(0,-1))
  }
  return( qsnps )
} 
