quantifySNPs <- function(snps, freqthresh = 0.1 , shiftit = FALSE, replaceWithF=T, traitvecin=NA, trainvec=NA ) {
  if (nargs() == 0) {
    print("Usage:  x_b<-quantifySNPs( x ) ")
    return(1)
  }
  qsnps<-snps
  okrow<-rep(FALSE,ncol(qsnps))
  traitvec<-as.numeric( traitvecin )
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
  qsnps<-qsnps[,okrow]
  if ( shiftit ) {
    qsnps<-qsnps+ashift(qsnps,c(0,1))+ashift(qsnps,c(0,-1))
    qsnps<-qsnps+ashift(qsnps,c(0,1))+ashift(qsnps,c(0,-1))
  }
  return( qsnps )
} 
