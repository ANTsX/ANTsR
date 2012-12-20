binarizeSNPs <- function( snps )
  {
  if ( nargs() == 0 )
    {
    print("Usage:  x_b<-binarizeSNPs( x ) ")
    return(1)
    }
  nrep<-3
  binsnps<-(matrix( rep(NA, nrow(snps)*ncol(snps)*nrep) , nrow=nrow(snps), ncol=ncol(snps)*nrep  ))
  binsnpsdf<-data.frame(matrix( rep(NA, nrow(snps)*ncol(snps)*nrep) , nrow=nrow(snps), ncol=ncol(snps)*nrep  ))
  ct<-1
  for ( x in 1:ncol(snps) ) 
    {
    binsnps[ snps[,x] == 0 , ct ]<-1 ; 
    names(binsnpsdf)[ct]<-paste(names(snps)[x],".0",sep='')
    binsnps[ snps[,x] != 0 , ct ]<-0 ; ct<-ct+1
    binsnps[ snps[,x] == 1 , ct ]<-1 ; 
    names(binsnpsdf)[ct]<-paste(names(snps)[x],".1",sep='')
    binsnps[ snps[,x] != 1 , ct ]<-0 ; ct<-ct+1
    binsnps[ snps[,x] == 2 , ct ]<-1 ; 
    names(binsnpsdf)[ct]<-paste(names(snps)[x],".2",sep='')
    binsnps[ snps[,x] != 2 , ct ]<-0 ; ct<-ct+1
    }
  osnps<-data.frame(binsnps)
  names(osnps)<-names(binsnpsdf)
  return(osnps)
  }


