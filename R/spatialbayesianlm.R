spatialbayesianlm <- function( mylm, ymat, mask,
  smth=1, priorWeight=1, nhood=NA, regweights=NA,
  smoothcoeffmat=NA  )
  {
    if ( sum(mask==1) != ncol(ymat) )
    {
    print("spatialbayesianlm: mask does not match ymat dimensions")
    return(NA)
    }
    if ( all(is.na(nhood) ) ) nhood<-rep(1,mask@dimension)
    if ( all(is.na(regweights) ) )
      {
      regweights<-ymat
      regweights[]<-1
      }
    if ( is.null( dim(regweights) ) )
      {
      regweights<-ymat
      regweights[]<-1
      }
    if ( ! all( dim(regweights) == dim(ymat) ) )
      {
      regweights<-ymat
      regweights[]<-1
      }
    if ( all(is.na(smoothcoeffmat)) )
      smoothcoeffmat<-mylm$coefficients
    nmatimgs<-list()
    for ( i in 1:nrow(smoothcoeffmat) )
      {
      temp<-antsImageClone( mask )
      temp[ mask == 1 ] <- smoothcoeffmat[i,]
      SmoothImage(mask@dimension,temp,smth,temp)
      nmatimgs[[i]]<-antsGetNeighborhoodMatrix(temp,mask,
        nhood, boundary.condition = "mean")
      smoothcoeffmat[i,]<-temp[ mask==1 ]
      }
    covmat<-cov( t( smoothcoeffmat ) )
    invcov <- solve( covmat + diag(ncol(covmat))*1.e-6 )
    betaideal<-rep(0,ncol(ymat))
    blmX<-model.matrix( mylm )
    betamatrix<-ymat[1:(ncol(blmX)-1),]*0
    posteriorI<-rep(0,ncol(ymat))
    for ( v in 1:ncol(ymat) )
      {
      parammat<-nmatimgs[[1]][,v]
      for ( k in 2:length(nmatimgs))
        parammat<-cbind( parammat, nmatimgs[[k]][,v] )
      pcov<-cov( parammat )
      locinvcov<-tryCatch( solve( pcov ) ,
       error = function(e) return( invcov ) )
      if ( typeof(locinvcov)=='character')
        locinvcov<-invcov
      prior<-(smoothcoeffmat[,v])
      blm<-bayesianlm(  blmX, ymat[,v], prior,
        locinvcov*priorWeight, regweights=regweights[,v] )
      betamatrix[,v]<-blm$beta
      posteriorI[v]<-blm$posteriorProbability
      }
    mylist<-matrixToImages(betamatrix, mask)
    mylist<-lappend( mylist, makeImage(mask,posteriorI) )
    return( mylist )
}
