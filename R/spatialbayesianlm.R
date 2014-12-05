spatialbayesianlm <- function( mylm, ymat, mask,
  smth=1, priorWeight=1, nhood=NA, regweights=NA  )
  {
    if ( sum(mask==1) != ncol(ymat) )
    {
    print("spatialbayesianlm: mask does not match ymat dimensions")
    return(NA)
    }
    if ( all(is.na(nhood) ) ) nhood<-rep(1,mask@dimension)
    if ( all(is.na(regweights) ) ) regweights<-rep(1,nrow(ymat))
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
    invcov <- solve( cov( t( smoothcoeffmat ) ) )
    betaideal<-rep(0,ncol(ymat))
    blmX<-model.matrix( mylm )
    betamatrix<-ymat[1:(ncol(blmX)-1),]*0
    for ( v in 1:ncol(ymat) )
      {
      parammat<-nmatimgs[[1]][,v]
      for ( k in 2:length(nmatimgs))
        parammat<-cbind( parammat, nmatimgs[[k]][,v] )
      pcov<-cov( parammat )
      locinvcov<-tryCatch( solve( pcov ) ,
       error = function(e) solve( invcov ) )
      if ( typeof(locinvcov)=='character')
        locinvcov<-invcov
      prior<-(smoothcoeffmat[,v])
#      if ( skip == 1 ) regweights<-robvals[,v]
      blm<-bayesianlm(  blmX, ymat[,v], prior,
        locinvcov*priorWeight,
        regweights=regweights )
      betamatrix[,v]<-blm$beta[1]
      }
    return( matrixToImages(betamatrix, mask) )
}
