sparseDecom2boot <- function(inmatrix, inmask = c(NA, NA), sparseness = c(0.01, 0.01), nvecs = 50, its = 5, cthresh = c(0, 
  0), statdir = NA, perms = 0, uselong = 0, z = 0, smooth = 0, robust = 0, mycoption = 1, initializationList = list(), 
  initializationList2 = list(), ell1 = 0.05, nboot = 10, nsamp=0.9 ) {
  numargs <- nargs()
  if (numargs < 1 | missing(inmatrix)) {
    print( args( sparseDecom2boot ) ) 
    return(0)
  }
  nsubj<-nrow( inmatrix[[1]] ) 
  mysize<-round( nsamp * nsubj )
  mat1 <-inmatrix[[1]]
  mat2 <-inmatrix[[2]]
  mymask <- inmask
  cca1out<-0
  cca2out<-0
  bootccalist1<-list()
  bootccalist2<-list()
  for ( i in 1:nvecs )
    {
      bootccalist1<-lappend( bootccalist1 , matrix( rep(0,nboot*ncol(mat1)), ncol=ncol(mat1) )  )
      bootccalist2<-lappend( bootccalist2 , matrix( rep(0,nboot*ncol(mat2)), ncol=ncol(mat2) )  )
    }
  for ( boots in 1:nboot )
    {
      mysample<-sample(1:nsubj,size=mysize)
      submat1 <-mat1[mysample,]
      submat2 <-mat2[mysample,]
      sublist<-list(submat1,submat2)
      print(paste("boot",boots,"sample",mysize))
      myres <- sparseDecom2( inmatrix = sublist, inmask = mymask, sparseness = sparseness, nvecs = nvecs, its = its, cthresh = cthresh, statdir = statdir, perms = 0, uselong = uselong , z = z, smooth = smooth, robust = robust, mycoption = mycoption, initializationList = initializationList, initializationList2 = initializationList2, ell1 = ell1 )
      if ( length(dim(myres$eig1)) == 0 ) cca1<-imageListToMatrix( myres$eig1 , mymask[[1]] ) 
      if ( length(dim(myres$eig1)) == 2 ) cca1<-( myres$eig1 ) 
      if ( length(dim(myres$eig1)) == 0 ) cca2<-imageListToMatrix( myres$eig2 , mymask[[2]]  ) 
      if ( length(dim(myres$eig1)) == 2 ) cca2<-( myres$eig2 )
      cca1out<-cca1out+cca1
      cca2out<-cca2out+cca2
      for ( nv in 1:nvecs ) {
        bootccalist1[[nv]][boots,]<-abs(cca1[nv,])
        bootccalist2[[nv]][boots,]<-abs(cca2[nv,])
      }
    }
  for ( nv in 1:nvecs ) {
    bootmat<-bootccalist1[[nv]]
    vec1  <- apply(bootmat,FUN=mean,MARGIN=2)
    mysd1 <- apply(bootmat,FUN=sd,MARGIN=2)
    vec1[ mysd1 > 0 ]  <- vec1[ mysd1 > 0 ] /  mysd1[ mysd1 > 0 ]
    cca1out[ nv, ] <- vec1
    bootmat<-bootccalist1[[nv]]
    vec2 <- apply(bootmat,FUN=mean,MARGIN=2)
    mysd2 <- apply(bootmat,FUN=sd,MARGIN=2)
    vec2[ mysd2 > 0 ]  <- vec2[ mysd2 > 0 ] /  mysd2[ mysd2 > 0 ] 
    cca2out[ nv, ] <- vec2
  }
  fakemask1<-makeImage( c(ncol(mat1),1,1) , 1 )
  fakemask2<-makeImage( c(ncol(mat2),1,1) , 1 )
  if ( length(dim(myres$eig1)) == 2 ) inmask<-c( fakemask1, fakemask2 ) 
#  return( list( bootcca1=cca1out*(1.0/nboot) , bootcca2=cca2out*(1.0/nboot) ) ) 
  myres<-sparseDecom2( inmatrix = inmatrix, inmask = inmask, sparseness = sparseness, nvecs = nvecs, its = its, cthresh = cthresh, statdir = statdir, perms = 0, uselong = uselong , z = z, smooth = smooth, robust = robust, mycoption = mycoption, initializationList = matrixToImages(cca1out,inmask[[1]]), initializationList2 = matrixToImages(cca2out,inmask[[2]]), ell1 = ell1 )
  ###
#  print("Got Final Results")
  return( list( projections = myres$projections, projections2 = myres$projections2, 
        eig1 = myres$eig1, eig2 = myres$eig2, ccasummary = myres$ccasummary , bootccalist1=bootccalist1 , bootccalist2=bootccalist2 ) )
}
