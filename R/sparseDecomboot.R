sparseDecomboot <- function(inmatrix = NA, inmask = 0, sparseness = 0.01, nvecs = 50, its = 5, cthresh = 250, statdir = NA, 
  z = 0, smooth = 0, initializationList = list(), mycoption = 0, nboot = 10, nsamp=0.9 , doseg = TRUE ) {
  numargs <- nargs()
  if (numargs < 1 | missing(inmatrix)) {
    print( args( sparseDecomboot ) ) 
    return(0)
  }
  if (is.na(statdir)) 
    statdir <- paste(tempdir(), "/", sep = "")
  nsubj<-nrow( inmatrix ) 
  mysize<-round( nsamp * nsubj )
  mat1 <-inmatrix
  mymask <- inmask
  cca1out<-0
  cca1outAuto<-0
  bootccalist1<-list()
  for ( i in 1:nvecs )
    {
      makemat<-matrix( rep(0,nboot*ncol(mat1)), ncol=ncol(mat1) )
      bootccalist1<-lappend( bootccalist1 , makemat )
    }
  for ( boots in 1:nboot )
    {
      mysample<-sample(1:nsubj,size=mysize)
      submat1 <-mat1[mysample,]
      print(paste("boot",boots,"sample",mysize))
      myres <- sparseDecom( inmatrix = submat1, inmask = mymask, sparseness = sparseness, nvecs = nvecs, its = its, cthresh = cthresh, statdir = statdir, 
  z = z, smooth = smooth, initializationList = initializationList, mycoption = mycoption )
      if ( length(dim(myres$eigenanatomyimages)) == 0 ) cca1<-t( imageListToMatrix( myres$eigenanatomyimages , mymask )  )
      if ( length(dim(myres$eigenanatomyimages)) == 2 ) cca1<-( myres$eigenanatomyimages )
      if ( boots > 1 & TRUE  )
        {
          cca1copy<-cca1
          # compute the "closest" eigenvector and store the difference
          # in a difference matrix called mymult 
          mymult<-matrix( rep(0,ncol(cca1)*ncol(cca1)) , ncol=ncol(cca1) )
          for ( j in 1:ncol(cca1out) ) {
          for ( k in 1:ncol(cca1) ) {
            temp1<-abs( cca1out[,j] )
            temp2<-abs( cca1[,k] )
            mymult[j,k]<-sum( abs( temp1/sum(temp1) - temp2/sum(temp2) ) )
            }
          }
          # find the best match and reorder appropriately 
          for ( ct in 1:(ncol(cca1)) )
            {
              arrind<-which( mymult == min(mymult) , arr.ind=T)
              cca1copy[,arrind[1]]<-cca1[,arrind[2]]
              mymult[arrind[1],]<-0
              mymult[,arrind[2]]<-0
            }
          cca1<-cca1copy
        }
      cca1out<-cca1out+(cca1) 
      for ( nv in 1:nvecs ) {
        bootccalist1[[nv]][boots,]<-abs(cca1[,nv])
      }
  }
  cca1outAuto<-cca1out
  for ( nv in 1:nvecs ) {
    bootmat<-bootccalist1[[nv]]
    vec1  <- apply(bootmat,FUN=mean,MARGIN=2)
#    mysd1 <- apply(bootmat,FUN=sd,MARGIN=2)
#    vec1[ mysd1 > 0 ]  <- vec1[ mysd1 > 0 ]  /  mysd1[ mysd1 > 0 ]
#    vec1  <- apply(bootmat,FUN=t.test,MARGIN=2)
#    vec1 <- as.numeric( do.call(rbind, vec1)[,1] )
    vec1[ is.na( vec1 ) ]<-0
    if ( ( nv > 1 ) & ( abs( sparseness * nvecs ) < 1) & TRUE  )
      {
#        for ( j in 1:(nv-1) )
#            {
#            prevec <- cca1out[ , j ]
#            vec1[ prevec > 0 ] <- 0 
#            }
        cca1out[ , nv ] <-  sparsify( vec1 , abs( sparseness ) )
        cca1outAuto[ , nv ] <-  vec1 
      } else {
        cca1out[ , nv ] <-  sparsify( vec1 , abs( sparseness ) )
        cca1outAuto[ , nv ] <-  vec1 
      }
  }
  for ( i in 1:ncol(cca1outAuto) )
    {
    mynorm<-sqrt(sum( cca1outAuto[,i] * cca1outAuto[,i] ))
    if ( mynorm > 0 ) cca1outAuto[,i]<-cca1outAuto[,i]/mynorm
    }
  fakemask1<-makeImage( c(1,1,ncol(mat1)) , 1 )
  usefakemask<-( length(dim(myres$eigenanatomyimages)) == 2 )
  locmask <- inmask
  if ( usefakemask )
    {
    locmask<-fakemask1
    if ( doseg ) cca1outAuto<-matrixSeg( t(cca1outAuto )  )
      else cca1outAuto<-t(cca1outAuto ) 
    cca1out<-cca1outAuto
    } else {
        cca1outAuto<-matrixToImages( t(cca1outAuto),locmask )
        autoseg1<-eigSeg(locmask ,cca1outAuto, doseg )
        cca1outAuto<-t( imageListToMatrix( cca1outAuto, locmask ) )
        cca1out<-t( cca1outAuto )
    }
####################################################################################
####################################################################################
  print(paste("Get Final Results",statdir))
  finalinit <- matrixToImages( cca1out,locmask )
  myres <- sparseDecom( inmatrix = inmatrix, inmask = locmask, sparseness = sparseness, nvecs = nvecs, its = its, cthresh = cthresh, statdir = statdir, 
  z = z, smooth = smooth, initializationList = finalinit, mycoption = mycoption )
  ###
  if ( usefakemask ) myres$eigenanatomyimages<-t( imageListToMatrix( myres$eigenanatomyimages , fakemask1 )  )
  return( list( projections = myres$projections,  
         eigenanatomyimages = myres$eigenanatomyimages,
               bootccalist1 = bootccalist1,
               cca1outAuto  = cca1outAuto ) )
}
