sparseDecom2boot <- function(inmatrix, inmask = c(NA, NA), sparseness = c(0.01, 0.01), nvecs = 50, its = 5, cthresh = c(0, 
  0), statdir = NA, perms = 0, uselong = 0, z = 0, smooth = 0, robust = 0, mycoption = 1, initializationList = list(), 
  initializationList2 = list(), ell1 = 0.05, nboot = 10, nsamp=1 , doseg = FALSE ) {
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
  cca1outAuto<-0
  cca2outAuto<-0
  bootccalist1<-list()
  bootccalist2<-list()
  for ( i in 1:nvecs )
    {
      makemat<-matrix( rep(0,nboot*ncol(mat1)), ncol=ncol(mat1) )
      bootccalist1<-lappend( bootccalist1 , makemat )
      makemat <-  matrix( rep(0,nboot*ncol(mat2)), ncol=ncol(mat2) ) 
      bootccalist2<-lappend( bootccalist2 , makemat )
    }
  if (  nsamp >= 0.999999999  ) doreplace<-TRUE else doreplace<-FALSE
  for ( boots in 1:nboot )
    {
      mysample<-sample(1:nsubj,size=mysize, replace = doreplace )
      submat1 <-mat1[mysample,]
      submat2 <-mat2[mysample,]
      sublist<-list(submat1,submat2)
      print(paste("boot",boots,"sample",mysize))
      ( myres <- sparseDecom2( inmatrix = sublist, inmask = mymask, sparseness = sparseness, nvecs = nvecs, its = its, cthresh = cthresh, statdir = statdir, perms = 0, uselong = uselong , z = z, smooth = smooth, robust = robust, mycoption = mycoption, initializationList = initializationList, initializationList2 = initializationList2, ell1 = ell1 ) )
      myressum<-abs(diag(cor(myres$projections,myres$projections2)))
      print( myressum )
      if ( length(dim(myres$eig1)) == 0 ) cca1<-t( imageListToMatrix( myres$eig1 , mymask[[1]] )  )
      if ( length(dim(myres$eig1)) == 2 ) cca1<-( myres$eig1 ) 
      if ( length(dim(myres$eig2)) == 0 ) cca2<-t( imageListToMatrix( myres$eig2 , mymask[[2]]  ) )
      if ( length(dim(myres$eig2)) == 2 ) cca2<-( myres$eig2 )
      if ( boots > 1 & TRUE  )
        {
          cca1copy<-cca1
          mymult<-matrix( rep(0,ncol(cca1)*ncol(cca1)) , ncol=ncol(cca1) )
          for ( j in 1:ncol(cca1out) ) {
          for ( k in 1:ncol(cca1) ) {
            temp1<-abs( cca1out[,j] )
            temp2<-abs( cca1[,k] )
            mymult[j,k]<-cosineDist( temp1,temp2 )
#                sum( abs( temp1/sum(temp1) - temp2/sum(temp2) ) )
#            mymult[j,k]<-( -1.0 * cor( temp1, temp2 ) )
            }
          }
          for ( ct in 1:(ncol(cca1)) )
            {
              arrind<-which( mymult == min(mymult) , arr.ind=T)
              cca1copy[,arrind[1]]<-cca1[,arrind[2]]
              mymult[arrind[1],]<-0
              mymult[,arrind[2]]<-0
            }
          cca1<-cca1copy
          ######nextview######
          cca2copy<-cca2
          mymult<-matrix( rep(0,ncol(cca2)*ncol(cca2)) , ncol=ncol(cca2) )
          for ( j in 1:ncol(cca2out) ) {
          for ( k in 1:ncol(cca2) ) {
            temp1<-abs( cca2out[,j] )
            temp2<-abs( cca2[,k] )
             mymult[j,k]<-cosineDist( temp1,temp2 )           
#            mymult[j,k]<-sum( abs( temp1/sum(temp1) - temp2/sum(temp2) ) )
#            mymult[j,k]<-( -1.0 * cor( temp1, temp2 ) )
            }
          }
          for ( ct in 1:(ncol(cca2)) )
            {
              arrind<-which( mymult == min(mymult) , arr.ind=T)
              cca2copy[,arrind[1]]<-cca2[,arrind[2]]
              mymult[arrind[1],]<-0
              mymult[,arrind[2]]<-0
            }
          cca2<-cca2copy
        }
      cca1out<-cca1out+(cca1) * myressum
      cca2out<-cca2out+(cca2) * myressum
      for ( nv in 1:nvecs ) {
        if ( sparseness[1] > 0 ) bootccalist1[[nv]][boots,]<-abs(cca1[,nv]) else bootccalist1[[nv]][boots,]<-(cca1[,nv])
        if ( sparseness[2] > 0 ) bootccalist2[[nv]][boots,]<-abs(cca2[,nv]) else bootccalist2[[nv]][boots,]<-(cca2[,nv])
      }
  }
  cca1outAuto<-cca1out
  cca2outAuto<-cca2out
  for ( nv in 1:nvecs ) {
    bootmat<-bootccalist1[[nv]]
    vec1  <- apply(bootmat,FUN=mean,MARGIN=2)
#    mysd1 <- apply(bootmat,FUN=sd,MARGIN=2)
#    vec1[ mysd1 > 0 ]  <- vec1[ mysd1 > 0 ]  /  mysd1[ mysd1 > 0 ]
#    vec1  <- apply(bootmat,FUN=t.test,MARGIN=2)
#    vec1 <- as.numeric( do.call(rbind, vec1)[,1] )
    vec1[ is.na( vec1 ) ]<-0
    if ( ( nv > 1 ) & ( abs( sparseness[1] * nvecs ) < 1) & TRUE  )
      {
        for ( j in 1:(nv-1) )
            {
            prevec <- cca1out[ , j ]
            vec1[ prevec > 0 ] <- 0 
            }
        cca1out[ , nv ] <-  sparsify( vec1 , abs( sparseness[1] ) )
        cca1outAuto[ , nv ] <-  vec1 
      } else {
        cca1out[ , nv ] <-  sparsify( vec1 , abs( sparseness[1] ) )
        cca1outAuto[ , nv ] <-  vec1 
      }
    ### now vec 2 ### 
    bootmat<-bootccalist2[[nv]]
    vec2 <- apply(bootmat,FUN=mean,MARGIN=2)
#    mysd2 <- apply(bootmat,FUN=sd,MARGIN=2)
#    vec2[ mysd2 > 0 ]  <- vec2[ mysd2 > 0 ] /  mysd2[ mysd2 > 0 ] 
#    vec2  <- apply(bootmat,FUN=t.test,MARGIN=2)
#    vec2 <- as.numeric( do.call(rbind, vec2)[,1] )
    vec2[ is.na( vec2 ) ]<-0
    if ( ( nv > 1 ) & ( abs( sparseness[2] * nvecs ) < 1) & TRUE  )
      {
        for ( j in 1:(nv-1) )
            {
            prevec <- cca2out[ , j ]
            vec2[ prevec > 0 ] <- 0 
            }
        cca2out[ , nv ] <-  sparsify( vec2 , abs( sparseness[2] ) )
        cca2outAuto[ , nv ] <-  vec2
      } else {
        cca2out[ , nv ] <-  sparsify( vec2 , abs( sparseness[2] ) )
        cca2outAuto[ , nv ] <-  vec2 
      }
  }
  for ( i in 1:ncol(cca1outAuto) )
    {
    mynorm<-sqrt(sum( cca1outAuto[,i] * cca1outAuto[,i] ))
    if ( mynorm > 0 ) cca1outAuto[,i]<-cca1outAuto[,i]/mynorm
    mynorm<-sqrt(sum( cca2outAuto[,i] * cca2outAuto[,i] ))
    if ( mynorm > 0 ) cca2outAuto[,i]<-cca2outAuto[,i]/mynorm
    }
  fakemask1<-makeImage( c(1,1,ncol(mat1)) , 1 )
  fakemask2<-makeImage( c(1,1,ncol(mat2)) , 1 )
  usefakemask<-c( ( length(dim(myres$eig1)) == 2 ), ( length(dim(myres$eig2)) == 2 ) )
  locmask <- inmask
  if ( usefakemask[1] )
    {
      if ( doseg ) cca1outAuto<-matrixSeg( t(cca1outAuto )  )
      if ( dim( cca1outAuto )[2] != nvecs ) cca1outAuto<-t( cca1outAuto )
      cca1out<-(cca1outAuto )
    } else {
    cca1outAuto<-matrixToImages( t(cca1outAuto),locmask[[1]])
    autoseg1<-eigSeg(locmask[[1]],cca1outAuto, doseg )
    cca1outAuto<-t( imageListToMatrix(cca1outAuto,locmask[[1]]) )
    cca1out<-cca1outAuto
    }
  if ( usefakemask[2] )
    {
      if ( doseg ) cca2outAuto<-matrixSeg( t(cca2outAuto )  )
      if ( dim( cca2outAuto )[2] != nvecs ) cca2outAuto<-t( cca2outAuto )
      cca2out<-(cca2outAuto )
    } else {
    cca2outAuto<-matrixToImages( t(cca2outAuto),locmask[[2]])
    autoseg2<-eigSeg(locmask[[2]],cca2outAuto, doseg )
    cca2outAuto<-t( imageListToMatrix(cca2outAuto,locmask[[2]]) )
    cca2out<-cca2outAuto
    }
####################################################################################
####################################################################################
  if ( usefakemask[1] ) init1<-matrixToImages( t(cca1out),fakemask1) else init1<-matrixToImages( t(cca1out),locmask[[1]])
  if ( usefakemask[2] ) init2<-matrixToImages( t(cca2out),fakemask2) else init2<-matrixToImages( t(cca2out),locmask[[2]])
  print("Get Final Results")
  if (!usefakemask[1] & !usefakemask[2] ) maskinit<-locmask
  if ( usefakemask[1] &  usefakemask[2] ) maskinit<-c(fakemask1,fakemask2)
  if ( usefakemask[1] & !usefakemask[2] ) maskinit<-c(fakemask1,locmask[[2]])
  if (!usefakemask[1] &  usefakemask[2] ) maskinit<-c(locmask[[1]],fakemask2)
  myres<-sparseDecom2( inmatrix = inmatrix, inmask = maskinit, sparseness = sparseness, nvecs = nvecs, its = its, cthresh = cthresh, statdir = statdir, perms = perms, uselong = uselong , z = z, smooth = smooth, robust = robust, mycoption = mycoption, initializationList = init1, initializationList2 = init2, ell1 = ell1 )
  ###
  if ( usefakemask[1] ) myres$eig1<-t( imageListToMatrix( myres$eig1 , fakemask1 )  )
  if ( usefakemask[2] ) myres$eig2<-t( imageListToMatrix( myres$eig2 , fakemask2 )  )
  return( list( projections = myres$projections, projections2 = myres$projections2, 
        eig1 = myres$eig1, eig2 = myres$eig2, ccasummary = myres$ccasummary , bootccalist1=bootccalist1 , bootccalist2=bootccalist2,   cca1outAuto=(cca1outAuto), cca2outAuto=(cca2outAuto) ) )
}


cosineDist <- function(xin,yin){
  x<-t(as.matrix(xin))
  y<-t(as.matrix(yin))
  return( as.numeric(1 - x%*%t(y)/(sqrt(rowSums(x^2) %*% t(rowSums(y^2)))) ) )
}
