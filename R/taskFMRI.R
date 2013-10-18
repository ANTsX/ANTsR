taskFMRI <- function( mat , hrf, myvars , correctautocorr=FALSE, residualizedesignmatrix = FALSE, myformula="globalsignal + motion1 +   
      motion2 + motion3 + compcorr1 + compcorr2 + compcorr3"  )
  {
  if ( nargs() == 0 )
    {
    print("Usage:  betas<-taskFMRI( x , hrf ) ")
    return(1)
    }
  avg<-myvars$avgImage
  mask<-myvars$mask 
  nuis<-( myvars$nuisancevariables )
  betas<-rep(NA, ncol( mat ) )
  desmat<-as.matrix( cbind( myvars$globalsignal, nuis ) )
  if ( residualizedesignmatrix ) desmat<-residuals(lm(desmat~hrf))
  desmat<-cbind( hrf, desmat )
  colnames( desmat )<-c(colnames(hrf),"globalsignal",colnames(nuis))
  print( colnames( desmat ) )
############################################
# Statistical methods of estimation and inference for  #
# functional MR image analysis Bullmore 1996            #
############################################
  amat <- mat
  adesmat <- desmat
  myformhrf<-as.formula( paste( "amat ~  hrf + ", myformula ) )
  myformNOhrf<-as.formula( paste( "amat ~ 1 + ", myformula ) )
  print( myformhrf )
  if ( correctautocorr )
  {
  for ( i in 1:11 ) {
  residmat<-residuals( lm( myformhrf , 
      data = data.frame( adesmat )  )  )
  residsig <- apply( residmat, FUN=mean, MARGIN=1 ) 
  arcoefs<-ar( residsig, FALSE, 2 )$ar  # use something similar to  SPM's  autoregression estimate
  if ( i == 1 ) initialarcoefs <- arcoefs
  if ( abs( arcoefs[1] ) > 0.05 ) {
  print( arcoefs )
  mat1 <- ashift(amat,c(1,0))
  mat1[1,] <- amat[1, ]
  mat2 <- ashift(amat,c(2,0))
  mat2[1,] <- amat[1, ]
  mat2[2,] <- amat[2, ]
  if ( length( arcoefs ) == 2 )  amat <- amat - mat1 * arcoefs[1]  - mat2 * arcoefs[2] 
  if ( length( arcoefs ) == 1 )  amat <- amat - mat1 * arcoefs[1]   
  mat1 <- ashift(adesmat,c(1,0))
  mat1[1,] <- adesmat[1, ]
  mat2 <- ashift(adesmat,c(2,0))
  mat2[1,] <- adesmat[1, ]
  mat2[2,] <- adesmat[2, ]
  if ( length( arcoefs ) == 2 )  adesmat <- adesmat - mat1 * arcoefs[1]  - mat2 *  arcoefs[2]
  if ( length( arcoefs ) == 1 )  adesmat <- adesmat - mat1 * arcoefs[1] 
  } # if gt 0.05
  }
  #
  # new regression 
  amat<-residuals( lm( myformNOhrf, data = data.frame( adesmat )  ) )
  residsig <- apply( amat, FUN=mean, MARGIN=1 ) 
  arcoefs<-ar( residsig, FALSE, length(arcoefs) )$ar  # use something similar to  SPM's  autoregression estimate
  print(paste("final arcoefs", arcoefs ) )
  print(paste("initi arcoefs",initialarcoefs ) )
  }
  progress <- txtProgressBar(min = 0, max = ncol(mat),      style = 3)
  for ( i in 1:ncol(amat) ) {
    vox<-amat[ , i ]
#    mdl<-lmrob( vox ~  hrf, data = data.frame( adesmat )  )
    mdl<-lm( vox ~  hrf , data = data.frame( desmat )  ) 
    betas[i]<-coefficients(summary(mdl))[2,3] # probably better way
#    betas[i]<-cor.test( vox , hrf )$est  
    setTxtProgressBar(progress, i)
    }
  close(progress)
  betas[ is.na( betas ) ] <- 0
  return( list( beta = betas , fmrimat=amat , adesmat = adesmat ) )
  }
