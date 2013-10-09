taskFMRI <- function( mat , hrf, myvars )
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
  desmat<-cbind( myvars$globalsignal, nuis[,2:7] )
#  desmat<-residuals( lm( desmat ~ hrf ) )
  desmat<-cbind( hrf, desmat )
  colnames( desmat )<-c("hrf","globsig",colnames(nuis)[2:7])
  residmat<-residuals( lm( mat ~  hrf + globsig + motion1 +   
      motion2 + motion3 + compcorr1 + compcorr2 + compcorr3 , 
      data = data.frame( desmat )  )  )

############################################
# Statistical methods of estimation and inference for  #
# functional MR image analysis Bullmore 1996            #
############################################
  residsig <- apply( residmat, FUN=mean, MARGIN=1 ) 
  arcoefs<-ar( residsig, FALSE, 2 )$ar  # use something similar to  SPM's  autoregression estimate
  print( arcoefs )
  mat1 <- ashift(mat,c(1,0))
  mat1[1,] <- mat[1, ]
  mat2 <- ashift(mat,c(2,0))
  mat2[1,] <- mat[1, ]
  mat2[2,] <- mat[2, ]
  amat <- mat - mat1 * arcoefs[1]  - mat2 * arcoefs[2] 
  mat1 <- ashift(desmat,c(1,0))
  mat1[1,] <- desmat[1, ]
  mat2 <- ashift(desmat,c(2,0))
  mat2[1,] <- desmat[1, ]
  mat2[2,] <- desmat[2, ]
  adesmat <- desmat - mat1 * arcoefs[1]  - mat2 *  arcoefs[2] 
  #
  # new regression 
  amat<-residuals( lm( amat ~ globsig + motion1 + motion2 + motion3 + compcorr1
    + compcorr2 + compcorr3, data = data.frame( adesmat )  ) )
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
  return( betas ) 
  }


