#' Convenience wrapper for eigenanatomy decomposition.
#'
#' Decomposes a matrix into sparse eigenevectors to maximize explained
#' variance. Note: we do not scale the matrices internally.
#' We leave scaling choices to the user.
#'
#' @param inmatrix n by p input images , subjects or time points by row ,
#' spatial variable lies along columns
#' @param inmask optional antsImage mask
#' @param sparseness lower values equal more sparse
#' @param nvecs number of vectors
#' @param its number of iterations
#' @param cthresh cluster threshold
#' @param statdir place on disk to save results
#' @param z u penalty, experimental
#' @param smooth smoothness eg 0.5
#' @param initializationList see initializeEigenanatomy
#' @param mycoption 0, 1 or 2 all produce different output 0 is combination
#'   of 1 (spatial orthogonality) and 2 (subject space orthogonality)
#' @param robust rank transform input data - good for data checking
#' @param ell1 the ell1 grad descent param
#' @param getSmall try to get smallest evecs (bool)
#' @param verbose activates verbose output
#' @param powerit alternative power iteration implementation, faster
#' @param priorWeight scalar weight typically in range zero to two
#' @param maxBased boolean that chooses max-based thresholding
#' @return outputs a decomposition of a population or time series matrix
#' @author Avants BB
#' @examples
#'
#' mat<-replicate(100, rnorm(20))
#' mydecom<-sparseDecom( mat )
#' mat<-scale(mat)
#' mydecom2<-sparseDecom( mat )
#' # params that lead to algorithm similar to NMF
#' mydecom3<-sparseDecom( mat, z=1, sparseness=1 )
#'
#' \dontrun{
#' # for prediction
#' if ( usePkg("randomForest") & usePkg("spls")  & usePkg('BGLR') ) {
#' data(lymphoma) # from spls
#' training<-sample( rep(c(TRUE,FALSE),31)  )
#' sp<-0.02 ; myz<-0
#' ldd<-sparseDecom( lymphoma$x[training,], nvecs=5 , sparseness=( sp ),
#'   mycoption=1, z=myz ) # NMF style
#' traindf<-data.frame( lclass=as.factor(lymphoma$y[ training  ]),
#'   eig = lymphoma$x[training,]  %*% as.matrix(ldd$eigenanatomyimages ))
#' testdf<-data.frame(  lclass=as.factor(lymphoma$y[ !training ]),
#'  eig = lymphoma$x[!training,] %*% as.matrix(ldd$eigenanatomyimages ))
#' myrf<-randomForest( lclass ~ . ,   data=traindf )
#' predlymp<-predict(myrf, newdata=testdf)
#' print(paste('N-errors:',sum(abs( testdf$lclass != predlymp ) ),
#'   ' non-zero ',sum(abs( ldd$eigenanatomyimages ) > 0 ) ) )
#' # compare to http://arxiv.org/pdf/0707.0701v2.pdf
#' # now SNPs
#' data(mice)
#' snps<-quantifySNPs( mice.X, shiftit = TRUE )
#' numericalpheno<-as.matrix( mice.pheno[,c(4,5,13,15) ] )
#' nfolds<-6
#' train<-sample( rep( c(1:nfolds), 1800/nfolds ) )
#' train<-( train < 4 )
#' lrmat<-lowrankRowMatrix( as.matrix( snps[train,] ) ,  50 )
#' lrmat=scale(lrmat)
#' snpd<-sparseDecom( lrmat-min(lrmat), nvecs=20 , sparseness=( 0.001), z=-1 )
#' projmat<-as.matrix( snpd$eig )
#' snpse<-as.matrix( snps[train, ]  ) %*% projmat
#' traindf<-data.frame( bmi=numericalpheno[train,3] , snpse=snpse)
#' snpse<-as.matrix( snps[!train, ]  ) %*% projmat
#' testdf <-data.frame( bmi=numericalpheno[!train,3] , snpse=snpse )
#' myrf<-randomForest( bmi ~ . , data=traindf )
#' preddf<-predict(myrf, newdata=testdf )
#' cor.test(preddf, testdf$bmi )
#' plot(preddf, testdf$bmi )
#' } # check for packages
#' # prior-based example
#' set.seed(123)
#' ref<-antsImageRead( getANTsRData("r16"))
#' ref<-iMath(ref,"Normalize")
#' mi<-antsImageRead( getANTsRData("r27"))
#' mi2<-antsImageRead( getANTsRData("r30"))
#' mi3<-antsImageRead( getANTsRData("r62"))
#' mi4<-antsImageRead( getANTsRData("r64"))
#' mi5<-antsImageRead( getANTsRData("r85"))
#' refmask<-getMask(ref)
#' refmask<-iMath(refmask,"ME",2) # just to speed things up
#' ilist<-list(mi,mi2,mi3,mi4,mi5)
#' for ( i in 1:length(ilist) )
#' {
#' ilist[[i]]<-iMath(ilist[[i]],"Normalize")
#' mytx<-antsRegistration(fixed=ref , moving=ilist[[i]] ,
#'   typeofTransform = c("Affine") )
#' mywarpedimage<-antsApplyTransforms(fixed=ref,moving=ilist[[i]],
#'   transformlist=mytx$fwdtransforms)
#' ilist[[i]]=mywarpedimage
#' }
#' mat=imageListToMatrix( ilist , refmask )
#' kmseg=kmeansSegmentation( ref, 3, refmask )
#' initlist=list()
#' for ( k in 1:3 )
#'  initlist[[k]]=
#'    thresholdImage(kmseg$probabilityimages[[k]],0.1,Inf) *
#'    kmseg$probabilityimages[[k]]
#' eanat<-sparseDecom( mat,
#'   inmask=refmask, ell1=0.1,
#'   sparseness=0.0, smooth=0.5, verbose=1,
#'   initializationList=initlist, cthresh=25,
#'   nvecs=3, priorWeight=0.5 )
#' ee=matrixToImages( eanat$eigenanatomyimages, refmask )
#' eseg=eigSeg(  refmask, ee )
#' priormat=imageListToMatrix( initlist, refmask )
#' cor( t(eanat$eigenanatomyimages), t(priormat) )
#' plot( ref, eseg )
#' }
#' @export sparseDecom
sparseDecom <- function(inmatrix = NA, inmask = NULL,
                        sparseness = 0.1,
                        nvecs = 10,
                        its = 5, cthresh = 50,
                        statdir = NA, z = 0, smooth = 0, initializationList = list(),
                        mycoption = 0, robust = 0, ell1 = 1, getSmall = 0, verbose=FALSE,
                        powerit=0, priorWeight=0,
                        maxBased=FALSE ) {
  numargs <- nargs()
  if (numargs < 1 | missing(inmatrix)) {
    cat(" sparseDecom( inmatrix=NA,  inmask=NULL , sparseness=0.01 , nvecs=50 , its=5 , cthresh=250 ) \n")
    return(0)
  }
  idim=3
  if (!is.null(inmask)) {
    inmask = check_ants(inmask)
    idim = inmask@dimension
    if ( sum( inmask >= 0.5 ) != ncol( inmatrix )  ) {
      stop("mask size not equal to matrix column count")
    }
  } else {
    inmask = new("antsImage", "float", idim)
  }
  verbose = as.numeric( verbose )
  time1 <- (Sys.time())
  if ( robust > 0 )
  {
    outval = ANTsRCore::eigenanatomyCpp(
                    robustMatrixTransform(inmatrix),
                    inmask, sparseness, nvecs, its, cthresh, z, smooth,
                    initializationList, mycoption, ell1, verbose, powerit,
                    priorWeight, maxBased)
  } else {
    outval = ANTsRCore::eigenanatomyCpp(
                    inmatrix,
                    inmask, sparseness, nvecs, its, cthresh, z, smooth,
                    initializationList, mycoption, ell1, verbose, powerit,
                    priorWeight, maxBased)
  }
  time2 <- (Sys.time())
  outval = lappend( outval,  (time2 - time1) )
  names(outval)[length(outval)]='computationtime'
  if ( verbose )
  {
    temp=lm( inmatrix ~  ( inmatrix %*% t(outval$eigenanatomyimages) ) )
    reconmat = predict( temp )
    reconerr = 0
    for ( i in 1:ncol(inmatrix) )
    {
      temp = abs( cor(inmatrix[,i],reconmat[,i]) )
      reconerr = reconerr + temp
    }
    reconerr = reconerr / ncol( inmatrix )
    #  reconerr=mean( abs( inmatrix - reconmat ) )
    outval[length(outval)-1]=reconerr
    #  names(outval)[length(outval)-1]='meanReconstructionError'
    outval[[length(outval)+1]]=reconmat
    names(outval)[length(outval)]='Reconstruction'
  }
  return( outval )
  #  return(list(projections = mydecomp, eigenanatomyimages = fnl, umatrix = fnu,
}
