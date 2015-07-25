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
#' @param priorWeight scalar weight
#' @return outputs a decomposition of a population or time series matrix
#' @author Avants BB
#' @examples
#'
#' mat<-replicate(100, rnorm(20))
#' mydecom<-sparseDecom( mat )
#' mat<-scale(mat)
#' mydecom2<-sparseDecom( mat-min(mat) ) # non-neg matrix
#' # params that lead to algorithm similar to NMF
#' mydecom3<-sparseDecom( mat-min(mat), z=1, sparseness=1 )
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
#' }
#' }
#' @export sparseDecom
sparseDecom <- function(inmatrix = NA, inmask = NA,
  sparseness = 0.1,
  nvecs = 10,
  its = 5, cthresh = 50,
  statdir = NA, z = 0, smooth = 0, initializationList = list(),
  mycoption = 0, robust = 0, ell1 = 1, getSmall = 0, verbose=0,
  powerit=0, priorWeight=0 ) {
  numargs <- nargs()
  if (numargs < 1 | missing(inmatrix)) {
    cat(" sparseDecom( inmatrix=NA,  inmask=NA , sparseness=0.01 , nvecs=50 , its=5 , cthresh=250 ) \n")
    return(0)
  }
  if (class(inmask)[[1]] != "antsImage") # create a false mask that we dont use
    if ( is.na(inmask) ) inmask = new("antsImage", "float", 3)
  time1 <- (Sys.time())
  if ( robust > 0 )
    {
    outval = .Call( "eigenanatomyCpp",
        robustMatrixTransform(inmatrix),
        inmask, sparseness, nvecs, its, cthresh, z, smooth,
        initializationList, mycoption, ell1, verbose, powerit,
        priorWeight,
        PACKAGE="ANTsR" )
    } else {
    outval = .Call( "eigenanatomyCpp",
        inmatrix,
        inmask, sparseness, nvecs, its, cthresh, z, smooth,
        initializationList, mycoption, ell1, verbose, powerit,
        priorWeight,
        PACKAGE="ANTsR" )
    }
  time2 <- (Sys.time())
  outval = lappend( outval,  (time2 - time1) )
  names(outval)[length(outval)]='computationtime'
  return( outval )
#  return(list(projections = mydecomp, eigenanatomyimages = fnl, umatrix = fnu,
}
