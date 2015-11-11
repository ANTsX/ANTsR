#' Convenience wrapper for 2-view eigenanatomy decomposition.
#'
#' Decomposes two matrices into paired sparse eigenevectors to maximize
#' canonical correlation. Note: we do not scale the matrices internally.
#' We leave scaling choices to the user.
#'
#' @param inmatrix input as inmatrix=list(mat1,mat2). n by p input matrix and n
#' by q input matrix , spatial variable lies along columns.
#' @param inmask optional pair of antsImage masks
#' @param sparseness a c(.,.) pair of values e.g c(0.01,0.1) enforces an
#' unsigned 99 percent and 90 percent sparse solution for each respective view
#' @param nvecs number of eigenvector pairs
#' @param its number of iterations, 10 or 20 usually sufficient
#' @param cthresh cluster threshold pair
#' @param statdir temporary directory if you want to look at full output
#' @param perms number of permutations
#' @param uselong enforce solutions of both views to be the same - requires
#'  matrices to be the same size
#' @param z subject space (low-dimensional space) sparseness value
#' @param smooth smooth the data (only available when mask is used)
#' @param robust rank transform input matrices
#' @param mycoption enforce 1 - spatial orthogonality, 2 - low-dimensional
#' orthogonality or 0 - both
#' @param initializationList initialization for first view
#' @param initializationList2 initialization for 2nd view
#' @param ell1 gradient descent parameter, if negative then l0 otherwise use l1
#' @param priorWeight Scalar value weight on prior between 0 (prior is weak)
#' and 1 (prior is strong).  Only engaged if initialization is used
#' @param verbose activates verbose output to screen
#' @param rejector rejects small correlation solutions
#' @return outputs a decomposition of a pair of matrices
#' @author Avants BB
#' @examples
#'
#' mat<-replicate(100, rnorm(20))
#' mat2<-replicate(100, rnorm(20))
#' mat<-scale(mat)
#' mat<-mat-min(mat)
#' mat2<-scale(mat2)
#' mat2<-mat2-min(mat2)
#' mydecom<-sparseDecom2( inmatrix=list(mat,mat2),
#'   sparseness=c(0.1,0.3) , nvecs=3, its=3, perms=0)
#' wt<-0.666
#' mat3<-mat*wt+mat2*(1-wt)
#' mat3<-mat3-min(mat3)
#' mydecom<-sparseDecom2( inmatrix=list(mat,mat3),
#'   sparseness=c(0.2,0.2), nvecs=5, its=10, perms=5 )
#'
#' \dontrun{
#' # a masked example
#' im<-antsImageRead( getANTsRData("r64"))
#' dd<- im > 250
#' mask<-antsImageClone( im )
#' mask[ !dd ]<-0
#' mask[ dd ]<-1
#' mat1<-matrix( rnorm(sum(dd)*10) , nrow=10 )
#' mat2<-matrix( rnorm(sum(dd)*10) , nrow=10 )
#' initlist<-list()
#' for ( nvecs in 1:2 ) {
#'   init1<-antsImageClone( mask )
#'   init1[dd]<-rnorm(sum(dd))
#'   initlist<-lappend( initlist, init1 )
#' }
#' ff<-sparseDecom2( inmatrix=list(mat1,mat2), inmask=list(mask,mask),
#'   sparseness=c(0.1,0.1) ,nvecs=length(initlist) , smooth=1,
#'   cthresh=c(0,0), initializationList = initlist ,ell1 = 11 )
#' ### now SNPs ###
#' rf<-usePkg('randomForest')
#' bg<-usePkg('BGLR')
#' if ( bg & rf ) {
#' data(mice)
#' snps<-mice.X
#' numericalpheno<-as.matrix( mice.pheno[,c(4,5,13,15) ] )
#' numericalpheno<-residuals( lm( numericalpheno ~
#'   as.factor(mice.pheno$Litter) ) )
#' nfolds<-6
#' train<-sample( rep( c(1:nfolds), 1800/nfolds ) )
#' train<-( train < 4 )
#' snpd<-sparseDecom2( inmatrix=list( ( as.matrix(snps[train,]) ),
#'   numericalpheno[train,] ), nvecs=20, sparseness=c( 0.001, -0.5 ),
#'   its=3, ell1=0.1 , z=-1 )
#' for ( j in 3:3) {
#' traindf<-data.frame( bmi=numericalpheno[ train,j] ,
#'    snpse=as.matrix( snps[train, ] ) %*% as.matrix( snpd$eig1 ) )
#' testdf <-data.frame( bmi=numericalpheno[!train,j] ,
#'    snpse=as.matrix( snps[!train,] ) %*% as.matrix( snpd$eig1 ) )
#' myrf<-randomForest( bmi ~ . , data=traindf )
#' preddf<-predict(myrf, newdata=testdf )
#' print( cor.test(preddf, testdf$bmi ) )
#' plot( preddf, testdf$bmi )
#' }
#' } # check bg and rf
#' }
#'
#' @export sparseDecom2
sparseDecom2 <- function(
  inmatrix,
  inmask = c(NA, NA),
  sparseness = c(0.01, 0.01),
  nvecs = 3,
  its = 20,
  cthresh = c(0, 0),
  statdir = NA,
  perms = 0,
  uselong = 0,
  z = 0,
  smooth = 0,
  robust = 0,
  mycoption = 0,
  initializationList = list(),
  initializationList2 = list(),
  ell1 = 10,
  priorWeight = 0,
  verbose = FALSE,
  rejector=0  ) {
  idim=3
  if (class(inmask[[1]])[[1]] == "antsImage" ) idim=inmask[[1]]@dimension
  if (class(inmask[[2]])[[1]] == "antsImage" ) idim=inmask[[2]]@dimension
  if (class(inmask[[1]])[[1]] != "antsImage")
     maskx = new("antsImage", "float",idim) else maskx = antsImageClone( inmask[[1]] )
  if (class(inmask[[2]])[[1]] != "antsImage")
     masky = new("antsImage", "float",idim) else masky = antsImageClone( inmask[[2]] )
  if ( nrow(inmatrix[[1]]) != nrow(inmatrix[[2]]) )
    stop("Matrices must have same number of rows")
  inmask = c( maskx, masky )
  verbose = as.numeric( verbose )
  if ( robust > 0 )
    {
    inputMatrices = list(
      robustMatrixTransform( inmatrix[[1]] ),
      robustMatrixTransform( inmatrix[[2]] )
    )
    } else inputMatrices = inmatrix
  # helper function allows easier R-based permutation
  sccaner=.sparseDecom2helper2(
    inputMatrices,
    inmask,
    sparseness,
    nvecs,
    its,
    cthresh,
    statdir,
    uselong,
    z,
    smooth,
    robust,
    mycoption,
    initializationList,
    initializationList2,
    ell1,
    priorWeight,
    verbose
    )
  ccasummary = data.frame(
    corrs = sccaner$corrs,
    pvalues = rep(NA,nvecs)
    )
  if ( perms >  0 )
  {
  ccasummary$pvalues = rep(0 , nvecs )
  nsubs = nrow( inputMatrices[[1]] )
  for ( permer in 1:perms )
  {
  m1 = data.matrix( inputMatrices[[1]][ sample( 1:nsubs ) ,  ] )
  m2 = data.matrix( inputMatrices[[2]][ sample( 1:nsubs ) ,  ] )
  permmatrix = list( m1, m2 )
  sccanerp=.sparseDecom2helper2(
    permmatrix,
    inmask,
    sparseness,
    nvecs,
    its,
    cthresh,
    statdir,
    uselong,
    z,
    smooth,
    robust,
    mycoption,
    initializationList,
    initializationList2,
    ell1,
    priorWeight,
    verbose
    )
    counter = as.numeric( abs(ccasummary$corrs) < abs(sccanerp$corrs)   )
    ccasummary$pvalues = ccasummary$pvalues + counter
    }
    ccasummary$pvalues = ccasummary$pvalues / perms
  }
  mynames = paste(0:(nvecs-1),sep='')
  if ( length(mynames) <= 10 ) mynames = paste( "00", mynames, sep='')
  if ( length(mynames) >10 ) {
    mynames[1:10] = paste( "00", mynames[1:10], sep='')
    tt = nvecs
    if ( tt > 99 ) tt = 99
    mynames[11:tt] = paste( "0", mynames[11:tt], sep='')
  }
  mynames=paste("Variate",mynames,sep='')
  if ( nvecs > 1 )
    {
    colnames( sccaner$eig1 ) = mynames[1:nvecs]
    colnames( sccaner$eig2 ) = mynames[1:nvecs]
    colnames( sccaner$projections ) = mynames[1:nvecs]
    colnames( sccaner$projections2 ) = mynames[1:nvecs]
    }
  if ( rejector > 0 )
    {
    selector=abs(ccasummary$corrs) > rejector
    if ( sum( selector ) > 1 )
      {
      sccaner$eig1 = sccaner$eig1[ , selector ]
      sccaner$eig2 = sccaner$eig2[ , selector ]
      ccasummary = ccasummary[ selector, ]
      }
    }
  return(
    list(
      projections = sccaner$projections,
      projections2 = sccaner$projections2,
      eig1 = sccaner$eig1,
      eig2 = sccaner$eig2,
      ccasummary = ccasummary,
      sparseness = sparseness
      )
    )
}


.sparseDecom2helper2 <- function(
  inputMatrices,
  inmask,
  sparseness,
  nvecs,
  its,
  cthresh,
  statdir,
  uselong,
  z,
  smooth,
  robust,
  mycoption,
  initializationList,
  initializationList2,
  ell1,
  priorWeight,
  verbose) {
  outval = .Call( "sccanCpp",
    inputMatrices[[1]],
    inputMatrices[[2]],
    inmask[[1]],
    inmask[[2]],
    sparseness[1],
    sparseness[2],
    nvecs,
    its,
    cthresh[1],
    cthresh[2],
    z,
    smooth,
    initializationList,
    initializationList2,
    mycoption,
    ell1,
    verbose,
    priorWeight,
    PACKAGE="ANTsR" )
  p1 = inputMatrices[[1]] %*% t(outval$eig1)
  p2 = inputMatrices[[2]] %*% t(outval$eig2)
  outcorrs = diag( cor( p1 , p2  ) )
  if ( priorWeight < 1.e-10 )
    {
    myord = rev( order( abs( outcorrs ) ) )
    outcorrs = outcorrs[ myord ]
    p1 = p1[ , myord ]
    p2 = p2[ , myord ]
    outval$eig1 = outval$eig1[ myord, ]
    outval$eig2 = outval$eig2[ myord, ]
    }
  return(
      list(
        projections = p1,
        projections2 = p2,
        eig1 = t(outval$eig1),
        eig2 = t(outval$eig2),
        corrs = outcorrs
        )
      )
}
