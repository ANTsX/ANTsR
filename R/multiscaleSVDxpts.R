#' Create sparse distance, covariance or correlation matrix
#'
#' Exploit k-nearest neighbor algorithms to estimate a sparse similarity matrix.
#' Critical to the validity of this function is the basic mathematical
#' relationships between euclidean distance and correlation and between
#' correlation and covariance.  For applications of such matrices, one may see
#' relevant publications by Mauro Maggioni and other authors.
#'
#' @param x input matrix, should be n (samples) by p (measurements)
#' @param k number of neighbors
#' @param r radius of epsilon-ball
#' @param sigma parameter for kernel PCA.
#' @param kmetric similarity or distance metric determining k nearest neighbors
#' @param eps epsilon error for rapid knn
# #' @param mypkg set either nabor, RANN, rflann
#' @param ncores number of cores to use
#' @return matrix sparse p by p matrix is output with p by k nonzero entries
#' @author Avants BB
#' @references
#' \url{http://www.math.jhu.edu/~mauro/multiscaledatageometry.html}
#' @examples
#' \dontrun{
#' mat = matrix( rnorm(60), ncol=10 )
#' smat = sparseDistanceMatrix( mat, 2 )
#' r16 = antsImageRead( getANTsRData( 'r16' ) )
#' mask = getMask( r16 )
#' mat <- getNeighborhoodInMask(image = r16, mask = mask, radius = c(0,0),
#'   physical.coordinates=TRUE, spatial.info=TRUE )
#' smat = sparseDistanceMatrix( t(mat$indices), 10 ) # close points
#' }
#' @export sparseDistanceMatrix
sparseDistanceMatrix <- function( x, k = 3, r = Inf, sigma = NA,
  kmetric = c("euclidean", "correlation", "covariance", "gaussian"  ),
  eps = 1.e-6, ncores=NA ) # , mypkg = "nabor"  )
{
  if ( any( is.na( x ) ) ) stop("input matrix has NA values")
  mypkg = 'rflann'
  # note that we can convert from distance to covariance
  #   d_ij^2 = sigma_i^2 +  \sigma_j^2  - 2 * cov_ij
  # and from correlation to covariance   diag(sd) %*% corrMat %*% diag(sd)
  # and from euclidean distance of standardized data to correlation
  # 1.0 - dist^2 / ( 2 * nrow( x ) )
  # TODO / FIXME - implement covariance
  if ( ! usePkg("Matrix") )
    stop("Please install the Matrix package")
  if ( ! usePkg( mypkg ) )
    stop( paste("Please install the",mypkg,"package") )
  kmetric <- match.arg( kmetric )
  if ( kmetric == "gaussian" & is.na( sigma ) )
    stop("Please set the sigma parameter")
  cometric = ( kmetric == "correlation" | kmetric == "covariance" )
  if ( cometric & r == Inf ) r = -Inf
#  if ( ! usePkg("irlba") )
#    stop("Please install the irlba package")
# see http://www.analytictech.com/mb876/handouts/distance_and_correlation.htm
# euclidean distance to correlation - xin contains correlations
  ecor <- function( xin ) { 1.0 - xin^2 / ( 2 * nrow( x ) ) }
  if ( kmetric == "covariance" ) mycov = apply( x, FUN=sd, MARGIN=2 )
  if ( cometric ) {
    x = scale( x, center = TRUE, scale = (kmetric == "correlation" ) )
    }
  if ( mypkg[1] == "nabor" ) bknn = nabor::knn( t( x ) , k=k, eps=eps )
  if ( mypkg[1] == "RANN" )  bknn = RANN::nn2( t( x ) , k=k, eps=eps  )
  if ( mypkg[1] == "rflann" )  {
    myncores = as.numeric( system('getconf _NPROCESSORS_ONLN', intern = TRUE) )
    if ( !is.na( ncores  ) ) myncores = ncores
    bknn = rflann::Neighbour( t(x), t(x), k=k, "kdtree", cores=myncores, 1 )
    names( bknn ) = c( "nn.idx", "nn.dists" )
    }
#  if ( mypkg[1] == "naborpar" ) bknn = .naborpar( t( x ), t( x ) , k=k, eps=eps  )
  if ( cometric ) bknn$nn.dists = ecor( bknn$nn.dists )
  tct = 0
  for ( i in 1:ncol( x ) )
    {
    inds = bknn$nn.idx[i,]    # index
    locd = bknn$nn.dists[i,]  # dist
    inds[ inds <= i ] = NA # we want a symmetric matrix
    tct = tct + sum( !is.na(inds) )
    }
  # build triplet representation for sparse matrix
  myijmat = matrix( nrow=(tct), ncol=3 )
  tct2 = 1
  for ( i in 1:ncol( x ) )
    {
    inds = bknn$nn.idx[i,]
    locd = bknn$nn.dists[i,]
    if ( kmetric == "gaussian" & !is.na( sigma ) )
      locd = exp( -1.0 * locd^2 / ( 2.0 * sigma^2 ) )
    inds[ inds <= i ] = NA # we want a symmetric matrix
    tctinc = sum( !is.na(inds) )
    if ( kmetric == "covariance" )
      {
      loccov = mycov[ i ] * mycov[  inds ]
      locd = locd * loccov
      }
    if ( tctinc > 0 )
      {
      upinds = tct2:(tct2+tctinc-1)
      myijmat[ upinds, 1 ] = i
      myijmat[ upinds, 2 ] = inds[ !is.na( inds ) ]
      myijmat[ upinds, 3 ] = locd[ !is.na( inds ) ]
      tct2 = tct2 + tctinc
      }
    }
  kmatSparse = Matrix::sparseMatrix(
    i=myijmat[,1],
    j=myijmat[,2],
    x=myijmat[,3], symmetric = TRUE
  )
  if ( kmetric == "gaussian" ) diag( kmatSparse ) = 1
  if ( cometric )
    {
    if ( kmetric == "covariance" )  diag( kmatSparse ) = mycov^2
    if ( kmetric == "correlation" ) diag( kmatSparse ) = 1
    kmatSparse[ kmatSparse < r ] = 0
    }  else {
    kmatSparse[ kmatSparse > r ] = 0
    }
  return( kmatSparse )
#
#  mysvd = irlba::partial_eigen( kmatSparse, nvec )
#  sparsenessParam = ( range( abs( mysvd ) ) * 0.00001 )[ 2 ]
#  sparsenessParam = 1e-6
#
}




#' Create sparse distance, covariance or correlation matrix from x, y
#'
#' Exploit k-nearest neighbor algorithms to estimate a sparse matrix measuring
#' the distance, correlation or covariance between two matched datasets.
#' Critical to the validity of this function is the basic mathematical
#' relationships between euclidean distance and correlation and between
#' correlation and covariance.  For applications of such matrices, one may see
#' relevant publications by Mauro Maggioni and other authors.
#'
#' @param x input matrix, should be n (samples) by p (measurements)
#' @param y input matrix second view, should be n (samples) by q (measurements)
#' @param k number of neighbors
#' @param r radius of epsilon-ball
#' @param sigma parameter for kernel PCA.
#' @param kmetric similarity or distance metric determining k nearest neighbors
#' @param eps epsilon error for rapid knn
# #' @param mypkg set either nabor, RANN, rflann
#' @param ncores number of cores to use
#' @return matrix sparse p by q matrix is output with p by k nonzero entries
#' @author Avants BB
#' @references
#' \url{http://www.math.jhu.edu/~mauro/multiscaledatageometry.html}
#' @examples
#' \dontrun{
#' mat = matrix( rnorm(60), nrow=6 )
#' mat2 = matrix( rnorm(120), nrow=6 )
#' smat = sparseDistanceMatrixXY( mat, mat2, 3 )
#' smat2 = sparseDistanceMatrixXY( mat2, mat, 3 )
#' }
#' @export sparseDistanceMatrixXY
sparseDistanceMatrixXY <- function( x, y, k = 3, r = Inf, sigma = NA,
  kmetric = c("euclidean", "correlation", "covariance", "gaussian"  ),
  eps = 1.e-6, ncores=NA ) # , mypkg = "nabor" )
{
  if ( any( is.na( x ) ) ) stop("input matrix x has NA values")
  if ( any( is.na( y ) ) ) stop("input matrix y has NA values")
  mypkg = 'rflann'
  if ( ! usePkg("Matrix") )
    stop("Please install the Matrix package")
  if ( ! usePkg( mypkg ) )
    stop( paste("Please install the",mypkg,"package") )
  kmetric <- match.arg( kmetric )
  if ( kmetric == "gaussian" & is.na( sigma ) )
    stop("Please set the sigma parameter")
  cometric = ( kmetric == "correlation" | kmetric == "covariance" )
  if ( cometric & r == Inf ) r = -Inf
  ecor <- function( xin ) { 1.0 - xin^2 / ( 2 * nrow( x ) ) }
  if ( cometric ) {
    x = scale( x, center=TRUE, scale = (kmetric == "correlation" )  )
    y = scale( y, center=TRUE, scale = (kmetric == "correlation" )  )
    }
  if ( mypkg[1] == "nabor" ) bknn = nabor::knn( t( y ), t( x ) , k=k, eps=eps )
  if ( mypkg[1] == "RANN" )  bknn = RANN::nn2( t( y ), t( x ) , k=k, eps=eps )
  if ( mypkg[1] == "rflann" )  {
    myncores = as.numeric( system('getconf _NPROCESSORS_ONLN', intern = TRUE) )
    if ( !is.na( ncores  ) ) myncores = ncores
    bknn = rflann::Neighbour( t(y), t(x), k=k, "kdtree", cores=myncores, 1 )
    names( bknn ) = c( "nn.idx", "nn.dists" )
    }
#  if ( mypkg[1] == "naborpar" ) bknn = .naborpar( t( y ), t( x ) , k=k, eps=eps  )
  if ( cometric ) bknn$nn.dists = ecor( bknn$nn.dists )
  tct = 0
  nna = rep( FALSE, nrow( bknn$nn.idx ) )
  #
  for ( i in 1:nrow( bknn$nn.idx ) )
    {
    inds = bknn$nn.idx[i,]    # index
    locd = bknn$nn.dists[i,]  # dist
    nna[ i ] = any( is.na( inds ) )
    tct = tct + sum( !is.na(inds) )
    }
  # build triplet representation for sparse matrix
  myijmat = matrix( nrow=(tct), ncol=3 )
  tct2 = 1
  for ( i in 1:ncol( y ) )
    {
    inds = bknn$nn.idx[i,]
    locd = bknn$nn.dists[i,]
    if ( kmetric == "gaussian" & !is.na( sigma ) )
      locd = exp( -1.0 * locd^2 / ( 2.0 * sigma^2 ) )
    tctinc = sum( !is.na(inds) )
    if ( kmetric == "covariance" )
      {
      locd = cov( y[,i], x[,inds] )
      }
    else if ( kmetric == "correlation" )
      {
      locd = cor( y[,i], x[,inds] )
      }
    if ( tctinc > 0 )
      {
      upinds = tct2:(tct2+tctinc-1)
      myijmat[ upinds, 1 ] = i
      myijmat[ upinds, 2 ] = inds[ !is.na( inds ) ]
      myijmat[ upinds, 3 ] = locd[ !is.na( inds ) ]
      tct2 = tct2 + tctinc
      }
    }
  kmatSparse = Matrix::sparseMatrix(
    i=myijmat[,1],
    j=myijmat[,2],
    x=myijmat[,3],
    dims = c( ncol( y ), ncol( x ) ), symmetric = FALSE
  )
  if ( cometric )
    {
#    if ( kmetric == "covariance" )  diag( kmatSparse ) = mycov^2
#    if ( kmetric == "correlation" ) diag( kmatSparse ) = 1
    kmatSparse[ kmatSparse < r ] = 0
    }  else {
    kmatSparse[ kmatSparse > r ] = 0
    }
  return( kmatSparse )
}



#
# .naborpar <- function( xin, yin, k, eps ) {
#  if ( ! usePkg( "doParallel" ) ) stop( "need doParallel for naborpar" )
#  library( doParallel )
#  library( parallel )
#  invisible(capture.output(library( foreach, quietly=TRUE )))
#  ncor = parallel::detectCores( )
#  cl <- round( parallel::makeCluster( ncor )/2 )
#  doParallel::registerDoParallel( cl )
#  mycomb <- function( x, y ) {
#    x$nn.idx = rbind( x$nn.idx, y$nn.idx )
#    x$nn.dists = rbind( x$nn.dists, y$nn.dists )
#    x
#    }
#  mylen = ceiling( nrow( xin ) / ncor  )
#  selinds = rep( c(1:ncor), each=mylen )[1:nrow(xin)]
#  myout <- foreach( i=1:ncor, .combine=mycomb, .packages="ANTsR" ) %dopar% {
#    selector = selinds == i
#    nabor::knn( xin , yin[selector,], k=k, eps=eps )
#  }
#  myout
# }
#

#' Multi-scale svd
#'
#' Maggioni's multi-scale SVD algorithm explores the dimensionality of a dataset
#' by investigating the change in eigenvalues with respect to a scale parameter.
#' The scale parameter is defined by the radius of a ball that sits at each
#' point in the data.  The ball, at each scale, is moved across the dataset
#' and SVD is computed within the intersection of the ball and the data at each
#' point.  The shape in this collection of eigenvalues, with respect to scale,
#' enables us to estimate both signal and noise dimensionality and scale.  The
#' estimate can be computed efficiently on large datasets if the sampling is
#' chosen appropriately.  The challenge, in this algorithm, is classifying the
#' dimensions of noise, curvature and data.  This classification currently uses
#' variations on heuristics suggested in work by Maggioni et al.
#'
#' @param x input matrix, should be n (samples) by p (measurements)
#' @param r radii to explore
#' @param locn number of local samples to take at each scale
#' @param nev maximum number of eigenvalues to compute
#' @param knn randomly sample neighbors to assist with large datasets. set k with this value.
#' @param verbose boolean to control verbosity of output
#' @param plot boolean to control whether we plot results.  its value determines
#' which eigenvector off which to base the scale of the y-axis.'
#' @return list with a vector of tangent, curvature, noise dimensionality and a
#' a dataframe containing eigenvalues across scale, in correspondence with r:
#' \itemize{
#'   \item{dim: }{The tangent, curvature and noise dimensionality vector.  The
#' data dimensionality is the first entry, the curvature dimensionality exists
#' from the second to the first entry of the noise vector.}
#'   \item{noiseCutoffs: }{Dimensionalities where the noise may begin.  These
#' are candidate cutoffs but may contain some curvature information.'}
#'   \item{evalsVsScale: }{eigenvalues across scale}
#'   \item{evalClustering:}{data-driven clustering of the eigenvalues}
#' }
#' @author Avants BB
#' @references
#' \url{http://www.math.jhu.edu/~mauro/multiscaledatageometry.html}
#' @examples
#'
#' sphereDim = 9
#' embeddDim = 100
#' n = 1000
#' if ( ! usePkg( "pracma"  ) ) stop( "need pracma" )
#' sphereData = pracma::rands( n, sphereDim, 1. )
#' mysig = 0.1
#' spherEmbed = matrix( rnorm( n * embeddDim, 0, mysig ), nrow = n, ncol = embeddDim )
#' spherEmbed[ , 1:ncol( sphereData ) ] = spherEmbed[ , 1:ncol( sphereData ) ] + sphereData
#' myr = seq( 1.0, 2.2, 0.05 ) # scales at which to sample
#' mymssvd = multiscaleSVD( spherEmbed, myr, locn=5, nev=20, plot=1 )
#'
#' @export multiscaleSVD
multiscaleSVD <- function( x, r, locn, nev, knn = 0, verbose=FALSE, plot=0 )
{
mresponse = matrix( ncol = nev, nrow = length( r ) )
n = nrow( x )
calcRowMatDist <- function( xmat, xrow )
  {
  locmag <- function( x, xrow ) sqrt( sum( ( x - xrow )^2 ) )
  apply( xmat, FUN=locmag, MARGIN=1, xrow=xrow )
  }
for ( myscl in 1:length( r ) )
  {
  myr = r[ myscl ]
  locsam = sample( 1:n , locn )
  myevs = matrix( nrow=locn, ncol=nev )
  for ( i in 1:locn )
    {
    rowdist = calcRowMatDist( x, x[ locsam[i], ] )
    sel = rowdist < myr
    if ( sum( sel , na.rm = T ) >  2 ) {
      if ( knn > 0 & sum( sel ) > knn ) # take a subset of sel
        {
        selinds = sample( 1:length( sel ), knn )
        sel[ -selinds ] = FALSE
        }
      lmat = x[sel,]
      if ( nrow( lmat ) < ncol( lmat ) ) lcov = cov( t( lmat ) ) else lcov = cov( lmat )
      temp = svd( lcov, nv=(nrow(lcov)-1) )$d # * embeddDim / sum(sel)
       # lcov = sparseDistanceMatrix( x, k = knn, kmetric = "cov" )
       #  temp = irlba::irlba( lcov, nv=(nrow(lcov)-1) )$d
      temp = temp[ 1:min( c(nev,length(temp)) ) ]
      if ( length( temp ) < nev ) temp = c( temp, rep(NA,nev-length(temp)) )
      } else temp = rep( NA, nev )
    myevs[ i, 1:nev ] = temp
    if ( i == locn ) {
      mresponse[ myscl, ] = colMeans( myevs, na.rm=T )
      if ( verbose ) {
        print( paste( i, "r", myr, "localN", sum(sel) ) )
        print( mresponse[ myscl, ] )
        }
      }
    }
  }
colnames( mresponse ) = paste("EV",1:nev,sep='')
rownames( mresponse ) = paste("Scale",1:length(r),sep='')
# just use pam to cluster the eigenvalues
goodscales = !is.na( rowMeans( mresponse ) )
temp = t(mresponse)[1:nev,goodscales] # remove top evec
pamk = NA
krng = 5:min( dim(temp) - 1 ) # force a min of 4 clusters
if ( usePkg("fpc") )
  pamk = fpc::pamk( temp, krange=krng )$pamobject$clustering
############################################################
# noise dimensionality
scaleEvalCorrs = rep( NA, ncol( mresponse ) )
for ( i in 1:nev )
  {
  mylm = lm(  mresponse[,i] ~ stats::poly(r^2, 2 ) )
  coffs = coefficients( summary( mylm ) )
  scaleEvalCorrs[ i ] = summary( mylm )$r.squared # coffs[2,4]
  }
shiftinds = c(2:ncol( mresponse ), ncol( mresponse ) )
delt = scaleEvalCorrs - scaleEvalCorrs[shiftinds]
qdelt = quantile( delt[2:length(delt)], 0.9, na.rm=T )
noiseDim = which( delt > qdelt ) + 1
# curvature dimensionality
# find the dimensionality that maximizes the t-test difference across
# the multi-scale eigenvalues
myt = rep( NA,  nev )
for ( i in 3:( nev - 2 ) )
  {
  lowinds = 2:i
  hiinds  = (i+1):nev
  myt[ i ] = t.test( mresponse[,lowinds], mresponse[,hiinds], paired=FALSE )$sta
#  myt[ i ] = t.test( scaleEvalCorrs[lowinds], scaleEvalCorrs[hiinds], paired=FALSE )$sta
   }
dataDimCurv = which.max( myt )
# find singular values that do not grow with scale ( radius )
if ( length( r ) > 4 )
{
scaleEvalCorrs = rep( NA, ncol( mresponse ) )
ply = 4 # power for polynomial model
for ( i in 1:dataDimCurv )
  {
  mylm = lm(  mresponse[,i] ~ stats::poly(r, ply ) )
  coffs = coefficients( summary( mylm ) )
#  print( summary( mylm ) )
#  print( paste("EV",i) )
#  Sys.sleep( 3 )
  # linear term coffs[2,4]
  # quadratic term coffs[3,4]
  # noise term coffs[5,4]
  scaleEvalCorrs[ i ] = coffs[2,4]
  }
dataDim = max( which( scaleEvalCorrs < 0.05 ) )
}  else dataDim = 4
############################################################
if ( plot > 0 )
  {
  mycols = rainbow( nev )
  growthRate1 = mresponse[,1]
  plot( r, growthRate1, type='l', col = mycols[1], main='Evals by scale',
        ylim=c(0.00, max( mresponse[,plot], na.rm=T) ),
        xlab='ball-radius', ylab='Expected Eval' )
  for ( i in 2:ncol(mresponse) )
    {
    growthRatek = mresponse[,i] # magic :: shift(mresponse[,i],1)*0
    points( r, growthRatek, type='l',col=mycols[i])
    }
  }
return(
  list(
    dim            = c(dataDim,dataDimCurv),
    noiseCutoffs   = noiseDim,
    evalClustering = pamk,
    evalsVsScale   = mresponse )
    )
}




#' k-nearest neighbors constrained smoothing
#'
#' Compute a smoothing matrix based on an input matrix of point coordinates
#'
#' @param x input matrix of point coordinates of dimensions n-spatial
#' spatial dimensions by p points
#' @param k number of neighbors, higher causes more smoothing
#' @param sigma sigma for the gaussian function
#' @return sparse matrix is output
#' @author Avants BB
#' @examples
#'
#' \dontrun{
#' mask = getMask( antsImageRead( getANTsRData( 'r16' ) ) )
#' spatmat = t( imageDomainToSpatialMatrix( mask, mask ) )
#' smoothingMatrix = knnSmoothingMatrix( spatmat, k = 25, sigma = 3.0 )
#' rvec = rnorm( nrow( smoothingMatrix ) )
#' srvec = smoothingMatrix %*% rvec
#' rvi = makeImage( mask, rvec )
#' srv = makeImage( mask,  as.numeric( srvec ) )
#' }
#' @export knnSmoothingMatrix
knnSmoothingMatrix <- function( x, k, sigma ) {
  usePkg( "Matrix" )
  temp = sparseDistanceMatrix( x, k = k,
    kmetric = "gaussian", sigma = sigma )
  return( temp / Matrix::rowSums( temp ) )
}






#' smooth matrix prediction
#'
#' Reconstruct a n by p matrix given n by k basis functions or predictors.
#' The reconstruction can be regularized.
#' # norm( x - uv^t )
#' # d/dv ... leads to ( -u, x - uv^t  )
#' # u^t u v^t - u^t x
#'
#' @param modelFormula a formula object which has, on the left, the variable x
#' and the prediction formula on the right.
#' @param x input matrix to be predicted.
#' @param basisDf data frame for basis predictors
#' @param iterations number of gradient descent iterations
#' @param gamma step size for gradient descent
#' @param sparsenessQuantile quantile to control sparseness - higher is sparser.
#' @param positivity restrict to positive or negative solution (beta) weights.
#' choices are positive, negative or either as expressed as a string.
#' @param smoothingMatrix allows parameter smoothing, should be square and same
#' size as input matrix
#' @param repeatedMeasures list of repeated measurement identifiers. this will
#' allow estimates of per identifier intercept.
#' @param rowWeights vectors of weights with size n (assumes diagonal covariance)
#' @param LRR integer value sets rank for fast version exploiting matrix approximation
#' @param doOrth boolean enforce gram-schmidt orthonormality
#' @param verbose boolean option
#' @return matrix of size p by k is output
#' @author Avants BB
#' @examples
#'
#' \dontrun{
#' mask = getMask( antsImageRead( getANTsRData( 'r16' ) ) )
#' spatmat = t( imageDomainToSpatialMatrix( mask, mask ) )
#' smoomat = knnSmoothingMatrix( spatmat, k = 5, sigma = 1.0 )
#' mat <- matrix(rnorm(sum(mask)*50),ncol=sum(mask),nrow=50)
#' mat[ 1:25,100:10000]=mat[ 1:25,100:10000]+1
#' age = rnorm( 1:nrow(mat))
#' for ( i in c( 5000:6000, 10000:11000, 16000:17000 )  ){
#'   mat[ , i ] = age*0.1 + mat[,i]
#'   }
#' gen = c( rep("M",25), rep("F",12 ) , rep("T",13 ) )
#' repmeas = rep( c("A","B","C","D","E","F","G"), nrow( mat ) )[1:nrow(mat)]
#' mydf = data.frame( age = scale( age ), gen = gen )
#' fit = smoothMatrixPrediction( x=mat, basisDf=mydf, iterations = 10,
#'   gamma = 1.e-6, sparsenessQuantile = 0.5,
#'   smoothingMatrix = smoomat, repeatedMeasures=repmeas,
#'   verbose=T )
#' tt = mat %*% fit$v
#' print( cor.test( mydf$age, tt[,1] ) )
#' print( cor.test( fit$u[,"genM"], tt[,2] ) )
#' vimg = makeImage( mask, (fit$v[,1] ) ); print(range(vimg)*10)
#' plot( mask, vimg, window.overlay=range(abs(vimg)))
#' vimg = makeImage( mask, (fit$v[,2] ) ); print(range(vimg)*10)
#' plot( mask, vimg, window.overlay=range(abs(vimg)))
#' }
#' @export smoothMatrixPrediction
smoothMatrixPrediction <- function(
  x,
  basisDf,
  modelFormula = as.formula( " x ~ ." ),
  iterations = 10,
  gamma = 1.e-6,
  sparsenessQuantile = 0.5,
  positivity = c("positive","negative","either"),
  smoothingMatrix = NA,
  repeatedMeasures = NA,
  rowWeights = NA,
  LRR = NA,
  doOrth = FALSE,
  verbose = FALSE
  )
{
poschoices = c("positive","negative","either", TRUE, FALSE )
if ( sum( positivity == poschoices ) != 1 | length( positivity ) != 1 )
  stop( 'choice of positivity parameter is not good - see documentation')
if ( positivity == TRUE ) positivity = "positive"
if ( positivity == FALSE ) positivity = "either"
smoothingWeight = 1.0
if ( missing( "x" ) | missing( "basisDf" ) ) {
  message("this function needs input")
  return( NA )
  }
if ( nrow( x ) != nrow( basisDf ) ) {
  message("inconsistent row numbers between x and basisDf")
  return( NA )
  }
if ( ! any( is.na( repeatedMeasures ) ) ) {
  usubs = unique( repeatedMeasures )
  wtdf = data.frame( table( repeatedMeasures ) )
  # rowWeights should scale with counts
  repWeights = rep( 0, length( repeatedMeasures ) )
  for ( u in usubs ) {
    repWeights[ repeatedMeasures == u ] = 1.0 / wtdf$Freq[ wtdf$repeatedMeasures == u ]
  }
  rm( wtdf )
  if ( all( is.na( rowWeights ) ) ) {
    rowWeights = repWeights
    } else rowWeights = rowWeights * repWeights
  }
hasweights =  ! all( is.na( rowWeights ) )
if ( hasweights ) {
  locdf = basisDf
  locdf$wts = rowWeights
  wts = "this is just a placeholder"
  mdl = lm( modelFormula, data = locdf, weights = wts, na.action="na.exclude"  )
  rm( locdf )
  } else mdl = lm( modelFormula, data = basisDf, na.action="na.exclude" )
# bmdl = bigLMStats( mdl )
u = scale( model.matrix( mdl ) )
intercept = u[,1]
u = u[,-1]
v = t( mdl$coefficients[-1, ] )
v = v + matrix( rnorm( length( v ), 0, 0.01 ), nrow = nrow( v ), ncol = ncol( v ) )
if ( !is.na( LRR ) ) {
  u = lowrankRowMatrix( u, LRR )
  v = t( lowrankRowMatrix( t(v), LRR ) )
  x = icawhiten( x, LRR )
#  x = lowrankRowMatrix( x, LRR )
  }
if ( hasweights & is.na( LRR ) ) {
  u = diag( sqrt( rowWeights ) ) %*% u
  x = diag( sqrt( rowWeights ) ) %*% x
  }
intercept = rowMeans( x - ( u %*% t(v) ) )
err = mean( abs( x - ( u %*% t(v) + intercept ) ) )
if ( verbose ) print( paste( "iteration",0, "err",  err ) )
tu = t( u )
tuu = t( u ) %*% u
errs = rep( NA, length( iterations ) )
i = 1
while ( i <= iterations ) {
  v = as.matrix( smoothingMatrix %*% v )
  dedv = t( tuu %*% t( v ) - tu %*% x )
  v = v - dedv * gamma
#  v = rsvd::rsvd( v )$v
  for ( vv in 1:ncol( v ) ) {
    v[ , vv ] = v[ , vv ] / sqrt( sum( v[ , vv ] * v[ , vv ] ) )
    if ( vv > 1 )
      for ( vk in 1:(vv-1) ) {
        temp = v[,vk]
        denom = sum( temp * temp , na.rm=T )
        if ( denom > 0 ) ip = sum( temp * v[,vv] ) / denom else ip = 1
        v[ , vv ] = v[, vv ] - temp * ip
        }
    localv = v[ , vv ]
    myquant = quantile( localv , sparsenessQuantile, na.rm=T )
    if ( positivity == 'positive') {
      localv[ localv <= myquant ] = 0
    } else if ( positivity == 'negative' ) {
      localv[ localv > myquant ] = 0
    } else if ( positivity == 'either' ) {
      localv[ abs(localv) < quantile( abs(localv) , sparsenessQuantile, na.rm=T  ) ] = 0
    }
    v[ , vv ] = localv
  }
  intercept = rowMeans( x - ( u %*% t(v) ) )
  if ( ! any( is.na( repeatedMeasures ) ) & is.na( LRR ) ) { # estimate random intercepts
    for ( s in usubs ) {
      usel = repeatedMeasures == s
      intercept[ usel ] = mean( intercept[ usel ], na.rm=T  )
      }
    }
  err = mean( abs( x - ( u %*% t(v) + intercept  ) ) )
  errs[ i ] = err
  if ( i > 1 ) {
    if ( ( errs[ i ] > errs[ i - 1 ] ) &  ( i == 3 ) )
      {
#      message(paste("flipping sign of gradient step:", gamma))
#      gamma = gamma * ( -1.0 )
      }
    else if ( ( errs[ i ] > errs[ i - 1 ] ) )
      {
      gamma = gamma * ( 0.5 )
      message(paste("reducing gradient step:", gamma))
      }
    if ( abs(gamma) < 1.e-9 ) i = iterations
    }
  i = i + 1
  if ( verbose ) print( paste( i,  err ) )
}
if ( verbose ) print( paste( "end",  err ) )
colnames( v ) = colnames( u )
return( list( u = u, v=v, intercept = intercept ) )
}




#' smooth matrix-based regression
#'
#' Reconstruct a n by 1 vector given n by p matrix of predictors.
#'
#' @param x input matrix on which prediction is based
#' @param y target vector
#' @param iterations number of gradient descent iterations
#' @param sparsenessQuantile quantile to control sparseness - higher is sparser.
#' @param positivity restrict to positive or negative solution (beta) weights.
#' choices are positive, negative or either as expressed as a string.
#' @param smoothingMatrix allows parameter smoothing, should be square and same
#' size as input matrix
#' @param nv number of predictor spatial vectors
#' @param extraPredictors additional column predictors
#' @param verbose boolean option
#' @return vector of size p is output
#' @author Avants BB
#' @examples
#'
#' \dontrun{
#' mask = getMask( antsImageRead( getANTsRData( 'r16' ) ) )
#' spatmat = t( imageDomainToSpatialMatrix( mask, mask ) )
#' smoomat = knnSmoothingMatrix( spatmat, k = 200, sigma = 1.0 )
#' mat <- matrix(rnorm(sum(mask)*50),ncol=sum(mask),nrow=50)
#' mat[ 1:25,100:10000]=mat[ 1:25,100:10000]+1
#' age = rnorm( 1:nrow(mat))
#' for ( i in c( 5000:6000, 10000:11000, 16000:17000 )  ){
#'   mat[ , i ] = age*0.1 + mat[,i]
#'   }
#' sel = 1:25
#' fit = smoothRegression( x=mat[sel,], y=age[sel], iterations = 10,
#'   sparsenessQuantile = 0.5,
#'   smoothingMatrix = smoomat, verbose=T )
#' tt = mat %*% fit$v
#' print( cor.test( age[-sel], tt[-sel,1] ) )
#' vimg = makeImage( mask, (fit$v[,1] ) ); print(range(vimg)*10)
#' plot( mask, vimg, window.overlay=range(abs(vimg)))
#' }
#' @export smoothRegression
## #' @param gamma step size for gradient descent
smoothRegression <- function(
  x,
  y,
  iterations = 10,
  sparsenessQuantile = 0.5,
  positivity = FALSE,
  smoothingMatrix = NA,
  nv = 2,
  extraPredictors,
  verbose = FALSE
  )
{
x = scale( x, scale = FALSE )
poschoices = c("positive","negative","either", TRUE, FALSE )
if ( sum( positivity == poschoices ) != 1 | length( positivity ) != 1 )
  stop( 'choice of positivity parameter is not good - see documentation')
if ( positivity == TRUE ) positivity = "positive"
if ( positivity == FALSE ) positivity = "either"
gamma = 1.e-8
if ( missing( "x" ) | missing( "y" ) ) {
  message("this function needs input")
  return( NA )
  }
#
if ( is.na( smoothingMatrix[1] ) ) smoothingMatrix = diag( ncol( x ) )
originalN = ncol( x )
if ( ! missing( "extraPredictors" ) ) {
  temp = lm( y ~ . , data=data.frame(extraPredictors))
  mdlmatrix = scale( model.matrix( temp )[,-1], scale=TRUE )
  extraN = originalN + ncol( mdlmatrix )
  x = cbind( x, mdlmatrix )
  }
scaledY = as.numeric( scale( y ) )
xgy = scaledY %*% x
v = matrix( 0, nrow = nv, ncol = ncol( x ) )
for ( k in 1:nv )
  v[k,]= xgy + matrix( rnorm( ncol( x ), 0, 1.e-3 ), nrow = 1, ncol = ncol( x ) )
errs = rep( NA, length( iterations ) )
i = 1
while ( i <= iterations ) {
  temp = t( x %*% t( as.matrix( v ) ) ) %*% x
  dedv = temp * 0
  for ( k in 1:nv )
    dedv[k,] = xgy - temp[k,]
  v = ( v + dedv * gamma )
  v[ , 1:originalN ] = as.matrix( v[ , 1:originalN ] %*% smoothingMatrix )
  for ( k in 1:nv ) {
    if ( k > 1 )
      for ( vk in 1:(k-1) ) {
        temp = v[vk,]
        denom = as.numeric( temp  %*%  temp )
        if ( denom > 0 ) ip = as.numeric( temp %*%  v[k,] ) / denom else ip = 1
        v[k ,  ] = v[k, ] - temp * ip
        }
      }
  v[ , 1:originalN ] = as.matrix( v[ , 1:originalN ] %*% smoothingMatrix )
  v = t( orthogonalizeAndQSparsify( t(v), sparsenessQuantile, positivity ) )
  if ( i < 3 ) gamma = quantile( v[ abs(v) > 0 ] , 0.5 , na.rm=TRUE ) * 1.e-2
  proj = x %*% t( v )
  intercept = colMeans( scaledY - ( proj ) )
  for ( k in 1:nv ) proj[,k] = proj[,k] + intercept[ k ]
  ymdl = lm( scaledY ~ proj )
  err = mean( abs( scaledY - ( proj ) ) )
  errs[ i ] = err # summary(ymdl)$r.squared * ( -1 )
  if ( verbose ) print( paste("it/err/rsq", i,  errs[ i ], summary(ymdl)$r.squared, "gamma", gamma  ) )
#  coefwts = coefficients( ymdl )
#  for ( k in 1:nv ) v[k,] = v[k,] * coefwts[k+1]
#  proj = x %*% t( v ) # + coefwts[1]
  if ( i > 1 ) {
    if ( ( mean(errs[ 4:5 ]) > mean(errs[ 1:3 ])  ) &  ( i == 5 ) )
      {
#      message(paste("flipping sign of gradient step:", gamma))
      gamma = gamma * ( -1.0 )
      }
    else if ( ( errs[ i ] > errs[ i - 1 ] ) )
      {
      gamma = gamma * ( 0.5 )
      message(paste("reducing gradient step:", gamma))
      } else gamma = gamma * 1.05
    if ( abs(gamma) < 1.e-12 ) i = iterations
  }
  i = i + 1
  }
if ( verbose ) print( paste( "end",  errs[ i  -  1 ]  ) )
imagev = v[ , 1:originalN ]
return(
  list(
    v = as.matrix( t( imagev ) ),
    fullv = as.matrix( t( v ) )
    )
  )
}


#' Reconstruct a n by k vector given n by p matrix of predictors.
#'
#' @param x input matrix on which prediction is based
#' @param y target matrix
#' @param iterations number of gradient descent iterations
#' @param sparsenessQuantile quantile to control sparseness - higher is sparser.
#' @param positivity restrict to positive or negative solution (beta) weights.
#' choices are positive, negative or either as expressed as a string.
#' @param smoothingMatrixX allows parameter smoothing, should be square and same
#' size as input matrix
#' @param smoothingMatrixY allows parameter smoothing, should be square and same
#' size as input matrix
#' @param nv number of predictor spatial vectors
#' @param extraPredictors additional column predictors
#' @param verbose boolean option
#' @return vector of size p is output
#' @author Avants BB
#' @examples
#'
#' \dontrun{
#' mask = getMask( antsImageRead( getANTsRData( 'r16' ) ) )
#' spatmat = t( imageDomainToSpatialMatrix( mask, mask ) )
#' smoomat = knnSmoothingMatrix( spatmat, k = 200, sigma = 1.0 )
#' mat <- matrix(rnorm(sum(mask)*50),ncol=sum(mask),nrow=50)
#' mat[ 1:25,100:10000]=mat[ 1:25,100:10000]+1
#' age = matrix( rnorm( nrow(mat)*2 ), ncol=2 )
#' for ( i in c( 5000:6000, 10000:11000, 16000:17000 )  ){
#'   mat[ , i ] = age[,1]*0.1 + mat[,i]
#'   }
#' sel = 1:25
#' fit = smoothMultiRegression( x=mat[sel,], y=age[sel,], iterations = 10,
#'   sparsenessQuantile = 0.5,
#'   smoothingMatrixX = smoomat, smoothingMatrixY = diag(ncol(y)), verbose=T )
#' tt = mat %*% fit$v
#' print( cor.test( age[-sel,1], tt[-sel,1] ) )
#' vimg = makeImage( mask, (fit$v[,1] ) ); print(range(vimg)*10)
#' plot( mask, vimg, window.overlay=range(abs(vimg)))
#' }
#' @export smoothMultiRegression
## #' @param gamma step size for gradient descent
smoothMultiRegression <- function(
  x,
  y,
  iterations = 10,
  sparsenessQuantile = 0.5,
  positivity = FALSE,
  smoothingMatrixX = NA,  smoothingMatrixY = NA,
  nv = 2,
  extraPredictors,
  verbose = FALSE
  )
{
x = scale( x, scale = FALSE )
y = scale( y, scale = FALSE )
poschoices = c("positive","negative","either", TRUE, FALSE )
if ( sum( positivity == poschoices ) != 1 | length( positivity ) != 1 )
  stop( 'choice of positivity parameter is not good - see documentation')
if ( missing( "x" ) | missing( "y" ) ) {
  message("this function needs input")
  return( NA )
  }
  svdy = scale( rsvd::rsvd( y, nu=nv )$u )
  loy = as.numeric( svdy[,1] )
  ox = smoothRegression(  x, loy, iterations = iterations,
     sparsenessQuantile = sparsenessQuantile, positivity = positivity,
     smoothingMatrix = smoothingMatrixX, nv = 2, verbose = FALSE )

  for ( k in 1:iterations ) {
   print(paste("k",k))
   lox = scale( x %*% ( ox$v ) )
   oy = smoothRegression(  y, lox[,1], iterations = 10,
     sparsenessQuantile = sparsenessQuantile, positivity = positivity,
     smoothingMatrix = NA, nv = 2, verbose = verbose )
   loy = scale( y %*% ( oy$v ) )
   ox = smoothRegression(  x, loy[,1], iterations = 10,
     sparsenessQuantile = sparsenessQuantile, positivity = positivity,
     smoothingMatrix = NA, nv = 2, verbose = verbose )
   }
}






#' k-nearest neighbors constrained image smoothing
#'
#' Compute a smoothing matrix based on an input matrix of point coordinates as
#' well as neighborhood intensity patterns.  this performs a form of edge
#' preserving smoothing.
#'
#' @param img input image to smooth
#' @param mask input image mask
#' @param radius number of neighbors, higher causes more smoothing
#' @param intensityWeight weight for intensity component, value 1 will weight
#' local voxel intensity roughly equally to spatial component
#' @param spatialSigma for gaussian defining spatial distances
#' @param iterations number of iterations over which to apply smoothing kernel
#' @param returnMatrix boolean, will return smoothing matrix instead of image.
#' @return antsImage is output
#' @author Avants BB
#' @examples
#'
#' \dontrun{
#' img = antsImageRead( getANTsRData( 'r16' ) )
#' mask = getMask( img )
#' simg = knnSmoothImage( img=img, mask=mask, radius=2, intensityWeight=1,
#'   spatialSigma=1.5, iterations=1 )
#' }
#' @export knnSmoothImage
knnSmoothImage <- function(
  img,
  mask,
  radius,
  intensityWeight = 0.1,
  spatialSigma = 20.0,
  iterations = 1,
  returnMatrix = FALSE )
{
  if ( radius <= 0 ) return( img )
  ivec = img[ mask == 1 ]
  spatmat = t( imageDomainToSpatialMatrix( mask, mask ) )
  spatrange = range( spatmat, na.rm = TRUE )
  intrange = range( ivec, na.rm = TRUE )
  idelt = ( intrange[2] - intrange[1] )
  if ( idelt <= 0 ) {
    scl = 1
  } else {
    scl = ( spatrange[2] - spatrange[1] ) / idelt * intensityWeight
  }
  ivec2 = ivec * scl
  spatmat = rbind( spatmat, ivec2 )
  r = radius
#  imat = antsrimpute(
#    getNeighborhoodInMask( img, mask, rep( r, img@dimension), boundary.condition='image' ) )
#  imat = knnSmoothingMatrix( imat, k = 2*(r*2+1)^2, sigma = intensitySigma )
  jmat = knnSmoothingMatrix( spatmat, k = (r*2+1)^2, sigma = spatialSigma )
#  return( jmat )
#  image( jmat[4000:4500,4000:4500] )
#  print( jmat[4000:4010,4000:4010] )
#  imat = imat / Matrix::rowSums( imat )
#  jmat = imat * smoothingMatrix
  for ( i in 1:4 ) { # sinkhorn
    jmat = jmat / Matrix::rowSums( jmat )
    jmat = Matrix::t( Matrix::t(jmat) / Matrix::rowSums( Matrix::t(jmat) ) )
    }
  if ( returnMatrix ) return( jmat )
  for ( i in 1:iterations ) {
    ivec = jmat %*% ivec
  }
return(  makeImage( mask, as.numeric( ivec ) ) )
}






.xuvtHelper <- function( x, u, v, errs, iterations,
  smoothingMatrix, repeatedMeasures, intercept,
  positivity, gamma, sparsenessQuantile, usubs,
  doOrth, verbose ) {
  i = 1
  tu = t( u )
  tuu = t( u ) %*% u
  if ( is.na( gamma ) ) gamma = 1.e-6
  while ( i <= iterations ) {
    v = as.matrix( smoothingMatrix %*% v )
    dedv = t( tuu %*% t( v ) - tu %*% x )
    v = v + dedv * gamma
#    if ( abs( doOrth ) >  Inf ) {
#      vOrth = A.qr <- qr( v )
#      vOrth = qr.Q( vOrth )
#      v = v * ( 1 - doOrth ) - vOrth * doOrth
#    }
    for ( vv in 1:ncol( v ) ) {
      v[ , vv ] = v[ , vv ] / as.numeric( sqrt(  v[ , vv ] %*% v[ , vv ] ) )
      if ( vv > 1  &  doOrth )
        for ( vk in 1:(vv-1) ) {
          temp = v[,vk]
          denom = as.numeric( temp  %*%  temp )
          if ( denom > 0 ) ip = as.numeric( temp %*%  v[,vv] ) / denom else ip = 1
          v[ , vv ] = v[, vv ] - temp * ip
          }
      localv = v[ , vv ]
      myquant = quantile( localv , sparsenessQuantile, na.rm=T )
      if ( positivity == 'positive') {
        localv[ localv <= myquant ] = 0
      } else if ( positivity == 'negative' ) {
        localv[ localv > myquant ] = 0
      } else if ( positivity == 'either' ) {
        localv[ abs(localv) < quantile( abs(localv) , sparsenessQuantile, na.rm=T  ) ] = 0
      }
#        myquant = quantile( localv , sparsenessQuantile, na.rm=T )
#        if ( myquant < 0)
#          localv[ localv > myquant ] = 0 else localv[ localv <= myquant ] = 0
      v[ , vv ] = localv
      v[ , vv ] = v[ , vv ] / sqrt( sum( v[ , vv ] * v[ , vv ] ) )
    }
    intercept = rowMeans( x - ( u %*% t(v) ) )
    if ( ! any( is.na( repeatedMeasures ) ) ) { # estimate random intercepts
      for ( s in usubs ) {
        usel = repeatedMeasures == s
        intercept[ usel ] = mean( intercept[ usel ], na.rm=T  )
        }
      }
    err = mean( abs( x - ( u %*% t(v) + intercept  ) ) )
    errs[ i ] = err
    if ( i > 1 ) {
      if ( ( errs[ i ] > errs[ i - 1 ] ) &  ( i == 3 ) )
        {
#        message(paste("flipping sign of gradient step:", gamma))
        gamma = gamma * ( -1.0 )
        }
      else if ( ( errs[ i ] > errs[ i - 1 ] ) )
        {
        gamma = gamma * ( 0.5 )
#        message(paste("reducing gradient step:", gamma))
        }
      else if ( abs(gamma) < 1.e-9 ) i = iterations
      }
    i = i + 1
    if ( verbose ) print( paste( i,  err ) )
  }
  return( list( v = v, intercept = intercept, gamma=gamma*1.1 ) )
}


#' joint smooth matrix prediction
#'
#' Joints reconstruct a n by p, q, etc matrices or predictors.
#' The reconstruction can be regularized.
#' # norm( x - u_x v_x^t ) + norm( y - u_y v_y^t) + .... etc
#'
#' @param x input list of matrices to be jointly predicted.
#' @param parameters should be a ncomparisons by 3 matrix where the first two
#' columns define the pair to be matched and the last column defines the weight
#' in the objective function.
#' @param nvecs number of basis vectors to compute
#' @param iterations number of gradient descent iterations
#' @param subIterations number of gradient descent iterations in sub-algorithm
#' @param gamma step size for gradient descent
#' @param sparsenessQuantile quantile to control sparseness - higher is sparser
#' @param positivity restrict to positive or negative solution (beta) weights.
#' choices are positive, negative or either as expressed as a string.
#' @param smoothingMatrix a list containing smoothing matrices of the same
#' length as x.
#' @param rowWeights vectors of weights with size n (assumes diagonal covariance)
#' @param repeatedMeasures list of repeated measurement identifiers. this will
#' allow estimates of per identifier intercept.
#' @param doOrth boolean enforce gram-schmidt orthonormality
#' @param verbose boolean option
#' @return matrix list each of size p by k is output
#' @author Avants BB
#' @examples
#'
#' \dontrun{
#' mat<-replicate(100, rnorm(20))
#' mat2<-replicate(100, rnorm(20))
#' mat<-scale(mat)
#' mat2<-scale(mat2)
#' wt<-0.666
#' mat3<-mat*wt+mat2*(1-wt)
#' params = matrix( nrow = 2, ncol = 3 )
#' params[1,] = c(1,2,1)
#' params[2,] = c(2,1,1)
#' x = list( (mat), (mat3 ))
#' jj = jointSmoothMatrixReconstruction( x, 2, params,
#'  gamma = 1e-4, sparsenessQuantile=0.5, iterations=10,
#'  smoothingMatrix = list(NA,NA), verbose=TRUE )
#'
#' # latent feature
#' mask=getMask( antsImageRead(  getANTsRData( "r16" ) ))
#' spatmat = t( imageDomainToSpatialMatrix( mask, mask ) )
#' smoomat = knnSmoothingMatrix( spatmat, k = 27, sigma = 120.0 )
#' lfeats = t( replicate(100, rnorm(3)) )
#' # map these - via matrix - to observed features
#' n = sum( mask )
#' ofeats1 = ( lfeats + rnorm( length(lfeats), 0.0, 1.0 ) ) %*% rbind( rnorm(n), rnorm(n),rnorm(n) )
#' ofeats2 = ( lfeats + rnorm( length(lfeats), 0.0, 1.0 ) ) %*% rbind( rnorm(n),rnorm(n),rnorm(n) )
#' # only half of the matrix contains relevant data
#' ofeats1[,1:round(n/2)] = matrix( rnorm(round(n/2)*100), nrow=100 )
#' ofeats2[,1:round(n/2)] = matrix( rnorm(round(n/2)*100), nrow=100 )
#' x = list( (ofeats1), ( ofeats2 ))
#' jj = jointSmoothMatrixReconstruction( x, 2, params,
#'   gamma = 0.0001, sparsenessQuantile=0.75, iterations=19,
#'   subIterations=11,
#'   smoothingMatrix = list(smoomat,smoomat), verbose=TRUE )
#' p1 = ofeats2 %*% jj$v[[2]]
#' p2 = ofeats1  %*% jj$v[[1]]
#' cor( p1, lfeats )
#' cor( p2, lfeats )
#' print( cor( rowMeans(ofeats1), lfeats ) )
#' print( cor( rowMeans(ofeats2), lfeats ) )
#'
#' # a 2nd example with 3 modalities
#' imageIDs <- c( "r16", "r27", "r30", "r62", "r64", "r85" )
#' images <- list()
#' feature1Images <- list()
#' feature2Images <- list()
#' feature3Images <- list()
#' ref = antsImageRead( getANTsRData('r16') )
#' for( i in 1:length( imageIDs ) )
#'   {
#'   cat( "Processing image", imageIDs[i], "\n" )
#'   tar = antsRegistration( ref, antsImageRead( getANTsRData( imageIDs[i] ) ),
#'     typeofTransform='Affine' )$warpedmov
#'   images[[i]] <- tar
#'   feature1Images[[i]] <- iMath( images[[i]], "Grad", 1.0 )
#'   feature2Images[[i]] <- iMath( images[[i]], "Laplacian", 1.0 )
#'   feature3Images[[i]] <- reflectImage(tar,axis=0,tx='Affine')$warpedmovout
#'   }
#' i=1
#' mask=getMask( antsImageRead(  getANTsRData( imageIDs[i] ) ))
#' mask2 = iMath( mask, "ME", 2 )
#' spatmat = t( imageDomainToSpatialMatrix( mask, mask ) )
#' smoomat = knnSmoothingMatrix( spatmat, k = 125, sigma = 100.0 )
#' spatmat2 = t( imageDomainToSpatialMatrix( mask2, mask2 ) )
#' smoomat2 = knnSmoothingMatrix( spatmat2, k = 125, sigma = 100.0 )
#' params = matrix( nrow = 6, ncol = 3 )
#' params[1,] = c(1,2,1)
#' params[2,] = c(2,1,1)
#' params[3,] = c(1,3,1)
#' params[4,] = c(3,1,1)
#' params[5,] = c(2,3,1)
#' params[6,] = c(3,2,1)
#' mat = imageListToMatrix( feature1Images, mask )
#' mat2 = imageListToMatrix( feature2Images, mask2 )
#' mat3 = imageListToMatrix( feature3Images, mask )
#' scl=F
#' x = list( scale(mat, scale=scl), scale(mat2, scale=scl ), scale(mat3, scale=scl ))
#' slist = list(smoomat2,smoomat,smoomat,smoomat,smoomat,smoomat2)
#'
#' jj = jointSmoothMatrixReconstruction( x, 4, params, positivity=T,
#'  gamma = 1e-6, sparsenessQuantile=0.9, iterations=10,
#'  smoothingMatrix = slist, verbose=TRUE )
#' mm=makeImage( mask, abs(jj$v[[2]][,1]) ) %>% iMath("Normalize")
#' plot( ref, mm, doCropping=F, window.overlay=c(0.1,1) )
#'
#' p1 = mat2 %*% jj$v[[1]]
#' p2 = mat  %*% jj$v[[2]]
#' diag( cor( p1, p2 ) )
#'
#' p1 = mat3 %*% jj$v[[5]]
#' p2 = mat2  %*% jj$v[[6]]
#' diag( cor( p1, p2 ) )
#'
#' }
#' @export jointSmoothMatrixReconstruction
jointSmoothMatrixReconstruction <- function(
  x,
  nvecs,
  parameters,
  iterations = 10,
  subIterations = 5,
  gamma = 1.e-6,
  sparsenessQuantile = 0.5,
  positivity = FALSE,
  smoothingMatrix = NA,
  rowWeights = NA,
  repeatedMeasures = NA,
  doOrth = FALSE,
  verbose = FALSE
  )
{
  poschoices = c("positive","negative","either", TRUE, FALSE )
  if ( sum( positivity == poschoices ) != 1 | length( positivity ) != 1 )
    stop( 'choice of positivity parameter is not good - see documentation')
  if ( positivity == TRUE ) positivity = "positive"
  if ( positivity == FALSE ) positivity = "either"

  for ( k in 1:length(x) ) x[[ k ]] = x[[ k ]] / max( abs(x[[k]]) )
  gammas = rep( gamma, nrow( parameters ) )
  ulist = list()
  vlist = list()
  ilist = list()
  if ( ! any( is.na( repeatedMeasures ) ) ) {
    usubs = unique( repeatedMeasures )
    wtdf = data.frame( table( repeatedMeasures ) )
    # rowWeights should scale with counts
    repWeights = rep( 0, length( repeatedMeasures ) )
    for ( u in wtdf$usubs ) {
      repWeights[ repeatedMeasures == u ] = 1.0 / wtdf$Freq[ wtdf$repeatedMeasures == u ]
    }
    rm( wtdf )
    if ( all( is.na( rowWeights ) ) ) {
      rowWeights = repWeights
      } else rowWeights = rowWeights * repWeights
    }

  for ( i in 1:nrow( parameters ) ) {

    m1 = parameters[ i, 1 ]
    m2 = parameters[ i, 2 ]
    modelFormula = as.formula( " x[[ m2 ]]  ~ ." )
#    basisDf = data.frame( u=irlba::irlba( x[[ m1 ]], nu = nvecs, nv = 0 )$u )
#    basisDf = data.frame( u=rsvd::rsvd( x[[ m1 ]], nu = nvecs, nv = 0 )$u )
    basisDf = data.frame( u=RSpectra::svds( x[[ m1 ]], nvecs )$u )
    mdl = lm( modelFormula, data = basisDf )
    u = model.matrix( mdl )
    ilist[[ i ]] = u[,1] # intercept
    u = u[,-1]
    v = mdl$coefficients[-1, ]
    v = v + matrix( rnorm( length( v ), 0, 0.01 ), nrow = nrow( v ), ncol = ncol( v ) )
    v = t( v )
    for ( vv in 1:ncol(v) )
      v[ , vv ] = v[ , vv ] / sqrt( sum( v[ , vv ] * v[ , vv ] ) )
    ulist[[ i ]] = u
    vlist[[ i ]] = v

    }
errs = rep( NA, length( iterations ) )
if ( verbose ) print("part II")
perr = matrix( nrow = iterations, ncol = nrow( parameters ) )
k = 1
while ( k <= iterations ) {
  for ( i in 1:nrow( parameters ) ) {
    m1 = parameters[ i, 1 ]
    m2 = parameters[ i, 2 ]
    whichv = NA
    for ( pp in 1:nrow( parameters ) ) {
      if ( parameters[pp,2] == m1 & parameters[pp,1] == m2  )
        whichv = pp
      }
    if ( ! is.na( whichv ) ) {
      temp = vlist[[ whichv ]]
      ulist[[ i ]] =  ( x[[ m1 ]] %*% ( temp  ) )
    } else {
      ulist[[ i ]] = ulist[[ m2 ]]
      vlist[[ i ]] = t( t( ulist[[ m2 ]] ) %*% x[[m2]] )
    }
    if ( class( smoothingMatrix[[i]] ) == 'logical' )
      loSmoo = diag( ncol( x[[ m2 ]] ) ) else loSmoo = smoothingMatrix[[ i ]]
    temp = .xuvtHelper( x[[ m2 ]],
      ulist[[i]], vlist[[i]],
      errs, iterations=subIterations,
      smoothingMatrix=loSmoo,
      repeatedMeasures=repeatedMeasures, ilist[[ i ]],
      positivity=positivity, gammas[i] * parameters[i,3],
      sparsenessQuantile=sparsenessQuantile, usubs=usubs,
      doOrth=doOrth, verbose=F )
#    gammas[i] = temp$gamma * 1.0 # 5
    vlist[[ i ]] = temp$v
    ilist[[ i ]] = temp$intercept
    perr[ k, i ] = mean( abs( x[[ m2 ]] - ( ulist[[i]] %*% t( vlist[[i]] ) + ilist[[ i ]]  ) ) )
    }
    if ( verbose ) {
      print( perr[ k,  ] )
      print( paste( "overall", mean(perr[k,]) ) )
      }
    if ( k > 1 ) {
      e1 =  perr[k,]  * parameters[,3]
      e2 =  perr[k-1,]  * parameters[,3]
      if ( mean( e1 ) > mean( e2 ) ) gammas = gammas * 0.9 # k = iterations
    }

    for ( i in 1:nrow( parameters ) ) {
      m1 = parameters[ i, 1 ]
      m2 = parameters[ i, 2 ]
      whichv = NA
      for ( pp in 1:nrow( parameters ) )
        {
        if ( parameters[pp,2] == m1 & parameters[pp,1] == m2  )
          whichv = pp
        }
      if ( ! is.na( whichv ) ) {
        temp = ( vlist[[ whichv ]] )
        ulist[[ i ]] =  ( x[[ m1 ]] %*% ( temp  ) )
        } else {
          ulist[[ i ]] = ulist[[ m2 ]]
          vlist[[ i ]] = t( t( ulist[[ m2 ]] ) %*% x[[m2]] )
        }
      }

    k = k + 1
  }


  return( list( u=ulist, v=vlist, intercepts=ilist ) )
}



#' sparsify a matrix
#'
#' This implements a quantile based sparsification operation
#'
#' @param v input matrix
#' @param sparsenessQuantile quantile to control sparseness - higher is sparser
#' @param positivity restrict to positive or negative solution (beta) weights.
#' choices are positive, negative or either as expressed as a string.
#' @return matrix
#' @author Avants BB
#' @examples
#'
#' mat<-replicate(100, rnorm(20))
#' mat = orthogonalizeAndQSparsify( mat )
#'
#' @export orthogonalizeAndQSparsify
orthogonalizeAndQSparsify <- function( v,
  sparsenessQuantile = 0.5, positivity='either' ) {
  for ( vv in 1:ncol( v ) ) {
#    v[ , vv ] = v[ , vv ] / sqrt( sum( v[ , vv ] * v[ , vv ] ) )
    if ( vv > 1 )
      for ( vk in 1:(vv-1) ) {
        temp = v[,vk]
        denom = sum( temp * temp , na.rm=T )
        if ( denom > 0 ) ip = sum( temp * v[,vv] ) / denom else ip = 1
        v[ , vv ] = v[, vv ] - temp * ip
        }
    localv = v[ , vv ]
    myquant = quantile( localv , sparsenessQuantile, na.rm=T )
    if ( positivity == 'positive') {
      localv[ localv <= myquant ] = 0
    } else if ( positivity == 'negative' ) {
      localv[ localv > myquant ] = 0
    } else if ( positivity == 'either' ) {
      localv[ abs(localv) < quantile( abs(localv) , sparsenessQuantile, na.rm=T  ) ] = 0
    }
    v[ , vv ] = localv
  }
  return( v )
}


#' cca via sparse smooth matrix prediction
#'
#' This implements a sparse and graph-regularized version of CCA based on the
#' AppGrad style of implementation by Ma, Lu and Foster, 2015.
#'
#' @param x input view 1 matrix
#' @param y input view 2 matrix
#' @param smoox smoothingMatrix for x
#' @param smooy smoothingMatrix for y
#' @param sparsenessQuantile quantile to control sparseness - higher is sparser
#' @param positivity restrict to positive or negative solution (beta) weights.
#' choices are positive, negative or either as expressed as a string.
#' @param k number of basis vectors to compute
#' @param iterations number of gradient descent iterations
#' @param verbose boolean option
#' @return list with matrices each of size p or q by k
#' @author Avants BB
#' @examples
#'
#' mat<-replicate(100, rnorm(20))
#' mat2<-replicate(100, rnorm(20))
#' mat<-scale(mat)
#' mat2<-scale(mat2)
#' wt<-0.666
#' mat3<-mat*wt+mat2*(1-wt)
#' jj = smoothAppGradCCA( mat, mat3 )
#'
#' @export smoothAppGradCCA
smoothAppGradCCA <- function( x , y,
  smoox=NA, smooy=NA,
  sparsenessQuantile=0.5,
  positivity = 'either',
  k=2, iterations=100, verbose=FALSE ) {
  if ( nrow(x) != nrow(y)) stop("nrow x should equal nrow y")
  x = scale( icawhiten(x,nrow(x)), scale=T )
  y = scale( icawhiten(y,nrow(y)), scale=T )
  errs = rep( NA, iterations )
  poschoices = c("positive","negative","either", TRUE, FALSE )
  if ( sum( positivity == poschoices ) != 1 | length( positivity ) != 1 )
    stop( 'choice of positivity parameter is not good - see documentation')
  if ( positivity == TRUE ) positivity = "positive"
  if ( positivity == FALSE ) positivity = "either"
  ratio = norm( x ) / norm( y )
  x = x / norm( x )
  y = y / norm( y )
  if ( any(is.na(smoox))) smoox = diag( ncol( x ) )
  if ( any(is.na(smooy))) smooy = diag( ncol( y ) )
#  phix = RSpectra::svds( x, k=k )$v
#  phiy = RSpectra::svds( y, k=k )$v
#  phix = t( y ) %*% irlba::irlba( x, nu=k, nv=0, maxit=1000, tol=1.e-6 )$u
#  phiy = t( x ) %*% irlba::irlba( y, nu=k, nv=0, maxit=1000, tol=1.e-6 )$u
#  phix = t( y ) %*% svd( x, nu=k, nv=0  )$u
#  phiy = t( x ) %*% svd( y, nu=k, nv=0  )$u
  phix = svd( x, nv=k, nu=0  )$v
  phiy = svd( y, nv=k, nu=0  )$v
#  phix = matrix( rnorm( k * ncol(x), 0, 1e-4 ), ncol=k )
#  phiy = matrix( rnorm( k * ncol(y), 0, 1e-4 ), ncol=k )
  i = 1
  while ( i < iterations ) {
  if ( i < 3 ) gx =  1.e-6 # quantile( phix[ abs(phix) > 0 ] , 0.5 , na.rm=TRUE ) * ( -1.e4 )
  if ( i < 3 ) gy =  1.e-6 # quantile( phiy[ abs(phiy) > 0 ] , 0.5 , na.rm=TRUE ) * ( -1.e4 )
# gradient calculation
    delta = t(x) %*% ( x %*% phix - y %*% phiy )
    phix1 = phix - delta * gx
    delta = t(y) %*% ( y %*% phiy - x %*% phix )
    phiy1 = phiy - delta * gy
    phix1 = as.matrix( smoox %*% orthogonalizeAndQSparsify( phix1, sparsenessQuantile=sparsenessQuantile, positivity=positivity ) )
    phiy1 = as.matrix( smooy %*% orthogonalizeAndQSparsify( phiy1, sparsenessQuantile=sparsenessQuantile, positivity=positivity ) )
# now update
    phix =  phix1 / norm( x %*% phix1 )
    phiy =  phiy1 / norm( y %*% phiy1 )
    errs[ i ] = norm( y %*% phiy - x %*% phix )
    if ( verbose ) print( paste( i, errs[ i ], sum( abs(  diag(  cor( y %*% phiy,  x %*% phix  ) ) ) )  ) )
#    if ( i > 110 ) if ( errs[ i ]  > errs[i-1] ) i = iterations+1
    i = i + 1
    }
#  print( cor( x %*% phix ) )
#  print( cor( y %*% phiy ) )
#  print( cor( y %*% phiy,  x %*% phix  ) )
  return( list( phix = phix, phiy = phiy ) )
  }
