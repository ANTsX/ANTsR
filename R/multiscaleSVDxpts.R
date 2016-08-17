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
#' @param kmetric similarity or distance metric determining k nearest neighbors
#' @param eps epsilon error for rapid knn
#' @return matrix sparse p by p matrix is output with p by k nonzero entries
#' @author Avants BB
#' @references
#' \url{http://www.math.jhu.edu/~mauro/multiscaledatageometry.html}
#' @examples
#' \dontrun{
#' mat = matrix( rnorm(60), ncol=10 )
#' smat = sparseDistanceMatrix( mat, 2 )
#' }
#' @export sparseDistanceMatrix
sparseDistanceMatrix <- function( x, k = 3, r = Inf,
  kmetric = c("euclidean", "correlation", "covariance"  ),
  eps = 1.e-6 )
{
  # note that we can convert from distance to covariance
  #   d_ij^2 = sigma_i^2 +  \sigma_j^2  - 2 * cov_ij
  # and from correlation to covariance   diag(sd) %*% corrMat %*% diag(sd)
  # and from euclidean distance of standardized data to correlation
  # 1.0 - dist^2 / ( 2 * nrow( x ) )
  # TODO / FIXME - implement covariance
  if ( ! usePkg("Matrix") )
    stop("Please install the Matrix package")
  if ( ! usePkg("nabor") )
    stop("Please install the nabor package")
  kmetric <- match.arg( kmetric )
  cometric = ( kmetric == "correlation" | kmetric == "covariance" )
  if ( cometric & r == Inf ) r = -Inf
#  if ( ! usePkg("irlba") )
#    stop("Please install the irlba package")
# see http://www.analytictech.com/mb876/handouts/distance_and_correlation.htm
# euclidean distance to correlation - xin contains correlations
  ecor <- function( xin ) { 1.0 - xin^2 / ( 2 * nrow( x ) ) }
  if ( kmetric == "covariance" ) mycov = apply( x, FUN=sd, MARGIN=2 )
  if ( cometric ) x = scale( x )
  bknn = nabor::knn( t( x ) , k=k, eps=eps )
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
#' @param kmetric similarity or distance metric determining k nearest neighbors
#' @param eps epsilon error for rapid knn
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
sparseDistanceMatrixXY <- function( x, y, k = 3, r = Inf,
  kmetric = c("euclidean", "correlation", "covariance"  ),
  eps = 1.e-6 )
{
  if ( ! usePkg("Matrix") )
    stop("Please install the Matrix package")
  if ( ! usePkg("nabor") )
    stop("Please install the nabor package")
  kmetric <- match.arg( kmetric )
  cometric = ( kmetric == "correlation" | kmetric == "covariance" )
  if ( cometric & r == Inf ) r = -Inf
  ecor <- function( xin ) { 1.0 - xin^2 / ( 2 * nrow( x ) ) }
  if ( cometric ) {
    x = scale( x )
    y = scale( y )
    }
  bknn = nabor::knn( t( y ) , t( x ), k=k, eps=eps )
  if ( cometric ) bknn$nn.dists = ecor( bknn$nn.dists )
  tct = 0
  for ( i in 1:ncol( x ) )
    {
    inds = bknn$nn.idx[i,]    # index
    locd = bknn$nn.dists[i,]  # dist
    tct = tct + sum( !is.na(inds) )
    }
  # build triplet representation for sparse matrix
  myijmat = matrix( nrow=(tct), ncol=3 )
  tct2 = 1
  for ( i in 1:ncol( x ) )
    {
    inds = bknn$nn.idx[i,]
    locd = bknn$nn.dists[i,]
    tctinc = sum( !is.na(inds) )
    if ( kmetric == "covariance" )
      {
      locd = cov( x[,i], y[,inds] )
      }
    else if ( kmetric == "covariance" )
      {
      locd = cor( x[,i], y[,inds] )
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
    dims = c( ncol( x ), ncol( y ) ), symmetric = FALSE
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
qdelt = quantile( delt[2:length(delt)], 0.9 )
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
