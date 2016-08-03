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
#' chosen appropriately.
#'
#' @param x input matrix, should be n (samples) by p (measurements)
#' @param r radii to explore
#' @param locn number of local samples to take at each scale
#' @param nev maximum number of eigenvalues to compute
#' @param plot boolean to control whether we plot results
#' @return dataframe containing the estimated eigenvalues across scale
#' @author Avants BB
#' @references
#' \url{http://www.math.jhu.edu/~mauro/multiscaledatageometry.html}
#' @examples
#' \dontrun{
#' sphereDim = 9
#' embeddDim = 100
#' n = 1000
#' sphereData = pracma::rands( n, sphereDim, 1. )
#' mysig = 0.1
#' spherEmbed = matrix( rnorm( n * embeddDim, 0, mysig ), nrow = n, ncol = embeddDim )
#' spherEmbed[ , 1:ncol( sphereData ) ] = spherEmbed[ , 1:ncol( sphereData ) ] + sphereData
#' mymssvd = multiscaleSVD( spherEmbed, myxaxis, locn=40, nev=20, plot=TRUE )
#' }
#' @export multiscaleSVD
multiscaleSVD <- function( x, r, locn, nev, plot=FALSE )
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
    sel = calcRowMatDist( x, x[ locsam[i], ] ) < myr
    if ( sum( sel ) >  1 ) {
      lcov = cov( spherEmbed[sel,] )
      temp = svd( lcov )$d[ 1:nev ] # * embeddDim / sum(sel)
      } else temp = rep( 0, nev )
    myevs[ i, 1:nev ] = temp
    if ( i == locn ) {
      mresponse[ myscl, ] = colMeans( myevs, na.rm=T )
      }
    }
  }
colnames( mresponse ) = paste("EV",1:nev,sep='')
rownames( mresponse ) = paste("Scale",1:length(myxaxis),sep='')
if ( plot )
  {
  mycols = rainbow( nev )
  growthRate1 = magic::shift(mresponse[,1],0)-magic::shift(mresponse[,1],1)*0
  plot( myxaxis, growthRate1, type='l', col = mycols[1], main='Evals by scale',
        ylim=c(0.00, max( mresponse[,1]) ), xlab='ball-radius', ylab='Expected Eval' )
  for ( i in 2:ncol(mresponse) )
    {
    growthRatek = magic::shift(mresponse[,i],0)-magic::shift(mresponse[,i],1)*0
    points( myxaxis, growthRatek, type='l',col=mycols[i])
    }
  }
return( mresponse )
}
