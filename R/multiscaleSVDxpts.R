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
#' @param sinkhorn boolean
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
#' @importFrom ANTsRCore antsrimpute
#' @export sparseDistanceMatrix
sparseDistanceMatrix <- function( x, k = 3, r = Inf, sigma = NA,
                                  kmetric = c("euclidean", "correlation", "covariance", "gaussian"  ),
                                  eps = 1.e-6, ncores=NA, sinkhorn = TRUE ) # , mypkg = "nabor"  )
{
  myn = nrow( x )
  if ( k >= ncol( x ) ) k = ncol( x ) - 1
  if ( any( is.na( x ) ) ) stop("input matrix has NA values")
  mypkg = 'FNN'  # 'rflann'
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
  ecor <- function( xin, nn ) { 1.0 - xin / ( 2 * nn ) }
  if ( kmetric == "covariance" ) mycov = apply( x, FUN=sd, MARGIN=2 )
  if ( cometric ) {
    x = scale( x, center = TRUE, scale = (kmetric == "correlation" ) )
  }
  if ( mypkg[1] == "FNN" ) {
    bknn = FNN::get.knn( t( x ), k=k, algorithm = "cover_tree"  )
    names( bknn ) = c( "nn.idx", "nn.dists" )
  }
  if ( mypkg[1] == "nabor" ) bknn = nabor::knn( t( x ) , k=k, eps=eps )
  if ( mypkg[1] == "RANN" )  bknn = RANN::nn2( t( x ) , k=k, eps=eps  )


  # if ( mypkg[1] == "rflann" )  {
  #   myncores = as.numeric( system('getconf _NPROCESSORS_ONLN', intern = TRUE) )
  #   if ( !is.na( ncores  ) ) myncores = ncores
  #   bknn = rflann::Neighbour( t(x), t(x), k=k, "kdtree", cores=myncores, 1 )
  #   names( bknn ) = c( "nn.idx", "nn.dists" )
  # }

  #  if ( mypkg[1] == "naborpar" ) bknn = .naborpar( t( x ), t( x ) , k=k, eps=eps  )
  if ( cometric ) {
    bknn$nn.dists = ecor( bknn$nn.dists, myn )
  }
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
      locd = exp( -1.0 * locd / ( 2.0 * sigma^2 ) )
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
  if ( sinkhorn )
    for ( i in 1:4 ) {
      #      kmatSparse = kmatSparse / Matrix::rowSums( kmatSparse )
      #      kmatSparse = Matrix::t( Matrix::t(kmatSparse) / Matrix::rowSums( Matrix::t(kmatSparse) ) )
      kmatSparse = kmatSparse / Matrix::colSums( kmatSparse )
      kmatSparse = kmatSparse / Matrix::rowSums( kmatSparse )
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
  mypkg = 'FNN'
  if ( ! usePkg("Matrix") )
    stop("Please install the Matrix package")
  if ( ! usePkg( mypkg ) )
    stop( paste("Please install the",mypkg,"package") )
  kmetric <- match.arg( kmetric )
  if ( kmetric == "gaussian" & is.na( sigma ) )
    stop("Please set the sigma parameter")
  cometric = ( kmetric == "correlation" | kmetric == "covariance" )
  ecor <- function( xin ) { 1.0 - xin / ( 2 * nrow( x ) ) }
  if ( cometric ) {
    x = scale( x, center=TRUE, scale = (kmetric == "correlation" )  )
    y = scale( y, center=TRUE, scale = (kmetric == "correlation" )  )
  }
  if ( mypkg[1] == "FNN" ) {
    bknn = FNN::get.knnx( t( x ), t( y ), k=k, algorithm = "cover_tree" )
    names( bknn ) = c( "nn.idx", "nn.dists" )
  }
  if ( mypkg[1] == "nabor" ) bknn = nabor::knn( t( y ), t( x ) , k=k, eps=eps )
  if ( mypkg[1] == "RANN" )  bknn = RANN::nn2( t( y ), t( x ) , k=k, eps=eps )

  # if ( mypkg[1] == "rflann" )  {
  #   myncores = as.numeric( system('getconf _NPROCESSORS_ONLN', intern = TRUE) )
  #   if ( !is.na( ncores  ) ) myncores = ncores
  #   bknn = rflann::Neighbour( t(y), t(x), k=k, "kdtree", cores=myncores, 1 )
  #   names( bknn ) = c( "nn.idx", "nn.dists" )
  # }

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
      locd = exp( -1.0 * locd / ( 2.0 * sigma^2 ) )
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
#' if ( usePkg( "pracma"  ) ) {
#' sphereData = pracma::rands( n, sphereDim, 1. )
#' mysig = 0.1
#' spherEmbed = matrix( rnorm( n * embeddDim, 0, mysig ), nrow = n, ncol = embeddDim )
#' spherEmbed[ , 1:ncol( sphereData ) ] = spherEmbed[ , 1:ncol( sphereData ) ] + sphereData
#' myr = seq( 1.0, 2.2, 0.05 ) # scales at which to sample
#' mymssvd = multiscaleSVD( spherEmbed, myr, locn=5, nev=20, plot=1 )
#' }
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
      if ( sum( sel , na.rm = TRUE ) >  2 ) {
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
        mresponse[ myscl, ] = colMeans( myevs, na.rm=TRUE )
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
  qdelt = quantile( delt[2:length(delt)], 0.9, na.rm=TRUE )
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
          ylim=c(0.00, max( mresponse[,plot], na.rm=TRUE) ),
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
    mdl = lm( as.formula( modelFormula ),
              data = locdf, weights = wts, na.action="na.exclude"  )
    rm( locdf )
  } else {
    mdl = lm( as.formula( modelFormula ), data = basisDf, na.action="na.exclude" )
  }
  # bmdl = bigLMStats( mdl )
  u = scale( model.matrix( mdl ) )
  intercept = u[,1]
  if ( any( is.na( intercept ) ) ) intercept = rep( 0, length( intercept ) )
  u = u[,-1]
  v = antsrimpute( t( mdl$coefficients[-1, ] ) )
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
          denom = sum( temp * temp , na.rm=TRUE )
          if ( denom > 0 ) ip = sum( temp * v[,vv] ) / denom else ip = 1
          v[ , vv ] = v[, vv ] - temp * ip
        }
      localv = v[ , vv ]
      doflip = FALSE
      if ( sum( localv > 0 ) < sum( localv < 0 ) ) {
        localv = localv * (-1)
        doflip = TRUE
      }
      myquant = quantile( localv , sparsenessQuantile, na.rm=TRUE )
      if ( positivity == 'positive') {
        if ( myquant > 0 ) localv[ localv <= myquant ] = 0 else localv[ localv >= myquant ] = 0
      } else if ( positivity == 'negative' ) {
        localv[ localv > myquant ] = 0
      } else if ( positivity == 'either' ) {
        localv[ abs(localv) < quantile( abs(localv) , sparsenessQuantile, na.rm=TRUE  ) ] = 0
      }
      if ( doflip ) v[ , vv ] = localv * (-1) else v[ , vv ] = localv
    }
    intercept = rowMeans( x - ( u %*% t(v) ) )
    if ( ! any( is.na( repeatedMeasures ) ) & is.na( LRR ) ) { # estimate random intercepts
      for ( s in usubs ) {
        usel = repeatedMeasures == s
        intercept[ usel ] = mean( intercept[ usel ], na.rm=TRUE  )
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
        if ( verbose ) message(paste("reducing gradient step:", gamma))
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
  if ( all(is.na( smoothingMatrix ) )) smoothingMatrix = diag( ncol( x ) )
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
        if ( verbose ) message(paste("reducing gradient step:", gamma))
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
#'   smoothingMatrixX = smoomat, smoothingMatrixY = NA, verbose=T )
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
  ox = smoothRegression(  x, loy, iterations = 50,
                          sparsenessQuantile = sparsenessQuantile, positivity = positivity,
                          smoothingMatrix = smoothingMatrixX, nv = nv, verbose = FALSE )$v
  oy = matrix( nrow = ncol( y ), ncol = nv )
  for ( k in 1:iterations ) {
    lox = scale( x %*% ( ox ) )
    for ( j in 1:nv ) {
      if ( j == 1 ) rx = x else rx = residuals( lm( x ~ x %*% ox[,1:(j-1)] ) )
      if ( j == 1 ) ry = y else ry = residuals( lm( y ~ y %*% oy[,1:(j-1)] ) )
      lox = ( rx %*% ( ox ) )
      oy[,j] = smoothRegression(  ry, lox[,j], iterations = 50,
                                  sparsenessQuantile = sparsenessQuantile, positivity = positivity,
                                  smoothingMatrix = NA, nv = 2, verbose = F )$v[,1]
      loy = ( ry %*% ( oy ) )
      ox[,j] = smoothRegression(  rx, loy[,j], iterations = 50,
                                  sparsenessQuantile = sparsenessQuantile, positivity = positivity,
                                  smoothingMatrix = NA, nv = 2, verbose = F )$v[,1]
    }
    tt = x %*% ox
    ss = y %*% oy
    print( k )
    print( cor( tt, ss ) )
  }
  return(
    list(
      vx = ox,
      vy = oy
    )
  )
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
    #      vOrth = qr.Q( qr( v ) )
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
      doflip = FALSE
      if ( sum( localv > 0 ) < sum( localv < 0 ) ) {
        localv = localv * (-1)
        doflip = TRUE
      }
      myquant = quantile( localv , sparsenessQuantile, na.rm=TRUE )
      if ( positivity == 'positive') {
        if ( myquant > 0 ) localv[ localv <= myquant ] = 0 else localv[ localv >= myquant ] = 0
      } else if ( positivity == 'negative' ) {
        localv[ localv > myquant ] = 0
      } else if ( positivity == 'either' ) {
        localv[ abs(localv) < quantile( abs(localv) , sparsenessQuantile, na.rm=TRUE  ) ] = 0
      }
      if ( doflip ) v[ , vv ] = localv * (-1) else v[ , vv ] = localv
      v[ , vv ] = v[ , vv ] / sqrt( sum( v[ , vv ] * v[ , vv ] ) )
    }
    intercept = rowMeans( x - ( u %*% t(v) ) )
    if ( ! any( is.na( repeatedMeasures ) ) ) { # estimate random intercepts
      for ( s in usubs ) {
        usel = repeatedMeasures == s
        intercept[ usel ] = mean( intercept[ usel ], na.rm=TRUE  )
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
        if ( verbose ) message(paste("reducing gradient step:", gamma))
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
    basisDf = data.frame( u=irlba::irlba( x[[ m1 ]], nu = nvecs, nv = 0 )$u )
    #    basisDf = data.frame( u=rsvd::rsvd( x[[ m1 ]], nu = nvecs, nv = 0 )$u )
    #    basisDf = data.frame( u=RSpectra::svds( x[[ m1 ]], nvecs )$u )
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
#' @param orthogonalize run gram-schmidt if TRUE.
#' @param softThresholding use soft thresholding
#' @return matrix
#' @author Avants BB
#' @examples
#'
#' mat<-replicate(100, rnorm(20))
#' mat = orthogonalizeAndQSparsify( mat )
#'
#' @export orthogonalizeAndQSparsify
orthogonalizeAndQSparsify <- function( v,
                                       sparsenessQuantile = 0.5, positivity='either',
                                       orthogonalize = TRUE, softThresholding = FALSE ) {
  #  if ( orthogonalize ) v = qr.Q( qr( v ) )
  for ( vv in 1:ncol( v ) ) {
    if ( var( v[ , vv ] ) >  .Machine$double.eps ) {
      #      v[ , vv ] = v[ , vv ] / sqrt( sum( v[ , vv ] * v[ , vv ] ) )
      if ( vv > 1 & orthogonalize  ) {
        for ( vk in 1:(vv-1) ) {
          temp = v[,vk]
          #          temp = temp / sqrt( sum( temp * temp ) )
          denom = sum( temp * temp , na.rm=TRUE )
          if ( denom > .Machine$double.eps ) ip = sum( temp * v[,vv] ) / denom else ip = 1
          v[ , vv ] = v[, vv ] - temp * ip
        }
      }
      localv = v[ , vv ] # zerka
      doflip = FALSE
      if ( sum( localv > 0, na.rm=TRUE ) < sum( localv < 0, na.rm=TRUE ) ) {
        localv = localv * (-1)
        doflip = TRUE
      }
      myquant = quantile( localv , sparsenessQuantile, na.rm=TRUE )
      if ( ! softThresholding ) {
        if ( positivity == 'positive') {
          if ( myquant > 0 ) localv[ localv <= myquant ] = 0 else localv[ localv >= myquant ] = 0
        } else if ( positivity == 'negative' ) {
          localv[ localv > myquant ] = 0
        } else if ( positivity == 'either' ) {
          localv[ abs(localv) < quantile( abs(localv) , sparsenessQuantile, na.rm=TRUE  ) ] = 0
        }
      } else {
        if ( positivity == 'positive' ) localv[ localv < 0 ] = 0
        mysign = sign( localv )
        myquant = quantile( abs(localv) , sparsenessQuantile, na.rm=TRUE )
        temp = abs( localv ) - myquant
        temp[ temp < 0 ] = 0
        localv = mysign * temp
      }
      if ( doflip ) v[ , vv ] = localv * (-1) else v[ , vv ] = localv
    }
    cthresh = 100
    if ( cthresh >  0 ) {
      #      temp = makeImage( , )
    }
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
#' @param stochastic size of subset to use for stocastic gradient descent
#' @param initialization type of initialization, currently only supports a
#' character \code{randxy}
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
                              k=2, iterations=10,
                              stochastic = NA,
                              initialization = 'randxy',
                              verbose=FALSE ) {
  if ( nrow(x) != nrow(y)) stop("nrow x should equal nrow y")
  #  x = scale( icawhiten(x,nrow(x)), scale=T )
  #  y = scale( icawhiten(y,nrow(y)), scale=T )
  x = scale( x, scale=T )
  y = scale( y, scale=T )
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
  if ( initialization == 'randxy' ) {
    phiy = t( y ) %*%  ( x  %*% matrix( rnorm( k * ncol(x), 0, 1 ), ncol=k ) )
    phix = t( x ) %*%  ( y  %*% matrix( rnorm( k * ncol(y), 0, 1 ), ncol=k ) )
  }
  else if ( initialization == 'svd' ) {
    phix = svd( x, nv=k, nu=0  )$v
    phiy = svd( y, nv=k, nu=0  )$v
  }
  #  phix = matrix( rnorm( k * ncol(x), 0, 1e-4 ), ncol=k )
  #  phiy = matrix( rnorm( k * ncol(y), 0, 1e-4 ), ncol=k )

  i = 1
  if ( is.na( stochastic ) ) stoke = FALSE else stoke = TRUE
  while ( i < iterations ) {
    if ( stoke ) ind = sample( 1:nrow( x ) )[1:stochastic] else ind = 1:nrow( x )
    if ( i < 3 ) gx = -1e-4 # quantile( phix[ abs(phix) > 0 ] , 0.5 , na.rm=TRUE ) * ( -1.e4 )
    if ( i < 3 ) gy = -1e-4 # quantile( phiy[ abs(phiy) > 0 ] , 0.5 , na.rm=TRUE ) * ( -1.e4 )
    # gradient calculation
    delta = t(x[ind,]) %*% ( x[ind,] %*% phix - y[ind,] %*% phiy )
    phix1 = phix - delta * gx
    delta = t(y[ind,]) %*% ( y[ind,] %*% phiy - x[ind,] %*% phix )
    phiy1 = phiy - delta * gy
    #    phix1 = phix1 %*% dx
    #    phiy1 = phiy1 %*% dy
    phix1 = as.matrix( smoox %*% orthogonalizeAndQSparsify( phix1, sparsenessQuantile=sparsenessQuantile, positivity=positivity ) )
    phiy1 = as.matrix( smooy %*% orthogonalizeAndQSparsify( phiy1, sparsenessQuantile=sparsenessQuantile, positivity=positivity ) )
    # now update
    phix =  phix1 / norm( x %*% phix1 )
    phiy =  phiy1 / norm( y %*% phiy1 )
    errs[ i ] = norm( y %*% phiy - x %*% phix )
    #  print(  sum( abs(  diag(  cor( y %*% phiy,  x %*% phix  ) ) ) ) )
    if ( verbose ) print( paste( i, errs[ i ] ) )
    #    if ( i > 15 ) if ( errs[ i ]  < errs[i-1] ) i = iterations+1
    i = i + 1
  }

  #  print( cor( x %*% phix ) )
  #  print( cor( y %*% phiy ) )
  #  print( cor( y %*% phiy,  x %*% phix  ) )
  return( list( phix = phix, phiy = phiy ) )
}


#' Efficiently compute a multivariate, penalized image-based linear regression model (milr)
#'
#' This function simplifies calculating image-wide multivariate beta maps from
#' linear models.  Image-based variables are stored in the input
#' matrix list. They should be named consistently in the input formula and
#' in the image list.  If they are not, an error will be thrown.  All input
#' matrices should have the same number of rows and columns.  The model will
#' minimize a matrix energy similar to norm( X - UVt - UranVrant ) where the U
#' are standard design and random effect (intercept) design matrices.  The
#' random intercept matrix is only included if repeated measures are indicated.
#'
#' @param dataFrame This data frame contains all relevant predictors except for
#' the matrices associated with the image variables.
#' @param voxmats The named list of matrices that contains the changing predictors.
#' @param myFormula This is a character string that defines a valid regression formula.
#' @param smoothingMatrix allows parameter smoothing, should be square and same
#' size as input matrix
#' @param iterations number of gradient descent iterations
#' @param gamma step size for gradient descent
#' @param sparsenessQuantile quantile to control sparseness - higher is sparser
#' @param positivity restrict to positive or negative solution (beta) weights.
#' choices are positive, negative or either as expressed as a string.
#' @param repeatedMeasures list of repeated measurement identifiers. this will
#' allow estimates of per identifier intercept.
#' @param orthogonalize boolean to control whether we orthogonalize the v
#' @param verbose boolean to control verbosity of output
#' @return A list of different matrices that contain names derived from the
#' formula and the coefficients of the regression model.
#' @author BB Avants.
#' @examples
#'
#' set.seed(1500)
#' nsub = 12
#' npix = 100
#' outcome = rnorm( nsub )
#' covar = rnorm( nsub )
#' mat = replicate( npix, rnorm( nsub ) )
#' mat2 = replicate( npix, rnorm( nsub ) )
#' myform = " vox2 ~ covar + vox "
#' df = data.frame( outcome = outcome, covar = covar )
#' result = milr( df, list( vox = mat, vox2 = mat2 ), myform)
#'
#' \dontrun{
#' # a 2nd example with 3 modalities
#' imageIDs <- c( "r16", "r27", "r30", "r62", "r64", "r85" )
#' images <- list()
#' feature1Images <- list()
#' feature2Images <- list()
#' feature3Images <- list()
#' ref = antsImageRead( getANTsRData('r16') )
#' mask = ref * 0
#' for( i in 1:length( imageIDs ) )  {
#'   cat( "Processing image", imageIDs[i], "\n" )
#'   tar = antsImageRead( getANTsRData( imageIDs[i] ) )
#'   images[[i]] <- tar
#'   mask = mask + tar
#'   feature1Images[[i]] <- iMath( images[[i]], "Grad", 1.0 )
#'   feature2Images[[i]] <- iMath( images[[i]], "Laplacian", 1.0 )
#'   feature3Images[[i]] <- reflectImage(tar,axis=0,tx='Affine')$warpedmovout
#'   }
#' i=1
#' mask = getMask( mask )
#' spatmat = t( imageDomainToSpatialMatrix( mask, mask ) )
#' smoomat = knnSmoothingMatrix( spatmat, k = 23, sigma = 100.0 )
#' mat = imageListToMatrix( images, mask )
#' mat2 = imageListToMatrix( feature1Images, mask )
#' mat3 = imageListToMatrix( feature3Images, mask )
#' myscale <- function( x ) { return( scale(x, center=T,scale=T ) ) }
#' x = list( x1=myscale(mat[-5,]), x2=myscale(mat2[-5,]), x3=myscale(mat3[-5,]) )
#' df = data.frame( m1 = rowMeans( x$x1 ), m2 = rowMeans( x$x2 ), m3 = rowMeans( x$x3 ) )
#' myform = " x1 ~ x2 + x3 + m2 + m3 "
#' result = milr( df, x, myform, iterations=32,  smoothingMatrix = smoomat,
#'   gamma=1e-1, verbose=T )
#' k = 1
#' mm = makeImage( mask, (result$prediction[k,]) ) %>% iMath("Normalize")
#' tar = makeImage( mask, x$x1[k,] )
#' plot( mm, doCropping=FALSE  )
#' plot( tar, doCropping=FALSE )
#' cor.test( result$prediction[k,], x$x1[k,]  )
#' myol = makeImage( mask, abs(result$v[,"x2"]) ) %>% iMath("Normalize")
#' plot( tar, myol, doCropping=FALSE )
#' myol = makeImage( mask, abs(result$v[,"m3"]) ) %>% iMath("Normalize")
#' plot( tar, myol, doCropping=FALSE )
#' result = milr( df, x, myform, iterations=11,  smoothingMatrix = smoomat,
#'   sparsenessQuantile = 0.5, positivity = 'positive',
#'   gamma=1e-2, verbose=T )
#' myol = makeImage( mask, abs(result$v[,"x2"]) ) %>% iMath("Normalize")
#' plot( tar, myol, doCropping=FALSE, window.overlay=c(0.1,1) )
#' mm = makeImage( mask, (result$prediction[k,]) ) %>% iMath("Normalize")
#' tar = makeImage( mask, x$x1[k,] )
#' plot( mm, doCropping=FALSE  )
#' plot( tar, doCropping=FALSE )
#' cor.test( result$prediction[k,], x$x1[k,]  )
#' # univariate outcome
#' myform = " m1 ~ x2 + x3 + m2 + m3 "
#' result = milr( df, x, myform, iterations=11,  smoothingMatrix = smoomat,
#'   gamma=1e-2, verbose=T )
#'
#' }
#'
#' @export milr
milr <- function( dataFrame,  voxmats, myFormula, smoothingMatrix,
                  iterations = 10, gamma = 1.e-6,
                  sparsenessQuantile,
                  positivity = c("positive","negative","either"),
                  repeatedMeasures = NA,
                  orthogonalize = FALSE,
                  verbose = FALSE ) {
  milrorth = orthogonalize
  vdf = data.frame( dataFrame )
  matnames = names( voxmats )
  if ( length( matnames ) == 0 ) stop( 'please name the input list entries')
  n = nrow( voxmats[[1]] )
  p = ncol( voxmats[[1]] )
  if ( missing( smoothingMatrix ) ) smoothingMatrix = diag( p )
  poschoices = c("positive","negative","either", TRUE, FALSE )
  if ( ! missing( positivity ) ) {
    if ( sum( positivity == poschoices ) != 1 | length( positivity ) != 1 )
      stop( 'choice of positivity parameter is not good - see documentation')
    if ( positivity == TRUE ) positivity = "positive"
    if ( positivity == FALSE ) positivity = "either"
  }
  for ( k in 1:length( voxmats ) ) {
    vdf = cbind( vdf, voxmats[[k]][,1] )
    names( vdf )[  ncol( vdf ) ] = matnames[k]
    if ( ncol( voxmats[[k]] ) != p  )
      stop( paste( "matrix ", matnames[k], " does not have ", p, "entries" ) )
  }
  # get names from the standard lm
  temp = summary( lm( myFormula  , data=vdf))
  myrownames = rownames(temp$coefficients)
  mypvs = matrix( rep( NA, p * length( myrownames ) ),
                  nrow = length( myrownames ) )
  myestvs = mypvs
  myervs = mypvs
  mytvs = mypvs
  outcomevarname = trimws( unlist( strsplit( myFormula, "~" ) )[1] )
  outcomevarnum = which( outcomevarname == matnames  )
  outcomeisconstant = FALSE
  if ( length( outcomevarnum ) == 0 ) {
    outcomeisconstant = TRUE
    outcomevarnum = which( colnames(vdf) == outcomevarname  )
  }
  hasRanEff = FALSE
  vRan = NA
  if ( ! any( is.na( repeatedMeasures ) ) ) {
    hasRanEff = TRUE
    usubs = unique( repeatedMeasures )
    if ( length( repeatedMeasures ) != nrow( dataFrame ) )
      stop( "The length of the repeatedMeasures vector should equal the number of rows in the data frame." )
    ranEff = factor( repeatedMeasures )
    temp = lm( rnorm( nrow( dataFrame ) ) ~ ranEff )
    temp = model.matrix(  temp )
    ranEffNames = colnames( temp )
    ranEffNames[ 1 ] = paste0( "ranEff", as.character( levels( ranEff )[1] ) )
    temp[ ranEff == levels( ranEff )[1] ,1] = 1
    temp[ ranEff != levels( ranEff )[1] ,1] = 0
    zRan = ( temp[ , ] )
    colnames( zRan ) = ranEffNames
    tz = t( zRan )
    tzz = tz %*% zRan
    rm( ranEff )
  }
  mylm = lm( myFormula , data = vdf )
  u = ( model.matrix( mylm )[ , ] )
  unms = colnames( u )
  colnames( u ) = unms
  lvx = length( voxmats )
  predictormatrixnames = colnames( u )[  colnames( u ) %in% matnames ]
  myks = which( matnames %in% predictormatrixnames )
  v = matrix( rnorm( ncol(u)*p, 1, 1 ), nrow = p, ncol = ncol(u) ) * 0.0
  if ( hasRanEff ) {
    vRan = matrix( rnorm( ncol( zRan ) * p, 1, 1 ), nrow = p, ncol = ncol( zRan ) ) * 0.0
    dedrv = vRan * 0
    colnames( vRan ) = ranEffNames
  }
  colnames( v ) = unms
  hasIntercept = "(Intercept)" %in% colnames( v )
  if ( hasIntercept ) dospar = 2:ncol( v ) else dospar = 1:ncol( v )
  for ( i in 1:p ) {
    if ( length( myks ) > 0 )
      for ( k in 1:length(predictormatrixnames) ) {
        u[ ,  predictormatrixnames[k] ] = voxmats[[ myks[k] ]][,i]
      }
    tu = t( u )
    tuu = t( u ) %*% u
    if ( outcomeisconstant )
      myoc = vdf[ ,outcomevarnum ] else myoc = voxmats[[ outcomevarnum ]][,i]
    term2 = tu %*% myoc
    v[ i, ] = ( tuu %*% v[i,] - term2 )
  }
  v = as.matrix( smoothingMatrix %*% v )
  if ( !missing( sparsenessQuantile ) ) {
    v = orthogonalizeAndQSparsify( v, sparsenessQuantile, positivity,
                                   orthogonalize = milrorth )
  }
  dedv = v * 0
  predicted = voxmats[[ 1 ]] * 0
  # now fix dedv with the correct voxels
  for ( iter in 1:iterations ) {
    err = 0
    for ( i in 1:p ) {
      if ( length( myks ) > 0 )
        for ( k in 1:length(predictormatrixnames) )
          u[ ,  predictormatrixnames[k] ] = voxmats[[ myks[k] ]][,i]
        tu = t( u )
        tuu = t( u ) %*% u
        if ( outcomeisconstant )
          myoc = vdf[ ,outcomevarnum ] else myoc = voxmats[[ outcomevarnum ]][,i]
        term2 = tu %*% myoc
        dedv[ i, ] = tuu %*% v[i,] - term2
        if ( hasRanEff  ) dedv[ i,  ] = dedv[ i, ] + ( tu %*% zRan ) %*% vRan[i,]
        predicted[ , i ] = u %*% (v[i,])
        if ( hasRanEff  ) predicted[ , i ] = predicted[ , i ] + zRan %*% vRan[i,]
        # based on this explanation
        # https://m-clark.github.io/docs/mixedModels/mixedModels.html
        # the energy term for the regression model is:
        #   norm( y - uvt ) =>  grad update is wrt v is  tuu * v - tu * y
        # the energy term for the mixed regression model is:
        #   norm( y - uvt - z_ran v_rant ) =>
        # this derivative z_ran * ( y - uvt - z_ran v_rant  )
        #   grad update wrt v is     tuu * v    + tu * zRan * vRan - tu * y
        #   grad update wrt vran is  tzz * vran + tz * uvt - tz * y
        err = err + mean( abs( myoc - predicted[ , i ]  ) )
    }
    v = v - dedv * gamma
    v = as.matrix( smoothingMatrix %*% v )
    if ( !missing( sparsenessQuantile ) ) {
      v = orthogonalizeAndQSparsify( v, sparsenessQuantile, positivity, orthogonalize = milrorth )
    }
    gammamx = gamma * 0.1 # go a bit slower
    # simplified model here
    #      dedrv = t( ( t(zRan) %*% zRan ) %*% t( vRanX ) ) # t1
    #      dedrv = dedrvX + t( ( t(zRan) %*%  umat ) %*% t( vmat1 )  ) # t2
    #      dedrv = dedrv -  t( t(zRan) %*% voxmats[[1]] ) # t3
    #      vRan = smoothingMatrix %*% ( vRan + dedrvX * gammamx )


    if ( hasRanEff ) {
      vRan = as.matrix( smoothingMatrix %*% vRan )
      # update random effects
      for ( i in 1:p ) {
        if ( length( myks ) > 0 )
          for ( k in 1:length(predictormatrixnames) )
            u[ ,  predictormatrixnames[k] ] = voxmats[[ myks[k] ]][,i]
          tu = t( u )
          tuu = t( u ) %*% u
          if ( outcomeisconstant )
            myoc = vdf[ ,outcomevarnum ] else myoc = voxmats[[ outcomevarnum ]][,i]
          predicted[ , i ] = u %*% (v[i,]) + zRan %*% vRan[i,]
          #   grad update wrt vran is  tzz * vran + tz * uvt - tz * y
          #        rterm2 = tz %*% ( myoc - predicted[ , i ] ) # was this a bug or typo?  need to check math
          rterm2 = tz %*% ( myoc )
          dedrv[ i, ] = ( tz %*% u ) %*% v[ i ,  ] + tzz %*% vRan[ i ,  ] - rterm2
      }
      vRan = vRan - dedrv * gammamx
    }
    if ( verbose ) print( err / p )
  }
  colnames( v ) = unms
  return( list( u = u, v = v, prediction = predicted, vRan = vRan ) )
}





#' Predict from a milr output
#'
#' This function computes a prediction or low-dimensional embedding, given
#' \code{milr} output.  It will return a predictive model if the outcome variable
#' is a scalar.  Otherwise, it will return a low-dimensional embedding without
#' a specific predictive model.
#'
#' @param milrResult This output form milr
#' @param dataFrameTrain This data frame contains all relevant predictors
#' in the training data except for the matrices associated with the image variables.
#' @param voxmatsTrain The named list of matrices that contains the changing predictors.
#' @param dataFrameTest This data frame contains all relevant predictors
#' in the training data except for the matrices associated with the image variables in test data.
#' @param voxmatsTest The named list of matrices that contains the changing predictors in test data.
#' @param myFormula This is a character string that defines a valid regression formula.
#' @return the predicted matrix.
#' @author BB Avants.
#' @examples
#'
#' nsub = 24
#' npix = 100
#' outcome = rnorm( nsub )
#' covar = rnorm( nsub )
#' mat = replicate( npix, rnorm( nsub ) )
#' mat2 = replicate( npix, rnorm( nsub ) )
#' mat3 = replicate( npix, rnorm( nsub ) )
#' myform = " vox2 ~ covar + vox + vox3 "
#' istr = c( rep( TRUE, round(nsub*2/3) ), rep( FALSE, nsub - round(nsub*2/3)) )
#' df = data.frame( outcome = outcome, covar = covar )
#' ltr = list( vox = mat[ istr,], vox2 = mat2[istr,], vox3 = mat3[istr,] )
#' lte = list( vox = mat[!istr,], vox2 = mat2[!istr,], vox3 = mat3[!istr,]  )
#' result = milr( df[istr,], ltr, myform)
#' pred = milr.predict( result, df[istr,],ltr, df[!istr,], lte, myform )
#'
#' @seealso \code{\link{milr}}
#' @export milr.predict
milr.predict <- function(
  milrResult,
  dataFrameTrain,
  voxmatsTrain,
  dataFrameTest,
  voxmatsTest,
  myFormula )
{
  matnames = names( voxmatsTrain )
  vdf = data.frame( dataFrameTrain )
  vdfTe = data.frame( dataFrameTest )
  if ( length( matnames ) == 0 ) stop( 'please name the input list entries')
  outcomevarname = trimws( unlist( strsplit( myFormula, "~" ) )[1] )
  outcomevarnum = which( outcomevarname == matnames  )
  outcomeisconstant = FALSE
  if ( length( outcomevarnum ) == 0 ) {
    outcomeisconstant = TRUE
    outcomevarnum = which( colnames(vdf) == outcomevarname  )
  }
  # first build a unified training and testing dataFrame
  n = nrow( voxmatsTrain[[1]] )
  p = ncol( voxmatsTrain[[1]] )
  for ( k in 1:length( voxmatsTrain ) ) {
    vdf = cbind( vdf, voxmatsTrain[[k]][,1] )
    names( vdf )[  ncol( vdf ) ] = matnames[k]
    if ( ncol( voxmatsTrain[[k]] ) != p  )
      stop( paste( "train matrix ", matnames[k], " does not have ", p, "entries" ) )
    vdfTe = cbind( vdfTe, voxmatsTest[[k]][,1] )
    names( vdfTe )[  ncol( vdfTe ) ] = matnames[k]
    if ( ncol( voxmatsTest[[k]] ) != p  )
      stop( paste( "test matrix ", matnames[k], " does not have ", p, "entries" ) )
  }

  # get names from the standard lm
  temp = summary( lm( myFormula  , data=vdf))
  myrownames = rownames(temp$coefficients)
  mylm = lm( myFormula , data = vdf )
  u = model.matrix( mylm )
  unms = colnames( u[,] )
  u[,-1] = scale( u[,-1] )
  colnames( u ) = unms
  lvx = length( voxmatsTrain )
  predictormatrixnames = colnames( u )[  colnames( u ) %in% matnames ]
  myks = which( matnames %in% predictormatrixnames )
  # compute low-dimensional representations from the milr result for train-test
  if ( length( myks ) > 0 ) {
    for ( k in 1:length(predictormatrixnames) ) {
      vdf[ ,  predictormatrixnames[k] ] =
        voxmatsTrain[[ myks[k] ]] %*% milrResult$v[ , predictormatrixnames[k] ]
      vdfTe[ ,  predictormatrixnames[k] ] =
        voxmatsTest[[ myks[k] ]] %*% milrResult$v[ , predictormatrixnames[k] ]
    }
  }
  if ( outcomevarname %in% matnames ) {
    vdf = voxmatsTrain[[ outcomevarname ]] %*% milrResult$v
    vdfTe = voxmatsTest[[ outcomevarname ]] %*% milrResult$v
    return(
      list(
        predictionTrain = NA,
        predictionTest = NA,
        lowDimensionalProjectionTrain = vdf,
        lowDimensionalProjectionTest = vdfTe
      )
    )
  } else {
    trmdl = lm( myFormula, data = vdf )
    return(
      list(
        predictionTrain = predict( trmdl ),
        predictionTest = predict( trmdl, newdata = vdfTe ),
        lowDimensionalProjectionTrain = vdf[ ,  predictormatrixnames ],
        lowDimensionalProjectionTest = vdfTe[ ,  predictormatrixnames ]
      )
    )
  }
}




#' Efficiently compute a multivariate, image-based (mixed) linear decompositition (mild)
#'
#' This function simplifies calculating image-wide multivariate PCA maps.
#' The model will minimize a matrix energy similar to
#' norm( X - UVt - UranVrant ) where the U
#' are standard design and random effect (intercept) design matrices.  The
#' random intercept matrix is only included if repeated measures are indicated.
#'
#' @param dataFrame This data frame contains all relevant predictors except for
#' the matrices associated with the image variables.
#' @param voxmats The named list of matrices that contains the changing predictors.
#' @param basisK an integer determining the size of the basis.
#' @param myFormulaK This is a character string that defines a valid regression
#' which in this case should include predictors named as \code{paste0("mildBasis",1:basisK)}
#' @param smoothingMatrix allows parameter smoothing, should be square and same
#' size as input matrix
#' @param iterations number of gradient descent iterations
#' @param gamma step size for gradient descent
#' @param sparsenessQuantile quantile to control sparseness - higher is sparser
#' @param positivity restrict to positive or negative solution (beta) weights.
#' choices are positive, negative or either as expressed as a string.
#' @param initializationStrategy optional initialization matrix or seed.
#' seed should be a single number; matrix should be a n by k matrix.
#' @param repeatedMeasures list of repeated measurement identifiers. this will
#' allow estimates of per identifier intercept.
#' @param orthogonalize boolean to control whether we orthogonalize the v
#' @param verbose boolean to control verbosity of output
#' @return A list of different matrices that contain names derived from the
#' formula and the coefficients of the regression model.
#' @author BB Avants.
#' @examples
#'
#' set.seed(1500)
#' nsub = 12
#' npix = 100
#' outcome = rnorm( nsub )
#' covar = rnorm( nsub )
#' mat = replicate( npix, rnorm( nsub ) )
#' mat2 = replicate( npix, rnorm( nsub ) )
#' nk = 3
#' myform = paste(" vox2 ~ covar + vox + ",
#'   paste0( "mildBasis", 1:nk, collapse="+" ) )  # optional covariates
#' df = data.frame( outcome = outcome, covar = covar )
#' result = mild( df, list( vox = mat, vox2 = mat2 ), basisK = 3, myform,
#'   initializationStrategy = 10 )
#' result = mild( df, list( vox = mat, vox2 = mat2 ), basisK = 3, myform,
#'   initializationStrategy = 4 )
#' myumat = svd( mat2, nv=0, nu=3 )$u
#' result = mild( df, list( vox = mat, vox2 = mat2 ), basisK = 3, myform,
#'   initializationStrategy = 0 )
#'
#' @seealso \code{\link{milr}}
#' @export mild
mild <- function( dataFrame,  voxmats, basisK,
                  myFormulaK, smoothingMatrix,
                  iterations = 10, gamma = 1.e-6,
                  sparsenessQuantile = 0.5,
                  positivity = c("positive","negative","either"),
                  initializationStrategy = 0,
                  repeatedMeasures = NA,
                  orthogonalize = FALSE,
                  verbose = FALSE ) {
  positivity = positivity[1]
  mildorth = orthogonalize
  vdf = data.frame( dataFrame )
  matnames = names( voxmats )
  matnorm  = norm( voxmats[[1]] )
  if ( length( matnames ) == 0 ) stop( 'please name the input list entries')
  n = nrow( voxmats[[1]] )
  p = ncol( voxmats[[1]] )
  if ( missing( smoothingMatrix ) ) smoothingMatrix = diag( p )
  poschoices = c("positive","negative","either", TRUE, FALSE )
  if ( ! missing( positivity ) ) {
    if ( sum( positivity == poschoices ) != 1 | length( positivity ) != 1 )
      stop( 'choice of positivity parameter is not good - see documentation')
    if ( positivity == TRUE ) positivity = "positive"
    if ( positivity == FALSE ) positivity = "either"
  }
  for ( k in 1:length( voxmats ) ) {
    vdf = cbind( vdf, voxmats[[k]][,1] )
    names( vdf )[  ncol( vdf ) ] = matnames[k]
    if ( ncol( voxmats[[k]] ) != p  )
      stop( paste( "matrix ", matnames[k], " does not have ", p, "entries" ) )
  }
  outcomevarname = trimws( unlist( strsplit( myFormulaK, "~" ) )[1] )
  outcomevarnum = which( outcomevarname == matnames  )
  if ( class(initializationStrategy) == "numeric" ) {
    set.seed( initializationStrategy )
    initializationStrategy = scale( qr.Q( qr(
      replicate( basisK, rnorm( nrow( voxmats[[1]] ) ) ) ) ) )
  }
  if ( class(initializationStrategy) != "matrix" )
    stop("Please set valid initializationStrategy.")
  for ( k in 1:basisK ) {
    if ( k == 1 ) { # check matrix size
      stopifnot( nrow( initializationStrategy  ) == nrow( vdf ) )
      stopifnot( ncol( initializationStrategy ) == basisK )
    }
    initvec = initializationStrategy[,k]
    vdf = cbind( vdf, initvec )
    names( vdf )[  ncol( vdf ) ] = paste0( "mildBasis", k )
  }
  # augment the formula with the k-basis
  # get names from the standard lm
  temp = summary( lm( myFormulaK  , data=vdf))
  myrownames = rownames(temp$coefficients)
  knames = myrownames[ grep("mildBasis", myrownames ) ]
  mypvs = matrix( rep( NA, p * length( myrownames ) ),
                  nrow = length( myrownames ) )
  myestvs = mypvs
  myervs = mypvs
  mytvs = mypvs
  outcomeisconstant = FALSE
  if ( length( outcomevarnum ) == 0 ) {
    outcomeisconstant = TRUE
    outcomevarnum = which( colnames(vdf) == outcomevarname  )
  }
  hasRanEff = FALSE
  vRan = NA
  if ( ! any( is.na( repeatedMeasures ) ) ) {
    hasRanEff = TRUE
    usubs = unique( repeatedMeasures )
    if ( length( repeatedMeasures ) != nrow( dataFrame ) )
      stop( "The length of the repeatedMeasures vector should equal the number of rows in the data frame." )
    ranEff = factor( repeatedMeasures )
    temp = lm( rnorm( nrow( dataFrame ) ) ~ ranEff )
    temp = model.matrix(  temp )
    ranEffNames = colnames( temp )
    ranEffNames[ 1 ] = paste0( "ranEff", as.character( levels( ranEff )[1] ) )
    temp[ ranEff == levels( ranEff )[1] ,1] = 1
    temp[ ranEff != levels( ranEff )[1] ,1] = 0
    zRan = ( temp[ , ] )
    colnames( zRan ) = ranEffNames
    tz = t( zRan )
    tzz = tz %*% zRan
    rm( ranEff )
  }
  mylm = lm( myFormulaK , data = vdf )
  u = ( model.matrix( mylm )[ , ] )
  unms = colnames( u )
  colnames( u ) = unms
  lvx = length( voxmats )
  predictormatrixnames = colnames( u )[  colnames( u ) %in% matnames ]
  myks = which( matnames %in% predictormatrixnames )
  v = t( voxmats[[outcomevarnum]] ) %*% u
  if ( hasRanEff ) {
    vRan = matrix( rnorm( ncol( zRan ) * p, 1, 1 ), nrow = p, ncol = ncol( zRan ) ) * 0.0
    dedrv = vRan * 0
    colnames( vRan ) = ranEffNames
  }
  colnames( v ) = unms
  hasIntercept = "(Intercept)" %in% colnames( v )
  if ( hasIntercept ) dospar = 2:ncol( v ) else dospar = 1:ncol( v )

  for ( i in 1:p ) {
    if ( length( myks ) > 0 )
      for ( k in 1:length(predictormatrixnames) ) {
        u[ ,  predictormatrixnames[k] ] = voxmats[[ myks[k] ]][,i]
      }
    tu = t( u )
    tuu = t( u ) %*% u
    if ( outcomeisconstant )
      myoc = vdf[ ,outcomevarnum ] else myoc = voxmats[[ outcomevarnum ]][,i]/matnorm
    term2 = tu %*% myoc
    v[ i, ] = ( tuu %*% v[i,] - term2 ) * 0.01
  }
  v = as.matrix( smoothingMatrix %*% v )
  v = orthogonalizeAndQSparsify( v, sparsenessQuantile, positivity,
                                 orthogonalize = mildorth )
  dedv = v * 0
  predicted = voxmats[[ 1 ]] * 0
  # now fix dedv with the correct voxels
  for ( iter in 1:iterations ) {
    err = 0
    v = as.matrix( smoothingMatrix %*% v )
    for ( i in 1:p ) {
      if ( length( myks ) > 0 )
        for ( k in 1:length(predictormatrixnames) )
          u[ ,  predictormatrixnames[k] ] = voxmats[[ myks[k] ]][,i]
        tu = t( u )
        tuu = t( u ) %*% u
        if ( outcomeisconstant )
          myoc = vdf[ ,outcomevarnum ] else myoc = voxmats[[ outcomevarnum ]][,i]/matnorm
        term2 = tu %*% myoc
        dedv[ i, ] = tuu %*% v[i,] - term2
        if ( hasRanEff  ) dedv[ i,  ] = dedv[ i, ] + ( tu %*% zRan ) %*% vRan[i,]
        predicted[ , i ] = u %*% (v[i,])
        if ( hasRanEff  ) predicted[ , i ] = predicted[ , i ] + zRan %*% vRan[i,]
        err = err + mean( abs( myoc - predicted[ , i ]  ) )
    }
    v = v - dedv * gamma
    v = as.matrix( smoothingMatrix %*% v )
    if ( !missing( sparsenessQuantile ) ) {
      v = orthogonalizeAndQSparsify( v, sparsenessQuantile, positivity,
                                     orthogonalize = mildorth )
    }
    gammamx = gamma * 0.1 # go a bit slower
    if ( hasRanEff ) {
      vRan = as.matrix( smoothingMatrix %*% vRan )
      # update random effects
      for ( i in 1:p ) {
        if ( length( myks ) > 0 )
          for ( k in 1:length(predictormatrixnames) )
            u[ ,  predictormatrixnames[k] ] = voxmats[[ myks[k] ]][,i]
          tu = t( u )
          tuu = t( u ) %*% u
          if ( outcomeisconstant )
            myoc = vdf[ ,outcomevarnum ] else myoc = voxmats[[ outcomevarnum ]][,i]/matnorm
          predicted[ , i ] = u %*% (v[i,]) + zRan %*% vRan[i,]
          rterm2 = tz %*% ( myoc )
          dedrv[ i, ] = ( tz %*% u ) %*% v[ i ,  ] + tzz %*% vRan[ i ,  ] - rterm2
      }
      vRan = vRan - dedrv * gammamx
    }
    # reset the u variables
    if ( iter < iterations ) {
      for ( k in 1:length( knames ) ) {
        u[ ,  knames[k] ] = ( voxmats[[ outcomevarnum ]] %*% v[ , knames[k] ] )/matnorm
      }
      u[ , knames ] =  qr.Q(  qr( u[ , knames ] ) )
    }
    if ( verbose ) print( err / p )
  }
  colnames( v ) = unms
  return( list( u = u, v = v, prediction = predicted, vRan = vRan ) )
}


.symilr3 <- function( dataFrame,
                      voxmats,
                      basisK,
                      myFormulaK,
                      smoothingMatrixX,
                      smoothingMatrixY,
                      iterations = 10, gamma = 1.e-6,
                      sparsenessQuantileX = 0.5,
                      sparsenessQuantileY = 0.5,
                      positivityX = c("positive","negative","either"),
                      positivityY = c("positive","negative","either"),
                      initializationStrategyX = list( seed = 0, matrix = NA, voxels = NA ),
                      initializationStrategyY = list( seed = 0, matrix = NA, voxels = NA ),
                      repeatedMeasures = NA,
                      verbose = FALSE ) {
  ################################
  for ( k in 1:length( voxmats ) ) {
    voxmats[[ k ]] = scale( voxmats[[ k ]] )
    voxmats[[ k ]] = voxmats[[ k ]] / norm( voxmats[[ k ]] )
  }
  myorth <- function( u )
  {
    vecorth <- function( v1, v2 ) {
      ni = sum( v1 * v1 )
      nip1 = sum( v2 * v1 )
      if ( ni > 0 ) ratio = nip1/ni else ratio = 1
      #    if ( cor( v1, v2 ) == 1 ) return( rnorm( length( v1 )))
      v2 = v2 - v1 * ratio
      return( v2 )
    }
    nc = ncol( u )
    for ( k in 1:(nc-1) ) {
      vi = u[,k]
      for ( i in (k+1):nc ) {
        vip1 = u[,i]
        vip1 = vecorth( vi, vip1 )
        mynorm = sum( vip1 * vip1 )
        u[,i] = vip1 / mynorm
      }
    }
    #  print( cor( u ) )
    return( u )
  }

  myorth2 <- function( x ) {  qr.Q(  qr( x ) ) }
  myorth3 <- function( u = NULL, basis = TRUE, norm = TRUE)
  {
    if (is.null(u))
      return(NULL)
    if (!(is.matrix(u)))
      u <- as.matrix(u)
    p <- nrow(u)
    n <- ncol(u)
    if (prod(abs(La.svd(u)$d) > 1e-08) == 0)
      stop("colinears vectors")
    if (p < n) {
      warning("too much vectors to orthogonalize.")
      u <- as.matrix(u[, 1:p])
      n <- p
    }
    if (basis & (p > n)) {
      base <- diag(p)
      coef.proj <- crossprod(u, base)/diag(crossprod(u))
      base2 <- base - u %*% matrix(coef.proj, nrow = n, ncol = p)
      norm.base2 <- diag(crossprod(base2))
      base <- as.matrix(base[, order(norm.base2) > n])
      u <- cbind(u, base)
      n <- p
    }
    v <- u
    if (n > 1) {
      for (i in 2:n) {
        coef.proj <- c(crossprod(u[, i], v[, 1:(i - 1)]))/diag(crossprod(v[,
                                                                           1:(i - 1)]))
        v[, i] <- u[, i] - matrix(v[, 1:(i - 1)], nrow = p) %*%
          matrix(coef.proj, nrow = i - 1)
      }
    }
    if (norm) {
      coef.proj <- 1/sqrt(diag(crossprod(v)))
      v <- t(t(v) * coef.proj)
    }
    return(v)
  }
  ################################
  orthogonalizeBasis = TRUE
  n = nrow( voxmats[[1]] )
  p = ncol( voxmats[[1]] )
  q = ncol( voxmats[[2]] )
  if ( missing( smoothingMatrixX ) ) smoothingMatrixX = diag( p )
  if ( missing( smoothingMatrixY ) ) smoothingMatrixY = diag( q )
  xmatname = names( voxmats )[ 1 ]
  ymatname = names( voxmats )[ 2 ]
  formx = paste( xmatname, myFormulaK )
  formy = paste( ymatname, myFormulaK )
  locits = 1
  ########################
  mildx = mild( dataFrame,
                voxmats[1], basisK, formx, smoothingMatrixX,
                iterations = 1, gamma = gamma,
                sparsenessQuantile = sparsenessQuantileX,
                positivity = positivityX[[1]],
                initializationStrategy = initializationStrategyX,
                repeatedMeasures = repeatedMeasures,
                verbose = FALSE )
  colinds = (ncol(mildx$u)-basisK + 1):ncol(mildx$u)
  ########################
  mildy = mild( dataFrame,
                voxmats[2], basisK, formy, smoothingMatrixY,
                iterations = 1, gamma = gamma,
                sparsenessQuantile = sparsenessQuantileY,
                positivity = positivityY[[1]],
                initializationStrategy = initializationStrategyY,
                repeatedMeasures = repeatedMeasures,
                verbose = FALSE )
  xOrth = mildy$u[,-1]
  yOrth = mildx$u[,-1]
  #    xOrth = svd( antsrimpute( voxmats[[2]] %*% mildy$v[,-1] ) )$u
  #    yOrth = svd( antsrimpute( voxmats[[1]] %*% mildx$v[,-1] ) )$u
  # mildy$v = matrix(  rnorm( length( mildy$v ) ), nrow = nrow( mildy$v ) )
  # mildx$v = matrix(  rnorm( length( mildx$v ) ), nrow = nrow( mildx$v ) )
  for ( i in 1:iterations ) {
    if ( orthogonalizeBasis == TRUE ) {
      xOrthN = myorth( voxmats[[2]] %*% mildy$v[,-1] )
      yOrthN = myorth( voxmats[[1]] %*% mildx$v[,-1] )
      if ( i == 1  ) {
        xOrth = xOrthN
        yOrth = yOrthN
      } else  {
        wt1 = 0.5
        wt2 = 1.0 - wt1
        xOrth = xOrth * wt1 + xOrthN * wt2
        yOrth = yOrth * wt1 + yOrthN * wt2
      }
    }
    xOrth = yOrth
    xorthinds = c( ( ncol(xOrth) - basisK + 1):ncol( xOrth ) )
    colnames( xOrth[ , xorthinds  ] ) = colnames( mildx$u[, colinds ] )
    colnames( yOrth[ , xorthinds  ] ) = colnames( mildx$u[, colinds ] )
    dataFramex = cbind( dataFrame, xOrth[ , xorthinds  ] )
    dataFramey = cbind( dataFrame, yOrth[ , xorthinds  ] )
    dfinds = c( ( ncol(dataFramex) - basisK + 1):ncol(dataFramex) )
    colnames( dataFramex )[dfinds] = colnames( mildx$u[, colinds ] )
    colnames( dataFramey )[dfinds] = colnames( mildy$u[, colinds ] )
    mildx = milr( dataFramex,
                  voxmats[1], formx, smoothingMatrixX,
                  iterations = locits, gamma = gamma * (1),
                  sparsenessQuantile = sparsenessQuantileX,
                  positivity = positivityX[[1]],
                  repeatedMeasures = repeatedMeasures,
                  verbose = F )

    mildy = milr( dataFramey,
                  voxmats[2], formy, smoothingMatrixY,
                  iterations = locits, gamma = gamma * (1),
                  sparsenessQuantile = sparsenessQuantileY,
                  positivity = positivityY[[1]],
                  repeatedMeasures = repeatedMeasures,
                  verbose = F )
    ###############################################
    p1 = antsrimpute( voxmats[[1]] %*% mildx$v[,-1] )
    p2 = antsrimpute( voxmats[[2]] %*% mildy$v[,-1] )
    locor = cor( p1, p2 )
    overall = mean( abs( diag(locor)))
    if ( verbose & i > 0 ) {
      print( paste( "it:", i - 1, "=>", overall ) )
      #    print( ( locor ) )
      #    print( diag( locor ) )
    }
  }
  return( list( symilrX = mildx, symilrY = mildy ) )


  if ( FALSE ) {
    xv = mildx$v
    yv = mildy$v
    xvup = t( xOrth ) %*% voxmats[[1]]
    yvup = t( yOrth ) %*% voxmats[[2]]
    xvup[,-colinds] = 0
    yvup[,-colinds] = 0
    xvup = xvup / norm( xvup )
    yvup = yvup / norm( yvup )
    # now make the above sparse
    xvup = as.matrix( xvup[ , ] %*% smoothingMatrixX )
    xv = xv + t( xvup ) * gamma
    xv = as.matrix( smoothingMatrixX %*% xv[ , ] )
    xv = orthogonalizeAndQSparsify( xv, sparsenessQuantileX, positivityX[[1]] )
    xv = as.matrix( smoothingMatrixX %*% xv[ , ] )

    # now make the above sparse
    yvup = as.matrix( yvup[ , ] %*% smoothingMatrixY )
    yv = yv + t( yvup ) * gamma
    yv = as.matrix( smoothingMatrixY %*% yv[ , ]  )
    yv = orthogonalizeAndQSparsify( yv, sparsenessQuantileY, positivityY[[1]] )
    yv = as.matrix( smoothingMatrixY %*% yv[ , ]  )

    mildy$u[,colinds] = yOrth[,colinds] = scale( voxmats[[1]] %*% ( xv ) )[,colinds]
    mildx$u[,colinds] = xOrth[,colinds] = scale( voxmats[[2]] %*% ( yv ) )[,colinds]
  } # end if


}



.symilr2 <- function( dataFrame,
                      voxmats,
                      basisK,
                      myFormulaK,
                      smoothingMatrixX,
                      smoothingMatrixY,
                      iterations = 10, gamma = 1.e-6,
                      sparsenessQuantileX,
                      sparsenessQuantileY,
                      positivityX = c("positive","negative","either"),
                      positivityY = c("positive","negative","either"),
                      initializationStrategyX = list( seed = 0, matrix = NA, voxels = NA ),
                      initializationStrategyY = list( seed = 0, matrix = NA, voxels = NA ),
                      repeatedMeasures = NA,
                      verbose = FALSE ) {
  ################################
  for ( k in 1:length( voxmats ) ) {
    #  voxmats[[ k ]] = scale( voxmats[[ k ]] )
    #  voxmats[[ k ]] = voxmats[[ k ]] / norm( voxmats[[ k ]] )
  }
  myorth <- function( u )
  {
    vecorth <- function( v1, v2 ) {
      ni = sum( v1 * v1 )
      nip1 = sum( v2 * v1 )
      if ( ni > 0 ) ratio = nip1/ni else ratio = 1
      v2 = v2 - v1 * ratio
      return( v2 )
    }
    nc = ncol( u )
    for ( k in 1:(nc-1) ) {
      vi = u[,k]
      for ( i in (k+1):nc ) {
        vip1 = u[,i]
        vip1 = vecorth( vi, vip1 )
        mynorm = sum( vip1 * vip1 )
        u[,i] = vip1 / mynorm
      }
    }
    return( scale( u ) )
  }

  myorthx <- function( x ) {
    scale( qr.Q(  qr( x ) )  )
  }
  myorth3 <- function( u = NULL, basis = TRUE, norm = TRUE)
  {
    if (is.null(u))
      return(NULL)
    if (!(is.matrix(u)))
      u <- as.matrix(u)
    p <- nrow(u)
    n <- ncol(u)
    if (prod(abs(La.svd(u)$d) > 1e-08) == 0)
      stop("colinears vectors")
    if (p < n) {
      warning("too much vectors to orthogonalize.")
      u <- as.matrix(u[, 1:p])
      n <- p
    }
    if (basis & (p > n)) {
      base <- diag(p)
      coef.proj <- crossprod(u, base)/diag(crossprod(u))
      base2 <- base - u %*% matrix(coef.proj, nrow = n, ncol = p)
      norm.base2 <- diag(crossprod(base2))
      base <- as.matrix(base[, order(norm.base2) > n])
      u <- cbind(u, base)
      n <- p
    }
    v <- u
    if (n > 1) {
      for (i in 2:n) {
        coef.proj <- c(crossprod(u[, i], v[, 1:(i - 1)]))/diag(crossprod(v[,
                                                                           1:(i - 1)]))
        v[, i] <- u[, i] - matrix(v[, 1:(i - 1)], nrow = p) %*%
          matrix(coef.proj, nrow = i - 1)
      }
    }
    if (norm) {
      coef.proj <- 1/sqrt(diag(crossprod(v)))
      v <- t(t(v) * coef.proj)
    }
    return(v)
  }
  ################################
  orthogonalizeBasis = TRUE
  n = nrow( voxmats[[1]] )
  p = ncol( voxmats[[1]] )
  q = ncol( voxmats[[2]] )
  if ( missing( smoothingMatrixX ) ) smoothingMatrixX = diag( p )
  if ( missing( smoothingMatrixY ) ) smoothingMatrixY = diag( q )
  xmatname = names( voxmats )[ 1 ]
  ymatname = names( voxmats )[ 2 ]
  formx = paste( xmatname, myFormulaK )
  formy = paste( ymatname, myFormulaK )
  locits = 3
  ########################
  mildx = mild( dataFrame,
                voxmats[1], basisK, formx, smoothingMatrixX,
                iterations = locits, gamma = gamma,
                sparsenessQuantile = sparsenessQuantileX,
                positivity = positivityX[[1]],
                initializationStrategy = initializationStrategyX,
                repeatedMeasures = repeatedMeasures,
                verbose = FALSE )
  colinds = (ncol(mildx$u)-basisK + 1):ncol(mildx$u)
  ########################
  mildy = mild( dataFrame,
                voxmats[2], basisK, formy, smoothingMatrixY,
                iterations = locits, gamma = gamma,
                sparsenessQuantile = sparsenessQuantileY,
                positivity = positivityY[[1]],
                initializationStrategy = initializationStrategyY,
                repeatedMeasures = repeatedMeasures,
                verbose = FALSE )
  umat = myorth( ( mildy$u[,-1] +  mildx$u[,-1] ) * 0.5 )
  for ( i in 1:iterations ) {

    xorthinds = c( ( ncol(umat) - basisK + 1):ncol( umat ) )
    colnames( umat[ , xorthinds  ] ) = colnames( mildx$u[, colinds ] )
    colnames( umat[ , xorthinds  ] ) = colnames( mildx$u[, colinds ] )
    dataFramex = cbind( dataFrame, umat[ , xorthinds  ] )
    dataFramey = cbind( dataFrame, umat[ , xorthinds  ] )
    dfinds = c( ( ncol(dataFramex) - basisK + 1):ncol(dataFramex) )
    colnames( dataFramex )[dfinds] = colnames( mildx$u[, colinds ] )
    colnames( dataFramey )[dfinds] = colnames( mildy$u[, colinds ] )

    ###############################################
    mildx = milr( dataFramex,
                  voxmats[1], formx, smoothingMatrixX,
                  iterations = locits, gamma = gamma * (1),
                  sparsenessQuantile = sparsenessQuantileX,
                  positivity = positivityX[[1]],
                  repeatedMeasures = repeatedMeasures,
                  verbose = F )

    mildy = milr( dataFramey,
                  voxmats[2], formy, smoothingMatrixY,
                  iterations = locits, gamma = gamma * (1),
                  sparsenessQuantile = sparsenessQuantileY,
                  positivity = positivityY[[1]],
                  repeatedMeasures = repeatedMeasures,
                  verbose = F )
    ###############################################

    vmat1 = mildx$v[,-1] # matrix(  rnorm( p * basisK ), nrow=p  )
    vmat2 = mildy$v[,-1] # matrix(  rnorm( q * basisK ), nrow=q  )
    # dEnergy / du = -vt ( x - uvt ) = xv - uvv
    dedu1 = voxmats[[1]] %*% vmat1 - ( umat %*% t(vmat1) ) %*% vmat1
    dedu2 = voxmats[[2]] %*% vmat2 - ( umat %*% t(vmat2) ) %*% vmat2
    umup = ( dedu1 + dedu2 )
    #  umup[,-colinds]=0
    umat = myorth( umat + umup/norm(umup)*gamma )

    if ( verbose & i > 0 )
      print( paste(  "it:", i - 1, "=>",
                     norm(  voxmats[[1]] - umat %*% t(vmat1 ) ),
                     norm(  voxmats[[2]] - umat %*% t(vmat2 ) ) ) )


  }
  return( list( symilrX = mildx, symilrY = mildy ) )
}





#' Symmetric multivariate, penalized image-based linear regression model (symilr2) for two modalities
#'
#' This function simplifies calculating image-wide multivariate beta maps from
#' that is similar to CCA.
#'
#' @param voxmats A list that contains the named x and y matrices.
#' @param basisK an integer determining the size of the basis.
#' @param smoothingMatrixX allows parameter smoothing, should be square and same
#' size as input matrix on left side of equation
#' @param smoothingMatrixY allows parameter smoothing, should be square and same
#' size as input matrix on right side of equation
#' @param iterations number of gradient descent iterations
#' @param gamma step size for gradient descent
#' @param sparsenessQuantileX quantile to control sparseness - higher is sparser
#' @param sparsenessQuantileY quantile to control sparseness - higher is sparser
#' @param positivityX restrict to positive or negative solution (beta) weights.
#' choices are positive, negative or either as expressed as a string.
#' @param positivityY restrict to positive or negative solution (beta) weights.
#' choices are positive, negative or either as expressed as a string.
#' @param initialUMatrix initialization matrix size \code{n} by \code{k}.
#' If this is missing, a random matrix will be used.
#' @param orthogonalize boolean to control whether we orthogonalize the solutions explicitly
#' @param repeatedMeasures list of repeated measurement identifiers. this will
#' allow estimates of per identifier intercept.
#' @param verbose boolean to control verbosity of output
#' @return A list of u, x and y-related matrices.
#' @author BB Avants.
#' @examples
#'
#' set.seed(1500)
#' nsub = 12
#' npix = 100
#' outcome = rnorm( nsub )
#' covar = rnorm( nsub )
#' mat = replicate( npix, rnorm( nsub ) )
#' mat2 = replicate( npix + 10, rnorm( nsub ) )
#' mat3 = replicate( npix + 10, rnorm( nsub ) )
#' nk = 3
#' result = symilr2( list( vox = mat, vox2 = mat2, vox3 = mat3 ), basisK = 3 )
#'
#' @seealso \code{\link{milr}} \code{\link{mild}}
#' @export symilr2
symilr2 <- function(
  voxmats,
  basisK,
  smoothingMatrixX,
  smoothingMatrixY,
  iterations = 10,
  gamma = 1.e-6,
  sparsenessQuantileX = 0.5,
  sparsenessQuantileY = 0.5,
  positivityX = c("positive","negative","either"),
  positivityY = c("positive","negative","either"),
  initialUMatrix,
  orthogonalize = TRUE,
  repeatedMeasures = NA,
  verbose = FALSE ) {
  if (length(positivityX) == 1 && isTRUE(positivityX)) {
    positivityX = "positive"
  }
  if (length(positivityY) == 1 && isTRUE(positivityY)) {
    positivityY = "positive"
  }
  positivityX = match.arg(positivityX, choices = c("positive","negative","either"))
  positivityY = match.arg(positivityY, choices = c("positive","negative","either"))
  if (positivityX == "negative") {
    positivityX = "either"
  }
  if (positivityY == "negative") {
    positivityY = "either"
  }
  # if ( positivityX == TRUE | positivityX == 'positive' ) positivityX = 'positive' else positivityX = 'either'
  # if ( positivityY == TRUE | positivityY == 'positive' ) positivityY = 'positive' else positivityY = 'either'
  matnorms = rep( NA, length( voxmats ) )
  for ( i in 1:length( voxmats ) ) matnorms[ i ] = norm( voxmats[[ i ]] )
  n = nrow( voxmats[[1]] )
  p = ncol( voxmats[[1]] )
  q = ncol( voxmats[[2]] )

  hasRanEff = FALSE
  zRan = NA
  if ( ! any( is.na( repeatedMeasures ) ) ) {
    hasRanEff = TRUE
    usubs = unique( repeatedMeasures )
    if ( length( repeatedMeasures ) != n )
      stop( "The length of the repeatedMeasures vector should equal the number of rows in the data." )
    ranEff = factor( repeatedMeasures )
    temp = lm( rnorm( n ) ~ ranEff )
    temp = model.matrix(  temp )
    ranEffNames = colnames( temp )
    ranEffNames[ 1 ] = paste0( "ranEff", as.character( levels( ranEff )[1] ) )
    temp[ ranEff == levels( ranEff )[1] ,1] = 1
    temp[ ranEff != levels( ranEff )[1] ,1] = 0
    zRan = ( temp[ , ] )
    colnames( zRan ) = ranEffNames
    tz = t( zRan )
    tzz = tz %*% zRan
    rm( ranEff )
  }

  if ( missing( smoothingMatrixX ) ) smoothingMatrixX = diag( p )
  if ( missing( smoothingMatrixY ) ) smoothingMatrixY = diag( q )
  xmatname = names( voxmats )[ 1 ]
  ymatname = names( voxmats )[ 2 ]

  if ( missing( initialUMatrix ) ) {
    umatX = umatY = scale( qr.Q( qr( matrix(  rnorm( n * basisK ), nrow=n  ) ) ), T, T )
  } else { umatX = umatY = initialUMatrix }

  vRanX = vRanY = NA
  if ( hasRanEff ) {
    vRanX = matrix( rnorm( ncol( zRan ) * p, 1, 1 ), nrow = p, ncol = ncol( zRan ) ) * 0.0
    vRanY = matrix( rnorm( ncol( zRan ) * q, 1, 1 ), nrow = q, ncol = ncol( zRan ) ) * 0.0
    dedrvX = vRanX * 0
    dedrvY = dedrvX
    colnames( vRanX ) = colnames( vRanY ) = ranEffNames
  }
  gammamx = gamma * 0.01
  for ( i in 1:iterations ) {
    vmat1 = as.matrix( ( t( voxmats[[1]] /  matnorms[1] ) %*% umatY ) )
    vmat2 = as.matrix( ( t( voxmats[[2]] /  matnorms[2]  ) %*% umatX ) )
    vmat1 = orthogonalizeAndQSparsify(
      as.matrix( smoothingMatrixX %*% (vmat1) ), sparsenessQuantileX,
      orthogonalize = orthogonalize, positivity = positivityX  )
    vmat2 = orthogonalizeAndQSparsify(
      as.matrix( smoothingMatrixY %*% (vmat2) ), sparsenessQuantileY,
      orthogonalize = orthogonalize, positivity = positivityY  )
    # dEnergy / du = -vt ( x - uvt ) = xv - uvtv
    dedu1 = ( voxmats[[1]] /  matnorms[1]  ) %*% vmat1 - ( umatX %*% t(vmat1) ) %*% vmat1
    dedu2 = ( voxmats[[2]] /  matnorms[2]  ) %*% vmat2 - ( umatY %*% t(vmat2) ) %*% vmat2
    if ( hasRanEff ) {
      dedu1 = dedu1 + zRan %*% ( t( vRanX ) %*% vmat1 )
      dedu2 = dedu2 + zRan %*% ( t( vRanY ) %*% vmat2 )
    }
    # the gradient update - could be weighted
    umatX = umatX + ( dedu1 ) * gamma
    umatY = umatY + ( dedu2 ) * gamma
    #    umatX = scale( umatX + ( dedu1 ) * gamma, center=TRUE, scale=TRUE )
    #    umatY = scale(umatY + ( dedu2 ) * gamma, center=TRUE, scale=TRUE )
    #    umatY =  scale( ( umatX %*% t(umatX ) ) %*% umatY, center=TRUE, scale=TRUE )
    orthogonalizesymilr = F
    if ( orthogonalizesymilr ) {
      umatX =  qr.Q(  qr( umatX ) )
      umatY =  ( umatX %*% t(umatX ) ) %*% qr.Q(  qr( umatY ) )
    }

    # the energy term for the regression model is:
    #   norm( x - uvt ) =>  grad update is wrt u is  xv - uvtv
    # the energy term for the mixed regression model is:
    #   norm( x - uvt - z_ran v_rant ) =>
    #   grad update wrt u is  xv - uvtv -  zRan * vRan * tv
    #   grad update wrt vran is  tzz * vran + tz * uvt - tz * x
    #   t1= tzz * vran +
    #   t2 = tz * uvt -
    #   t3 = tz * x

    if ( hasRanEff  ) {
      dedrvX = vRanX %*% ( t( zRan ) %*% zRan ) # t1
      dedrvX = dedrvX + vmat1 %*% ( t( umatX ) %*% zRan ) # t2
      dedrvX = dedrvX - t( voxmats[[1]] ) %*% zRan # t3

      dedrvY = vRanY %*% ( t( zRan ) %*% zRan ) # t1
      dedrvY = dedrvY + vmat2 %*% ( t( umatY ) %*% zRan ) # t2
      dedrvY = dedrvY - t( voxmats[[2]] ) %*% zRan # t3

      vRanX = smoothingMatrixX %*% ( vRanX + dedrvX * gammamx )
      vRanY = smoothingMatrixY %*% ( vRanY + dedrvY * gammamx )
    }

    if ( verbose & hasRanEff ) {
      print( paste(i, "=>",
                   norm(  voxmats[[1]] /  matnorms[1] - umatX %*% t( vmat1 ) - zRan %*% t(vRanX) ),
                   norm(  voxmats[[2]] /  matnorms[2] - umatY %*% t( vmat2 ) - zRan %*% t(vRanY) ) ) )
    }
    if ( verbose & !hasRanEff ) {
      print( paste(i, "=>",
                   norm(  voxmats[[1]] /  matnorms[1] - umatX %*% t( vmat1 ) ),
                   norm(  voxmats[[2]] /  matnorms[2] - umatY %*% t( vmat2 ) ) ) )
    }
  }
  return(
    list(
      uX  = as.matrix( umatX ),
      uY  = as.matrix( umatY ),
      vX = as.matrix( vmat1 ),
      vY = as.matrix( vmat2 ),
      vRanX = vRanX,
      vRanY = vRanY )
  )
}




#' Symmetric multivariate, penalized image-based linear regression model (symilr) for N modalities
#'
#' SyMILR minimizes reconstruction error across related modalities.  That is,
#' SyMILR will reconstruct each modality matrix from a basis set derived from
#' the other modalities.  The basis set can be derived from SVD, ICA or a
#' simple sum of basis representations.
#' This function produces dataset-wide multivariate beta maps for each of the
#' related matrices.  The multivariate beta maps are regularized by user
#' input matrices that encode relationships between variables.  The idea is
#' overall similar to canonical correlation analysis but generalizes the basis
#' construction and to arbitrary numbers of modalities.
#'
#' @param voxmats A list that contains the named matrices.  Note: the optimization will likely perform much more smoothly if the input matrices are each scaled to zero mean unit variance e.g. by the \code{scale} function.
#' @param smoothingMatrices list of (sparse) matrices that allow parameter smoothing/regularization.  These should be square and same order and size of input matrices.
#' @param iterations number of gradient descent iterations
#' @param sparsenessQuantiles vector of quantiles to control sparseness - higher is sparser
#' @param positivities vector that sets for each matrix if we restrict to positive or negative solution (beta) weights.
#' choices are positive, negative or either as expressed as a string.
#' @param initialUMatrix list of initialization matrix size \code{n} by \code{k} for each modality.  Otherwise, pass a single scalar to control the
#' number of basis functions in which case random initialization occurs. One
#' may also pass a single initialization matrix to be used for all matrices.
#' If this is set to a scalar, or is missing, a random matrix will be used.
#' @param mixAlg 'svd', 'ica', 'rrpca-l', 'rrpca-s' or 'avg' denotes the algorithm employed when estimating the mixed modality bases
#' @param orthogonalize boolean to control whether we orthogonalize the solutions explicitly
#' @param repeatedMeasures list of repeated measurement identifiers. this will
#' allow estimates of per identifier intercept.
#' @param lineSearchRange lower and upper limit used in \code{optimize}
#' @param lineSearchTolerance tolerance used in \code{optimize}, will be multiplied by each matrix norm such that it scales appropriately with input data
#' @param randomSeed controls repeatability of ica-based decomposition
#' @param lowDimensionalError development option
#' @param verbose boolean to control verbosity of output
#' @return A list of u, x, y, z etc related matrices.
#' @author BB Avants.
#' @examples
#'
#' set.seed(1500)
#' nsub = 25
#' npix = c(100,200,133)
#' nk = 5
#' outcome = matrix(rnorm( nsub * nk ),ncol=nk)
#' outcome1 = matrix(rnorm( nsub * nk ),ncol=nk)
#' outcome2 = matrix(rnorm( nsub * nk ),ncol=nk)
#' outcome3 = matrix(rnorm( nsub * nk ),ncol=nk)
#' view1tx = matrix( rnorm( npix[1]  * nk ), nrow=nk )
#' view2tx = matrix( rnorm( npix[2]  * nk ), nrow=nk )
#' view3tx = matrix( rnorm( npix[3]  * nk ), nrow=nk )
#' mat1 = (outcome %*% t(outcome1) %*% (outcome1)) %*% view1tx
#' mat2 = (outcome %*% t(outcome2) %*% (outcome2)) %*% view2tx
#' mat3 = (outcome %*% t(outcome3) %*% (outcome3)) %*% view3tx
#' result = symilr(list( vox = mat1, vox2 = mat2, vox3 = mat3 ),
#'    initialUMatrix = nk , verbose=TRUE, iterations=5  )
#' p1 = mat1 %*% (result$v[[1]])
#' p2 = mat2 %*% (result$v[[2]])
#' p3 = mat3 %*% (result$v[[3]])
#'
#' # compare to permuted data
#' s1 = sample( 1:nsub)
#' s2 = sample( 1:nsub)
#' resultp = symilr(list( vox = mat1, vox2 = mat2[s1,], vox3 = mat3[s2,] ),
#'    initialUMatrix = nk , verbose=TRUE, iterations=5  )
#' p1p = mat1 %*% (resultp$v[[1]])
#' p2p = mat2[s1,] %*% (resultp$v[[2]])
#' p3p = mat3[s2,] %*% (resultp$v[[3]])
#'
#' # compare to SVD
#' svd1 = svd( mat1, nu=nk, nv=0 )$u
#' svd2 = svd( mat2, nu=nk, nv=0 )$u
#' svd3 = svd( mat3, nu=nk, nv=0 )$u
#'
#' # real
#' range(cor(p1,p2))
#' range(cor(p1,p3))
#' range(cor(p3,p2))
#'
#' # permuted
#' range(cor(p1p,p2p))
#' range(cor(p1p,p3p))
#' range(cor(p3p,p2p))
#'
#' # svd
#' print( range(cor( svd1,svd2) ))
#'
#' @seealso \code{\link{milr}} \code{\link{mild}} \code{\link{symilr2}}  \code{\link{symilrU}}
#' @export symilr
symilr <- function(
  voxmats,
  smoothingMatrices,
  iterations = 10,
  sparsenessQuantiles,
  positivities,
  initialUMatrix,
  mixAlg,
  orthogonalize = TRUE,
  repeatedMeasures = NA,
  lineSearchRange = c( -10, 10 ),
  lineSearchTolerance = 0.001,
  randomSeed,
  lowDimensionalError = FALSE,
  verbose = FALSE ) {
  if ( ! missing( "randomSeed" ) ) set.seed( randomSeed )
  # \sum_i  \| X_i - \sum_{ j ne i } u_j v_i^t \|^2 + \| G_i \star v_i \|_1
  # \sum_i  \| X_i - \sum_{ j ne i } u_j v_i^t - z_r v_r^ T \|^2 + constraints
  mixAlgs = c( 'svd', 'ica', 'avg', 'rrpca-l', 'rrpca-s' )
  if ( missing( mixAlg ) ) mixAlg = mixAlgs[1]
  if ( ! mixAlg %in% mixAlgs ) {
    message( paste(mixAlgs, collapse=' or ' ) )
    stop("pass valid mixing method")
  }
  # 0.0 adjust length of input data
  gamma = rep( 1, length(voxmats) )
  if  ( missing( positivities ) )
    positivities = rep( "positive", length( voxmats ) )
  if ( any( ( positivities %in%  c("positive","negative","either") ) == FALSE  ) )
    stop( "positivities must be one of positive, negative, either" )
  if ( length( positivities ) ==  1 )
    positivities = rep( positivities[1], length( voxmats ) )
  matnames = matnorms = p = rep( NA, length( voxmats ) )
  n = nrow( voxmats[[1]] )
  if ( missing( sparsenessQuantiles ) )
    sparsenessQuantiles = rep( 0.5, length( voxmats ) )

  # 1.0 adjust matrix norms
  for ( i in 1:length( voxmats ) ) {
    if ( is.null( voxmats[[ i ]] ) | is.na( voxmats[[ i ]] ) ) 
      stop( paste( "voxmat", i, "is null" ) )
    matnorms[ i ] = norm( voxmats[[ i ]] )
    p[ i ] = ncol( voxmats[[ i ]] )
    matnames =  names( voxmats )[ i ]
  }

  # 2.0 setup random effects
  hasRanEff = FALSE
  zRan = NA
  if ( ! any( is.na( repeatedMeasures ) ) ) {
    hasRanEff = TRUE
    usubs = unique( repeatedMeasures )
    if ( length( repeatedMeasures ) != n )
      stop( "The length of the repeatedMeasures vector should equal the number of rows in the data." )
    ranEff = factor( repeatedMeasures )
    temp = lm( rnorm( n ) ~ ranEff )
    temp = model.matrix(  temp )
    ranEffNames = colnames( temp )
    ranEffNames[ 1 ] = paste0( "ranEff", as.character( levels( ranEff )[1] ) )
    temp[ ranEff == levels( ranEff )[1] ,1] = 1
    temp[ ranEff != levels( ranEff )[1] ,1] = 0
    zRan = ( temp[ , ] )
    colnames( zRan ) = ranEffNames
    tz = t( zRan )
    tzz = tz %*% zRan
    rm( ranEff )
  }

  # 3.0 setup regularization
  if ( missing( smoothingMatrices ) ) {
    smoothingMatrices = list( )
    for ( i in 1:length( voxmats ) )
      smoothingMatrices[[ i ]] = diag( p[ i ] )
  }
  for ( i in 1:length( smoothingMatrices ) ) {
    if ( is.null( smoothingMatrices[[ i ]] ) is.na( smoothingMatrices[[ i ]] ) | )
      message( paste( "smoothingMatrices", i, "is null or NA." ) )
    smoothingMatrices[[ i ]] = smoothingMatrices[[i]] /
      Matrix::rowSums( smoothingMatrices[[i]] )
    }
  # some gram schmidt code
  localGS <- function( x, orthogonalize = TRUE ) {
    if ( !orthogonalize ) return( x )
    n <- dim(x)[1]
    m <- dim(x)[2]
    q <- matrix(0,n,m)
    r <- matrix(0,m,m)
    qi <- x[,1]
    si <- sqrt(sum(qi ^ 2))
    q[,1] <- qi / si
    r[1,1] <- si
    for (i in 2:m) {
      xi <- x[,i]
      qj <- q[,1:(i - 1)]
      rj <- t(qj) %*% xi
      qi <- xi - qj %*% rj
      r[1:(i - 1),i] <- rj
      si <- sqrt(sum(qi ^ 2))
      q[,i] <- qi / si
      r[i,i] <- si
    }
    return( q )
    return(list(q = q,r = r))
  }
  randmat = 0

  # 4.0 setup initialization
  if ( missing( initialUMatrix ) )
    initialUMatrix = length( voxmats )

  if ( class(initialUMatrix) == 'matrix' ) {
    randmat = initialUMatrix
    initialUMatrix = list( )
    for ( i in 1:length( voxmats ) )
      initialUMatrix[[ i ]] = randmat
  }

  if ( length( initialUMatrix ) != length( voxmats ) &
       !is.matrix(initialUMatrix) ) {
    message(paste("initializing with random matrix: ",initialUMatrix,'columns'))
    randmat = scale(
      (( matrix(  rnorm( n * initialUMatrix ), nrow=n  ) ) ), TRUE, TRUE )
    initialUMatrix = list( )
    for ( i in 1:length( voxmats ) )
      initialUMatrix[[ i ]] = randmat
  }

  vRan = list( )
  dedrv = list( )
  if ( hasRanEff ) {
    for ( i in 1:length( voxmats ) ) {
      vRan[[ i ]] = matrix( rnorm( ncol( zRan ) * p[i], 1, 1 ),
                            nrow = p[i], ncol = ncol( zRan ) ) * 0.0
      dedrv[[ i ]] = vRan[[ i ]]
      colnames(  vRan[[ i ]] ) = ranEffNames
    }
  }
  basisK = ncol( initialUMatrix[[ 1 ]] )
  vmats = list()
  dedu = list()
  for ( i in 1:length( voxmats ) )
    vmats[[ i ]] = matrix( 0, nrow = p[ i ], ncol = basisK )
  matnorms = rep( 1, length(matnorms) )
  gradnorms = matnorms
  gradnormsRanEff = matnorms
  itThresh = 1 # scales random effects gradient
  # function for computing error term
  npower = "F"
  getSyME <- function( tempv, i, returnEnergy=TRUE )  {
    tempvmat = matrix( tempv, ncol = ncol( vmats[[1]] ) )
    tempvmat = orthogonalizeAndQSparsify(
      as.matrix( smoothingMatrices[[i]] %*% tempvmat),
      sparsenessQuantiles[i],
      orthogonalize = FALSE, positivity = positivities[i] )
    prediction = 0
    if ( hasRanEff ) prediction = zRan %*% t(vRan[[i]])
    for ( j in 1:length( voxmats ) ) {
      if ( i != j ) {
        prediction = prediction + initialUMatrix[[j]] %*% t( tempvmat )
      }
    }
    if ( returnEnergy ) return( norm( prediction - voxmats[[i]], "F" )  )
    return( norm( voxmats[[i]], "F" ) /
              norm( prediction, "F" ) )
  }

  nc = ncol( initialUMatrix[[ 1 ]] )
  myw = matrix( rnorm( nc^2), nc, nc )
  getAvgU <- function( i, myw, mixAlg ) {
    avgU = NULL
    if ( mixAlg == 'avg' ) {
      avgU = initialUMatrix[[ 1 ]] * 0.0
      for ( j in 1:length(voxmats) )
        if ( i != j ) avgU = avgU + initialUMatrix[[ j ]]
        return( avgU )
    }
    nc = ncol( initialUMatrix[[ 1 ]] )
    for ( j in 1:length(voxmats) )
      if ( i != j ) avgU = cbind( avgU, initialUMatrix[[ j ]] )
    if ( mixAlg == 'rrpca-l' )
      return( rsvd::rrpca( avgU, rand=F )$L[,1:nc] )
    else if ( mixAlg == 'rrpca-s' )
      return( rsvd::rrpca( avgU, rand=F )$S[,1:nc] )
    else if ( mixAlg == 'ica' )
      return( fastICA::fastICA( avgU,  method = 'C', w.init = myw,
                                n.comp=nc )$S )
    return( svd( avgU, nu = ncol( initialUMatrix[[1]] ), nv=0 )$u )
  }

  getSyME2 <- function( lineSearch, gradient, myw, mixAlg, lowDimensionalError )  {
    prediction = 0
    myenergysearchv = ( vmats[[i]]+gradient * lineSearch )  # update the i^th v matrix
    myenergysearchv = orthogonalizeAndQSparsify(            # make sparse
      as.matrix( smoothingMatrices[[i]] %*% myenergysearchv ),
      sparsenessQuantiles[i],
      orthogonalize = FALSE, positivity = positivities[i] )
    if ( hasRanEff ) prediction = zRan %*% t(vRan[[i]])     # FIXME - need to check this
    avgU = symilrU( initialUMatrix, i, mixAlg, myw, orthogonalize = orthogonalize ) # get U for this prediction
    if ( lowDimensionalError ) {
      prediction = predict( lm( voxmats[[i]] %*% myenergysearchv ~ avgU ) )  # new way, faster
      return( norm( prediction - voxmats[[i]]  %*% myenergysearchv, "F" ) )  # new way, faster
      }
    prediction = avgU %*% t( myenergysearchv ) # old way, slower
    return(
      norm(
        prediction/norm(prediction,'F') -
        voxmats[[i]]/norm(voxmats[[i]],'F'),
          "F" )  )  # old way, slower
  }

  getSyMG <- function( v, i, myw, mixAlg )  {
    avgU = symilrU( initialUMatrix, i, mixAlg, myw, orthogonalize = orthogonalize  ) # getAvgU( i, myw, mixAlg )
    temperv = 0
    if ( hasRanEff )
      temperv = t(( t(avgU) %*% zRan ) %*% t(vRan[[i]]))
    temperv = temperv + t( voxmats[[i]] ) %*% avgU
    temperv = temperv - v %*% (  t( avgU ) %*% avgU )
    return( temperv )
  }

  ################################################################################
  # below is the primary optimization loop - grad for v then for vran
  ################################################################################
  datanorm = rep( 0.0, length(voxmats) )
  for ( myit in 1:iterations ) {
    errterm = rep( 1.0, length(voxmats) )
    prednorm = rep( 0.0, length(voxmats) )
    matrange = 1:length( voxmats )
    #    if ( myit > 1 ) matrange = 1 # length( voxmats ):length( voxmats )
    for ( i in matrange ) {
      if ( myit == 1 ) datanorm[ i ] = norm( voxmats[[ i ]], "F" )
      mytol = datanorm[ i ] * lineSearchTolerance / myit^2
      temperv = getSyMG( vmats[[i]], i, myw=myw, mixAlg = mixAlg ) # initialize gradient line search
      if ( myit <= iterations ) {
        temp = optimize( getSyME2, # computes the energy
                         interval = lineSearchRange, tol = mytol, gradient = temperv,
                         myw=myw, mixAlg = mixAlg, lowDimensionalError = lowDimensionalError )
        errterm[ i ] = temp$objective
        gamma[i] = temp$minimum
      } else errterm[ i ] = getSyME2( gamma[i], temperv, lowDimensionalError = lowDimensionalError  )
      vmats[[i]] = ( vmats[[i]] + temperv * gamma[i]  )
      #      temp = optim(
      #        vmats[[i]], fn=getSyME2, gr=getSyMG,
      #        method='BFGS', mixAlg=mixAlg, myw=myw  )
      #      vmats[[i]] = temp$par
      #      print( paste( i, temp$value ) )
      vmats[[i]] = orthogonalizeAndQSparsify(
        as.matrix( smoothingMatrices[[i]] %*% vmats[[i]] ),
        sparsenessQuantiles[i],
        orthogonalize = FALSE, positivity = positivities[i] )
      vmats[[i]] = vmats[[i]] / norm( voxmats[[i]] %*% vmats[[i]], "F" )
    }
    if ( verbose ) print( gamma )
    # project down to the basis U
    if ( myit <= ( iterations ) )
      for ( i in 1:length( voxmats ) ) {
        #        initialUMatrix[[i]] = voxmats[[i]] %*% vmats[[i]]
        initialUMatrix[[i]] = scale(voxmats[[i]] %*% vmats[[i]], TRUE, TRUE )
        initialUMatrix[[i]] = localGS( initialUMatrix[[i]], orthogonalize )
      }

    if ( hasRanEff ) {
      for ( kk in 1:1 ) {
        for ( i in 1:length( voxmats ) ) {
          avgU = initialUMatrix[[ 1 ]] * 0.0
          for ( j in 1:length(voxmats) )
            if ( i != j ) avgU = avgU + initialUMatrix[[ j ]]
            avgU = localGS( avgU , FALSE )
            dedrvX = ( t( voxmats[[i]] ) %*% zRan ) * (-1.0)
            dedrvX = dedrvX + vmats[[i]] %*% ( t( avgU ) %*% zRan )
            dedrvX = dedrvX + vRan[[i]] %*% ( t( zRan ) %*% zRan )
            if ( myit <= itThresh )
              gradnormsRanEff[ i ] = ( norm(voxmats[[i]]) / norm( dedrvX ) )
            vRan[[i]] = smoothingMatrices[[i]] %*%
              ( vRan[[i]] - gradnormsRanEff[ i ] * gamma[i] * dedrvX  )
        }
      }
    } # hasRanEff
    predictions = list()
    if ( FALSE ) {
      for ( i in 1:length( voxmats ) ) {
        if ( !hasRanEff ) {
          commonTerm = voxmats[[i]]
          predictions[[i]] = 0
        } else {
          predictions[[i]] = zRan %*% t(vRan[[i]])
          commonTerm = voxmats[[i]] - zRan %*% t(vRan[[i]])
        }
        for ( j in 1:length( voxmats ) )
          if ( i != j ) {
            commonTerm = commonTerm  - initialUMatrix[[j]] %*% t( vmats[[i]] )
            predictions[[i]] = predictions[[i]] + initialUMatrix[[j]] %*% t( vmats[[i]] )
          }
        errterm[ i ] = norm( voxmats[[i]] - predictions[[i]], npower )
        prednorm[ i ] = norm( predictions[[i]], npower )
        datanorm[ i ] = norm( voxmats[[i]], npower )
      }
    } else if ( FALSE ) {
      for ( jj in 1:(length( voxmats )-1) )
        for ( kk in (jj+1):length( voxmats ) ) {
          p1 = voxmats[[jj]] %*% vmats[[jj]]
          p2 = voxmats[[kk]] %*% vmats[[kk]]
          locr=abs(cor(p1,p2))
          print( paste( jj, kk ) )
          print( diag( locr ) )
        }
    }
    merr = mean(errterm)
    nzct = 0
    for ( loi in 1:length(vmats) )
      nzct = nzct+mean(abs(vmats[[loi]])) / length(vmats)
    tot = merr+nzct
    if ( verbose ) {
      print( paste( "myit =", myit, 'data-term', merr, 'Reg', nzct, 'tot', tot ))
      cat(c("e",errterm))
      cat("\n")
      #        cat(c("p",prednorm))
      #        cat("\n")
      #        cat(c("d",datanorm))
      #        cat("\n")
    }

  } # iterations
  return(
    list(
      u  = initialUMatrix,
      v  = vmats,
      vRan = vRan,
      initialRandomMatrix = randmat,
      predictions = predictions,
      finalError = errterm )
  )
}






#' Compute the low-dimensional u matrix for symilr
#'
#' SyMILR minimizes reconstruction error across related modalities.  One crucial
#' component of the reconstruction is the low-dimensional cross-modality basis.
#' This function computes that basis, given a mixing algorithm.
#'
#' @param projections A list that contains the low-dimensional projections.
#' @param i which modality to predict from the others.
#' @param mixingAlgorithm the elected mixing algorithm.  see \code{symilr}.  can
#' be 'svd', 'ica', 'rrpca-l', 'rrpca-s' or 'avg'.
#' @param initialW initialization matrix size \code{n} by \code{k} for fastICA.
#' @param orthogonalize boolean
#' @return u matrix for modality i
#' @author BB Avants.
#' @examples
#'
#' set.seed(1500)
#' nsub = 25
#' npix = c(100,200,133)
#' nk = 5
#' outcome = matrix(rnorm( nsub * nk ),ncol=nk)
#' outcome1 = matrix(rnorm( nsub * nk ),ncol=nk)
#' outcome2 = matrix(rnorm( nsub * nk ),ncol=nk)
#' u = symilrU( list( outcome, outcome1, outcome2 ), 2, 'avg' )
#'
#' @seealso \code{\link{symilr}}
#' @export
symilrU <- function( projections, i, mixingAlgorithm, initialW,
  orthogonalize = FALSE ) {
  # some gram schmidt code
  localGS <- function( x, orthogonalize = TRUE ) {
    if ( !orthogonalize ) return( x )
    n <- dim(x)[1]
    m <- dim(x)[2]
    q <- matrix(0,n,m)
    r <- matrix(0,m,m)
    qi <- x[,1]
    si <- sqrt(sum(qi ^ 2))
    q[,1] <- qi / si
    r[1,1] <- si
    for (i in 2:m) {
      xi <- x[,i]
      qj <- q[,1:(i - 1)]
      rj <- t(qj) %*% xi
      qi <- xi - qj %*% rj
      r[1:(i - 1),i] <- rj
      si <- sqrt(sum(qi ^ 2))
      q[,i] <- qi / si
      r[i,i] <- si
    }
    return( q )
  }
  avgU = NULL
  mixAlg = mixingAlgorithm
  nComponents = ncol( projections[[1]] )
  nmodalities = length( projections )
  wtobind = (1:nmodalities)[ -i ]
  if ( mixAlg == 'avg' ) {
    avgU = projections[[1]] * 0.0
    for ( j in wtobind )
      avgU = avgU + projections[[ j ]] / ( nmodalities - 1 )
    return( avgU )
  }
  nc = ncol( projections[[ 1 ]] )
  for ( j in wtobind )
    avgU = cbind( avgU, projections[[ j ]] )
  if ( mixAlg == 'rrpca-l' )
    basis = ( rsvd::rrpca( avgU, rand=F )$L[,1:nc] )
  else if ( mixAlg == 'rrpca-s' )
    basis = ( rsvd::rrpca( avgU, rand=F )$S[,1:nc] )
  else if ( mixAlg == 'ica' & ! missing( initialW ) )
    basis = ( fastICA::fastICA( avgU,  method = 'C', w.init = initialW,
                              n.comp=nc )$S )
  else if ( mixAlg == 'ica' & missing( initialW ) )
    basis = ( fastICA::fastICA( avgU,  method = 'C', n.comp=nc )$S )
  basis = ( svd( avgU, nu = nc, nv=0 )$u )
  if ( ! orthogonalize ) return( basis )
  return( localGS( basis ) )
}
