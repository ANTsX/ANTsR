#' No fail SVD function that switches to rsvd if svd fails
#'
#' This function performs SVD on a matrix using the built-in svd function in R.
#' The matrix will be divided by its maximum value before computing the SVD for 
#' the purposes of numerical stability (optional).
#' If svd fails, it automatically switches to random svd from the rsvd package.
#' svd may fail to converge when the matrix condition number is high; this can 
#' be checked with the kappa function.
#'
#' @param x Matrix to perform SVD on
#' @param nu Number of left singular vectors to return (default: min(nrow(x), ncol(x)))
#' @param nv Number of right singular vectors to return (default: min(nrow(x), ncol(x)))
#' @param dividebymax boolean
#'
#' @return A list containing the SVD decomposition of x
#'
#' @examples
#' avgU <- matrix(rnorm(100*50), nrow = 100, ncol = 50)
#' nc <- 10
#' u <- ba_svd( avgU, nu = nc, nv = 0)$u
#' @export
ba_svd <- function(x, nu = min(nrow(x), ncol(x)), nv = min(nrow(x), ncol(x)), dividebymax=FALSE ) {
  tryCatch(
    expr = {
      if ( dividebymax) {
        svd(x/max(x), nu = nu, nv = nv)
      } else svd(x, nu = nu, nv = nv)
    },
    error = function(e) {
      message("svd failed, using rsvd instead")
      if ( dividebymax) {
        rsvd(x/max(x), nu = nu, nv = nv)
      } else rsvd(x, nu = nu, nv = nv)
    }
  )
}


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
#' @param ncores number of cores to use
#' @param sinkhorn boolean
#' @param kPackage name of package to use for knn.  FNN is reproducbile but
#' RcppHNSW is much faster (with nthreads controlled by enviornment variable
#' ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS) for larger problems.  For large problems,
#' compute the regularization once and save to disk; load for repeatability.
#' @param verbose verbose output
#' @return matrix sparse p by p matrix is output with p by k nonzero entries
#' @author Avants BB
#' @references
#' \url{http://www.math.jhu.edu/~mauro/multiscaledatageometry.html}
#' @examples
#' \dontrun{
#' set.seed(120)
#' mat <- matrix(rnorm(60), ncol = 10)
#' smat <- sparseDistanceMatrix(mat, 2)
#' r16 <- antsImageRead(getANTsRData("r16"))
#' mask <- getMask(r16)
#' mat <- getNeighborhoodInMask(
#'   image = r16, mask = mask, radius = c(0, 0),
#'   physical.coordinates = TRUE, spatial.info = TRUE
#' )
#' smat <- sparseDistanceMatrix(t(mat$indices), 10) # close points
#' testthat::expect_is(smat, "Matrix")
#' testthat::expect_is(smat, "dgCMatrix")
#' testthat::expect_equal(sum(smat), 18017)
#' }
#' @export sparseDistanceMatrix
sparseDistanceMatrix <- function(
    x, k = 3, r = Inf, sigma = NA,
    kmetric = c("euclidean", "correlation", "covariance", "gaussian"),
    eps = 1.e-6, ncores = NA, sinkhorn = FALSE, kPackage = "RcppHNSW",
    verbose = FALSE) {
  myn <- nrow(x)
  if (k >= ncol(x)) k <- ncol(x) - 1
  if (any(is.na(x))) stop("input matrix has NA values")
  if (kPackage == "RcppHNSW") mypkg <- "RcppHNSW" else mypkg <- "FNN"
  # note that we can convert from distance to covariance
  #   d_ij^2 = sigma_i^2 +  \sigma_j^2  - 2 * cov_ij
  # and from correlation to covariance   diag(sd) %*% corrMat %*% diag(sd)
  # and from euclidean distance of standardized data to correlation
  # 1.0 - dist^2 / ( 2 * nrow( x ) )
  # TODO / FIXME - implement covariance
  if (!usePkg("Matrix")) {
    stop("Please install the Matrix package")
  }
  if (!usePkg(mypkg)) {
    stop(paste("Please install the", mypkg, "package"))
  }
  kmetric <- match.arg(kmetric)
  if (kmetric == "gaussian" & is.na(sigma)) {
    stop("Please set the sigma parameter")
  }
  cometric <- (kmetric == "correlation" | kmetric == "covariance")
  if (cometric & r == Inf) r <- -Inf
  #  if ( ! usePkg("irlba") )
  #    stop("Please install the irlba package")
  # see http://www.analytictech.com/mb876/handouts/distance_and_correlation.htm
  # euclidean distance to correlation - xin contains correlations
  ecor <- function(xin, nn) {
    1.0 - xin / (2 * nn)
  }
  if (kmetric == "covariance") mycov <- apply(x, FUN = sd, MARGIN = 2)
  if (cometric) {
    x <- scale(x, center = TRUE, scale = (kmetric == "correlation"))
  }
  if (mypkg[1] == "RcppHNSW") {
    nThreads <- as.numeric(Sys.getenv("ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS"))
    if (!is.na(ncores)) nThreads <- ncores
    efval <- min(c(4, ncol(x)))
    bknn <- RcppHNSW::hnsw_knn(t(x),
                               k = k, M = 16, ef = efval,
                               distance = "euclidean",
                               n_threads = nThreads,
                               grain_size = floor(ncol(x) / nThreads)
    )
    names(bknn) <- c("nn.idx", "nn.dists")
  }
  if (mypkg[1] == "FNN") {
    bknn <- FNN::get.knn(t(x), k = k, algorithm = "kd_tree")
    names(bknn) <- c("nn.idx", "nn.dists")
  }
  #  if ( mypkg[1] == "nabor" ) bknn = nabor::knn( t( x ) , k=k, eps=eps )
  #  if ( mypkg[1] == "RANN" )  bknn = RANN::nn2( t( x ) , k=k, eps=eps  )
  
  # if ( mypkg[1] == "rflann" )  {
  #   myncores = as.numeric( system('getconf _NPROCESSORS_ONLN', intern = TRUE) )
  #   if ( !is.na( ncores  ) ) myncores = ncores
  #   bknn = rflann::Neighbour( t(x), t(x), k=k, "kdtree", cores=myncores, 1 )
  #   names( bknn ) = c( "nn.idx", "nn.dists" )
  # }
  #  if ( mypkg[1] == "naborpar" ) bknn = .naborpar( t( x ), t( x ) , k=k, eps=eps  )
  if (cometric) {
    bknn$nn.dists <- ecor(bknn$nn.dists, myn)
  }
  tct <- 0
  for (i in 1:ncol(x))
  {
    inds <- bknn$nn.idx[i, ] # index
    locd <- bknn$nn.dists[i, ] # dist
    inds[inds <= i] <- NA # we want a symmetric matrix
    tct <- tct + sum(!is.na(inds))
  }
  # build triplet representation for sparse matrix
  myijmat <- matrix(NA, nrow = (tct), ncol = 3)
  tct2 <- 1
  for (i in 1:ncol(x))
  {
    inds <- bknn$nn.idx[i, ]
    locd <- bknn$nn.dists[i, ]
    if (kmetric == "gaussian" & !is.na(sigma)) {
      locd <- exp(-1.0 * locd / (2.0 * sigma^2))
    }
    inds[inds <= i] <- NA # we want a symmetric matrix
    tctinc <- sum(!is.na(inds))
    if (kmetric == "covariance") {
      loccov <- mycov[i] * mycov[inds]
      locd <- locd * loccov
    }
    if (tctinc > 0) {
      upinds <- tct2:(tct2 + tctinc - 1)
      myijmat[upinds, 1] <- i
      myijmat[upinds, 2] <- inds[!is.na(inds)]
      myijmat[upinds, 3] <- locd[!is.na(inds)]
      tct2 <- tct2 + tctinc
    }
  }
  kmatSparse <- Matrix::sparseMatrix(
    i = myijmat[, 1],
    j = myijmat[, 2],
    x = myijmat[, 3], symmetric = TRUE
  )
  if (kmetric == "gaussian") diag(kmatSparse) <- 1
  if (cometric) {
    if (kmetric == "covariance") diag(kmatSparse) <- mycov^2
    if (kmetric == "correlation") diag(kmatSparse) <- 1
    kmatSparse[kmatSparse < r] <- 0
  } else {
    kmatSparse[kmatSparse > r] <- 0
  }
  if (sinkhorn) {
    for (i in 1:4) {
      #      kmatSparse = kmatSparse / Matrix::rowSums( kmatSparse )
      #      kmatSparse = Matrix::t( Matrix::t(kmatSparse) / Matrix::rowSums( Matrix::t(kmatSparse) ) )
      kmatSparse <- kmatSparse / Matrix::colSums(kmatSparse)
      kmatSparse <- kmatSparse / Matrix::rowSums(kmatSparse)
    }
  }
  return(kmatSparse)
  #
  #  mysvd = irlba::partial_eigen( kmatSparse, nvec )
  #  sparsenessParam = ( range( abs( mysvd ) ) * 0.00001 )[ 2 ]
  #  sparsenessParam = 1e-6
  #
}


#' Whitening Matrix
#'
#' This function performs matrix whitening on the input matrix `X` using Singular Value Decomposition (SVD).
#' Whitening transforms the input matrix into one where the covariance matrix is the identity matrix.
#'
#' @param X A numeric matrix to be whitened. The matrix should have observations as rows and features as columns.
#' @return A list containing:
#' \item{whitened_matrix}{The whitened matrix where the covariance matrix is the identity matrix.}
#' \item{whitening_matrix}{The whitening transformation matrix used to whiten the input matrix.}
#' @examples
#' set.seed(123)
#' X <- matrix(rnorm(1000), nrow = 20, ncol = 50)  # Example with p = 50 and n = 20
#' result <- whiten_matrix(X)
#' X_whitened <- result$whitened_matrix
#' whitening_matrix <- result$whitening_matrix
#' # Verify that the covariance matrix of the whitened matrix is close to identity
#' cov_X_whitened <- cov(X_whitened)
#' print(round(cov_X_whitened, 2))
#' @export
whiten_matrix <- function(X) {
  # Center the matrix
  X_centered <- scale(X, center = TRUE, scale = FALSE)
  
  # Perform SVD
  svd_result <- ba_svd(X_centered)
  
  # Extract components
  U <- svd_result$u
  D <- svd_result$d
  V <- svd_result$v
  
  # Compute the whitening matrix
  D_inv_sqrt <- diag(1 / sqrt(D))
  whitening_matrix <- V %*% D_inv_sqrt %*% t(V)
  
  # Apply the whitening matrix to the centered data
  X_whitened <- X_centered %*% whitening_matrix
  
  return(list(whitened_matrix = X_whitened, whitening_matrix = whitening_matrix))
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
#' @param kPackage name of package to use for knn.  FNN is reproducbile but
#' RcppHNSW is much faster (with nthreads controlled by enviornment variable
#' ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS) for larger problems.  For large problems,
#' compute the regularization once and save to disk; load for repeatability.
#' @param ncores number of cores to use
#' @param verbose verbose output
#' @return matrix sparse p by q matrix is output with p by k nonzero entries
#' @author Avants BB
#' @references
#' \url{http://www.math.jhu.edu/~mauro/multiscaledatageometry.html}
#' @examples
#' \dontrun{
#' set.seed(120)
#' mat <- matrix(rnorm(60), nrow = 6)
#' mat2 <- matrix(rnorm(120), nrow = 6)
#' smat <- sparseDistanceMatrixXY(mat, mat2, 3)
#' smat2 <- sparseDistanceMatrixXY(mat2, mat, 3)
#' testthat::expect_is(smat, "Matrix")
#' testthat::expect_is(smat, "dgCMatrix")
#' testthat::expect_is(smat2, "Matrix")
#' testthat::expect_is(smat2, "dgCMatrix")
#' testthat::expect_equal(sum(smat), 154.628961265087)
#' testthat::expect_equal(sum(smat2), 63.7344262003899)
#' }
#' @export sparseDistanceMatrixXY
sparseDistanceMatrixXY <- function(x, y, k = 3, r = Inf, sigma = NA,
                                   kmetric = c("euclidean", "correlation", "covariance", "gaussian"),
                                   eps = 0.000001,
                                   kPackage = "RcppHNSW",
                                   ncores = NA,
                                   verbose = FALSE) # , mypkg = "nabor" )
{
  if (any(is.na(x))) stop("input matrix x has NA values")
  if (any(is.na(y))) stop("input matrix y has NA values")
  if (kPackage == "RcppHNSW") mypkg <- "RcppHNSW" else mypkg <- "FNN"
  if (!usePkg("Matrix")) {
    stop("Please install the Matrix package")
  }
  if (!usePkg(mypkg)) {
    stop(paste("Please install the", mypkg, "package"))
  }
  kmetric <- match.arg(kmetric)
  if (kmetric == "gaussian" & is.na(sigma)) {
    stop("Please set the sigma parameter")
  }
  cometric <- (kmetric == "correlation" | kmetric == "covariance")
  ecor <- function(xin) {
    1.0 - xin / (2 * nrow(x))
  }
  if (cometric) {
    x <- scale(x, center = TRUE, scale = (kmetric == "correlation"))
    y <- scale(y, center = TRUE, scale = (kmetric == "correlation"))
  }
  if (mypkg[1] == "RcppHNSW") {
    efval <- min(c(4, ncol(x)))
    nThreads <- as.numeric(Sys.getenv("ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS"))
    if (!is.na(ncores)) nThreads <- ncores
    ann <- RcppHNSW::hnsw_build(t(x),
                                distance = "euclidean",
                                M = 12,
                                ef = efval,
                                n_threads = nThreads,
                                grain_size = floor(ncol(x) / nThreads)
    )
    
    efval <- min(c(4, ncol(y)))
    bknn <- RcppHNSW::hnsw_search(t(y),
                                  ann,
                                  k = k,
                                  ef = efval,
                                  n_threads = nThreads,
                                  grain_size = floor(ncol(y) / nThreads)
    )
    names(bknn) <- c("nn.idx", "nn.dists")
  }
  if (mypkg[1] == "FNN") {
    knnalgs <- c("kd_tree", "brute")
    bknn <- FNN::get.knnx(t(x), t(y), k = k, algorithm = knnalgs[1])
    names(bknn) <- c("nn.idx", "nn.dists")
  }
  #  if ( mypkg[1] == "nabor" ) bknn = nabor::knn( t( y ), t( x ) , k=k, eps=eps )
  #  if ( mypkg[1] == "RANN" )  bknn = RANN::nn2( t( y ), t( x ) , k=k, eps=eps )
  # if ( mypkg[1] == "rflann" )  {
  #   myncores = as.numeric( system('getconf _NPROCESSORS_ONLN', intern = TRUE) )
  #   if ( !is.na( ncores  ) ) myncores = ncores
  #   bknn = rflann::Neighbour( t(y), t(x), k=k, "kdtree", cores=myncores, 1 )
  #   names( bknn ) = c( "nn.idx", "nn.dists" )
  # }
  
  
  #  if ( mypkg[1] == "naborpar" ) bknn = .naborpar( t( y ), t( x ) , k=k, eps=eps  )
  if (cometric) bknn$nn.dists <- ecor(bknn$nn.dists)
  tct <- 0
  nna <- rep(FALSE, nrow(bknn$nn.idx))
  #
  for (i in 1:nrow(bknn$nn.idx))
  {
    inds <- bknn$nn.idx[i, ] # index
    locd <- bknn$nn.dists[i, ] # dist
    nna[i] <- any(is.na(inds))
    tct <- tct + sum(!is.na(inds))
  }
  # build triplet representation for sparse matrix
  myijmat <- matrix(nrow = (tct), ncol = 3)
  tct2 <- 1
  for (i in 1:ncol(y))
  {
    inds <- bknn$nn.idx[i, ]
    locd <- bknn$nn.dists[i, ]
    if (kmetric == "gaussian" & !is.na(sigma)) {
      locd <- exp(-1.0 * locd / (2.0 * sigma^2))
    }
    tctinc <- sum(!is.na(inds))
    if (kmetric == "covariance") {
      locd <- cov(y[, i], x[, inds])
    } else if (kmetric == "correlation") {
      locd <- cor(y[, i], x[, inds])
    }
    if (tctinc > 0) {
      upinds <- tct2:(tct2 + tctinc - 1)
      myijmat[upinds, 1] <- i
      myijmat[upinds, 2] <- inds[!is.na(inds)]
      myijmat[upinds, 3] <- locd[!is.na(inds)]
      tct2 <- tct2 + tctinc
    }
  }
  kmatSparse <- Matrix::sparseMatrix(
    i = myijmat[, 1],
    j = myijmat[, 2],
    x = myijmat[, 3],
    dims = c(ncol(y), ncol(x)), symmetric = FALSE
  )
  if (cometric) {
    #    if ( kmetric == "covariance" )  diag( kmatSparse ) = mycov^2
    #    if ( kmetric == "correlation" ) diag( kmatSparse ) = 1
    kmatSparse[kmatSparse < r] <- 0
  } else {
    kmatSparse[kmatSparse > r] <- 0
  }
  return(kmatSparse)
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
#' \describe{
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
#' sphereDim <- 9
#' embeddDim <- 100
#' n <- 1000
#' if (usePkg("pracma")) {
#'   set.seed(20190919)
#'   sphereData <- pracma::rands(n, sphereDim, 1.)
#'   mysig <- 0.1
#'   spherEmbed <- matrix(rnorm(n * embeddDim, 0, mysig), nrow = n, ncol = embeddDim)
#'   spherEmbed[, 1:ncol(sphereData)] <- spherEmbed[, 1:ncol(sphereData)] + sphereData
#'   myr <- seq(1.0, 2.2, 0.05) # scales at which to sample
#'   mymssvd <- multiscaleSVD(spherEmbed, myr, locn = 5, nev = 20, plot = 1)
#'   if (getRversion() < "3.6.0") {
#'     testthat::expect_equal(mymssvd$noiseCutoffs, c(10, 11))
#'     cm <- unname(colMeans(mymssvd$evalsVsScale[11:25, ]))
#'     testthat::expect_equal(
#'       cm,
#'       c(
#'         0.133651668406975, 0.0985695151401464, 0.0914110478052329,
#'         0.086272017653314, 0.081188302173622, 0.0766100356616153, 0.0719736252996842,
#'         0.067588745051721, 0.0622331185687704, 0.0415236318358749, 0.0192976885668337,
#'         0.0183063537558787, 0.0174990088862745, 0.0170012938275551, 0.0163859378707545,
#'         0.0158265354487181, 0.0153357773252783, 0.0147933538908736, 0.0143510807701235,
#'         0.0140473978346935
#'       )
#'     )
#'   } else {
#'     testthat::expect_equal(mymssvd$noiseCutoffs, c(11, 15))
#'     cm <- unname(colMeans(mymssvd$evalsVsScale[13:25, ]))
#'     testthat::expect_equal(
#'       cm,
#'       c(
#'         0.138511257441516, 0.106071822485487, 0.0989441114152412, 0.092910922851038,
#'         0.0877970523897918, 0.0832570763653118, 0.0782599820599334, 0.0734433988152632,
#'         0.0678992413676906, 0.0432283615430504, 0.0202481578919003, 0.0191747572787057,
#'         0.0185718929604774, 0.0178301092823977, 0.0172423799670431, 0.0166981650233669,
#'         0.0162072551503541, 0.015784555784915, 0.0153600119986575, 0.0149084240854556
#'       )
#'     )
#'   }
#' }
#' @export multiscaleSVD
multiscaleSVD <- function(x, r, locn, nev, knn = 0, verbose = FALSE, plot = 0) {
  mresponse <- matrix(ncol = nev, nrow = length(r))
  n <- nrow(x)
  calcRowMatDist <- function(xmat, xrow) {
    locmag <- function(x, xrow) sqrt(sum((x - xrow)^2))
    apply(xmat, FUN = locmag, MARGIN = 1, xrow = xrow)
  }
  for (myscl in 1:length(r))
  {
    myr <- r[myscl]
    locsam <- sample(1:n, locn)
    myevs <- matrix(nrow = locn, ncol = nev)
    for (i in 1:locn)
    {
      rowdist <- calcRowMatDist(x, x[locsam[i], ])
      sel <- rowdist < myr
      if (sum(sel, na.rm = TRUE) > 2) {
        if (knn > 0 & sum(sel) > knn) # take a subset of sel
        {
          selinds <- sample(1:length(sel), knn)
          sel[-selinds] <- FALSE
        }
        lmat <- x[sel, ]
        if (nrow(lmat) < ncol(lmat)) lcov <- cov(t(lmat)) else lcov <- cov(lmat)
        temp <- ba_svd(lcov, nv = (nrow(lcov) - 1))$d # * embeddDim / sum(sel)
        # lcov = sparseDistanceMatrix( x, k = knn, kmetric = "cov" )
        #  temp = irlba::irlba( lcov, nv=(nrow(lcov)-1) )$d
        temp <- temp[1:min(c(nev, length(temp)))]
        if (length(temp) < nev) temp <- c(temp, rep(NA, nev - length(temp)))
      } else {
        temp <- rep(NA, nev)
      }
      myevs[i, 1:nev] <- temp
      if (i == locn) {
        mresponse[myscl, ] <- colMeans(myevs, na.rm = TRUE)
        if (verbose) {
          print(paste(i, "r", myr, "localN", sum(sel)))
          print(mresponse[myscl, ])
        }
      }
    }
  }
  colnames(mresponse) <- paste("EV", 1:nev, sep = "")
  rownames(mresponse) <- paste("Scale", 1:length(r), sep = "")
  # just use pam to cluster the eigenvalues
  goodscales <- !is.na(rowMeans(mresponse))
  temp <- t(mresponse)[1:nev, goodscales] # remove top evec
  pamk <- NA
  krng <- 5:min(dim(temp) - 1) # force a min of 4 clusters
  if (usePkg("fpc")) {
    pamk <- fpc::pamk(temp, krange = krng)$pamobject$clustering
  }
  ############################################################
  # noise dimensionality
  scaleEvalCorrs <- rep(NA, ncol(mresponse))
  for (i in 1:nev)
  {
    mylm <- lm(mresponse[, i] ~ stats::poly(r^2, 2))
    coffs <- coefficients(summary(mylm))
    scaleEvalCorrs[i] <- summary(mylm)$r.squared # coffs[2,4]
  }
  shiftinds <- c(2:ncol(mresponse), ncol(mresponse))
  delt <- scaleEvalCorrs - scaleEvalCorrs[shiftinds]
  qdelt <- quantile(delt[2:length(delt)], 0.9, na.rm = TRUE)
  noiseDim <- which(delt > qdelt) + 1
  # curvature dimensionality
  # find the dimensionality that maximizes the t-test difference across
  # the multi-scale eigenvalues
  myt <- rep(NA, nev)
  for (i in 3:(nev - 2))
  {
    lowinds <- 2:i
    hiinds <- (i + 1):nev
    myt[i] <- t.test(mresponse[, lowinds], mresponse[, hiinds], paired = FALSE)$sta
    #  myt[ i ] = t.test( scaleEvalCorrs[lowinds], scaleEvalCorrs[hiinds], paired=FALSE )$sta
  }
  dataDimCurv <- which.max(myt)
  # find singular values that do not grow with scale ( radius )
  if (length(r) > 4) {
    scaleEvalCorrs <- rep(NA, ncol(mresponse))
    ply <- 4 # power for polynomial model
    for (i in 1:dataDimCurv)
    {
      mylm <- lm(mresponse[, i] ~ stats::poly(r, ply))
      coffs <- coefficients(summary(mylm))
      #  print( summary( mylm ) )
      #  print( paste("EV",i) )
      #  Sys.sleep( 3 )
      # linear term coffs[2,4]
      # quadratic term coffs[3,4]
      # noise term coffs[5,4]
      scaleEvalCorrs[i] <- coffs[2, 4]
    }
    dataDim <- max(which(scaleEvalCorrs < 0.05))
  } else {
    dataDim <- 4
  }
  ############################################################
  if (plot > 0) {
    mycols <- rainbow(nev)
    growthRate1 <- mresponse[, 1]
    plot(r, growthRate1,
         type = "l", col = mycols[1], main = "Evals by scale",
         ylim = c(0.00, max(mresponse[, plot], na.rm = TRUE)),
         xlab = "ball-radius", ylab = "Expected Eval"
    )
    for (i in 2:ncol(mresponse))
    {
      growthRatek <- mresponse[, i] # magic :: shift(mresponse[,i],1)*0
      points(r, growthRatek, type = "l", col = mycols[i])
    }
  }
  return(
    list(
      dim            = c(dataDim, dataDimCurv),
      noiseCutoffs   = noiseDim,
      evalClustering = pamk,
      evalsVsScale   = mresponse
    )
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
#' @param segmentation optional boolean to restrict specific rows to have minimal respons
#' @param ... arguments passed to \code{sparseDistanceMatrix}
#' @return sparse matrix is output
#' @author Avants BB
#' @examples
#' \dontrun{
#' mask <- getMask(antsImageRead(getANTsRData("r16")))
#' spatmat <- t(imageDomainToSpatialMatrix(mask, mask))
#' smoothingMatrix <- knnSmoothingMatrix(spatmat, k = 25, sigma = 3.0)
#' rvec <- rnorm(nrow(smoothingMatrix))
#' srvec <- smoothingMatrix %*% rvec
#' rvi <- makeImage(mask, rvec)
#' srv <- makeImage(mask, as.numeric(srvec))
#' }
#' @export knnSmoothingMatrix
knnSmoothingMatrix <- function(x, k, sigma, segmentation, ...) {
  usePkg("Matrix")
  jmat <- sparseDistanceMatrix(x,
                               k = k, kmetric = "gaussian", sigma = sigma,
                               sinkhorn = FALSE, ...
  )
  if (!missing(segmentation) & FALSE) {
    diagSparse <- Matrix::sparseMatrix(
      i = 1:length(segmentation),
      j = 1:length(segmentation),
      x = as.numeric(segmentation), symmetric = TRUE
    )
    jmat <- diagSparse %*% jmat
  }
  if (FALSE) {
    for (i in 1:4) { # sinkhorn
      normalizer <- Matrix::rowSums(jmat)
      normalizer[normalizer == 0] <- Inf
      if (!missing(segmentation)) {
        normalizer[!segmentation] <- 1e9
      }
      jmat <- jmat / normalizer
      normalizer2 <- Matrix::rowSums(Matrix::t(jmat))
      normalizer2[normalizer2 == 0] <- Inf
      if (!missing(segmentation)) {
        normalizer2[!segmentation] <- 1e9
      }
      jmat <- Matrix::t(Matrix::t(jmat) / normalizer2)
    }
  }
  normalizer <- Matrix::rowSums(jmat)
  normalizer[normalizer == 0] <- Inf
  if (!missing(segmentation)) {
    normalizer[!segmentation] <- 1e9
  }
  jmat <- jmat / normalizer
  return(jmat)
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
#' \dontrun{
#' mask <- getMask(antsImageRead(getANTsRData("r16")))
#' spatmat <- t(imageDomainToSpatialMatrix(mask, mask))
#' smoomat <- knnSmoothingMatrix(spatmat, k = 5, sigma = 1.0)
#' mat <- matrix(rnorm(sum(mask) * 50), ncol = sum(mask), nrow = 50)
#' mat[1:25, 100:10000] <- mat[1:25, 100:10000] + 1
#' age <- rnorm(1:nrow(mat))
#' for (i in c(5000:6000, 10000:11000, 16000:17000)) {
#'   mat[, i] <- age * 0.1 + mat[, i]
#' }
#' gen <- c(rep("M", 25), rep("F", 12), rep("T", 13))
#' repmeas <- rep(c("A", "B", "C", "D", "E", "F", "G"), nrow(mat))[1:nrow(mat)]
#' mydf <- data.frame(age = scale(age), gen = gen)
#' fit <- smoothMatrixPrediction(
#'   x = mat, basisDf = mydf, iterations = 10,
#'   gamma = 1.e-6, sparsenessQuantile = 0.5,
#'   smoothingMatrix = smoomat, repeatedMeasures = repmeas,
#'   verbose = T
#' )
#' tt <- mat %*% fit$v
#' print(cor.test(mydf$age, tt[, 1]))
#' print(cor.test(fit$u[, "genM"], tt[, 2]))
#' vimg <- makeImage(mask, (fit$v[, 1]))
#' print(range(vimg) * 10)
#' plot(mask, vimg, window.overlay = range(abs(vimg)))
#' vimg <- makeImage(mask, (fit$v[, 2]))
#' print(range(vimg) * 10)
#' plot(mask, vimg, window.overlay = range(abs(vimg)))
#' }
#' @export smoothMatrixPrediction
smoothMatrixPrediction <- function(
    x,
    basisDf,
    modelFormula = as.formula(" x ~ ."),
    iterations = 10,
    gamma = 1.e-6,
    sparsenessQuantile = 0.5,
    positivity = c("positive", "negative", "either"),
    smoothingMatrix = NA,
    repeatedMeasures = NA,
    rowWeights = NA,
    LRR = NA,
    doOrth = FALSE,
    verbose = FALSE) {
  poschoices <- c("positive", "negative", "either", TRUE, FALSE)
  if (sum(positivity == poschoices) != 1 | length(positivity) != 1) {
    stop("choice of positivity parameter is not good - see documentation")
  }
  if (positivity == TRUE) positivity <- "positive"
  if (positivity == FALSE) positivity <- "either"
  smoothingWeight <- 1.0
  if (missing("x") | missing("basisDf")) {
    message("this function needs input")
    return(NA)
  }
  if (nrow(x) != nrow(basisDf)) {
    message("inconsistent row numbers between x and basisDf")
    return(NA)
  }
  if (!any(is.na(repeatedMeasures))) {
    usubs <- unique(repeatedMeasures)
    wtdf <- data.frame(table(repeatedMeasures))
    # rowWeights should scale with counts
    repWeights <- rep(0, length(repeatedMeasures))
    for (u in usubs) {
      repWeights[repeatedMeasures == u] <- 1.0 / wtdf$Freq[wtdf$repeatedMeasures == u]
    }
    rm(wtdf)
    if (all(is.na(rowWeights))) {
      rowWeights <- repWeights
    } else {
      rowWeights <- rowWeights * repWeights
    }
  }
  hasweights <- !all(is.na(rowWeights))
  if (hasweights) {
    locdf <- basisDf
    locdf$wts <- rowWeights
    wts <- "this is just a placeholder"
    mdl <- lm(as.formula(modelFormula),
              data = locdf, weights = wts, na.action = "na.exclude"
    )
    rm(locdf)
  } else {
    mdl <- lm(as.formula(modelFormula), data = basisDf, na.action = "na.exclude")
  }
  # bmdl = bigLMStats( mdl )
  u <- scale(model.matrix(mdl))
  intercept <- u[, 1]
  if (any(is.na(intercept))) intercept <- rep(0, length(intercept))
  u <- u[, -1]
  v <- antsrimpute(t(mdl$coefficients[-1, ]))
  v <- v + matrix(rnorm(length(v), 0, 0.01), nrow = nrow(v), ncol = ncol(v))
  if (!is.na(LRR)) {
    u <- lowrankRowMatrix(u, LRR)
    v <- t(lowrankRowMatrix(t(v), LRR))
    x <- icawhiten(x, LRR)
    #  x = lowrankRowMatrix( x, LRR )
  }
  if (hasweights & is.na(LRR)) {
    u <- diag(sqrt(rowWeights)) %*% u
    x <- diag(sqrt(rowWeights)) %*% x
  }
  intercept <- rowMeans(x - (u %*% t(v)))
  err <- mean(abs(x - (u %*% t(v) + intercept)))
  if (verbose) print(paste("iteration", 0, "err", err))
  tu <- t(u)
  tuu <- t(u) %*% u
  errs <- rep(NA, length(iterations))
  i <- 1
  while (i <= iterations) {
    v <- as.matrix(smoothingMatrix %*% v)
    dedv <- t(tuu %*% t(v) - tu %*% x)
    v <- v - dedv * gamma
    #  v = rsvd::rsvd( v )$v
    for (vv in 1:ncol(v)) {
      v[, vv] <- v[, vv] / sqrt(sum(v[, vv] * v[, vv]))
      if (vv > 1) {
        for (vk in 1:(vv - 1)) {
          temp <- v[, vk]
          denom <- sum(temp * temp, na.rm = TRUE)
          if (denom > 0) ip <- sum(temp * v[, vv]) / denom else ip <- 1
          v[, vv] <- v[, vv] - temp * ip
        }
      }
      localv <- v[, vv]
      doflip <- FALSE
      if (sum(localv > 0) < sum(localv < 0)) {
        localv <- localv * (-1)
        doflip <- TRUE
      }
      myquant <- quantile(localv, sparsenessQuantile, na.rm = TRUE)
      if (positivity == "positive") {
        if (myquant > 0) localv[localv <= myquant] <- 0 else localv[localv >= myquant] <- 0
      } else if (positivity == "negative") {
        localv[localv > myquant] <- 0
      } else if (positivity == "either") {
        localv[abs(localv) < quantile(abs(localv), sparsenessQuantile, na.rm = TRUE)] <- 0
      }
      if (doflip) v[, vv] <- localv * (-1) else v[, vv] <- localv
    }
    intercept <- rowMeans(x - (u %*% t(v)))
    if (!any(is.na(repeatedMeasures)) & is.na(LRR)) { # estimate random intercepts
      for (s in usubs) {
        usel <- repeatedMeasures == s
        intercept[usel] <- mean(intercept[usel], na.rm = TRUE)
      }
    }
    err <- mean(abs(x - (u %*% t(v) + intercept)))
    errs[i] <- err
    if (i > 1) {
      if ((errs[i] > errs[i - 1]) & (i == 3)) {
        #      message(paste("flipping sign of gradient step:", gamma))
        #      gamma = gamma * ( -1.0 )
      } else if ((errs[i] > errs[i - 1])) {
        gamma <- gamma * (0.5)
        if (verbose) message(paste("reducing gradient step:", gamma))
      }
      if (abs(gamma) < 1.e-9) i <- iterations
    }
    i <- i + 1
    if (verbose) print(paste(i, err))
  }
  if (verbose) print(paste("end", err))
  colnames(v) <- colnames(u)
  return(list(u = u, v = v, intercept = intercept))
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
#' \dontrun{
#' mask <- getMask(antsImageRead(getANTsRData("r16")))
#' spatmat <- t(imageDomainToSpatialMatrix(mask, mask))
#' smoomat <- knnSmoothingMatrix(spatmat, k = 200, sigma = 1.0)
#' mat <- matrix(rnorm(sum(mask) * 50), ncol = sum(mask), nrow = 50)
#' mat[1:25, 100:10000] <- mat[1:25, 100:10000] + 1
#' age <- rnorm(1:nrow(mat))
#' for (i in c(5000:6000, 10000:11000, 16000:17000)) {
#'   mat[, i] <- age * 0.1 + mat[, i]
#' }
#' sel <- 1:25
#' fit <- smoothRegression(
#'   x = mat[sel, ], y = age[sel], iterations = 10,
#'   sparsenessQuantile = 0.5,
#'   smoothingMatrix = smoomat, verbose = T
#' )
#' tt <- mat %*% fit$v
#' print(cor.test(age[-sel], tt[-sel, 1]))
#' vimg <- makeImage(mask, (fit$v[, 1]))
#' print(range(vimg) * 10)
#' plot(mask, vimg, window.overlay = range(abs(vimg)))
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
    verbose = FALSE) {
  x <- scale(x, scale = FALSE)
  poschoices <- c("positive", "negative", "either", TRUE, FALSE)
  if (sum(positivity == poschoices) != 1 | length(positivity) != 1) {
    stop("choice of positivity parameter is not good - see documentation")
  }
  if (positivity == TRUE) positivity <- "positive"
  if (positivity == FALSE) positivity <- "either"
  gamma <- 1.e-8
  if (missing("x") | missing("y")) {
    message("this function needs input")
    return(NA)
  }
  #
  if (all(is.na(smoothingMatrix))) smoothingMatrix <- diag(ncol(x))
  originalN <- ncol(x)
  if (!missing("extraPredictors")) {
    temp <- lm(y ~ ., data = data.frame(extraPredictors))
    mdlmatrix <- scale(model.matrix(temp)[, -1], scale = TRUE)
    extraN <- originalN + ncol(mdlmatrix)
    x <- cbind(x, mdlmatrix)
  }
  scaledY <- as.numeric(scale(y))
  xgy <- scaledY %*% x
  v <- matrix(0, nrow = nv, ncol = ncol(x))
  for (k in 1:nv) {
    v[k, ] <- xgy + matrix(rnorm(ncol(x), 0, 1.e-3), nrow = 1, ncol = ncol(x))
  }
  errs <- rep(NA, length(iterations))
  i <- 1
  while (i <= iterations) {
    temp <- t(x %*% t(as.matrix(v))) %*% x
    dedv <- temp * 0
    for (k in 1:nv) {
      dedv[k, ] <- xgy - temp[k, ]
    }
    v <- (v + dedv * gamma)
    v[, 1:originalN] <- as.matrix(v[, 1:originalN] %*% smoothingMatrix)
    for (k in 1:nv) {
      if (k > 1) {
        for (vk in 1:(k - 1)) {
          temp <- v[vk, ]
          denom <- as.numeric(temp %*% temp)
          if (denom > 0) ip <- as.numeric(temp %*% v[k, ]) / denom else ip <- 1
          v[k, ] <- v[k, ] - temp * ip
        }
      }
    }
    v[, 1:originalN] <- as.matrix(v[, 1:originalN] %*% smoothingMatrix)
    v <- t(orthogonalizeAndQSparsify(t(v), sparsenessQuantile, positivity))
    if (i < 3) gamma <- quantile(v[abs(v) > 0], 0.5, na.rm = TRUE) * 1.e-2
    proj <- x %*% t(v)
    intercept <- colMeans(scaledY - (proj))
    for (k in 1:nv) proj[, k] <- proj[, k] + intercept[k]
    ymdl <- lm(scaledY ~ proj)
    err <- mean(abs(scaledY - (proj)))
    errs[i] <- err # summary(ymdl)$r.squared * ( -1 )
    if (verbose) print(paste("it/err/rsq", i, errs[i], summary(ymdl)$r.squared, "gamma", gamma))
    #  coefwts = coefficients( ymdl )
    #  for ( k in 1:nv ) v[k,] = v[k,] * coefwts[k+1]
    #  proj = x %*% t( v ) # + coefwts[1]
    if (i > 1) {
      if ((mean(errs[4:5]) > mean(errs[1:3])) & (i == 5)) {
        #      message(paste("flipping sign of gradient step:", gamma))
        gamma <- gamma * (-1.0)
      } else if ((errs[i] > errs[i - 1])) {
        gamma <- gamma * (0.5)
        if (verbose) message(paste("reducing gradient step:", gamma))
      } else {
        gamma <- gamma * 1.05
      }
      if (abs(gamma) < 1.e-12) i <- iterations
    }
    i <- i + 1
  }
  if (verbose) print(paste("end", errs[i - 1]))
  imagev <- v[, 1:originalN]
  return(
    list(
      v = as.matrix(t(imagev)),
      fullv = as.matrix(t(v))
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
#' \dontrun{
#' mask <- getMask(antsImageRead(getANTsRData("r16")))
#' spatmat <- t(imageDomainToSpatialMatrix(mask, mask))
#' smoomat <- knnSmoothingMatrix(spatmat, k = 200, sigma = 1.0)
#' mat <- matrix(rnorm(sum(mask) * 50), ncol = sum(mask), nrow = 50)
#' mat[1:25, 100:10000] <- mat[1:25, 100:10000] + 1
#' age <- matrix(rnorm(nrow(mat) * 2), ncol = 2)
#' for (i in c(5000:6000, 10000:11000, 16000:17000)) {
#'   mat[, i] <- age[, 1] * 0.1 + mat[, i]
#' }
#' sel <- 1:25
#' fit <- smoothMultiRegression(
#'   x = mat[sel, ], y = age[sel, ], iterations = 10,
#'   sparsenessQuantile = 0.5,
#'   smoothingMatrixX = smoomat, smoothingMatrixY = NA, verbose = T
#' )
#' tt <- mat %*% fit$v
#' print(cor.test(age[-sel, 1], tt[-sel, 1]))
#' vimg <- makeImage(mask, (fit$v[, 1]))
#' print(range(vimg) * 10)
#' plot(mask, vimg, window.overlay = range(abs(vimg)))
#' }
#' @export smoothMultiRegression
## #' @param gamma step size for gradient descent
smoothMultiRegression <- function(
    x,
    y,
    iterations = 10,
    sparsenessQuantile = 0.5,
    positivity = FALSE,
    smoothingMatrixX = NA, smoothingMatrixY = NA,
    nv = 2,
    extraPredictors,
    verbose = FALSE) {
  x <- scale(x, scale = FALSE)
  y <- scale(y, scale = FALSE)
  poschoices <- c("positive", "negative", "either", TRUE, FALSE)
  if (sum(positivity == poschoices) != 1 | length(positivity) != 1) {
    stop("choice of positivity parameter is not good - see documentation")
  }
  if (missing("x") | missing("y")) {
    message("this function needs input")
    return(NA)
  }
  svdy <- scale(rsvd::rsvd(y, nu = nv)$u)
  loy <- as.numeric(svdy[, 1])
  ox <- smoothRegression(x, loy,
                         iterations = 50,
                         sparsenessQuantile = sparsenessQuantile, positivity = positivity,
                         smoothingMatrix = smoothingMatrixX, nv = nv, verbose = FALSE
  )$v
  oy <- matrix(nrow = ncol(y), ncol = nv)
  for (k in 1:iterations) {
    lox <- scale(x %*% (ox))
    for (j in 1:nv) {
      if (j == 1) rx <- x else rx <- residuals(lm(x ~ x %*% ox[, 1:(j - 1)]))
      if (j == 1) ry <- y else ry <- residuals(lm(y ~ y %*% oy[, 1:(j - 1)]))
      lox <- (rx %*% (ox))
      oy[, j] <- smoothRegression(ry, lox[, j],
                                  iterations = 50,
                                  sparsenessQuantile = sparsenessQuantile, positivity = positivity,
                                  smoothingMatrix = NA, nv = 2, verbose = FALSE
      )$v[, 1]
      loy <- (ry %*% (oy))
      ox[, j] <- smoothRegression(rx, loy[, j],
                                  iterations = 50,
                                  sparsenessQuantile = sparsenessQuantile, positivity = positivity,
                                  smoothingMatrix = NA, nv = 2, verbose = FALSE
      )$v[, 1]
    }
    tt <- x %*% ox
    ss <- y %*% oy
    print(k)
    print(cor(tt, ss))
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
#' \dontrun{
#' img <- antsImageRead(getANTsRData("r16"))
#' mask <- getMask(img)
#' simg <- knnSmoothImage(
#'   img = img, mask = mask, radius = 2, intensityWeight = 1,
#'   spatialSigma = 1.5, iterations = 1
#' )
#' }
#' @export knnSmoothImage
knnSmoothImage <- function(
    img,
    mask,
    radius,
    intensityWeight = 0.1,
    spatialSigma = 20.0,
    iterations = 1,
    returnMatrix = FALSE) {
  if (radius <= 0) {
    return(img)
  }
  ivec <- img[mask == 1]
  spatmat <- t(imageDomainToSpatialMatrix(mask, mask))
  spatrange <- range(spatmat, na.rm = TRUE)
  intrange <- range(ivec, na.rm = TRUE)
  idelt <- (intrange[2] - intrange[1])
  if (idelt <= 0) {
    scl <- 1
  } else {
    scl <- (spatrange[2] - spatrange[1]) / idelt * intensityWeight
  }
  ivec2 <- ivec * scl
  spatmat <- rbind(spatmat, ivec2)
  r <- radius
  #  imat = antsrimpute(
  #    getNeighborhoodInMask( img, mask, rep( r, img@dimension), boundary.condition='image' ) )
  #  imat = knnSmoothingMatrix( imat, k = 2*(r*2+1)^2, sigma = intensitySigma )
  jmat <- knnSmoothingMatrix(spatmat, k = (r * 2 + 1)^2, sigma = spatialSigma)
  #  return( jmat )
  #  image( jmat[4000:4500,4000:4500] )
  #  print( jmat[4000:4010,4000:4010] )
  #  imat = imat / Matrix::rowSums( imat )
  #  jmat = imat * smoothingMatrix
  for (i in 1:4) { # sinkhorn
    jmat <- jmat / Matrix::rowSums(jmat)
    jmat <- Matrix::t(Matrix::t(jmat) / Matrix::rowSums(Matrix::t(jmat)))
  }
  if (returnMatrix) {
    return(jmat)
  }
  for (i in 1:iterations) {
    ivec <- jmat %*% ivec
  }
  return(makeImage(mask, as.numeric(ivec)))
}






.xuvtHelper <- function(x, u, v, errs, iterations,
                        smoothingMatrix, repeatedMeasures, intercept,
                        positivity, gamma, sparsenessQuantile, usubs,
                        doOrth, verbose) {
  i <- 1
  tu <- t(u)
  tuu <- t(u) %*% u
  if (is.na(gamma)) gamma <- 1.e-6
  while (i <= iterations) {
    v <- as.matrix(smoothingMatrix %*% v)
    dedv <- t(tuu %*% t(v) - tu %*% x)
    v <- v + dedv * gamma
    #    if ( abs( doOrth ) >  Inf ) {
    #      vOrth = qr.Q( qr( v ) )
    #      v = v * ( 1 - doOrth ) - vOrth * doOrth
    #    }
    for (vv in 1:ncol(v)) {
      v[, vv] <- v[, vv] / as.numeric(sqrt(v[, vv] %*% v[, vv]))
      if (vv > 1 & doOrth) {
        for (vk in 1:(vv - 1)) {
          temp <- v[, vk]
          denom <- as.numeric(temp %*% temp)
          if (denom > 0) ip <- as.numeric(temp %*% v[, vv]) / denom else ip <- 1
          v[, vv] <- v[, vv] - temp * ip
        }
      }
      localv <- v[, vv]
      doflip <- FALSE
      if (sum(localv > 0) < sum(localv < 0)) {
        localv <- localv * (-1)
        doflip <- TRUE
      }
      myquant <- quantile(localv, sparsenessQuantile, na.rm = TRUE)
      if (positivity == "positive") {
        if (myquant > 0) localv[localv <= myquant] <- 0 else localv[localv >= myquant] <- 0
      } else if (positivity == "negative") {
        localv[localv > myquant] <- 0
      } else if (positivity == "either") {
        localv[abs(localv) < quantile(abs(localv), sparsenessQuantile, na.rm = TRUE)] <- 0
      }
      if (doflip) v[, vv] <- localv * (-1) else v[, vv] <- localv
      v[, vv] <- v[, vv] / sqrt(sum(v[, vv] * v[, vv]))
    }
    intercept <- rowMeans(x - (u %*% t(v)))
    if (!any(is.na(repeatedMeasures))) { # estimate random intercepts
      for (s in usubs) {
        usel <- repeatedMeasures == s
        intercept[usel] <- mean(intercept[usel], na.rm = TRUE)
      }
    }
    err <- mean(abs(x - (u %*% t(v) + intercept)))
    errs[i] <- err
    if (i > 1) {
      if ((errs[i] > errs[i - 1]) & (i == 3)) {
        #        message(paste("flipping sign of gradient step:", gamma))
        gamma <- gamma * (-1.0)
      } else if ((errs[i] > errs[i - 1])) {
        gamma <- gamma * (0.5)
        if (verbose) message(paste("reducing gradient step:", gamma))
      } else if (abs(gamma) < 1.e-9) i <- iterations
    }
    i <- i + 1
    if (verbose) print(paste(i, err))
  }
  return(list(v = v, intercept = intercept, gamma = gamma * 1.1))
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
#' \dontrun{
#' mat <- replicate(100, rnorm(20))
#' mat2 <- replicate(100, rnorm(20))
#' mat <- scale(mat)
#' mat2 <- scale(mat2)
#' wt <- 0.666
#' mat3 <- mat * wt + mat2 * (1 - wt)
#' params <- matrix(nrow = 2, ncol = 3)
#' params[1, ] <- c(1, 2, 1)
#' params[2, ] <- c(2, 1, 1)
#' x <- list((mat), (mat3))
#' jj <- jointSmoothMatrixReconstruction(x, 2, params,
#'   gamma = 1e-4, sparsenessQuantile = 0.5, iterations = 10,
#'   smoothingMatrix = list(NA, NA), verbose = TRUE
#' )
#'
#' # latent feature
#' mask <- getMask(antsImageRead(getANTsRData("r16")))
#' spatmat <- t(imageDomainToSpatialMatrix(mask, mask))
#' smoomat <- knnSmoothingMatrix(spatmat, k = 27, sigma = 120.0)
#' lfeats <- t(replicate(100, rnorm(3)))
#' # map these - via matrix - to observed features
#' n <- sum(mask)
#' ofeats1 <- (lfeats + rnorm(length(lfeats), 0.0, 1.0)) %*% rbind(rnorm(n), rnorm(n), rnorm(n))
#' ofeats2 <- (lfeats + rnorm(length(lfeats), 0.0, 1.0)) %*% rbind(rnorm(n), rnorm(n), rnorm(n))
#' # only half of the matrix contains relevant data
#' ofeats1[, 1:round(n / 2)] <- matrix(rnorm(round(n / 2) * 100), nrow = 100)
#' ofeats2[, 1:round(n / 2)] <- matrix(rnorm(round(n / 2) * 100), nrow = 100)
#' x <- list((ofeats1), (ofeats2))
#' jj <- jointSmoothMatrixReconstruction(x, 2, params,
#'   gamma = 0.0001, sparsenessQuantile = 0.75, iterations = 19,
#'   subIterations = 11,
#'   smoothingMatrix = list(smoomat, smoomat), verbose = TRUE
#' )
#' p1 <- ofeats2 %*% jj$v[[2]]
#' p2 <- ofeats1 %*% jj$v[[1]]
#' cor(p1, lfeats)
#' cor(p2, lfeats)
#' print(cor(rowMeans(ofeats1), lfeats))
#' print(cor(rowMeans(ofeats2), lfeats))
#'
#' # a 2nd example with 3 modalities
#' imageIDs <- c("r16", "r27", "r30", "r62", "r64", "r85")
#' images <- list()
#' feature1Images <- list()
#' feature2Images <- list()
#' feature3Images <- list()
#' ref <- antsImageRead(getANTsRData("r16"))
#' for (i in 1:length(imageIDs))
#' {
#'   cat("Processing image", imageIDs[i], "\n")
#'   tar <- antsRegistration(ref, antsImageRead(getANTsRData(imageIDs[i])),
#'     typeofTransform = "Affine"
#'   )$warpedmov
#'   images[[i]] <- tar
#'   feature1Images[[i]] <- iMath(images[[i]], "Grad", 1.0)
#'   feature2Images[[i]] <- iMath(images[[i]], "Laplacian", 1.0)
#'   feature3Images[[i]] <- reflectImage(tar, axis = 0, tx = "Affine")$warpedmovout
#' }
#' i <- 1
#' mask <- getMask(antsImageRead(getANTsRData(imageIDs[i])))
#' mask2 <- iMath(mask, "ME", 2)
#' spatmat <- t(imageDomainToSpatialMatrix(mask, mask))
#' smoomat <- knnSmoothingMatrix(spatmat, k = 125, sigma = 100.0)
#' spatmat2 <- t(imageDomainToSpatialMatrix(mask2, mask2))
#' smoomat2 <- knnSmoothingMatrix(spatmat2, k = 125, sigma = 100.0)
#' params <- matrix(nrow = 6, ncol = 3)
#' params[1, ] <- c(1, 2, 1)
#' params[2, ] <- c(2, 1, 1)
#' params[3, ] <- c(1, 3, 1)
#' params[4, ] <- c(3, 1, 1)
#' params[5, ] <- c(2, 3, 1)
#' params[6, ] <- c(3, 2, 1)
#' mat <- imageListToMatrix(feature1Images, mask)
#' mat2 <- imageListToMatrix(feature2Images, mask2)
#' mat3 <- imageListToMatrix(feature3Images, mask)
#' scl <- F
#' x <- list(scale(mat, scale = scl), scale(mat2, scale = scl), scale(mat3, scale = scl))
#' slist <- list(smoomat2, smoomat, smoomat, smoomat, smoomat, smoomat2)
#'
#' jj <- jointSmoothMatrixReconstruction(x, 4, params,
#'   positivity = T,
#'   gamma = 1e-6, sparsenessQuantile = 0.9, iterations = 10,
#'   smoothingMatrix = slist, verbose = TRUE
#' )
#' mm <- makeImage(mask, abs(jj$v[[2]][, 1])) %>% iMath("Normalize")
#' plot(ref, mm, doCropping = FALSE, window.overlay = c(0.1, 1))
#'
#' p1 <- mat2 %*% jj$v[[1]]
#' p2 <- mat %*% jj$v[[2]]
#' diag(cor(p1, p2))
#'
#' p1 <- mat3 %*% jj$v[[5]]
#' p2 <- mat2 %*% jj$v[[6]]
#' diag(cor(p1, p2))
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
    verbose = FALSE) {
  poschoices <- c("positive", "negative", "either", TRUE, FALSE)
  if (sum(positivity == poschoices) != 1 | length(positivity) != 1) {
    stop("choice of positivity parameter is not good - see documentation")
  }
  if (positivity == TRUE) positivity <- "positive"
  if (positivity == FALSE) positivity <- "either"
  
  for (k in 1:length(x)) x[[k]] <- x[[k]] / max(abs(x[[k]]))
  gammas <- rep(gamma, nrow(parameters))
  ulist <- list()
  vlist <- list()
  ilist <- list()
  if (!any(is.na(repeatedMeasures))) {
    usubs <- unique(repeatedMeasures)
    wtdf <- data.frame(table(repeatedMeasures))
    # rowWeights should scale with counts
    repWeights <- rep(0, length(repeatedMeasures))
    for (u in wtdf$usubs) {
      repWeights[repeatedMeasures == u] <- 1.0 / wtdf$Freq[wtdf$repeatedMeasures == u]
    }
    rm(wtdf)
    if (all(is.na(rowWeights))) {
      rowWeights <- repWeights
    } else {
      rowWeights <- rowWeights * repWeights
    }
  }
  
  for (i in 1:nrow(parameters)) {
    m1 <- parameters[i, 1]
    m2 <- parameters[i, 2]
    modelFormula <- as.formula(" x[[ m2 ]]  ~ .")
    basisDf <- data.frame(u = irlba::irlba(x[[m1]], nu = nvecs, nv = 0)$u)
    #    basisDf = data.frame( u=rsvd::rsvd( x[[ m1 ]], nu = nvecs, nv = 0 )$u )
    #    basisDf = data.frame( u=RSpectra::svds( x[[ m1 ]], nvecs )$u )
    mdl <- lm(modelFormula, data = basisDf)
    u <- model.matrix(mdl)
    ilist[[i]] <- u[, 1] # intercept
    u <- u[, -1]
    v <- mdl$coefficients[-1, ]
    v <- v + matrix(rnorm(length(v), 0, 0.01), nrow = nrow(v), ncol = ncol(v))
    v <- t(v)
    for (vv in 1:ncol(v)) {
      v[, vv] <- v[, vv] / sqrt(sum(v[, vv] * v[, vv]))
    }
    ulist[[i]] <- u
    vlist[[i]] <- v
  }
  errs <- rep(NA, length(iterations))
  if (verbose) print("part II")
  perr <- matrix(nrow = iterations, ncol = nrow(parameters))
  k <- 1
  while (k <= iterations) {
    for (i in 1:nrow(parameters)) {
      m1 <- parameters[i, 1]
      m2 <- parameters[i, 2]
      whichv <- NA
      for (pp in 1:nrow(parameters)) {
        if (parameters[pp, 2] == m1 & parameters[pp, 1] == m2) {
          whichv <- pp
        }
      }
      if (!is.na(whichv)) {
        temp <- vlist[[whichv]]
        ulist[[i]] <- (x[[m1]] %*% (temp))
      } else {
        ulist[[i]] <- ulist[[m2]]
        vlist[[i]] <- t(t(ulist[[m2]]) %*% x[[m2]])
      }
      if (is.logical(smoothingMatrix[[i]])) {
        loSmoo <- diag(ncol(x[[m2]]))
      } else {
        loSmoo <- smoothingMatrix[[i]]
      }
      temp <- .xuvtHelper(x[[m2]],
                          ulist[[i]], vlist[[i]],
                          errs,
                          iterations = subIterations,
                          smoothingMatrix = loSmoo,
                          repeatedMeasures = repeatedMeasures, ilist[[i]],
                          positivity = positivity, gammas[i] * parameters[i, 3],
                          sparsenessQuantile = sparsenessQuantile, usubs = usubs,
                          doOrth = doOrth, verbose = F
      )
      #    gammas[i] = temp$gamma * 1.0 # 5
      vlist[[i]] <- temp$v
      ilist[[i]] <- temp$intercept
      perr[k, i] <- mean(abs(x[[m2]] - (ulist[[i]] %*% t(vlist[[i]]) + ilist[[i]])))
    }
    if (verbose) {
      print(perr[k, ])
      print(paste("overall", mean(perr[k, ])))
    }
    if (k > 1) {
      e1 <- perr[k, ] * parameters[, 3]
      e2 <- perr[k - 1, ] * parameters[, 3]
      if (mean(e1) > mean(e2)) gammas <- gammas * 0.9 # k = iterations
    }
    
    for (i in 1:nrow(parameters)) {
      m1 <- parameters[i, 1]
      m2 <- parameters[i, 2]
      whichv <- NA
      for (pp in 1:nrow(parameters))
      {
        if (parameters[pp, 2] == m1 & parameters[pp, 1] == m2) {
          whichv <- pp
        }
      }
      if (!is.na(whichv)) {
        temp <- (vlist[[whichv]])
        ulist[[i]] <- (x[[m1]] %*% (temp))
      } else {
        ulist[[i]] <- ulist[[m2]]
        vlist[[i]] <- t(t(ulist[[m2]]) %*% x[[m2]])
      }
    }
    
    k <- k + 1
  }
  
  
  return(list(u = ulist, v = vlist, intercepts = ilist))
}



#' Optimize Binary Indicator Matrix with Row Uniformity
#'
#' This function optimizes the sum of the matrix `m * I`, where `I` is a binary indicator matrix 
#' with the constraint that each column may have only one non-zero entry. It ensures that the distribution 
#' of 1's is uniform across rows, softens the constraint to avoid infinite loops, and includes an optional 
#' verbose output to report the progress of the optimization.
#'
#' @param m A numeric matrix to optimize.
#' @param max_iter The maximum number of iterations to avoid infinite loops. Default is 1000.
#' @param tol A numeric value representing the tolerance for convergence. If the change in the 
#'        objective function is less than this value, the loop stops. Default is 1e-6.
#' @param preprocess Logical. If TRUE, flips the sign of each row where the entries are predominantly negative.
#' @param verbose Logical. If TRUE, reports the objective value and convergence progress at each iteration.
#' @return  `m * I`
#' @examples
#' set.seed(123)
#' m <- matrix(rnorm(500), nrow = 5)
#' result <- optimize_indicator_matrix(m, max_iter = 1000, tol = 1e-6, verbose = TRUE)
#' print(result)
#' @export
optimize_indicator_matrix <- function(m, max_iter = 1000, tol = 1e-6, preprocess = TRUE, verbose = FALSE) {
  if (preprocess) {
    # Pre-process by flipping rows where negative values dominate
    for (i in 1:nrow(m)) {
      if (sum(m[i, ] < 0) > sum(m[i, ] > 0)) {
        m[i, ] <- -m[i, ]
      }
    }
  }
  
  # Initialize the indicator matrix I with zeros
  I <- matrix(0, nrow = nrow(m), ncol = ncol(m))
  
  # Track the previous objective value
  prev_sum <- -Inf
  
  # Initialize iteration counter
  iter <- 0
  
  # While loop for optimizing the indicator matrix
  while (iter < max_iter) {
    iter <- iter + 1
    
    # Initialize I with zeros again in each iteration
    I <- matrix(0, nrow = nrow(m), ncol = ncol(m))
    
    # Assign one '1' in each column based on the largest positive entry in m
    for (j in 1:ncol(m)) {
      max_val <- -Inf
      selected_row <- NULL
      for (i in 1:nrow(m)) {
        if (m[i, j] > max_val && sum(I[i, ]) == 0) {  # Ensure no row gets multiple 1's
          max_val <- m[i, j]
          selected_row <- i
        }
      }
      if (!is.null(selected_row)) {
        I[selected_row, j] <- 1
      }
    }
    
    # Calculate the current objective value (sum of m * I)
    current_sum <- sum(m * I)
    
    # Check for convergence (if the objective value change is below tolerance)
    if (abs(current_sum - prev_sum) < tol) {
      if (verbose) message("Converged in ", iter, " iterations with objective value: ", current_sum)
      break
    }
    
    # Update previous sum
    prev_sum <- current_sum
    
    # If verbose, report the current status
    if (verbose) {
      message("Iteration: ", iter, " | Objective Value: ", current_sum)
    }
  }
  
  # If max iterations are reached, provide a message
  if (iter == max_iter && verbose) {
    message("Reached the maximum number of iterations (", max_iter, ") without full convergence.")
  }
  
  return( m * I )
}

#' Helper Function to Optimize Indicator Matrix with Best Sum
#'
#' This function runs the optimization on both the input matrix `m` and 
#' its negative counterpart `m * (-1)`, returning the result that maximizes the sum `sum(m * I)`.
#'
#' @param m A numeric matrix for which the optimization will be performed.
#' @param verbose Logical. If TRUE, reports the objective value and convergence progress at each iteration.
#' @return m*I
#' @examples
#' set.seed(123)
#' m <- matrix(rnorm(500), nrow = 5)
#' indicator_opt_both_ways(m)
#' @export
indicator_opt_both_ways <- function( m, verbose=FALSE ) {
  # Optimize for original matrix
  I_m <- optimize_indicator_matrix(m, preprocess = FALSE, verbose=verbose )
  sum_m <- sum(m * I_m)
  
  # Optimize for negated matrix
  I_neg_m <- optimize_indicator_matrix(-m, preprocess = FALSE, verbose=verbose )
  sum_neg_m <- sum(m * I_neg_m)  # Note: using original `m` to compute sum, not `-m`
  
  # Return the result with the maximum sum
  if (sum_m >= sum_neg_m) {
    return( m * I_m )
  } else {
    return( (-m) * I_neg_m )
  }
}

#' rank-based segmentation of a matrix
#'
#' @param v input matrix
#' @param sparsenessQuantile a value in zero to one
#' @param positivity one of positive, negative, either
#' @param basic simplest keep top k-entries in each row
#' @param transpose work on the transpose
#' @return matrix
#' @author Avants BB
#' @examples
#'
#' mat <- matrix(1:200, nrow = 10)
#' matr <- rankBasedMatrixSegmentation(mat, 0.9, basic = FALSE, positivity = "positive")
#'
#' @export rankBasedMatrixSegmentation
rankBasedMatrixSegmentation <- function(v, sparsenessQuantile, basic = FALSE, positivity = "positive", transpose = FALSE) {
  if ( ! basic ) {
    outmat = indicator_opt_both_ways( v, verbose=FALSE )
    return( outmat  )
  }
  if (transpose) v <- t(v)
  mycols <- 1:ncol(v)
  ntokeep <- round(quantile(mycols, 1.0 - sparsenessQuantile))
  outmat <- matrix(0, nrow = nrow(v), ncol = ncol(v))
  
  for (k in 1:nrow(v)) {
    row_values <- v[k, ]
    
    # Handle all-zero rows
    if (all(row_values == 0)) {
      next  # Leave this row as all zeros in outmat
    }
    
    if (positivity == "either") {
      locord <- order(abs(row_values), decreasing = TRUE)[1:ntokeep]
    } else if (positivity == "positive" | positivity == "negative" ) {
      # Clever handling of mixed signs for positive case
      pos_values <- row_values[row_values > 0]
      neg_values <- row_values[row_values < 0]
      
      if (length(pos_values) == 0 && length(neg_values) == 0) {
        next  # All values are zero, so skip this row
      }
      
      # Compare absolute sums of positive and negative values
      pos_sum <- sum(abs(pos_values))
      neg_sum <- sum(abs(neg_values))
      
      if (pos_sum >= neg_sum) {
        # Focus on positive values and zero out negatives
        locord <- order(row_values, decreasing = TRUE)[1:ntokeep]
        row_values <- pmax(row_values, 0)  # Ensure all negative values are zeroed out
      } else {
        # Focus on negative values, as they dominate
        locord <- order(-row_values, decreasing = TRUE)[1:ntokeep]
        row_values <- pmin(row_values, 0)  # Keep only negative values
      }
    }
    
    outmat[k, locord] <- row_values[locord]
  }
  
  if (transpose) {
    return(t(outmat))
  }
  return(outmat)
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
#' @param unitNorm set each vector to unit norm
#' @param sparsenessAlg NA is default otherwise basic, spmp or orthorank
#' @return matrix
#' @author Avants BB
#' @examples
#'
#' mat <- replicate(100, rnorm(20))
#' mat <- orthogonalizeAndQSparsify(mat)
#'
#' @export orthogonalizeAndQSparsify
orthogonalizeAndQSparsifyOld <- function(
    v,
    sparsenessQuantile = 0.5, positivity = "either",
    orthogonalize = TRUE, softThresholding = FALSE, unitNorm = FALSE, sparsenessAlg = NA) {
  if (!is.na(sparsenessAlg)) {
    if (sparsenessAlg == "orthorank") {
      return(rankBasedMatrixSegmentation(v, sparsenessQuantile, basic = FALSE, positivity = positivity, transpose = TRUE))
    } else {
      return(rankBasedMatrixSegmentation(v, sparsenessQuantile, basic = TRUE, positivity = positivity, transpose = TRUE))
    }
  }
  if (sparsenessQuantile == 0) {
    return(v)
  }
  epsval <- 0.0 # .Machine$double.eps
  #  if ( orthogonalize ) v = qr.Q( qr( v ) )
  binaryOrth <- function(x) { # BROKEN => DONT USE
    minormax <- function(x) {
      if (max(x) == 0) {
        return(which.min(x))
      }
      return(which.max(x))
    }
    b <- x * 0
    wm <- apply(abs(x), FUN = minormax, MARGIN = 2)
    for (i in 1:ncol(x)) b[wm[i], i] <- x[wm[i], i]
    for (i in 1:nrow(b)) b[i, ] <- b[i, ] / sqrt(sum(b[i, ]^2))
    b
  }
  for (vv in 1:ncol(v)) {
    if (var(v[, vv]) > epsval) {
      #      v[ , vv ] = v[ , vv ] / sqrt( sum( v[ , vv ] * v[ , vv ] ) )
      if (vv > 1 & orthogonalize) {
        for (vk in 1:(vv - 1)) {
          temp <- v[, vk]
          denom <- sum(temp * temp, na.rm = TRUE)
          if (denom > epsval) ip <- sum(temp * v[, vv]) / denom else ip <- 1
          v[, vv] <- v[, vv] - temp * ip
        }
      }
      localv <- v[, vv] # zerka
      doflip <- FALSE
      if (sum(localv > 0, na.rm = TRUE) < sum(localv < 0, na.rm = TRUE)) {
        localv <- localv * (-1)
        doflip <- TRUE
      }
      myquant <- quantile(localv, sparsenessQuantile, na.rm = TRUE)
      if (!softThresholding) {
        if (positivity == "positive") {
          if (myquant > 0) localv[localv <= myquant] <- 0 else localv[localv >= myquant] <- 0
        } else if (positivity == "negative") {
          localv[localv > myquant] <- 0
        } else if (positivity == "either") {
          localv[abs(localv) < quantile(abs(localv), sparsenessQuantile, na.rm = TRUE)] <- 0
        }
      } else {
        if (positivity == "positive") localv[localv < 0] <- 0
        mysign <- sign(localv)
        myquant <- quantile(abs(localv), sparsenessQuantile, na.rm = TRUE)
        temp <- abs(localv) - myquant
        temp[temp < 0] <- 0
        localv <- mysign * temp
      }
      if (doflip) v[, vv] <- localv * (-1) else v[, vv] <- localv
    }
    cthresh <- 100
    if (cthresh > 0) {
      #      temp = makeImage( , )
    }
  }
  if (unitNorm) {
    for (i in 1:ncol(v)) {
      locnorm <- sqrt(sum(v[, i]^2))
      if (locnorm > 0) v[, i] <- v[, i] / locnorm
    }
  }
  return(v)
}


#' Sparsify and optionally orthogonalize a matrix
#'
#' This function implements a quantile-based sparsification operation.
#'
#' @param v Input matrix
#' @param sparsenessQuantile Quantile to control sparseness - higher is sparser
#' @param positivity Restrict to positive or negative solution (beta) weights. Choices are "positive", "negative", or "either".
#' @param orthogonalize Run Gram-Schmidt if TRUE.
#' @param softThresholding Use soft thresholding if TRUE.
#' @param unitNorm Normalize each vector to unit norm if TRUE.
#' @param sparsenessAlg If specified, use a matrix partition algorithm ("orthorank", "spmp", "sum_preserving_matrix_partition" or "basic").
#' @return A sparsified and optionally orthogonalized matrix.
#' @examples
#' mat <- replicate(100, rnorm(20))
#' mat <- orthogonalizeAndQSparsify(mat)
#' @export
orthogonalizeAndQSparsify <- function(
    v,
    sparsenessQuantile = 0.5, positivity = "either",
    orthogonalize = TRUE, softThresholding = FALSE, unitNorm = FALSE, sparsenessAlg = NA
) {
  if (!is.na(sparsenessAlg)) {
    if ( sparsenessAlg %in% c("spmp","sum_preserving_matrix_partition") ) return( t(sum_preserving_matrix_partition( t(v) )) )
    basic <- sparsenessAlg != "orthorank"
    return(rankBasedMatrixSegmentation(v, sparsenessQuantile, basic = basic, positivity = positivity, transpose = TRUE))
  }
  if (sparsenessQuantile == 0) return(v)
  
  safequantile <- function(x, probs, na.rm = TRUE) {
    if (all(x <= 0)) {
      x <- -x
    }
    q <- quantile(x, probs, na.rm = na.rm)
    if (q == max(x, na.rm = na.rm)) {
      x <- sort(x)
      q <- x[tail(which(x < q), 1)]
    }
    return(q)
  }
  safe_thresholding <- function(x, threshold, op = "<") {
    if (op == "<") {
      result <- ifelse(x < threshold, 0, x)
    } else if (op == ">") {
      result <- ifelse(x > threshold, 0, x)
    } else {
      stop("Invalid operation. Only '<' or '>' allowed.")
    }
    
    if (all(result == 0)) {
      result <- x
    }
    
    return(result)
  }  
  epsval <- 0.0
  
  for (vv in 1:ncol(v)) {
    if (var(v[, vv]) > epsval) {
      if (vv > 1 && orthogonalize) {
        for (vk in 1:(vv - 1)) {
          temp <- v[, vk]
          denom <- sum(temp * temp, na.rm = TRUE)
          ip <- if (denom > epsval) sum(temp * v[, vv]) / denom else 1
          v[, vv] <- v[, vv] - temp * ip
        }
      }
      
      localv <- v[, vv]
      doflip <- sum(localv > 0, na.rm = TRUE) < sum(localv < 0, na.rm = TRUE)
      if (doflip) localv <- localv * -1
      
      myquant <- safequantile(localv, sparsenessQuantile, na.rm = TRUE)
      if (!softThresholding) {
        if (positivity == "positive") {
          localv[localv <= myquant] <- 0
        } else if (positivity == "negative") {
          localv[localv > myquant] <- 0
        } else {
          localvnz=abs(localv)
          localv[abs(localv) < safequantile(localvnz, sparsenessQuantile, na.rm = TRUE)] <- 0
        }
      } else {
        if (positivity == "positive") localv[localv < 0] <- 0
        mysign <- sign(localv)
        localvnz=abs(localv)
        myquant <- safequantile(localvnz, sparsenessQuantile, na.rm = TRUE)
        temp <- abs(localv) - myquant
        temp[temp < 0] <- 0
        localv <- mysign * temp
      }
      v[, vv] <- if (doflip) localv * -1 else localv
    }
  }
  
  if (unitNorm) {
    for (i in 1:ncol(v)) {
      locnorm <- sqrt(sum(v[, i]^2))
      if (locnorm > 0) v[, i] <- v[, i] / locnorm
    }
  }
  
  return(v)
}

#' Divide each column by the sum of column absolute values
#'
#' @param m A numeric matrix
#'
#' @return A matrix with each column divided by its sum
#'
divide_by_column_sum <- function(m) t(t(m)/colSums(abs(m)))

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
#' mat <- replicate(100, rnorm(20))
#' mat2 <- replicate(100, rnorm(20))
#' mat <- scale(mat)
#' mat2 <- scale(mat2)
#' wt <- 0.666
#' mat3 <- mat * wt + mat2 * (1 - wt)
#' jj <- smoothAppGradCCA(mat, mat3)
#'
#' @export smoothAppGradCCA
smoothAppGradCCA <- function(x, y,
                             smoox = NA, smooy = NA,
                             sparsenessQuantile = 0.5,
                             positivity = "either",
                             k = 2, iterations = 10,
                             stochastic = NA,
                             initialization = "randxy",
                             verbose = FALSE) {
  if (nrow(x) != nrow(y)) stop("nrow x should equal nrow y")
  #  x = scale( icawhiten(x,nrow(x)), scale=T )
  #  y = scale( icawhiten(y,nrow(y)), scale=T )
  x <- scale(x, scale = T)
  y <- scale(y, scale = T)
  errs <- rep(NA, iterations)
  poschoices <- c("positive", "negative", "either", TRUE, FALSE)
  if (sum(positivity == poschoices) != 1 | length(positivity) != 1) {
    stop("choice of positivity parameter is not good - see documentation")
  }
  if (positivity == TRUE) positivity <- "positive"
  if (positivity == FALSE) positivity <- "either"
  ratio <- norm(x) / norm(y)
  x <- x / norm(x)
  y <- y / norm(y)
  if (any(is.na(smoox))) smoox <- diag(ncol(x))
  if (any(is.na(smooy))) smooy <- diag(ncol(y))
  #  phix = RSpectra::svds( x, k=k )$v
  #  phiy = RSpectra::svds( y, k=k )$v
  #  phix = t( y ) %*% irlba::irlba( x, nu=k, nv=0, maxit=1000, tol=1.e-6 )$u
  #  phiy = t( x ) %*% irlba::irlba( y, nu=k, nv=0, maxit=1000, tol=1.e-6 )$u
  #  phix = t( y ) %*% svd( x, nu=k, nv=0  )$u
  #  phiy = t( x ) %*% svd( y, nu=k, nv=0  )$u
  if (initialization == "randxy") {
    phiy <- t(y) %*% (x %*% matrix(rnorm(k * ncol(x), 0, 1), ncol = k))
    phix <- t(x) %*% (y %*% matrix(rnorm(k * ncol(y), 0, 1), ncol = k))
  } else if (initialization == "svd") {
    phix <- ba_svd(x, nv = k, nu = 0)$v
    phiy <- ba_svd(y, nv = k, nu = 0)$v
  }
  #  phix = matrix( rnorm( k * ncol(x), 0, 1e-4 ), ncol=k )
  #  phiy = matrix( rnorm( k * ncol(y), 0, 1e-4 ), ncol=k )
  
  i <- 1
  if (is.na(stochastic)) stoke <- FALSE else stoke <- TRUE
  while (i < iterations) {
    if (stoke) ind <- sample(1:nrow(x))[1:stochastic] else ind <- 1:nrow(x)
    if (i < 3) gx <- -1e-4 # quantile( phix[ abs(phix) > 0 ] , 0.5 , na.rm=TRUE ) * ( -1.e4 )
    if (i < 3) gy <- -1e-4 # quantile( phiy[ abs(phiy) > 0 ] , 0.5 , na.rm=TRUE ) * ( -1.e4 )
    # gradient calculation
    delta <- t(x[ind, ]) %*% (x[ind, ] %*% phix - y[ind, ] %*% phiy)
    phix1 <- phix - delta * gx
    delta <- t(y[ind, ]) %*% (y[ind, ] %*% phiy - x[ind, ] %*% phix)
    phiy1 <- phiy - delta * gy
    #    phix1 = phix1 %*% dx
    #    phiy1 = phiy1 %*% dy
    phix1 <- as.matrix(smoox %*% orthogonalizeAndQSparsify(phix1, sparsenessQuantile = sparsenessQuantile, positivity = positivity))
    phiy1 <- as.matrix(smooy %*% orthogonalizeAndQSparsify(phiy1, sparsenessQuantile = sparsenessQuantile, positivity = positivity))
    # now update
    phix <- phix1 / norm(x %*% phix1)
    phiy <- phiy1 / norm(y %*% phiy1)
    errs[i] <- norm(y %*% phiy - x %*% phix)
    #  print(  sum( abs(  diag(  cor( y %*% phiy,  x %*% phix  ) ) ) ) )
    if (verbose) print(paste(i, errs[i]))
    #    if ( i > 15 ) if ( errs[ i ]  < errs[i-1] ) i = iterations+1
    i <- i + 1
  }
  
  #  print( cor( x %*% phix ) )
  #  print( cor( y %*% phiy ) )
  #  print( cor( y %*% phiy,  x %*% phix  ) )
  return(list(phix = phix, phiy = phiy))
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
#' nsub <- 12
#' npix <- 100
#' outcome <- rnorm(nsub)
#' covar <- rnorm(nsub)
#' mat <- replicate(npix, rnorm(nsub))
#' mat2 <- replicate(npix, rnorm(nsub))
#' myform <- " vox2 ~ covar + vox "
#' df <- data.frame(outcome = outcome, covar = covar)
#' result <- milr(df, list(vox = mat, vox2 = mat2), myform)
#'
#' \dontrun{
#' # a 2nd example with 3 modalities
#' imageIDs <- c("r16", "r27", "r30", "r62", "r64", "r85")
#' images <- list()
#' feature1Images <- list()
#' feature2Images <- list()
#' feature3Images <- list()
#' ref <- antsImageRead(getANTsRData("r16"))
#' mask <- ref * 0
#' for (i in 1:length(imageIDs)) {
#'   cat("Processing image", imageIDs[i], "\n")
#'   tar <- antsImageRead(getANTsRData(imageIDs[i]))
#'   images[[i]] <- tar
#'   mask <- mask + tar
#'   feature1Images[[i]] <- iMath(images[[i]], "Grad", 1.0)
#'   feature2Images[[i]] <- iMath(images[[i]], "Laplacian", 1.0)
#'   feature3Images[[i]] <- reflectImage(tar, axis = 0, tx = "Affine")$warpedmovout
#' }
#' i <- 1
#' mask <- getMask(mask)
#' spatmat <- t(imageDomainToSpatialMatrix(mask, mask))
#' smoomat <- knnSmoothingMatrix(spatmat, k = 23, sigma = 100.0)
#' mat <- imageListToMatrix(images, mask)
#' mat2 <- imageListToMatrix(feature1Images, mask)
#' mat3 <- imageListToMatrix(feature3Images, mask)
#' myscale <- function(x) {
#'   return(scale(x, center = TRUE, scale = T))
#' }
#' x <- list(x1 = myscale(mat[-5, ]), x2 = myscale(mat2[-5, ]), x3 = myscale(mat3[-5, ]))
#' df <- data.frame(m1 = rowMeans(x$x1), m2 = rowMeans(x$x2), m3 = rowMeans(x$x3))
#' myform <- " x1 ~ x2 + x3 + m2 + m3 "
#' result <- milr(df, x, myform,
#'   iterations = 32, smoothingMatrix = smoomat,
#'   gamma = 1e-1, verbose = T
#' )
#' k <- 1
#' mm <- makeImage(mask, (result$prediction[k, ])) %>% iMath("Normalize")
#' tar <- makeImage(mask, x$x1[k, ])
#' plot(mm, doCropping = FALSE)
#' plot(tar, doCropping = FALSE)
#' cor.test(result$prediction[k, ], x$x1[k, ])
#' myol <- makeImage(mask, abs(result$v[, "x2"])) %>% iMath("Normalize")
#' plot(tar, myol, doCropping = FALSE)
#' myol <- makeImage(mask, abs(result$v[, "m3"])) %>% iMath("Normalize")
#' plot(tar, myol, doCropping = FALSE)
#' result <- milr(df, x, myform,
#'   iterations = 11, smoothingMatrix = smoomat,
#'   sparsenessQuantile = 0.5, positivity = "positive",
#'   gamma = 1e-2, verbose = T
#' )
#' myol <- makeImage(mask, abs(result$v[, "x2"])) %>% iMath("Normalize")
#' plot(tar, myol, doCropping = FALSE, window.overlay = c(0.1, 1))
#' mm <- makeImage(mask, (result$prediction[k, ])) %>% iMath("Normalize")
#' tar <- makeImage(mask, x$x1[k, ])
#' plot(mm, doCropping = FALSE)
#' plot(tar, doCropping = FALSE)
#' cor.test(result$prediction[k, ], x$x1[k, ])
#' # univariate outcome
#' myform <- " m1 ~ x2 + x3 + m2 + m3 "
#' result <- milr(df, x, myform,
#'   iterations = 11, smoothingMatrix = smoomat,
#'   gamma = 1e-2, verbose = T
#' )
#' }
#'
#' @export milr
milr <- function(dataFrame, voxmats, myFormula, smoothingMatrix,
                 iterations = 10, gamma = 1.e-6,
                 sparsenessQuantile,
                 positivity = c("positive", "negative", "either"),
                 repeatedMeasures = NA,
                 orthogonalize = FALSE,
                 verbose = FALSE) {
  milrorth <- orthogonalize
  vdf <- data.frame(dataFrame)
  matnames <- names(voxmats)
  if (length(matnames) == 0) stop("please name the input list entries")
  n <- nrow(voxmats[[1]])
  p <- ncol(voxmats[[1]])
  if (missing(smoothingMatrix)) smoothingMatrix <- diag(p)
  poschoices <- c("positive", "negative", "either", TRUE, FALSE)
  if (!missing(positivity)) {
    if (sum(positivity == poschoices) != 1 | length(positivity) != 1) {
      stop("choice of positivity parameter is not good - see documentation")
    }
    if (positivity == TRUE) positivity <- "positive"
    if (positivity == FALSE) positivity <- "either"
  }
  for (k in 1:length(voxmats)) {
    vdf <- cbind(vdf, voxmats[[k]][, 1])
    names(vdf)[ncol(vdf)] <- matnames[k]
    if (ncol(voxmats[[k]]) != p) {
      stop(paste("matrix ", matnames[k], " does not have ", p, "entries"))
    }
  }
  # get names from the standard lm
  temp <- summary(lm(myFormula, data = vdf))
  myrownames <- rownames(temp$coefficients)
  mypvs <- matrix(rep(NA, p * length(myrownames)),
                  nrow = length(myrownames)
  )
  myestvs <- mypvs
  myervs <- mypvs
  mytvs <- mypvs
  outcomevarname <- trimws(unlist(strsplit(myFormula, "~"))[1])
  outcomevarnum <- which(outcomevarname == matnames)
  outcomeisconstant <- FALSE
  if (length(outcomevarnum) == 0) {
    outcomeisconstant <- TRUE
    outcomevarnum <- which(colnames(vdf) == outcomevarname)
  }
  hasRanEff <- FALSE
  vRan <- NA
  if (!any(is.na(repeatedMeasures))) {
    hasRanEff <- TRUE
    usubs <- unique(repeatedMeasures)
    if (length(repeatedMeasures) != nrow(dataFrame)) {
      stop("The length of the repeatedMeasures vector should equal the number of rows in the data frame.")
    }
    ranEff <- factor(repeatedMeasures)
    temp <- lm(rnorm(nrow(dataFrame)) ~ ranEff)
    temp <- model.matrix(temp)
    ranEffNames <- colnames(temp)
    ranEffNames[1] <- paste0("ranEff", as.character(levels(ranEff)[1]))
    temp[ranEff == levels(ranEff)[1], 1] <- 1
    temp[ranEff != levels(ranEff)[1], 1] <- 0
    zRan <- (temp[, ])
    colnames(zRan) <- ranEffNames
    tz <- t(zRan)
    tzz <- tz %*% zRan
    rm(ranEff)
  }
  mylm <- lm(myFormula, data = vdf)
  u <- (model.matrix(mylm)[, ])
  unms <- colnames(u)
  colnames(u) <- unms
  lvx <- length(voxmats)
  predictormatrixnames <- colnames(u)[colnames(u) %in% matnames]
  myks <- which(matnames %in% predictormatrixnames)
  v <- matrix(rnorm(ncol(u) * p, 1, 1), nrow = p, ncol = ncol(u)) * 0.0
  if (hasRanEff) {
    vRan <- matrix(rnorm(ncol(zRan) * p, 1, 1), nrow = p, ncol = ncol(zRan)) * 0.0
    dedrv <- vRan * 0
    colnames(vRan) <- ranEffNames
  }
  colnames(v) <- unms
  hasIntercept <- "(Intercept)" %in% colnames(v)
  if (hasIntercept) dospar <- 2:ncol(v) else dospar <- 1:ncol(v)
  for (i in 1:p) {
    if (length(myks) > 0) {
      for (k in 1:length(predictormatrixnames)) {
        u[, predictormatrixnames[k]] <- voxmats[[myks[k]]][, i]
      }
    }
    tu <- t(u)
    tuu <- t(u) %*% u
    if (outcomeisconstant) {
      myoc <- vdf[, outcomevarnum]
    } else {
      myoc <- voxmats[[outcomevarnum]][, i]
    }
    term2 <- tu %*% myoc
    v[i, ] <- (tuu %*% v[i, ] - term2)
  }
  v <- as.matrix(smoothingMatrix %*% v)
  if (!missing(sparsenessQuantile)) {
    v <- orthogonalizeAndQSparsify(v, sparsenessQuantile, positivity,
                                   orthogonalize = milrorth
    )
  }
  dedv <- v * 0
  predicted <- voxmats[[1]] * 0
  # now fix dedv with the correct voxels
  for (iter in 1:iterations) {
    err <- 0
    for (i in 1:p) {
      if (length(myks) > 0) {
        for (k in 1:length(predictormatrixnames)) {
          u[, predictormatrixnames[k]] <- voxmats[[myks[k]]][, i]
        }
      }
      tu <- t(u)
      tuu <- t(u) %*% u
      if (outcomeisconstant) {
        myoc <- vdf[, outcomevarnum]
      } else {
        myoc <- voxmats[[outcomevarnum]][, i]
      }
      term2 <- tu %*% myoc
      dedv[i, ] <- tuu %*% v[i, ] - term2
      if (hasRanEff) dedv[i, ] <- dedv[i, ] + (tu %*% zRan) %*% vRan[i, ]
      predicted[, i] <- u %*% (v[i, ])
      if (hasRanEff) predicted[, i] <- predicted[, i] + zRan %*% vRan[i, ]
      # based on this explanation
      # https://m-clark.github.io/docs/mixedModels/mixedModels.html
      # the energy term for the regression model is:
      #   norm( y - uvt ) =>  grad update is wrt v is  tuu * v - tu * y
      # the energy term for the mixed regression model is:
      #   norm( y - uvt - z_ran v_rant ) =>
      # this derivative z_ran * ( y - uvt - z_ran v_rant  )
      #   grad update wrt v is     tuu * v    + tu * zRan * vRan - tu * y
      #   grad update wrt vran is  tzz * vran + tz * uvt - tz * y
      err <- err + mean(abs(myoc - predicted[, i]))
    }
    v <- v - dedv * gamma
    v <- as.matrix(smoothingMatrix %*% v)
    if (!missing(sparsenessQuantile)) {
      v <- orthogonalizeAndQSparsify(v, sparsenessQuantile, positivity, orthogonalize = milrorth)
    }
    gammamx <- gamma * 0.1 # go a bit slower
    # simplified model here
    #      dedrv = t( ( t(zRan) %*% zRan ) %*% t( vRanX ) ) # t1
    #      dedrv = dedrvX + t( ( t(zRan) %*%  umat ) %*% t( vmat1 )  ) # t2
    #      dedrv = dedrv -  t( t(zRan) %*% voxmats[[1]] ) # t3
    #      vRan = smoothingMatrix %*% ( vRan + dedrvX * gammamx )
    
    
    if (hasRanEff) {
      vRan <- as.matrix(smoothingMatrix %*% vRan)
      # update random effects
      for (i in 1:p) {
        if (length(myks) > 0) {
          for (k in 1:length(predictormatrixnames)) {
            u[, predictormatrixnames[k]] <- voxmats[[myks[k]]][, i]
          }
        }
        tu <- t(u)
        tuu <- t(u) %*% u
        if (outcomeisconstant) {
          myoc <- vdf[, outcomevarnum]
        } else {
          myoc <- voxmats[[outcomevarnum]][, i]
        }
        predicted[, i] <- u %*% (v[i, ]) + zRan %*% vRan[i, ]
        #   grad update wrt vran is  tzz * vran + tz * uvt - tz * y
        #        rterm2 = tz %*% ( myoc - predicted[ , i ] ) # was this a bug or typo?  need to check math
        rterm2 <- tz %*% (myoc)
        dedrv[i, ] <- (tz %*% u) %*% v[i, ] + tzz %*% vRan[i, ] - rterm2
      }
      vRan <- vRan - dedrv * gammamx
    }
    if (verbose) print(err / p)
  }
  colnames(v) <- unms
  return(list(u = u, v = v, prediction = predicted, vRan = vRan))
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
#' nsub <- 24
#' npix <- 100
#' outcome <- rnorm(nsub)
#' covar <- rnorm(nsub)
#' mat <- replicate(npix, rnorm(nsub))
#' mat2 <- replicate(npix, rnorm(nsub))
#' mat3 <- replicate(npix, rnorm(nsub))
#' myform <- " vox2 ~ covar + vox + vox3 "
#' istr <- c(rep(TRUE, round(nsub * 2 / 3)), rep(FALSE, nsub - round(nsub * 2 / 3)))
#' df <- data.frame(outcome = outcome, covar = covar)
#' ltr <- list(vox = mat[istr, ], vox2 = mat2[istr, ], vox3 = mat3[istr, ])
#' lte <- list(vox = mat[!istr, ], vox2 = mat2[!istr, ], vox3 = mat3[!istr, ])
#' result <- milr(df[istr, ], ltr, myform)
#' pred <- milr.predict(result, df[istr, ], ltr, df[!istr, ], lte, myform)
#'
#' @seealso \code{\link{milr}}
#' @export milr.predict
milr.predict <- function(
    milrResult,
    dataFrameTrain,
    voxmatsTrain,
    dataFrameTest,
    voxmatsTest,
    myFormula) {
  matnames <- names(voxmatsTrain)
  vdf <- data.frame(dataFrameTrain)
  vdfTe <- data.frame(dataFrameTest)
  if (length(matnames) == 0) stop("please name the input list entries")
  outcomevarname <- trimws(unlist(strsplit(myFormula, "~"))[1])
  outcomevarnum <- which(outcomevarname == matnames)
  outcomeisconstant <- FALSE
  if (length(outcomevarnum) == 0) {
    outcomeisconstant <- TRUE
    outcomevarnum <- which(colnames(vdf) == outcomevarname)
  }
  # first build a unified training and testing dataFrame
  n <- nrow(voxmatsTrain[[1]])
  p <- ncol(voxmatsTrain[[1]])
  for (k in 1:length(voxmatsTrain)) {
    vdf <- cbind(vdf, voxmatsTrain[[k]][, 1])
    names(vdf)[ncol(vdf)] <- matnames[k]
    if (ncol(voxmatsTrain[[k]]) != p) {
      stop(paste("train matrix ", matnames[k], " does not have ", p, "entries"))
    }
    vdfTe <- cbind(vdfTe, voxmatsTest[[k]][, 1])
    names(vdfTe)[ncol(vdfTe)] <- matnames[k]
    if (ncol(voxmatsTest[[k]]) != p) {
      stop(paste("test matrix ", matnames[k], " does not have ", p, "entries"))
    }
  }
  
  # get names from the standard lm
  temp <- summary(lm(myFormula, data = vdf))
  myrownames <- rownames(temp$coefficients)
  mylm <- lm(myFormula, data = vdf)
  u <- model.matrix(mylm)
  unms <- colnames(u[, ])
  u[, -1] <- scale(u[, -1])
  colnames(u) <- unms
  lvx <- length(voxmatsTrain)
  predictormatrixnames <- colnames(u)[colnames(u) %in% matnames]
  myks <- which(matnames %in% predictormatrixnames)
  # compute low-dimensional representations from the milr result for train-test
  if (length(myks) > 0) {
    for (k in 1:length(predictormatrixnames)) {
      vdf[, predictormatrixnames[k]] <-
        voxmatsTrain[[myks[k]]] %*% milrResult$v[, predictormatrixnames[k]]
      vdfTe[, predictormatrixnames[k]] <-
        voxmatsTest[[myks[k]]] %*% milrResult$v[, predictormatrixnames[k]]
    }
  }
  if (outcomevarname %in% matnames) {
    vdf <- voxmatsTrain[[outcomevarname]] %*% milrResult$v
    vdfTe <- voxmatsTest[[outcomevarname]] %*% milrResult$v
    return(
      list(
        predictionTrain = NA,
        predictionTest = NA,
        lowDimensionalProjectionTrain = vdf,
        lowDimensionalProjectionTest = vdfTe
      )
    )
  } else {
    trmdl <- lm(myFormula, data = vdf)
    return(
      list(
        predictionTrain = predict(trmdl),
        predictionTest = predict(trmdl, newdata = vdfTe),
        lowDimensionalProjectionTrain = vdf[, predictormatrixnames],
        lowDimensionalProjectionTest = vdfTe[, predictormatrixnames]
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
#' nsub <- 12
#' npix <- 100
#' outcome <- rnorm(nsub)
#' covar <- rnorm(nsub)
#' mat <- replicate(npix, rnorm(nsub))
#' mat2 <- replicate(npix, rnorm(nsub))
#' nk <- 3
#' myform <- paste(
#'   " vox2 ~ covar + vox + ",
#'   paste0("mildBasis", 1:nk, collapse = "+")
#' ) # optional covariates
#' df <- data.frame(outcome = outcome, covar = covar)
#' result <- mild(df, list(vox = mat, vox2 = mat2),
#'   basisK = 3, myform,
#'   initializationStrategy = 10
#' )
#' result <- mild(df, list(vox = mat, vox2 = mat2),
#'   basisK = 3, myform,
#'   initializationStrategy = 4
#' )
#' myumat <- svd(mat2, nv = 0, nu = 3)$u
#' result <- mild(df, list(vox = mat, vox2 = mat2),
#'   basisK = 3, myform,
#'   initializationStrategy = 0
#' )
#'
#' @seealso \code{\link{milr}}
#' @export mild
mild <- function(dataFrame, voxmats, basisK,
                 myFormulaK, smoothingMatrix,
                 iterations = 10, gamma = 1.e-6,
                 sparsenessQuantile = 0.5,
                 positivity = c("positive", "negative", "either"),
                 initializationStrategy = 0,
                 repeatedMeasures = NA,
                 orthogonalize = FALSE,
                 verbose = FALSE) {
  positivity <- positivity[1]
  mildorth <- orthogonalize
  vdf <- data.frame(dataFrame)
  matnames <- names(voxmats)
  matnorm <- norm(voxmats[[1]])
  if (length(matnames) == 0) stop("please name the input list entries")
  n <- nrow(voxmats[[1]])
  p <- ncol(voxmats[[1]])
  if (missing(smoothingMatrix)) smoothingMatrix <- diag(p)
  poschoices <- c("positive", "negative", "either", TRUE, FALSE)
  if (!missing(positivity)) {
    if (sum(positivity == poschoices) != 1 | length(positivity) != 1) {
      stop("choice of positivity parameter is not good - see documentation")
    }
    if (positivity == TRUE) positivity <- "positive"
    if (positivity == FALSE) positivity <- "either"
  }
  for (k in 1:length(voxmats)) {
    vdf <- cbind(vdf, voxmats[[k]][, 1])
    names(vdf)[ncol(vdf)] <- matnames[k]
    if (ncol(voxmats[[k]]) != p) {
      stop(paste("matrix ", matnames[k], " does not have ", p, "entries"))
    }
  }
  outcomevarname <- trimws(unlist(strsplit(myFormulaK, "~"))[1])
  outcomevarnum <- which(outcomevarname == matnames)
  if (is.numeric(initializationStrategy)) {
    set.seed(initializationStrategy)
    initializationStrategy <- scale(qr.Q(qr(
      replicate(basisK, rnorm(nrow(voxmats[[1]])))
    )))
  }
  if (!is.matrix(initializationStrategy)) {
    stop("Please set valid initializationStrategy.")
  }
  for (k in 1:basisK) {
    if (k == 1) { # check matrix size
      stopifnot(nrow(initializationStrategy) == nrow(vdf))
      stopifnot(ncol(initializationStrategy) == basisK)
    }
    initvec <- initializationStrategy[, k]
    vdf <- cbind(vdf, initvec)
    names(vdf)[ncol(vdf)] <- paste0("mildBasis", k)
  }
  # augment the formula with the k-basis
  # get names from the standard lm
  temp <- summary(lm(myFormulaK, data = vdf))
  myrownames <- rownames(temp$coefficients)
  knames <- myrownames[grep("mildBasis", myrownames)]
  mypvs <- matrix(rep(NA, p * length(myrownames)),
                  nrow = length(myrownames)
  )
  myestvs <- mypvs
  myervs <- mypvs
  mytvs <- mypvs
  outcomeisconstant <- FALSE
  if (length(outcomevarnum) == 0) {
    outcomeisconstant <- TRUE
    outcomevarnum <- which(colnames(vdf) == outcomevarname)
  }
  hasRanEff <- FALSE
  vRan <- NA
  if (!any(is.na(repeatedMeasures))) {
    hasRanEff <- TRUE
    usubs <- unique(repeatedMeasures)
    if (length(repeatedMeasures) != nrow(dataFrame)) {
      stop("The length of the repeatedMeasures vector should equal the number of rows in the data frame.")
    }
    ranEff <- factor(repeatedMeasures)
    temp <- lm(rnorm(nrow(dataFrame)) ~ ranEff)
    temp <- model.matrix(temp)
    ranEffNames <- colnames(temp)
    ranEffNames[1] <- paste0("ranEff", as.character(levels(ranEff)[1]))
    temp[ranEff == levels(ranEff)[1], 1] <- 1
    temp[ranEff != levels(ranEff)[1], 1] <- 0
    zRan <- (temp[, ])
    colnames(zRan) <- ranEffNames
    tz <- t(zRan)
    tzz <- tz %*% zRan
    rm(ranEff)
  }
  mylm <- lm(myFormulaK, data = vdf)
  u <- (model.matrix(mylm)[, ])
  unms <- colnames(u)
  colnames(u) <- unms
  lvx <- length(voxmats)
  predictormatrixnames <- colnames(u)[colnames(u) %in% matnames]
  myks <- which(matnames %in% predictormatrixnames)
  v <- t(voxmats[[outcomevarnum]]) %*% u
  if (hasRanEff) {
    vRan <- matrix(rnorm(ncol(zRan) * p, 1, 1), nrow = p, ncol = ncol(zRan)) * 0.0
    dedrv <- vRan * 0
    colnames(vRan) <- ranEffNames
  }
  colnames(v) <- unms
  hasIntercept <- "(Intercept)" %in% colnames(v)
  if (hasIntercept) dospar <- 2:ncol(v) else dospar <- 1:ncol(v)
  
  for (i in 1:p) {
    if (length(myks) > 0) {
      for (k in 1:length(predictormatrixnames)) {
        u[, predictormatrixnames[k]] <- voxmats[[myks[k]]][, i]
      }
    }
    tu <- t(u)
    tuu <- t(u) %*% u
    if (outcomeisconstant) {
      myoc <- vdf[, outcomevarnum]
    } else {
      myoc <- voxmats[[outcomevarnum]][, i] / matnorm
    }
    term2 <- tu %*% myoc
    v[i, ] <- (tuu %*% v[i, ] - term2) * 0.01
  }
  v <- as.matrix(smoothingMatrix %*% v)
  v <- orthogonalizeAndQSparsify(v, sparsenessQuantile, positivity,
                                 orthogonalize = mildorth
  )
  dedv <- v * 0
  predicted <- voxmats[[1]] * 0
  # now fix dedv with the correct voxels
  for (iter in 1:iterations) {
    err <- 0
    v <- as.matrix(smoothingMatrix %*% v)
    for (i in 1:p) {
      if (length(myks) > 0) {
        for (k in 1:length(predictormatrixnames)) {
          u[, predictormatrixnames[k]] <- voxmats[[myks[k]]][, i]
        }
      }
      tu <- t(u)
      tuu <- t(u) %*% u
      if (outcomeisconstant) {
        myoc <- vdf[, outcomevarnum]
      } else {
        myoc <- voxmats[[outcomevarnum]][, i] / matnorm
      }
      term2 <- tu %*% myoc
      dedv[i, ] <- tuu %*% v[i, ] - term2
      if (hasRanEff) dedv[i, ] <- dedv[i, ] + (tu %*% zRan) %*% vRan[i, ]
      predicted[, i] <- u %*% (v[i, ])
      if (hasRanEff) predicted[, i] <- predicted[, i] + zRan %*% vRan[i, ]
      err <- err + mean(abs(myoc - predicted[, i]))
    }
    v <- v - dedv * gamma
    v <- as.matrix(smoothingMatrix %*% v)
    if (!missing(sparsenessQuantile)) {
      v <- orthogonalizeAndQSparsify(v, sparsenessQuantile, positivity,
                                     orthogonalize = mildorth
      )
    }
    gammamx <- gamma * 0.1 # go a bit slower
    if (hasRanEff) {
      vRan <- as.matrix(smoothingMatrix %*% vRan)
      # update random effects
      for (i in 1:p) {
        if (length(myks) > 0) {
          for (k in 1:length(predictormatrixnames)) {
            u[, predictormatrixnames[k]] <- voxmats[[myks[k]]][, i]
          }
        }
        tu <- t(u)
        tuu <- t(u) %*% u
        if (outcomeisconstant) {
          myoc <- vdf[, outcomevarnum]
        } else {
          myoc <- voxmats[[outcomevarnum]][, i] / matnorm
        }
        predicted[, i] <- u %*% (v[i, ]) + zRan %*% vRan[i, ]
        rterm2 <- tz %*% (myoc)
        dedrv[i, ] <- (tz %*% u) %*% v[i, ] + tzz %*% vRan[i, ] - rterm2
      }
      vRan <- vRan - dedrv * gammamx
    }
    # reset the u variables
    if (iter < iterations) {
      for (k in 1:length(knames)) {
        u[, knames[k]] <- (voxmats[[outcomevarnum]] %*% v[, knames[k]]) / matnorm
      }
      u[, knames] <- qr.Q(qr(u[, knames]))
    }
    if (verbose) print(err / p)
  }
  colnames(v) <- unms
  return(list(u = u, v = v, prediction = predicted, vRan = vRan))
}


.simlr3 <- function(dataFrame,
                    voxmats,
                    basisK,
                    myFormulaK,
                    smoothingMatrixX,
                    smoothingMatrixY,
                    iterations = 10, gamma = 1.e-6,
                    sparsenessQuantileX = 0.5,
                    sparsenessQuantileY = 0.5,
                    positivityX = c("positive", "negative", "either"),
                    positivityY = c("positive", "negative", "either"),
                    initializationStrategyX = list(seed = 0, matrix = NA, voxels = NA),
                    initializationStrategyY = list(seed = 0, matrix = NA, voxels = NA),
                    repeatedMeasures = NA,
                    verbose = FALSE) {
  ################################
  for (k in 1:length(voxmats)) {
    voxmats[[k]] <- scale(voxmats[[k]])
    voxmats[[k]] <- voxmats[[k]] / norm(voxmats[[k]])
  }
  myorth <- function(u) {
    vecorth <- function(v1, v2) {
      ni <- sum(v1 * v1)
      nip1 <- sum(v2 * v1)
      if (ni > 0) ratio <- nip1 / ni else ratio <- 1
      #    if ( cor( v1, v2 ) == 1 ) return( rnorm( length( v1 )))
      v2 <- v2 - v1 * ratio
      return(v2)
    }
    nc <- ncol(u)
    for (k in 1:(nc - 1)) {
      vi <- u[, k]
      for (i in (k + 1):nc) {
        vip1 <- u[, i]
        vip1 <- vecorth(vi, vip1)
        mynorm <- sum(vip1 * vip1)
        u[, i] <- vip1 / mynorm
      }
    }
    #  print( cor( u ) )
    return(u)
  }
  
  myorth2 <- function(x) {
    qr.Q(qr(x))
  }
  myorth3 <- function(u = NULL, basis = TRUE, norm = TRUE) {
    if (is.null(u)) {
      return(NULL)
    }
    if (!(is.matrix(u))) {
      u <- as.matrix(u)
    }
    p <- nrow(u)
    n <- ncol(u)
    if (prod(abs(La.svd(u)$d) > 1e-08) == 0) {
      stop("colinears vectors")
    }
    if (p < n) {
      warning("too much vectors to orthogonalize.")
      u <- as.matrix(u[, 1:p])
      n <- p
    }
    if (basis & (p > n)) {
      base <- diag(p)
      coef.proj <- crossprod(u, base) / diag(crossprod(u))
      base2 <- base - u %*% matrix(coef.proj, nrow = n, ncol = p)
      norm.base2 <- diag(crossprod(base2))
      base <- as.matrix(base[, order(norm.base2) > n])
      u <- cbind(u, base)
      n <- p
    }
    v <- u
    if (n > 1) {
      for (i in 2:n) {
        coef.proj <- c(crossprod(u[, i], v[, 1:(i - 1)])) / diag(crossprod(v[
          ,
          1:(i - 1)
        ]))
        v[, i] <- u[, i] - matrix(v[, 1:(i - 1)], nrow = p) %*%
          matrix(coef.proj, nrow = i - 1)
      }
    }
    if (norm) {
      coef.proj <- 1 / sqrt(diag(crossprod(v)))
      v <- t(t(v) * coef.proj)
    }
    return(v)
  }
  ################################
  orthogonalizeBasis <- TRUE
  n <- nrow(voxmats[[1]])
  p <- ncol(voxmats[[1]])
  q <- ncol(voxmats[[2]])
  if (missing(smoothingMatrixX)) smoothingMatrixX <- diag(p)
  if (missing(smoothingMatrixY)) smoothingMatrixY <- diag(q)
  xmatname <- names(voxmats)[1]
  ymatname <- names(voxmats)[2]
  formx <- paste(xmatname, myFormulaK)
  formy <- paste(ymatname, myFormulaK)
  locits <- 1
  ########################
  mildx <- mild(dataFrame,
                voxmats[1], basisK, formx, smoothingMatrixX,
                iterations = 1, gamma = gamma,
                sparsenessQuantile = sparsenessQuantileX,
                positivity = positivityX[[1]],
                initializationStrategy = initializationStrategyX,
                repeatedMeasures = repeatedMeasures,
                verbose = FALSE
  )
  colinds <- (ncol(mildx$u) - basisK + 1):ncol(mildx$u)
  ########################
  mildy <- mild(dataFrame,
                voxmats[2], basisK, formy, smoothingMatrixY,
                iterations = 1, gamma = gamma,
                sparsenessQuantile = sparsenessQuantileY,
                positivity = positivityY[[1]],
                initializationStrategy = initializationStrategyY,
                repeatedMeasures = repeatedMeasures,
                verbose = FALSE
  )
  xOrth <- mildy$u[, -1]
  yOrth <- mildx$u[, -1]
  #    xOrth = svd( antsrimpute( voxmats[[2]] %*% mildy$v[,-1] ) )$u
  #    yOrth = svd( antsrimpute( voxmats[[1]] %*% mildx$v[,-1] ) )$u
  # mildy$v = matrix(  rnorm( length( mildy$v ) ), nrow = nrow( mildy$v ) )
  # mildx$v = matrix(  rnorm( length( mildx$v ) ), nrow = nrow( mildx$v ) )
  for (i in 1:iterations) {
    if (orthogonalizeBasis == TRUE) {
      xOrthN <- myorth(voxmats[[2]] %*% mildy$v[, -1])
      yOrthN <- myorth(voxmats[[1]] %*% mildx$v[, -1])
      if (i == 1) {
        xOrth <- xOrthN
        yOrth <- yOrthN
      } else {
        wt1 <- 0.5
        wt2 <- 1.0 - wt1
        xOrth <- xOrth * wt1 + xOrthN * wt2
        yOrth <- yOrth * wt1 + yOrthN * wt2
      }
    }
    xOrth <- yOrth
    xorthinds <- c((ncol(xOrth) - basisK + 1):ncol(xOrth))
    colnames(xOrth[, xorthinds]) <- colnames(mildx$u[, colinds])
    colnames(yOrth[, xorthinds]) <- colnames(mildx$u[, colinds])
    dataFramex <- cbind(dataFrame, xOrth[, xorthinds])
    dataFramey <- cbind(dataFrame, yOrth[, xorthinds])
    dfinds <- c((ncol(dataFramex) - basisK + 1):ncol(dataFramex))
    colnames(dataFramex)[dfinds] <- colnames(mildx$u[, colinds])
    colnames(dataFramey)[dfinds] <- colnames(mildy$u[, colinds])
    mildx <- milr(dataFramex,
                  voxmats[1], formx, smoothingMatrixX,
                  iterations = locits, gamma = gamma * (1),
                  sparsenessQuantile = sparsenessQuantileX,
                  positivity = positivityX[[1]],
                  repeatedMeasures = repeatedMeasures,
                  verbose = FALSE
    )
    
    mildy <- milr(dataFramey,
                  voxmats[2], formy, smoothingMatrixY,
                  iterations = locits, gamma = gamma * (1),
                  sparsenessQuantile = sparsenessQuantileY,
                  positivity = positivityY[[1]],
                  repeatedMeasures = repeatedMeasures,
                  verbose = FALSE
    )
    ###############################################
    p1 <- antsrimpute(voxmats[[1]] %*% mildx$v[, -1])
    p2 <- antsrimpute(voxmats[[2]] %*% mildy$v[, -1])
    locor <- cor(p1, p2)
    overall <- mean(abs(diag(locor)))
    if (verbose & i > 0) {
      print(paste("it:", i - 1, "=>", overall))
      #    print( ( locor ) )
      #    print( diag( locor ) )
    }
  }
  return(list(simlrX = mildx, simlrY = mildy))
  
  
  if (FALSE) {
    xv <- mildx$v
    yv <- mildy$v
    xvup <- t(xOrth) %*% voxmats[[1]]
    yvup <- t(yOrth) %*% voxmats[[2]]
    xvup[, -colinds] <- 0
    yvup[, -colinds] <- 0
    xvup <- xvup / norm(xvup)
    yvup <- yvup / norm(yvup)
    # now make the above sparse
    xvup <- as.matrix(xvup[, ] %*% smoothingMatrixX)
    xv <- xv + t(xvup) * gamma
    xv <- as.matrix(smoothingMatrixX %*% xv[, ])
    xv <- orthogonalizeAndQSparsify(xv, sparsenessQuantileX, positivityX[[1]])
    xv <- as.matrix(smoothingMatrixX %*% xv[, ])
    
    # now make the above sparse
    yvup <- as.matrix(yvup[, ] %*% smoothingMatrixY)
    yv <- yv + t(yvup) * gamma
    yv <- as.matrix(smoothingMatrixY %*% yv[, ])
    yv <- orthogonalizeAndQSparsify(yv, sparsenessQuantileY, positivityY[[1]])
    yv <- as.matrix(smoothingMatrixY %*% yv[, ])
    
    mildy$u[, colinds] <- yOrth[, colinds] <- scale(voxmats[[1]] %*% (xv))[, colinds]
    mildx$u[, colinds] <- xOrth[, colinds] <- scale(voxmats[[2]] %*% (yv))[, colinds]
  } # end if
}





#' Initialize simlr
#'
#' Four initialization approaches for simlr.  Returns either a single matrix
#' derived from dimensionality reduction on all matrices bound together
#' (\code{jointReduction=TRUE}) or a list of reduced
#' dimensionality matrices, one for each input.  Primarily, a helper function
#' for SiMLR.
#'
#' @param voxmats list that contains the named matrices.
#' @param k rank of U matrix
#' @param jointReduction boolean determining whether one or length of list bases are returned.
#' @param zeroUpper boolean determining whether upper triangular part of
#' initialization is zeroed out
#' @param uAlgorithm either \code{"svd"}, \code{"random"}, \code{"randomProjection"}, \code{eigenvalue}, \code{"pca"} (default), \code{"ica"} or \code{"cca"}
#' @param addNoise scalar value that adds zero mean unit variance noise, multiplied
#' by the value of \code{addNoise}
#'
#' @return A single matrix or list of matrices
#' @author BB Avants.
#' @examples
#'
#' set.seed(1500)
#' nsub <- 3
#' npix <- c(10, 6, 13)
#' nk <- 2
#' outcome <- initializeSimlr(
#'   list(
#'     matrix(rnorm(nsub * npix[1]), ncol = npix[1]),
#'     matrix(rnorm(nsub * npix[2]), ncol = npix[2]),
#'     matrix(rnorm(nsub * npix[3]), ncol = npix[3])
#'   ),
#'   k = 2, uAlgorithm = "pca"
#' )
#'
#' @export
initializeSimlr <- function(
    voxmats, k, jointReduction = FALSE,
    zeroUpper = FALSE, uAlgorithm = "svd", addNoise = 0) {
  nModalities <- length(voxmats)
  localAlgorithm <- uAlgorithm
  if (uAlgorithm == "avg") {
    uAlgorithm <- "svd"
    localAlgorithm <- "svd"
  }
  if (uAlgorithm == "randomProjection" & jointReduction) {
    jointReduction <- FALSE
  }
  if (jointReduction) {
    X <- Reduce(cbind, voxmats) # bind all matrices
    if (uAlgorithm == "pca" | uAlgorithm == "svd") {
      X.pcr <- stats::prcomp(t(X), rank. = k, scale. = uAlgorithm == "svd") # PCA
      u <- (X.pcr$rotation)
    } else if (uAlgorithm == "ica") {
      u <- t(fastICA::fastICA(t(X), method = "C", n.comp = k)$A)
    } else if (uAlgorithm == "cca") {
      X <- Reduce(cbind, voxmats[-1]) # bind all matrices
      #      u = randcca( voxmats[[1]], X, k )$svd$u
      u <- sparseDecom2(list(voxmats[[1]], X),
                        sparseness = c(0.5, 0.5), nvecs = k, its = 3, ell1 = 0.1
      )$projections2
    } else {
      u <- replicate(k, rnorm(nrow(voxmats[[1]])))
    }
    if (addNoise > 0) u <- u + replicate(k, rnorm(nrow(voxmats[[1]]))) * addNoise
    if (zeroUpper) u[upper.tri(u)] <- 0
    return(u)
  }
  uOut <- list()
  uRand <- replicate(k, rnorm(nrow(voxmats[[1]])))
  for (s in 1:nModalities) {
    X <- Reduce(cbind, voxmats[-s])
    if (localAlgorithm == "pca " | localAlgorithm == "svd") {
      uOut[[s]] <- (stats::prcomp(t(X), rank. = k, scale. = uAlgorithm == "svd")$rotation)
    } else if (localAlgorithm == "ica") {
      uOut[[s]] <- t(fastICA::fastICA(t(X), method = "C", n.comp = k)$A)
    } else if (localAlgorithm == "cca") {
      uOut[[s]] <- sparseDecom2(list(X, voxmats[[s]]),
                                sparseness = c(0.5, 0.5), nvecs = k, its = 3, ell1 = 0.1
      )$projections2
    } else if (localAlgorithm == "randomProjection") {
      uOut[[s]] <- t((t(uRand) %*% voxmats[[s]]) %*% t(voxmats[[s]]))
      uOut[[s]] <- uOut[[s]] / norm(uOut[[s]], "F")
    } else {
      uOut[[s]] <- (stats::prcomp(t(X), rank. = k, scale. = uAlgorithm == "svd")$rotation)
    }
    if (addNoise > 0) uOut[[s]] <- uOut[[s]] + replicate(k, rnorm(nrow(voxmats[[1]]))) * addNoise
    if (zeroUpper) uOut[[s]][upper.tri(uOut[[s]])] <- 0
  }
  return(uOut)
}

#' Automatically produce regularization matrices for simlr
#'
#' @param x A list that contains the named matrices.
#' Note: the optimization will likely perform much more smoothly if the input
#' matrices are each scaled to zero mean unit variance e.g. by the \code{scale} function.
#' Note: x may also contain a mixture of raw matrix data and masks which are
#' binary antsImages. If a mask is passed, this function will assume the user
#' wants spatial regularization for that entry.
#' @param knn A vector of knn values (integers, same length as matrices)
#' @param fraction optional single scalar value to determine knn
#' @param sigma optional sigma vector for regularization (same length as matrices)
#' @param kPackage name of package to use for knn.  FNN is reproducbile but
#' RcppHNSW is much faster (with nthreads controlled by enviornment variable
#' ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS) for larger problems.  For large problems,
#' compute the regularization once and save to disk; load for repeatability.
#' @return A list of regularization matrices.
#' @author BB Avants.
#' @examples
#' # see simlr examples
#' @export
regularizeSimlr <- function(x, knn, fraction = 0.1, sigma, kPackage = "FNN") {
  getSpatialRegularization <- function(inmask, myk, mysig) {
    spatmat <- t(imageDomainToSpatialMatrix(inmask, inmask))
    regs <- knnSmoothingMatrix(spatmat,
                               k = myk,
                               sigma = mysig, kPackage = "FNN"
    )
    return(regs)
  }
  
  if (missing(knn)) {
    knn <- rep(NA, length(x))
    for (i in 1:length(x)) {
      temp <- round(fraction * ncol(x[[i]]))
      if (temp < 3) temp <- 3
      knn[i] <- temp
    }
  }
  if (missing(sigma)) sigma <- rep(10, length(x))
  slist <- list()
  for (i in 1:length(x)) {
    if (inherits(x[[i]], "antsImage")) {
      slist[[i]] <- getSpatialRegularization(x[[i]], knn[i], sigma[i])
    } else {
      slist[[i]] <- knnSmoothingMatrix(scale(data.matrix(x[[i]]), T, T),
                                       k = knn[i],
                                       sigma = sigma[i], kPackage = kPackage
      )
    }
  }
  return(slist)
}


#' predict output matrices from simlr solution
#'
#' @param x A list that contains the named matrices.
#' @param simsol the simlr solution
#' @param targetMatrix an optional index that sets a fixed target (outcome) matrix.
#' If not set, each basis set will be used to predict its corresponding matrix.
#' @param sourceMatrices an optional index vector that sets predictor matrices.
#' If not set, each basis set will be used to predict its corresponding matrix.
#' @param projectv boolean to determine whether raw u or x * v is used; default to TRUE which uses the x * v approach
#' @return A list of variance explained, predicted matrices and error metrics:
#' \describe{
#'   \item{varx: }{Mean variance explained for \code{u_i} predicting \code{x_i}.}
#'   \item{predictions: }{Predicted \code{x_i} matrix.}
#'   \item{initialErrors: }{Initial matrix norm.}
#'   \item{finalErrors: }{Matrix norm of the error term.}
#'   \item{aggregateTstats: }{Summary t-stats for each matrix and each \code{u_i}.}
#'   \item{uOrder: }{Estimated order of the \code{u_i} predictors aggregated over all terms.}
#' }
#' @author BB Avants.
#' @examples
#' # see simlr examples
#' @export
predictSimlr <- function(x, simsol, targetMatrix, sourceMatrices, projectv = TRUE) {
  if (missing(sourceMatrices)) {
    sourceMatrices <- 1:length(x)
  } else {
    if (!any(sourceMatrices %in% 1:length(x))) {
      stop("sourceMatrices are not within the range of the number of input matrices")
    }
  }
  varxfull <- list()
  initialErrors <- rep(0, length(sourceMatrices))
  finalErrors <- rep(0, length(sourceMatrices))
  predictions <- list()
  sortOrder <- list()
  if (projectv) {
    for (jj in 1:length(simsol$u)) {
      simsol$u[[jj]] <- x[[jj]] %*% simsol$v[[jj]]
    }
  }
  myBetas <- matrix(rep(0, ncol(simsol$u[[1]]) * length(sourceMatrices)),
                    ncol = ncol(simsol$u[[1]])
  )
  myBetasQ5 <- myBetas
  ct <- 1
  for (i in 1:length(sourceMatrices)) {
    if (missing(targetMatrix)) mytm <- i else mytm <- targetMatrix
    mdl <- lm(x[[mytm]] ~ simsol$u[[sourceMatrices[i]]])
    predictions[[i]] <- predict(mdl)
    smdl <- summary(mdl)
    blm <- bigLMStats(mdl, 0.0001)
    myBetas[i, ] <- rowMeans(abs(blm$beta.t))
    q5betas <- abs(blm$beta.t)
    q5betas[q5betas < quantile(q5betas, 0.5, na.rm = TRUE)] <- NA
    myBetasQ5[i, ] <- rowMeans(q5betas, na.rm = T)
    varxj <- rep(NA, length(smdl))
    for (j in 1:length(smdl)) {
      varxj[j] <- smdl[[j]]$r.squared
    }
    varxfull[[i]] <- varxj
    finalErrors[i] <- norm(predictions[[i]] - x[[mytm]], "F")
    initialErrors[i] <- norm(x[[mytm]], "F")
  }
  varx <- unlist(lapply(varxfull, FUN = "mean"))
  uOrder <- order(colSums(myBetas), decreasing = TRUE)
  list(
    varx = varx,
    varxfull = varxfull,
    predictions = predictions,
    initialErrors = initialErrors,
    finalErrors = finalErrors,
    aggregateTstats = myBetas,
    tstatsQ5 = myBetasQ5,
    rawTstats = blm$beta.t,
    uOrder = uOrder
  )
}

#' Calculate the invariant orthogonality defect that is zero for diagonal matrices
#'
#' @param A Input matrix (n x p, where n >> p)
#' @return The invariant orthogonality defect that is zero for diagonal matrices
#' @export
invariant_orthogonality_defect <- function( A ) 
{
  norm_A_F <- sqrt(sum(A^2))
  Ap <- A / norm_A_F
  AtA <- t(Ap) %*% Ap
  D <- diag(diag(AtA))
  defect <- sum((AtA - D)^2)
  return(defect)
}


#' Sum preserving matrix partition
#'
#' This function takes a matrix as input and partitions each column such that
#' only one entry in each row is non-zero, and the sums of each row are roughly
#' equivalent.
#'
#' @param X A matrix of size p x k
#' @param option An integer indicating whether to allow +/- values (option 1)
#'               or only + values (option 2). Default is 1.
#' @param tol A numeric value indicating the tolerance for the row sums.
#'            Default is 0.1.
#'
#' @return A matrix with segmented rows
#'
#' @details The function uses a greedy algorithm to select the entry with the
#'          maximum absolute value (or maximum positive value) in each column.
#'          The rows are then normalized to ensure that the sums are roughly
#'          equivalent.
#'
#' @examples
#' X <- matrix(rnorm(3000), nrow = 1000)
#' Y <- sum_preserving_matrix_partition(X, option = 1)
#' Y <- sum_preserving_matrix_partition(X, option = 2)
#'
#' @export
sum_preserving_matrix_partition <- function(X, option = 2, tol = 1e-3 ) {
  # Check if option is valid
  if (option != 1 & option != 2) {
    stop("Invalid option. Please choose 1 for +/- values or 2 for + values only.")
  }
  
  # Get dimensions of the matrix
  p <- nrow(X)
  k <- ncol(X)
  
  # Normalize row sums to 1
  row_sums <- rowSums(abs(X))
  row_sums[row_sums == 0] <- 1  # Avoid division by zero
  X <- X / row_sums
  
  # Initialize output matrix
  Y <- matrix(0, nrow = p, ncol = k)
  
  # Loop through each column
  for (j in 1:k) {
    # Get the absolute values of the column
    abs_col <- abs(X[, j])
    
    # Check if all entries in the column are zero
    if (all(abs_col == 0)) {
      next
    }
    
    # Get the index of the maximum absolute value
    max_idx <- which.max(abs_col)
    
    # If option 1, use the largest absolute value
    if (option == 1) {
      Y[max_idx, j] <- X[max_idx, j]
    } 
    # If option 2, use the largest positive value
    else if (option == 2) {
      pos_col <- X[, j][X[, j] > 0]
      if (length(pos_col) > 0) {
        max_idx_pos <- which.max(pos_col)
        Y[max_idx_pos, j] <- pos_col[max_idx_pos]
      } else {
        # If no positive values, use the smallest negative value
        neg_col <- X[, j][X[, j] < 0]
        if (length(neg_col) > 0) {
          min_idx_neg <- which.min(neg_col)
          Y[min_idx_neg, j] <- -neg_col[min_idx_neg]
        }
      }
    }
  }
  
  # Ensure that each row has at least one non-zero entry
  for (i in 1:p) {
    if (all(Y[i, ] == 0)) {
      # Find the column with the largest absolute value
      max_idx <- which.max(abs(X[i, ]))
      Y[i, max_idx] <- X[i, max_idx]
    }
  }
  # Normalize rows to ensure sums are roughly equivalent
  row_sums <- rowSums(abs(Y))
  row_sums[row_sums == 0] <- 1  # Avoid division by zero
  Y <- Y / row_sums * mean(row_sums, na.rm = TRUE)
  
  # Check if row sums are within tolerance
  if (any(abs(rowSums(abs(Y)) - mean(rowSums(abs(Y)), na.rm = TRUE)) > tol, na.rm = TRUE)) {
    warning("Row sums are not within tolerance. Consider adjusting tol parameter.")
  }
  
  return(Y)
}

#' Compute the Gradient of the Orthogonality Defect
#'
#' This function computes the gradient of the orthogonality defect for a matrix \code{A},
#' The orthogonality defect is defined as the sum of the squared off-diagonal elements 
#' of \code{t(A) \%*\% A}.
#'
#' @param A A numeric matrix.
#' 
#' @return A matrix representing the gradient of the orthogonality defect with respect to \code{Ap}.
#'
#' @export
gradient_invariant_orthogonality_defect <- function(A) {
  Ap = A / norm( A, "F")  
  AtA <- t(Ap) %*% Ap
  D <- diag(diag(AtA))
  orthogonality_diff <- AtA - D
  gradient <- 4 * (Ap %*% orthogonality_diff)
  return(gradient)
}


#' Gradient of the Invariant Orthogonality Defect Measure
#'
#' This function computes the gradient of the orthogonality defect measure with respect to the input matrix `A`.
#' The gradient is useful for optimization techniques that require gradient information. The gradient will be zero
#' for matrices where `AtA` equals the diagonal matrix `D`.
#'
#' @param A A numeric matrix.
#' @return A numeric matrix representing the gradient of the orthogonality defect measure.
#' @examples
#' # library(salad)
#' # A <- matrix(runif(20), nrow = 10, ncol = 2)
#' # gradient_invariant_orthogonality_salad(A)
#' @export
gradient_invariant_orthogonality_salad<- function(A) {
  #### place holder until we get the correct analytical derivative
  f1=invariant_orthogonality_defect
  if (!usePkg("salad")) {
    stop("Please install package hdf5r in order to use gradient_invariant_orthogonality_salad")
  }
  matrix( salad::d( f1( salad::dual(A) ) ), nrow=nrow(A) )
}



#' Similarity-driven multiview linear reconstruction model (simlr) for N modalities
#'
#' simlr minimizes reconstruction error across related modalities.  That is,
#' simlr will reconstruct each modality matrix from a basis set derived from
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
#' @param mixAlg 'svd', 'ica', 'rrpca-l', 'rrpca-s', 'stochastic', 'pca' or 'avg' denotes the algorithm employed when estimating the mixed modality bases
#' @param orthogonalize boolean to control whether we orthogonalize the solutions explicitly
#' @param repeatedMeasures list of repeated measurement identifiers. this will
#' allow estimates of per identifier intercept.
#' @param lineSearchRange lower and upper limit used in \code{optimize}
#' @param lineSearchTolerance tolerance used in \code{optimize}, will be multiplied by each matrix norm such that it scales appropriately with input data
#' @param randomSeed controls repeatability of ica-based decomposition
#' @param constraint one of none, Grassmann, GrassmannInv or Stiefel
#' @param energyType one of regression, normalized, lowRank, cca or ucca
#' @param vmats optional initial \code{v} matrix list
#' @param connectors a list ( length of projections or number of modalities )
#' that indicates which modalities should be paired with current modality
#' @param optimizationStyle one of \code{c("mixed","greedy","linesearch")}
#' @param scale options to standardize each matrix. e.g. divide by the square root
#' of its number of variables (Westerhuis, Kourti, and MacGregor 1998), divide
#' by the number of variables or center or center and scale or ... (see code).
#' can be a vector which will apply each strategy in order.
#' @param expBeta if greater than zero, use exponential moving average on gradient.
#' @param jointInitialization boolean for initialization options, default TRUE
#' @param sparsenessAlg NA is default otherwise basic, spmp or orthorank
#' @param verbose boolean to control verbosity of output - set to level \code{2}
#' in order to see more output, specifically the gradient descent parameters.
#' @return A list of u, x, y, z etc related matrices.
#' @author BB Avants.
#' @examples
#'
#' \dontrun{
#' set.seed(1500)
#' nsub <- 25
#' npix <- c(100, 200, 133)
#' nk <- 5
#' outcome <- matrix(rnorm(nsub * nk), ncol = nk)
#' outcome1 <- matrix(rnorm(nsub * nk), ncol = nk)
#' outcome2 <- matrix(rnorm(nsub * nk), ncol = nk)
#' outcome3 <- matrix(rnorm(nsub * nk), ncol = nk)
#' view1tx <- matrix(rnorm(npix[1] * nk), nrow = nk)
#' view2tx <- matrix(rnorm(npix[2] * nk), nrow = nk)
#' view3tx <- matrix(rnorm(npix[3] * nk), nrow = nk)
#' mat1 <- (outcome %*% t(outcome1) %*% (outcome1)) %*% view1tx
#' mat2 <- (outcome %*% t(outcome2) %*% (outcome2)) %*% view2tx
#' mat3 <- (outcome %*% t(outcome3) %*% (outcome3)) %*% view3tx
#' matlist <- list(vox = mat1, vox2 = mat2, vox3 = mat3)
#' result <- simlr(matlist)
#' p1 <- mat1 %*% (result$v[[1]])
#' p2 <- mat2 %*% (result$v[[2]])
#' p3 <- mat3 %*% (result$v[[3]])
#' regs <- regularizeSimlr(matlist)
#' result2 <- simlr(matlist)
#' pred1 <- predictSimlr(matlist, result)
#' pred2 <- predictSimlr(matlist, result2)
#'
#' # compare to permuted data
#' s1 <- sample(1:nsub)
#' s2 <- sample(1:nsub)
#' resultp <- simlr(list(vox = mat1, vox2 = mat2[s1, ], vox3 = mat3[s2, ]))
#' p1p <- mat1 %*% (resultp$v[[1]])
#' p2p <- mat2[s1, ] %*% (resultp$v[[2]])
#' p3p <- mat3[s2, ] %*% (resultp$v[[3]])
#'
#' # compare to SVD
#' svd1 <- svd(mat1, nu = nk, nv = 0)$u
#' svd2 <- svd(mat2, nu = nk, nv = 0)$u
#' svd3 <- svd(mat3, nu = nk, nv = 0)$u
#'
#' # real
#' range(cor(p1, p2))
#' range(cor(p1, p3))
#' range(cor(p3, p2))
#'
#' # permuted
#' range(cor(p1p, p2p))
#' range(cor(p1p, p3p))
#' range(cor(p3p, p2p))
#'
#' # svd
#' print(range(cor(svd1, svd2)))
#'
#' resultp <- simlr(list(vox = mat1, vox2 = mat2[s1, ], vox3 = mat3[s2, ]),
#'   initialUMatrix = nk, verbose = TRUE, iterations = 5,
#'   energyType = "normalized"
#' )
#' }
#' @seealso \code{\link{milr}} \code{\link{mild}} \code{\link{simlrU}}
#' @export simlr
simlr <- function(
    voxmats,
    smoothingMatrices,
    iterations = 10,
    sparsenessQuantiles,
    positivities,
    initialUMatrix,
    mixAlg = c("svd", "ica", "avg", "rrpca-l", "rrpca-s", "pca", "stochastic"),
    orthogonalize = FALSE,
    repeatedMeasures = NA,
    lineSearchRange = c(-1e10, 1e10),
    lineSearchTolerance = 1e-8,
    randomSeed,
    constraint = c( "Grassmannx1000x1000", "Stiefelx1000x1000", "orthox1000x1000", "none"),
    energyType = c("cca", "regression", "normalized", "ucca", "lowRank", "lowRankRegression",'rv_coefficient'),
    vmats,
    connectors = NULL,
    optimizationStyle = c("lineSearch", "mixed", "greedy"),
    scale = c("centerAndScale", "sqrtnp", "np", "center", "norm", "none", "impute", "eigenvalue", "robust", 'whiten', 'lowrank', 'rank'  ),
    expBeta = 0.0,
    jointInitialization = TRUE,
    sparsenessAlg = NA,
    verbose = FALSE) {
  parse_constraint <- function(x) {
    num1=num2=NA
    temp=unlist(strsplit( x, "x"))
    if ( length(temp) > 1 ) num1=as.numeric(temp[2])
    if ( length(temp) > 2 ) num2=as.numeric(temp[3])
    return(c(temp[1], as.numeric(num1), as.numeric(num2)))
  }
  if (missing(scale)) scale <- c("centerAndScale")
  if (missing(energyType)) energyType <- "cca"
  if (missing(mixAlg)) mixAlg <- "svd"
  if (missing(optimizationStyle)) optimizationStyle <- "lineSearch"
  if (!missing("randomSeed")) set.seed(randomSeed) #  else set.seed( 0 )
  energyType <- match.arg(energyType)
  constraint <- parse_constraint( constraint[1] )
  if ( verbose ) print(constraint)
  optimizationStyle <- match.arg(optimizationStyle)
  scalechoices = c(
    "sqrtnp", "np", "centerAndScale",
    "norm", "none", "impute", "eigenvalue", "center", "robust", 'lowrank','whiten','rank'
  )
  scaleList <- c()
  if (length(scale) == 1) {
    scaleList[1] <- match.arg(scale[1], choices = scalechoices )
  }
  if (length(scale) > 1) {
    for (kk in 1:length(scale)) {
      scaleList[kk] <- match.arg(scale[kk],
                                 choices = scalechoices
      )
    }
  }
  # \sum_i  \| X_i - \sum_{ j ne i } u_j v_i^t \|^2 + \| G_i \star v_i \|_1
  # \sum_i  \| X_i - \sum_{ j ne i } u_j v_i^t - z_r v_r^ T \|^2 + constraints
  #
  constrainG <- function(vgrad, i, constraint) {
    if (constraint == "Grassmann") {
      # grassmann manifold - see https://stats.stackexchange.com/questions/252633/optimization-with-orthogonal-constraints
      # Edelman, A., Arias, T. A., & Smith, S. T. (1998). The geometry of algorithms with orthogonality constraints. SIAM journal on Matrix Analysis and Applications, 20(2), 303-353.
      projjer <- diag(ncol(vgrad)) - t(vmats[[i]]) %*% vmats[[i]]
      return(vgrad %*% projjer)
    }
    if (constraint == "GrassmannInv") {
      # grassmann manifold - see https://stats.stackexchange.com/questions/252633/optimization-with-orthogonal-constraints
      temp = vmats[[i]] / norm( vmats[[i]], "F")
      projjer <- diag(ncol(vgrad)) - t(temp) %*% temp
      return(vgrad %*% projjer)
    }
    if (constraint == "Stiefel") { # stiefel manifold
      vgrad <- vgrad - vmats[[i]] %*% (t(vgrad) %*% (vmats[[i]]))
    }
    return(vgrad)
  }
  
  normalized <- FALSE
  ccaEnergy <- FALSE
  nModalities <- length(voxmats)
  if (missing(connectors)) {
    temp <- 1:nModalities
    connectors <- list()
    for (i in temp) {
      connectors[[i]] <- temp[-i]
    }
  }
  if (energyType == "normalized") {
    normalized <- TRUE
    ccaEnergy <- FALSE
  }
  if (energyType == "cca" | energyType == "ucca") {
    normalized <- FALSE
    ccaEnergy <- TRUE
  }
  mixAlg <- match.arg(mixAlg)
  # 0.0 adjust length of input data
  gamma <- rep(1, nModalities)
  if (missing(positivities)) {
    positivities <- rep("positive", nModalities)
  }
  if (any((positivities %in% c("positive", "negative", "either")) == FALSE)) {
    stop("positivities must be one of positive, negative, either")
  }
  if (length(positivities) == 1) {
    positivities <- rep(positivities[1], nModalities)
  }
  matnames <- p <- rep(NA, nModalities)
  for (i in 1:nModalities) {
    p[i] <- ncol(voxmats[[i]])
  }
  n <- nrow(voxmats[[1]])
  if (missing(sparsenessQuantiles)) {
    sparsenessQuantiles <- rep(0.5, nModalities)
  }
  
  lrbasis = length( voxmats )
  if ( ! missing( initialUMatrix ) ) {
    if ( is.integer(initialUMatrix) ) lrbasis=initialUMatrix
    if ( is.matrix( initialUMatrix ) ) lrbasis=ncol(initialUMatrix)
    if ( is.list( initialUMatrix ) ) if ( is.matrix(initialUMatrix[[1]])) 
      lrbasis=ncol(initialUMatrix[[1]])
  }
  
  # 1.0 adjust matrix norms
  if (!(any(scaleList == "none"))) {
    for (i in 1:nModalities) {
      if (any(is.null(voxmats[[i]])) | any(is.na(voxmats[[i]]))) {
        stop(paste("input matrix", i, "is null or NA."))
      }
      matnames <- names(voxmats)[i]
      if (any(is.na(voxmats[[i]]))) {
        voxmats[[i]][is.na(voxmats[[i]])] <- mean(voxmats[[i]], na.rm = T)
      }
      for (j in 1:length(scaleList)) {
        if (scaleList[j] == "norm") {
          voxmats[[i]] <- voxmats[[i]] / norm(voxmats[[i]], type = "F")
        }
        if (scaleList[j] == "np") {
          voxmats[[i]] <- voxmats[[i]] / prod(dim(voxmats[[i]]))
        }
        if (scaleList[j] == "sqrtnp") {
          voxmats[[i]] <- voxmats[[i]] / sqrt(prod(dim(voxmats[[i]])))
        }
        if (scaleList[j] == "center") {
          voxmats[[i]] <- base::scale(voxmats[[i]], center = TRUE, scale = FALSE)
        }
        if (scaleList[j] == "centerAndScale") {
          voxmats[[i]] <- base::scale(voxmats[[i]], center = TRUE, scale = TRUE)
        }
        if (scaleList[j] == "eigenvalue") {
          voxmats[[i]] <- voxmats[[i]] / sum(ba_svd(voxmats[[i]])$d)
        }
        if ( scaleList[j] %in% c("robust","rank") ) {
          voxmats[[i]] <- robustMatrixTransform(voxmats[[i]])
        }
        if (scaleList[j] == "whiten") {
          voxmats[[i]] <- whiten_matrix( data.matrix(voxmats[[i]]) )$whitened_matrix
        }
        if (scaleList[j] == "lowrank") {
          voxmats[[i]] <- lowrankRowMatrix( data.matrix(voxmats[[i]]), lrbasis*2 )
        }
      }
    }
  }
  
  # 3.0 setup regularization
  if (missing(smoothingMatrices)) {
    smoothingMatrices <- list()
    for (i in 1:nModalities) {
      smoothingMatrices[[i]] <- diag(ncol(voxmats[[i]]))
    }
  }
  for (i in 1:length(smoothingMatrices)) {
    if (any(is.null(smoothingMatrices[[i]])) | any(is.na(smoothingMatrices[[i]]))) {
      stop(paste("smoothing-matrix", i, "is null or NA."))
    }
    smoothingMatrices[[i]] <- smoothingMatrices[[i]] /
      Matrix::rowSums(smoothingMatrices[[i]])
  }
  # some gram schmidt code
  localGS <- function(x, orthogonalize = TRUE) {
    if (!orthogonalize) {
      return(x)
    }
    n <- dim(x)[1]
    m <- dim(x)[2]
    q <- matrix(0, n, m)
    r <- matrix(0, m, m)
    qi <- x[, 1]
    si <- sqrt(sum(qi^2))
    if (si < .Machine$double.eps) si <- 1
    q[, 1] <- qi / si
    r[1, 1] <- si
    for (i in 2:m) {
      xi <- x[, i]
      qj <- q[, 1:(i - 1)]
      rj <- t(qj) %*% xi
      qi <- xi - qj %*% rj
      r[1:(i - 1), i] <- rj
      si <- sqrt(sum(qi^2))
      if (si < .Machine$double.eps) si <- 1
      q[, i] <- qi / si
      r[i, i] <- si
    }
    return(q)
    return(list(q = q, r = r))
  }
  randmat <- 0
  
  # 4.0 setup initialization
  if (missing(initialUMatrix)) {
    initialUMatrix <- nModalities
  }
  
  if (is.matrix(initialUMatrix)) {
    randmat <- initialUMatrix
    initialUMatrix <- list()
    for (i in 1:nModalities) {
      initialUMatrix[[i]] <- randmat
    }
  } else if (is.numeric(initialUMatrix)) {
    if (jointInitialization) {
      temp <- initializeSimlr(voxmats, initialUMatrix, uAlgorithm = mixAlg, jointReduction = jointInitialization)
      initialUMatrix <- list()
      for (i in 1:nModalities) initialUMatrix[[i]] <- temp
    } else {
      initialUMatrix <- initializeSimlr(voxmats, initialUMatrix, uAlgorithm = mixAlg, jointReduction = jointInitialization)
    }
  }
  
  if (length(initialUMatrix) != nModalities &
      !is.matrix(initialUMatrix)) {
    message(paste("initializing with random matrix: ", initialUMatrix, "columns"))
    randmat <- scale(
      ((matrix(rnorm(n * initialUMatrix), nrow = n))), TRUE, TRUE
    )
    initialUMatrix <- list()
    for (i in 1:nModalities) {
      initialUMatrix[[i]] <- randmat
    }
  }
  
  basisK <- ncol(initialUMatrix[[1]])
  buildV <- FALSE
  if (missing(vmats)) buildV <- TRUE else if (is.null(vmats)) buildV <- TRUE
  if (buildV) {
    if (verbose) print("     <0> BUILD-V <0> BUILD-V <0> BUILD-V <0> BUILD-V <0>    ")
    vmats <- list()
    for (i in 1:nModalities) {
      vmats[[i]] <- t(voxmats[[i]]) %*% initialUMatrix[[i]]
      # 0 # svd( temp, nu=basisK, nv=0 )$u
      # for ( kk in 1:nModalities ) vmats[[ i ]] = vmats[[ i ]] + t(voxmats[[i]]) %*% initialUMatrix[[kk]]
      #      vmats[[ i ]] = # svd( vmats[[ i ]], nu=basisK, nv=0 )$u
      #        ( stats::prcomp( vmats[[ i ]], retx=TRUE, rank.=basisK, scale.=TRUE )$x )
      vmats[[i]] <- vmats[[i]] / norm(vmats[[i]], "F")
    }
  }
  
  nc <- ncol(initialUMatrix[[1]])
  myw <- matrix(rnorm(nc^2), nc, nc) # initialization for fastICA
  getSyME2 <- function(lineSearch, gradient, myw, mixAlg,
                       avgU, whichModality, last_energy=0, 
                       constraint=c('ortho',0.5,2.0), verbose = FALSE ) {
    prediction <- 0
    myenergysearchv <- (vmats[[whichModality]] + gradient * lineSearch) # update the i^th v matrix
    if (verbose) {
      print(paste("getSyME2", whichModality))
      print(dim(vmats[[whichModality]]))
    }
    if (sparsenessQuantiles[whichModality] != 0 ) {
      myenergysearchv=as.matrix(smoothingMatrices[[whichModality]] %*% myenergysearchv)
      myenergysearchv <- orthogonalizeAndQSparsify( # make sparse
        myenergysearchv,
        sparsenessQuantiles[whichModality],
        orthogonalize = FALSE,
        positivity = positivities[whichModality],
        softThresholding = TRUE,
        sparsenessAlg = sparsenessAlg
      )
    }
    if ( constraint[1] %in% c('ortho','Stiefel','Grassmann','GrassmannInv') ) {
      # print("Begin orth energy")
      myorthEnergy = invariant_orthogonality_defect( myenergysearchv )
      if ( is.na( last_energy )) last_energy=0.0
      # print(paste("myorthEnergy",myorthEnergy,'last',last_energy))
      if ( abs(last_energy) > .Machine$double.eps & myorthEnergy > .Machine$double.eps ) {
        myorthEnergy = as.numeric(constraint[2]) * myorthEnergy # *(abs(last_energy)/myorthEnergy)
      }
    } else myorthEnergy = 0.0
    if (ccaEnergy) {
      # ( v'*X'*Y )/( norm2(X*v ) * norm2( u ) )
      t0 <- norm(voxmats[[whichModality]] %*% myenergysearchv, "F")
      t1 <- norm(avgU, "F")
      mynorm <- -1.0 / (t0 * t1)
      if (verbose) {
        print("CCA")
        print(dim(avgU))
        print(dim(voxmats[[whichModality]]))
      }
      #      secondterm = sum(abs(diag( t(avgU/t1) %*% ( (voxmats[[whichModality]] %*% myenergysearchv) /t0 ))))
      #      print( paste( "myorthEnergy", myorthEnergy, 'mynorm', mynorm, 'secondterm', secondterm ))
      return( myorthEnergy + mynorm *
                sum(abs(diag(t(avgU) %*% (voxmats[[whichModality]] %*% myenergysearchv)))))
      # t(avgU/t1) %*% ( (voxmats[[whichModality]] %*% myenergysearchv) /t0 ) )) )
    }
    
    # ACC tr( abs( U' * X * V ) ) / ( norm2(U)^0.5 * norm2( X * V )^0.5 )
    if (energyType == "rv_coefficient") {
      # This objective is J = tr(U'XV) / ||V'X'XV||_F^0.5
      # It is self-normalizing and more stable.
      
      # Numerator term: tr(U' * X * V)
      numerator <- sum(diag(t(avgU) %*% (voxmats[[whichModality]] %*% myenergysearchv)))
      
      # Denominator term: ||V'X'XV||_F^0.5
      inner_term <- t(myenergysearchv) %*% (t(voxmats[[whichModality]]) %*% voxmats[[whichModality]]) %*% myenergysearchv
      denominator <- sqrt(sum(inner_term^2)) # Frobenius norm
      
      # Handle potential division by zero
      if (denominator < .Machine$double.eps) {
        objective_value <- 0
      } else {
        objective_value <- numerator / denominator
      }
      
      # Return the negative, because optimize() MINIMIZES
      return(myorthEnergy - objective_value)
    }
    # low-dimensional error approximation
    if (energyType == "lowRank") {
      vpro <- voxmats[[whichModality]] %*% (myenergysearchv)
      # energy = norm( scale(avgU,F,F)  - scale(vpro,F,F), "F" )
      energy <- norm(avgU / norm(avgU, "F") - vpro / norm(vpro, "F"), "F")
      return(myorthEnergy +energy)
    }
    #
    # low-dimensional error approximation
    if (energyType == "lowRankRegression") {
      vpro <- voxmats[[whichModality]] %*% (myenergysearchv)
      energy <- norm(avgU - vpro, "F")
      return(myorthEnergy + energy)
    }
    
    prediction <- avgU %*% t(myenergysearchv)
    #    print( paste("Norm(avgU)",norm(avgU,'F')))
    #    print( paste("Norm(myenergysearchv)",norm(myenergysearchv,'F')))
    prediction <- prediction - (colMeans(prediction) - colMeans(voxmats[[whichModality]]))
    #    print( paste( "norm(prediction)", norm(prediction,'F'), 
    #      "norm(voxmats[[whichModality]])", norm(voxmats[[whichModality]],'F')))
    if (!normalized) energy <- norm(prediction - voxmats[[whichModality]], "F")
    if (normalized) energy <- norm(prediction / norm(prediction, "F") - voxmats[[whichModality]] / norm(voxmats[[whichModality]], "F"), "F")
    if ( verbose )
      print(paste("basic regression", 'myorthEnergy',myorthEnergy,'energy',energy))
    return(myorthEnergy +energy)
  }
  
  energyPath <- matrix(Inf, nrow = iterations, ncol = nModalities)
  initialEnergy <- 0
  
  for (i in 1:nModalities) {
    loki <- getSyME2(0, 0,
                     myw = myw, mixAlg = mixAlg,
                     avgU = initialUMatrix[[i]],
                     whichModality = i,
                     constraint=constraint,
                     last_energy=1
    )
    initialEnergy <- initialEnergy + loki / nModalities
  }
  bestU <- initialUMatrix
  bestV <- vmats
  totalEnergy <- c(initialEnergy)
  if (verbose) {
    print(paste(
      "initialDataTerm:", initialEnergy,
      " <o> mixer:", mixAlg, " <o> E: ", energyType
    ))
  }
  
  getSyMGnorm <- function(v, i, myw, mixAlg) {
    # norm2( X/norm2(X) - U * V'/norm2(U * V') )^2
    u <- initialUMatrix[[i]]
    x <- voxmats[[i]]
    t0 <- v %*% t(u)
    t1 <- u %*% t(v)
    t2 <- norm(t1, "F")
    t3 <- t(x) / norm(x, "F") - t0 / norm(t0)
    tr <- function(x) sum(diag(x))
    gradV <- -2.0 / t2 * t3 %*% u +
      2.0 / t2^3 * tr(t1 %*% t3) * (t0 %*% u)
    return(gradV)
  }
  wm <- "matrix"
  if (ccaEnergy) wm <- "ccag"
  getSyMGccamse <- function(v, i, myw, mixAlg, whichModel = wm) {
    u <- initialUMatrix[[i]]
    x <- voxmats[[i]]
    if (whichModel == "matrix") {
      if (energyType == "lowRankRegression") {
        xv <- x %*% (v)
        return(1.0 / norm(xv - u, "F") * t(x) %*% (xv - u))
      }
      if (energyType == "rv_coefficient") {
        # This is the gradient of the minimization problem -J, where J is the
        # self-normalizing RV-like objective.
        # The gradient is: -X'U + lambda * (X'X) * V
        
        # 1. Calculate lambda = tr(V'X'U) / ||V'X'XV||_F
        numerator_lambda <- sum(diag(t(v) %*% t(x) %*% u))
        inner_term_lambda <- t(v) %*% (t(x) %*% x) %*% v
        denominator_lambda <- sqrt(sum(inner_term_lambda^2)) # Frobenius norm
        
        lambda <- if (denominator_lambda > .Machine$double.eps) {
          numerator_lambda / denominator_lambda
        } else {
          0
        }
        
        # 2. Calculate the two parts of the gradient
        # The gradient of the MAXIMIZATION objective J is: X'U - lambda*(X'X)V
        # For the main loop's additive update, we need a DESCENT direction for the
        # MINIMIZATION problem E = -J. A descent direction for E is \nabla J.
        
        ascent_direction_J <- (t(x) %*% u) - (lambda * ((t(x) %*% x) %*% v))
        
        # We return the ascent direction for J, which is a descent direction for E=-J
        return(ascent_direction_J)
      }
      if (energyType == "lowRank") {
        #          term1 = 2.0 * t( x ) %*% ( u - x %*% v ) #  norm2( U - X * V )^2
        if (TRUE) {
          t0 <- x %*% v
          t1 <- norm(t0, "F")
          t2 <- t((u) / norm(u, "F") - (t0) / t1)
          tr <- function(x) sum(diag(x))
          vgrad1 <- -2.0 / t1 * (t2 %*% x)
          lowx <- (t(x) %*% (x %*% v))
          vgrad2 <- (2.0 / t1^3 * tr((t2) %*% (t0)) * t(lowx))
          return(t(vgrad2 + vgrad1))
        }
        return(term1)
      } else {
        term1 <- 2.0 * (t(x) %*% u - (v %*% t(u)) %*% u)
      } #  norm2( X - U * V' )^2
      term2 <- 0
      if (i %in% connectors[i]) { # d/dV ( norm2( X - X * V * V' )^2 ) => reconstruction energy
        temp <- (x - (x %*% v) %*% t(v)) # n by p
        term2 <- (t(temp) %*% (x %*% v)) -
          t(x) %*% (temp %*% v)
        term1 <- term1 * 0.5
      }
      return(term1 + term2) # + sign( v ) * 0.001 # pure data-term
    }
    if (whichModel == "regression") {
      mdl <- lm(x ~ u)
      intercept <- coefficients(mdl)[1, ]
      return(t(coefficients(mdl)[-1, ]) * 0.05) # move toward these coefficients
    }
    if (whichModel == "ccag") {
      # ( v'*X'*u)/( norm2(X*v ) * norm2( u ) ) # CCA objective
      subg <- function(x, u, v) {
        t0 <- norm(x %*% v, "F")
        t1 <- norm(u, "F")
        mytr <- sum(diag(t(u) %*% (x %*% v)))
        return(1.0 / (t0 * t1) * (t(x) %*% u) -
                 1 / (t0^3 * t1) * mytr * (t(x) %*% (x %*% v)) +
                 sign(v) * 0.0)
      }
      # tr( abs( v'*X'*u) )/( norm2(X*v ) * norm2( u ) ) # CCA style objective
      subg <- function(x, u, v) {
        t0 <- norm(x %*% v, "F")
        t1 <- norm(u, "F")
        t2 <- t(u) %*% (x %*% v)
        mytr <- sum(abs(diag(t2)))
        signer <- t2 * 0
        diag(signer) <- sign(diag(t2))
        return(1.0 / (t0 * t1) * (t(x) %*% u) %*% signer -
                 1.0 / (t0^3 * t1) * mytr * (t(x) %*% (x %*% v)) +
                 sign(v) * 0.0)
      }
      
      #        detg <- function( x, u, v ) {
      #          t0 = x %*% v
      #          t1 = norm( t0, "F" )
      #          t2 = norm( u, "F" )
      #          term1 = 1.0/(t1*t2) * ( t(x) %*% u ) %*% matlib::adjoint( t(t0) %*% u )
      #          term2 = det( t( u ) %*% ( x %*% v ) ) / ( t1^3 * t2 ) * ( t(x) %*% ( x %*% v ) )
      #          return( term1 - term2 )
      #          }
      
      if (energyType == "cca") {
        return(subg(x, u, v))
      }
      gradder <- v * 0
      for (j in 1:nModalities) {
        if (j != i) {
          gradder <- gradder +
            subg(x, scale(voxmats[[j]] %*% vmats[[j]], TRUE, TRUE), v)
        }
      }
      return(gradder / (nModalities - 1))
    }
    if (whichModel == "ccag2") { # THIS IS WIP / not recommended
      # tr( (x'*X'/norm2(X*x ))*(Y/norm2( Y )))
      t0 <- x %*% v
      poster <- t(t(x) %*% t0)
      preposter <- (t(u) %*% (t0))
      t1 <- norm(t0, "F")
      t2 <- norm(u, "F")
      mytr <- sum(diag(t(u) %*% (x %*% v)))
      (1.0 / (t1 * t2) * (t(x) %*% u) -
          1.0 / (t1^3 * t2) * t(preposter %*% poster)) * 0.5
    }
  }
  if (normalized) getSyMG <- getSyMGnorm else getSyMG <- getSyMGccamse
  
  lineSearchLogic <- function(x) { # energy path is input
    nna <- sum(!is.na(x))
    if (nna <= 1) {
      return(TRUE)
    }
    xx <- na.omit(x)
    if ((xx[length(xx) - 1] > xx[length(xx)])) {
      return(FALSE)
    }
    return(TRUE)
    # mdl = loess( xx ~ as.numeric(1:length(xx)) ) # for slope estimate
  }
  
  optimizationLogic <- function(energy, iteration, i) {
    if (optimizationStyle == "greedy" & iteration < 3) {
      return(TRUE)
    }
    if (optimizationStyle == "greedy" & iteration >= 3) {
      return(FALSE)
    }
    if (optimizationStyle == "mixed") {
      return(lineSearchLogic(energy[, i]) | iteration < 3)
    }
    if (optimizationStyle == "lineSearch") {
      return(TRUE)
    }
  }
  ################################################################################
  # below is the primary optimization loop - grad for v then for vran
  ################################################################################
  datanorm <- rep(0.0, nModalities)
  bestTot <- Inf
  lastV <- list()
  lastG <- list()
  for (myit in 1:iterations) {
    errterm <- rep(1.0, nModalities)
    matrange <- 1:nModalities
    for (i in matrange) { # get update for each V_i
      if (ccaEnergy) {
        vmats[[i]] <- vmats[[i]] / norm(vmats[[i]], "F")
        initialUMatrix[[i]] <- initialUMatrix[[i]] / norm(initialUMatrix[[i]], "F")
      }
      # initialize gradient line search
      temperv <- getSyMG(vmats[[i]], i, myw = myw, mixAlg = mixAlg)
      temperv <- constrainG(temperv, i, constraint = constraint[1] )
      
      useAdam <- FALSE
      if (useAdam) { 
        if (myit == 1 & i == 1) {
          m <- list()
          v <- list()
        }
        beta_1 <- 0.8
        beta_2 <- 0.998
        if (myit <= 1) {
          m[[i]] <- temperv * 0
          v[[i]] <- temperv * 0
        } else {
          m[[i]] <- beta_1 * m[[i]] + (1 - beta_1) * temperv
          v[[i]] <- beta_2 * v[[i]] + (1 - beta_2) * temperv^2.0
        }
        m_hat <- m[[i]] / (1 - beta_1^myit)
        v_hat <- v[[i]] / (1 - beta_2^myit)
      }
      
      regnorm = norm(temperv,'F')
      if ( is.nan( regnorm ) ) temperv[] = 0.0
      if (expBeta > 0) {
        if (myit == 1) lastG[[i]] <- 0
        temperv <- temperv * (1.0 - expBeta) + lastG[[i]] * (expBeta)
        lastG[[i]] <- temperv
      }
      if ( constraint[1] == 'ortho' ) {
        orthgrad = gradient_invariant_orthogonality_defect( vmats[[i]] )
        orthgradnorm = norm(orthgrad,"F")
        if ( orthgradnorm > 0 )
          temperv = temperv - orthgrad * as.numeric(constraint[3])
        #          temperv = temperv - orthgrad * norm(temperv,"F")/orthgradnorm*as.numeric(constraint[3])
      }
      if ( myit > 1 ) laste = energyPath[ myit - 1 ] else laste = 1e9
      if (optimizationLogic(energyPath, myit, i)) {
        #        if ( is.nan( norm(initialUMatrix[[i]],'F') ) ) {
        #          print("initialUMatrix[[i]]")
        #          derka
        #        }
        temp <- optimize(getSyME2, # computes the energy
                         interval = lineSearchRange,
                         tol = lineSearchTolerance,
                         gradient = temperv,
                         myw = myw, mixAlg = mixAlg,
                         avgU = initialUMatrix[[i]], whichModality = i, 
                         last_energy=laste, constraint=constraint
        )
        errterm[i] <- temp$objective
        gamma[i] <- temp$minimum
      } else {
        gamma[i] <- gamma[i] * 0.5
        errterm[i] <- getSyME2(
          gamma[i], temperv,
          myw = myw, mixAlg = mixAlg,
          avgU = initialUMatrix[[i]],
          whichModality = i, last_energy=laste,
          constraint=constraint
        )
      }
      if (errterm[i] <= min(energyPath[, i], na.rm = T) |
          optimizationStyle == "greedy") # ok to update
      {
        if (!useAdam) vmats[[i]] <- (vmats[[i]] + (temperv) * gamma[i])
        if (useAdam) vmats[[i]] <- vmats[[i]] - gamma[i] * m_hat / (sqrt(v_hat) + 1) # adam
        if (sparsenessQuantiles[i] != 0) {
          vmats[[i]] <- orthogonalizeAndQSparsify(
            as.matrix(smoothingMatrices[[i]] %*% vmats[[i]]),
            sparsenessQuantiles[i],
            orthogonalize = FALSE, positivity = positivities[i],
            unitNorm = FALSE,
            softThresholding = TRUE,
            sparsenessAlg = sparsenessAlg
          )
        }
        if (normalized) vmats[[i]] <- vmats[[i]] / norm(vmats[[i]], "F")
      }
      if (ccaEnergy) {
        vmats[[i]] <- vmats[[i]] / norm(vmats[[i]], "F")
        initialUMatrix[[i]] <- initialUMatrix[[i]] / norm(initialUMatrix[[i]], "F")
      }
    } # matrange
    if (verbose == 2) print(gamma)
    
    # run the basis calculation for each U_i - convert self to other
    nn <- !ccaEnergy
    if (TRUE) {
      for (jj in 1:nModalities) {
        initialUMatrix[[jj]] <- scale(voxmats[[jj]] %*% vmats[[jj]], nn, nn)
        #        if ( is.nan( norm( initialUMatrix[[jj]], 'F') ) ) {
        #          initialUMatrix[[jj]] <- antsrimpute( initialUMatrix[[jj]] )
        #          print( paste( "IMPUTED", norm( initialUMatrix[[jj]], 'F') ) )
        #        }
      }
      temp <- simlrU(initialUMatrix, mixAlg, myw,
                     orthogonalize = orthogonalize,
                     connectors = connectors
      )
      for (jj in 1:nModalities) {
        initialUMatrix[[jj]] <-
          localGS(temp[[jj]], orthogonalize = orthogonalize)
        # below exp avg for u updates --- not tested
        #        initialUMatrix[[jj]] = initialUMatrix[[jj]] * 0.9 +
        #            localGS( temp[[jj]], orthogonalize = orthogonalize ) * 0.1
      }
    }
    
    
    # evaluate new energies
    for (jj in 1:nModalities) {
      loki <- getSyME2(0, 0,
                       myw = myw, mixAlg = mixAlg,
                       avgU = initialUMatrix[[jj]],
                       whichModality = jj, 
                       constraint=constraint,
                       last_energy=1,
                       verbose = FALSE
      )
      energyPath[myit, jj] <- loki
    } # matrix loop
    if (myit == 1) {
      bestEv <- mean(energyPath[1, ], na.rm = TRUE)
      bestRow <- 1
    } else {
      bestEv <- min(rowMeans(energyPath[1:(myit), ], na.rm = TRUE))
      bestRow <- which.min(rowMeans(na.omit(energyPath[1:(myit), ]), na.rm = TRUE))
    }
    if (optimizationStyle == "greedy") {
      #      bestEv = ( mean( energyPath[(myit),], na.rm = TRUE  ) )
      bestRow <- myit
    }
    totalEnergy[myit] <- bestEv
    if (mean(energyPath[myit, ], na.rm = TRUE) <= bestEv |
        optimizationStyle == "greedy") {
      bestU <- initialUMatrix
      bestV <- vmats
    } else { # FIXME - open question - should we reset or not?
      #      initialUMatrix = bestU ; vmats = bestV
    }
    if (verbose > 0) {
      orthE=0
      for ( zee in 1:length(vmats) ) orthE=orthE+invariant_orthogonality_defect(vmats[[zee]])
      orthE=orthE/length(vmats)
      outputString <- paste("Iteration:", myit, "bestEv:", bestEv, "bestIt:", bestRow)
      #  if ( optimizationStyle == 'greedy' )
      outputString <- paste(outputString, "CE:", mean(energyPath[myit, ]), "featOrth:",orthE)
      print(outputString)
    }
    if ((myit - bestRow - 1) >= 5) break # consider converged
  } # iterations
  
  names(bestV)=names(voxmats)
  for ( k in 1:length(voxmats)) {
    rownames(bestV[[k]])=colnames(voxmats[[k]])
    colnames(bestV[[k]])=paste0("PC",1:ncol(bestV[[k]]))
  }
  
  energyPath <- na.omit(energyPath)
  return(
    list(
      u = bestU,
      v = bestV,
      initialRandomMatrix = randmat,
      energyPath = energyPath,
      finalError = bestEv,
      totalEnergy = totalEnergy,
      connectors = connectors,
      energyType = energyType,
      optimizationStyle = optimizationStyle
    )
  )
}



#' Compute the low-dimensional u matrix for simlr
#'
#' simlr minimizes reconstruction error across related modalities.  One crucial
#' component of the reconstruction is the low-dimensional cross-modality basis.
#' This function computes that basis, given a mixing algorithm.
#'
#' @param projections A list that contains the low-dimensional projections.
#' @param mixingAlgorithm the elected mixing algorithm.  see \code{simlr}.  can
#' be 'svd', 'ica', 'rrpca-l', 'rrpca-s', 'pca', 'stochastic' or 'avg'.
#' @param initialW initialization matrix size \code{n} by \code{k} for fastICA.
#' @param orthogonalize boolean
#' @param connectors a list ( length of projections or number of modalities )
#' that indicates which modalities should be paired with current modality
#' @return u matrix for modality i
#' @author BB Avants.
#' @examples
#'
#' set.seed(1500)
#' nsub <- 25
#' npix <- c(100, 200, 133)
#' nk <- 5
#' outcome <- matrix(rnorm(nsub * nk), ncol = nk)
#' outcome1 <- matrix(rnorm(nsub * nk), ncol = nk)
#' outcome2 <- matrix(rnorm(nsub * nk), ncol = nk)
#' u <- simlrU(list(outcome, outcome1, outcome2), 2, "avg")
#'
#' @seealso \code{\link{simlr}}
#' @export

simlrU <- function(
    projections, mixingAlgorithm, initialW,
    orthogonalize = FALSE, connectors = NULL) {
  # some gram schmidt code
  projectionsU <- projections
  for (kk in 1:length(projections)) {
    mynorm = norm(projectionsU[[kk]], "F")
    if ( is.nan( mynorm ) ) {
      projectionsU[[kk]]=antsrimpute(projectionsU[[kk]])
      mynorm = norm(projectionsU[[kk]], "F")
      message(paste("Warning NaN in simlrU",kk,mynorm))
    }
    projectionsU[[kk]] <- projectionsU[[kk]] / mynorm
  }
  if (!is.null(connectors)) {
    if (length(connectors) != length(projections)) {
      stop("length( connectors ) should equal length( projections )")
    }
  }
  localGS <- function(x, orthogonalize = TRUE) {
    if (!orthogonalize) {
      return(x)
    }
    n <- dim(x)[1]
    m <- dim(x)[2]
    q <- matrix(0, n, m)
    r <- matrix(0, m, m)
    qi <- x[, 1]
    si <- sqrt(sum(qi^2))
    q[, 1] <- qi / si
    if (si < .Machine$double.eps) si <- 1
    r[1, 1] <- si
    for (i in 2:m) {
      xi <- x[, i]
      qj <- q[, 1:(i - 1)]
      rj <- t(qj) %*% xi
      qi <- xi - qj %*% rj
      r[1:(i - 1), i] <- rj
      si <- sqrt(sum(qi^2))
      if (si < .Machine$double.eps) si <- 1
      q[, i] <- qi / si
      r[i, i] <- si
    }
    return(q)
  }
  subU <- function(
    projectionsU, mixingAlgorithm, initialW,
    orthogonalize, i, wtobind) {
    avgU <- NULL
    mixAlg <- mixingAlgorithm
    nComponents <- ncol(projectionsU[[1]])
    nmodalities <- length(projectionsU)
    if (missing(wtobind)) wtobind <- (1:nmodalities)[-i]
    if (mixAlg == "avg") {
      avgU <- projectionsU[[1]] * 0.0
      for (j in wtobind) {
        avgU <- avgU + projectionsU[[j]] / (nmodalities - 1)
      }
      return(avgU)
    }
    if (mixAlg == "stochastic") { # FIXME
      avgU <- Reduce(cbind, projectionsU[wtobind])
      G <- scale(replicate(nComponents, rnorm(nrow(avgU))), FALSE, FALSE)
      Y <- localGS(t(avgU) %*% G) # project onto random basis - no localGS because of numerical issues when self-mapping
      avgU <- scale(avgU %*% Y, FALSE, FALSE)
      return(avgU)
    }
    nc <- ncol(projectionsU[[1]])
    avgU <- Reduce(cbind, projectionsU[wtobind])
    if (mixAlg == "pca") {
      basis <- tryCatch(
        expr = {
          stats::prcomp(scale(avgU, T, T), retx = FALSE, rank. = nc, scale. = TRUE)$rotation
        },
        error = function(e) {
          message("prcomp failed, using svd instead")
          tryCatch(
            expr = {
              svd(scale(avgU, T, T), nu = nc, nv = 0)$u
            },
            error = function(e) {
              message("svd failed, using rsvd instead")
              rsvd(scale(avgU, T, T), nu = nc, nv = 0)$u
            }
          )
        }
      )
    }
    if (mixAlg == "rrpca-l") {
      basis <- (rsvd::rrpca(avgU, rand = FALSE)$L[, 1:nc])
    } else if (mixAlg == "rrpca-s") {
      basis <- (rsvd::rrpca(avgU, rand = FALSE)$S[, 1:nc])
    } else if (mixAlg == "ica" & !missing(initialW)) {
      basis <- (fastICA::fastICA(avgU,
                                 method = "C", w.init = initialW,
                                 n.comp = nc
      )$S)
      if ( is.nan( norm(basis,"F"))) {
        message(paste("fastICA produced NaN - svd instead"))
        basis <- (ba_svd(avgU, nu = nc, nv = 0)$u)
      }
    } else if (mixAlg == "ica" & missing(initialW)) {
      basis <- (fastICA::fastICA(avgU, method = "C", n.comp = nc)$S)
    } else {
      basis <- (ba_svd(scale(avgU,T,T), nu = nc, nv = 0)$u)
    }
    colnames(basis)=paste0("PC",1:nc)
    # print(paste("Basis norm",norm(basis,'F'),'avgUnorm',norm(avgU,'F')))
    # print(paste("Basis norm",paste(dim(basis),collapse='x'),'avgUnorm',paste(dim(avgU),collapse='x')))
    # if ( is.nan(norm(basis,'F') ) ) {
    #  write.csv( avgU, '/tmp/temp.csv',row.names=FALSE )
    #  write.csv( basis, '/tmp/temp2.csv',row.names=FALSE )
    #  derka
    # }
    if (!orthogonalize) {
      return(basis)
    }
    return(localGS(basis, orthogonalize = orthogonalize))
  }
  
  outU <- list()
  for (i in 1:length(projectionsU)) {
    if (is.null(connectors)) {
      outU[[i]] <- subU(projectionsU, mixingAlgorithm, initialW, orthogonalize, i)
    }
    if (!is.null(connectors)) {
      outU[[i]] <- subU(projectionsU, mixingAlgorithm, initialW, orthogonalize,
                        i,
                        wtobind = connectors[[i]]
      )
    }
  }
  return(outU)
}





#' Assess Significance of SiMLR Call
#'
#' This function performs permutation tests to assess the significance of a SiMLR analysis.
#' For more detail on input parameters, see the original function.
#'
#' @param voxmats A list of voxel matrices.
#' @param smoothingMatrices A list of smoothing matrices.
#' @param iterations Number of iterations. Default is 10.
#' @param sparsenessQuantiles A vector of sparseness quantiles.
#' @param positivities A vector of positivity constraints.
#' @param initialUMatrix Initial U matrix for the algorithm.
#' @param mixAlg The mixing algorithm to use. Default is 'svd'.
#' @param orthogonalize Logical indicating whether to orthogonalize. Default is FALSE.
#' @param repeatedMeasures Repeated measures data. Default is NA.
#' @param lineSearchRange Range for line search. Default is c(-1e+10, 1e+10).
#' @param lineSearchTolerance Tolerance for line search. Default is 1e-08.
#' @param randomSeed Seed for random number generation.
#' @param constraint The constraint type. Default is 'none'.
#' @param energyType The energy type. Default is 'cca'.
#' @param vmats List of V matrices - optional initialization matrices
#' @param connectors List of connectors. Default is NULL.
#' @param optimizationStyle The optimization style. Default is 'lineSearch'.
#' @param scale Scaling method. Default is 'centerAndScale'.
#' @param expBeta Exponential beta value. Default is 0.
#' @param jointInitialization Logical indicating joint initialization. Default is TRUE.
#' @param sparsenessAlg Sparseness algorithm. Default is NA.
#' @param verbose Logical indicating whether to print verbose output. Default is FALSE. values > 1 lead to more verbosity
#' @param nperms Number of permutations for significance testing. Default is 50.
#' @param FUN function for summarizing variance explained 
#' @return A data frame containing p-values for each permutation.
#' @export
simlr.perm <- function(voxmats, smoothingMatrices, iterations = 10, sparsenessQuantiles, 
                       positivities, initialUMatrix, mixAlg = c("svd", "ica", "avg", 
                                                                "rrpca-l", "rrpca-s", "pca", "stochastic"), orthogonalize = FALSE, 
                       repeatedMeasures = NA, lineSearchRange = c(-1e+10, 1e+10), 
                       lineSearchTolerance = 1e-08, randomSeed, constraint = c("none", 
                                                                               "Grassmann", "Stiefel"), energyType = c("cca", "regression", 
                                                                                                                       "normalized", "ucca", "lowRank", "lowRankRegression"), 
                       vmats, connectors = NULL, optimizationStyle = c("lineSearch", 
                                                                       "mixed", "greedy"), scale = c("centerAndScale", "sqrtnp", 
                                                                                                     "np", "center", "norm", "none", "impute", "eigenvalue", 
                                                                                                     "robust"), expBeta = 0, jointInitialization = TRUE, sparsenessAlg = NA, 
                       verbose = FALSE, nperms = 50, FUN='mean') {
  
  # Set up permutations
  myseeds <- 1:1000000
  
  # Initial SiMLR run
  simlr_result <- simlr( voxmats, 
                         smoothingMatrices, iterations, sparsenessQuantiles, 
                         positivities, initialUMatrix, mixAlg, orthogonalize, 
                         repeatedMeasures, lineSearchRange, lineSearchTolerance, randomSeed, constraint, 
                         energyType, vmats, connectors, optimizationStyle, scale, expBeta, 
                         jointInitialization, sparsenessAlg, verbose=verbose > 0 )
  for ( k in 1:length(voxmats)) {
    simlr_result$v[[k]]=take_abs_unsigned(simlr_result$v[[k]])
    simlr_result$v[[k]]=divide_by_column_sum( simlr_result$v[[k]] )
    rownames(simlr_result$v[[k]])=colnames(voxmats[[k]])
  }
  
  refvarxmeans = pairwise_matrix_similarity( voxmats, simlr_result$v, FUN=FUN )
  simlrpermvarx = data.frame( n=ncol(initialUMatrix), perm=0:nperms ) 
  refvarxmeansnms=names(refvarxmeans)
  simlrpermvarx[1, refvarxmeansnms]=refvarxmeans
  
  # begin permutation  
  if ( nperms > 1 )
    for (nperm in 1:nperms) {
      set.seed(myseeds[nperm])
      
      voxmats_perm <- lapply(voxmats, function(mat) mat[sample(1:nrow(mat)), ])
      
      simlr_result_perm <- simlr(voxmats_perm, smoothingMatrices, iterations, sparsenessQuantiles, 
                                 positivities, initialUMatrix, mixAlg, orthogonalize, 
                                 repeatedMeasures, lineSearchRange, lineSearchTolerance, randomSeed, constraint, 
                                 energyType, vmats, connectors, optimizationStyle, scale, expBeta, 
                                 jointInitialization, sparsenessAlg, verbose=verbose > 3)
      for ( k in 1:length(voxmats)) {
        simlr_result$v[[k]]=take_abs_unsigned(simlr_result$v[[k]])
        simlr_result_perm$v[[k]] = divide_by_column_sum( simlr_result_perm$v[[k]] )
      }
      refvarxmeans_perm = pairwise_matrix_similarity( voxmats_perm, simlr_result_perm$v, FUN=FUN )
      simlrpermvarx[nperm + 1, refvarxmeansnms ] <- refvarxmeans_perm
      if ( verbose > 2 ) {
        print( simlrpermvarx[c(1,nperm+1),])
      }
    }
  
  # Statistical significance testing
  simlrpermvarx_ttest <- c()
  if ( nperms >  1  ) {
    for (varname in refvarxmeansnms ) {
      mytt <- t.test(simlrpermvarx[1, varname] - simlrpermvarx[-1, varname], alternative='greater')
      simlrpermvarx_ttest[varname] = mytt$statistic   
    }
    nexter=nrow(simlrpermvarx)+1
    simlrpermvarx[nexter,'perm']='ttest'
    simlrpermvarx[nexter,'n']=ncol(initialUMatrix)
    simlrpermvarx[nexter,refvarxmeansnms]=simlrpermvarx_ttest
    nexter=nrow(simlrpermvarx)+1
    simlrpermvarx[nexter,'n']=ncol(initialUMatrix)
    simlrpermvarx[nexter,'perm']='pvalue'
    for ( zz in refvarxmeansnms ) 
      simlrpermvarx[nexter,zz]=sum( simlrpermvarx[2:(1+nperms),zz] > simlrpermvarx[1,zz] )/nperms
    if ( verbose > 1 ) print( simlrpermvarx[nexter,] )
  }
  return( list( simlr_result=simlr_result, significance=simlrpermvarx))
}

# --- Internal Implementation 1: Trace Method (for WIDE data) ---
# Returns a list of components.
rvcoef_trace_impl <- function(X_centered, Y_centered) {
  S_XX <- X_centered %*% t(X_centered)
  S_YY <- Y_centered %*% t(Y_centered)
  S_XY <- X_centered %*% t(Y_centered)
  
  numerator <- sum(diag(S_XY %*% t(S_XY)))
  
  denom_part1 <- sum(diag(S_XX %*% S_XX))
  denom_part2 <- sum(diag(S_YY %*% S_YY))
  denominator <- sqrt(denom_part1 * denom_part2)
  
  if (denominator == 0) {
    return(list(rv = 0, numerator = numerator, denominator = 0))
  }
  
  rv <- numerator / denominator
  return(list(rv = rv, numerator = numerator, denominator = denominator))
}

# --- Internal Implementation 2: Gram Matrix Method (for TALL data) ---
# Returns a list of components.
rvcoef_gram_impl <- function(X_centered, Y_centered) {
  cross_product_matrix <- t(X_centered) %*% Y_centered
  svd_C <- svd(cross_product_matrix, nu = 0, nv = 0)
  numerator <- sum(svd_C$d^2)
  
  G_X <- t(X_centered) %*% X_centered
  G_Y <- t(Y_centered) %*% Y_centered
  
  denom_part1 <- sum(diag(G_X %*% G_X))
  denom_part2 <- sum(diag(G_Y %*% G_Y))
  denominator <- sqrt(denom_part1 * denom_part2)
  
  if (denominator == 0) {
    return(list(rv = 0, numerator = numerator, denominator = 0))
  }
  
  rv <- numerator / denominator
  return(list(rv = rv, numerator = numerator, denominator = denominator))
}

# --- Internal Dispatcher: Returns all components from the fastest method ---
rvcoef_components <- function(X, Y) {
  n <- nrow(X)
  p <- ncol(X)
  q <- ncol(Y)
  
  X_centered <- scale(X, center = TRUE, scale = FALSE)
  Y_centered <- scale(Y, center = TRUE, scale = FALSE)
  
  if (n < (p + q)) {
    return(rvcoef_trace_impl(X_centered, Y_centered))
  } else {
    return(rvcoef_gram_impl(X_centered, Y_centered))
  }
}

#' Computes the RV-Coefficient (Optimized Public API)
#'
#' This function automatically selects the fastest algorithm based on data dimensions.
#'
#' @param X A numeric matrix (n observations, p variables).
#' @param Y A numeric matrix (n observations, q variables).
#' @return A single scalar value for the RV-coefficient.
#' @export
rvcoef <- function(X, Y) {
  # Call the component dispatcher and return only the final rv value
  rvcoef_components(X, Y)$rv
}

#' Computes the Adjusted RV-Coefficient (Highly Optimized)
#'
#' This function avoids all redundant computations by getting all necessary
#' components from a single call to an internal dispatcher.
#'
#' @param X A numeric matrix (n observations, p variables).
#' @param Y A numeric matrix (n observations, q variables).
#' @return A single scalar value for the adjusted RV-coefficient.
#' @export
adjusted_rvcoef <- function(X, Y) {
  n <- nrow(X)
  if (n <= 1) return(0)

  # Step 1: Get all components in one go from the fastest implementation
  components <- rvcoef_components(X, Y)
  
  rv_obs <- components$rv
  rv_den <- components$denominator
  
  if (rv_den == 0) return(0)

  # Step 2: Calculate the expected value (computationally cheap)
  # Note: We must re-center here as the components function doesn't return it.
  X_centered <- scale(X, center = TRUE, scale = FALSE)
  Y_centered <- scale(Y, center = TRUE, scale = FALSE)
  
  tr_S_XX <- sum(X_centered^2)
  tr_S_YY <- sum(Y_centered^2)
  
  exp_rv_num <- tr_S_XX * tr_S_YY / (n - 1)
  exp_rv <- exp_rv_num / rv_den
  
  # Step 3: Apply the adjustment
  # Handle the case where exp_rv might be >= 1
  if (exp_rv >= 1) return(NA)
  
  adj_rv <- (rv_obs - exp_rv) / (1 - exp_rv)
  
  return(adj_rv)
}



#' Construct SiMLR Feature Names
#'
#' A helper function to generate a character vector of SiMLR feature names
#' based on a set of modality prefixes and numeric indices.
#'
#' @param base_names A character vector of modality prefixes (e.g., c("t1", "dt", "rsf")).
#' @param indices An integer vector of component indices (e.g., 1:10).
#'
#' @return A character vector of combined names (e.g., "t1PC1", "dtPC1", ... "rsfPC10").
#' @export
#'
#' @examples
#' modalities <- c("t1", "dt", "rsf")
#' components <- 1:3
#' construct_sim_names(modalities, components)
construct_sim_names <- function(base_names, indices) {
  # Ensure inputs are character and integer vectors
  base_names <- as.character(base_names)
  indices <- as.integer(indices)
  
  # Create combinations using outer and paste
  sim_names <- as.vector(outer(base_names, indices, FUN = function(name, i) paste0(name, "PC", i)))
  
  return(sim_names)
}



#' pairwise application of matrix similarity to matrix List projected onto a feature list
#'
#' Computes a measure such as the RV coefficient between low-rank projections of all pairs of matrices in a list.
#'
#' @param mat_list List of matrices
#' @param feat_list List of feature lists corresponding to each matrix
#' @param FUN function to apply 
#'
#' @return Matrix of RV coefficients, where each entry [i, j] represents the similarity between the low-rank projections of matrices i and j
#'
#' @export
pairwise_matrix_similarity <- function(mat_list, feat_list, FUN=adjusted_rvcoef) {
  
  # Initialize an empty matrix to store RV coefficients
  rv_coeffs <- matrix(NA, nrow = length(mat_list), ncol = length(mat_list))
  rv_coeffs_nms <- matrix("", nrow = length(mat_list), ncol = length(mat_list))
  k = ncol( feat_list[[1]] )
  nms = names(mat_list)
  # Loop through each pair of matrices (i, j) where i != j
  for (i in 1:length(mat_list)) {
    for (j in (i+1):length(mat_list)) {
      if (i != j & j <= length(mat_list)) {
        # Extract matrices and feature lists for current pair
        X_i <- mat_list[[i]]
        V_i <- feat_list[[i]]
        X_j <- mat_list[[j]]
        V_j <- feat_list[[j]]
        
        # Compute low-rank projections
        L_i <- X_i %*% V_i
        L_j <- X_j %*% V_j
        
        # Compute RV coefficient
        rv <- FUN(L_i, L_j)
        
        # Store RV coefficient in matrix
        rv_coeffs[i, j] <- rv
        rv_coeffs_nms[i,j] = paste0( "rvcoef_", nms[i],"_",nms[j])
      }
    }
  }
  flatrv = rv_coeffs[upper.tri(rv_coeffs)]
  names(flatrv)=rv_coeffs_nms[upper.tri(rv_coeffs)]
  return(flatrv)
}



#' Visualize Low-Rank Relationships
#'
#' Compute low-rank projections, pairwise correlations, and RV coefficient, and visualize the relationships using a heatmap and pairs plot.
#'
#' @param X1 First matrix
#' @param X2 Second matrix
#' @param V1 First feature matrix
#' @param V2 Second feature matrix
#' @param plot_title Title of the plot (optional)
#' @param nm1 Name of the first matrix (default: "X1")
#' @param nm2 Name of the second matrix (default: "X2")
#'
#' @return A list containing the heatmap, pairs plot, correlation matrix, and RV coefficient
#'
#' @examples
#' set.seed(123)
#' X1 <- matrix(rnorm(100), nrow = 10, ncol = 10)
#' X2 <- matrix(rnorm(100), nrow = 10, ncol = 10)
#' V1 <- matrix(rnorm(100), nrow = 10, ncol = 10)
#' V2 <- matrix(rnorm(100), nrow = 10, ncol = 10)
#' # result <- visualize_lowrank_relationships(X1, X2, V1, V2)
#' @export
visualize_lowrank_relationships <- function(X1, X2, V1, V2, plot_title, nm1='X1', nm2='X2') {
  
  unique_column_names <- function(df) {
    names <- colnames(df)
    new_names <- character(length(names))
    counter <- 1
    for (i in 1:length(names)) {
      if (sum(names == names[i]) == 1) {
        new_names[i] <- names[i]
      } else {
        new_names[i] <- paste0(names[i], counter)
        counter <- counter + 1
      }
    }
    colnames(df) <- new_names
    return(df)
  }
  
  if (missing(plot_title)) plot_title=paste("LRRc: ", nm1, " & ", nm2 )
  # Compute the low-rank projections
  projection1 <- X1 %*% V1
  projection2 <- X2 %*% V2
  cordf=  unique_column_names( cbind( data.frame(projection1), data.frame(projection2 )) )
  # Compute pairwise correlations
  correlation_matrix <- cor(projection1, projection2)
  
  # Perform CCA
  #   cca_result <- cancor(projection1, projection2)
  # canonical_correlations <- cca_result$cor
  
  # Perform Wilks' lambda test
  # wilks_test <- WilksLambda(projection1, projection2, cca_result)
  
  # Compute RV coefficient
  rv_coefficient <- rvcoef(projection1, projection2)
  adj_rv_coefficient <- adjusted_rvcoef(projection1, projection2)
  
  # Prepare data for plotting
  cor_data <- as.data.frame(as.table(correlation_matrix))
  
  # fix for no visible binding for global variable NOTE
  Var1 = Var2 = Freq = NULL
  rm(list = c("Var1", "Var2", "Freq"))
  # Plot the pairwise correlations
  p <- ggplot2::ggplot(cor_data, ggplot2::aes(Var1, Var2, fill = Freq)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Correlation") +
    ggplot2::geom_text(ggplot2::aes(label = format(Freq, digits = 2, nsmall = 2)), size = 3)+
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 45, vjust = 1, 
                                     size = 12, hjust = 1)) +
    ggplot2::coord_fixed() +
    ggplot2::labs(title = plot_title, x = "Projection 1", y = "Projection 2")  
  
  ggp=GGally::ggpairs( cordf,
               upper = list(continuous = "points"), 
               lower = list(continuous = "cor"),
               title = plot_title )
  
  # Return a list of results
  return(list(
    plot = p,
    pairsplot = ggp,
    correlations = correlation_matrix,
    # wilks_test = NA,
    rv_coefficient = rv_coefficient,
    adj_rv_coefficient=adj_rv_coefficient
  ))
}

#' Take Absolute Value of Unsigned Columns
#'
#' This function iterates through each column of a numeric matrix.
#' If a column contains only zero and positive values, or only zero and negative values,
#' the absolute value of that column is taken. Otherwise, the column is returned as is.
#'
#' @param m A numeric matrix.
#'
#' @return A matrix with absolute values taken for unsigned columns.
#' @export
take_abs_unsigned <- function(m) {
  if (!is.matrix(m) || !is.numeric(m)) {
    stop("Input must be a numeric matrix.")
  }

  # Handle edge case of empty matrix to avoid issues with ncol(m) if m is 0x0
  if (ncol(m) == 0) {
    return(m)
  }

  result_matrix <- m # Initialize with the original matrix

  for (j in 1:ncol(m)) {
    column <- m[, j]

    # Check if the column is unsigned (contains only non-negative or only non-positive values)
    is_non_negative <- all(column >= 0)
    is_non_positive <- all(column <= 0)

    if (is_non_negative || is_non_positive) {
      # Apply absolute value if unsigned
      result_matrix[, j] <- abs(column)
    }
  }

  return(result_matrix)
}

#' SiMLR Path Models helper
#'
#' Creates the list that should be passed to the connectors argument
#' to simlr.   the different options are default (0) which connects
#' each to all others but not itself, a single integer > 0 which forces
#' modeling to focus on a given target and then a pca like option.
#'
#' @param n The length of the output list.
#' @param type The type of modeling. Can be one of the following:
#'   - 0: "Exclude self" - each entry contains all integers except the one corresponding to its position.
#'   - Integer greater than 0: "focus on a target" - each entry contains the target integer, except for the entry at the target position, which contains all integers except the target.
#'   - 'pca': "Identity" - each entry contains its own position number.
#'
#' @return A list of length n with contents based on the input type.
#' @export
simlr_path_models <- function(n, type = 0) {
  # Initialize an empty list to store the results
  result <- vector("list", n)
  
  # If type is 0, create a list where each entry contains the integers that do not include the integer encoding the position
  if (type == 0) {
    for (i in 1:n) {
      result[[i]] <- setdiff(1:n, i)
    }
  }
  # If type is an integer greater than 0, create a list where each entry contains the integer itself, except for its own entry which contains the integers up to n that do not include itself
  else if (type > 0 && type != 'pca') {
    for (i in 1:n) {
      if (i == type) {
        result[[i]] <- setdiff(1:n, i)
      } else {
        result[[i]] <- type
      }
    }
  }
  # If type is 'pca', create a list where each entry contains the integer that encodes the list position
  else if (type == 'pca') {
    for (i in 1:n) {
      result[[i]] <- i
    }
  }
  
  # Return the resulting list
  return(result)
}


#' Convert a Vector to a Data Frame
#'
#' @param vector The input vector
#' @param column_name The base name for the columns
#'
#' @return A data frame with one row and columns determined by the length of the vector
#'
#' @examples
#' vector_to_df(1:10, "nm")
#' @export
vector_to_df <- function(vector, column_name) {
  df <- data.frame(t(vector))
  names(df) <- paste0(column_name, 1:length(vector))
  return(df)
}




#' Generate Parameter Grid for SIMLR Search
#'
#' This function creates and optionally subsets all possible combinations of input parameters for use in a SIMLR grid search.
#'
#' @param nsimlr_options A list of options for the `nsimlr` parameter.
#' @param prescaling_options A list of options for the `prescaling` parameter.
#' @param objectiver_options A list of options for the `objectiver` parameter.
#' @param mixer_options A list of options for the `mixer` parameter.
#' @param sparval_options A list of options for the `sparval` parameter.
#' @param expBeta_options A list of options for the `ebber` parameter.
#' @param positivities_options A list of options for the `pizzer` parameter.
#' @param optimus_options A list of options for the `optimus` parameter.
#' @param constraint_options A list of options for the `constraint` parameter, default is `list("none")`.
#' @param sparsenessAlg A list of options for the `sparsenessAlg` parameter, default is `list(NA)`.
#' @param num_samples if not full, then the size of the subset space
#' @param search_type string vector either full random or deterministic
#' 
#' @return A list containing all (or a subset of) combinations of the provided parameters. Each row in the data frame represents a unique combination of the parameters.
#' 
#' The columns of the returned data frame include:
#' \itemize{
#'   \item \code{nsimlr}: Values corresponding to the `nsimlr` parameter.
#'   \item \code{prescaling}: Values corresponding to the `prescaling` parameter.
#'   \item \code{objectiver}: Values corresponding to the `objectiver` parameter.
#'   \item \code{mixer}: Values corresponding to the `mixer` parameter.
#'   \item \code{constraint}: Values corresponding to the `constraint` parameter.
#'   \item \code{sparval}: Values corresponding to the `sparval` parameter.
#'   \item \code{ebber}: Values corresponding to the `ebber` parameter.
#'   \item \code{pizzer}: Values corresponding to the `pizzer` parameter.
#'   \item \code{optimus}: Values corresponding to the `optimus` parameter.
#' }
#'
#' Each row represents a specific set of parameters that can be passed to the `simlr.search` function for evaluation. The returned data frame is intended for use in parameter tuning and optimization.
#' 
#' @export
simlr.parameters <- function(
    nsimlr_options,
    prescaling_options,
    objectiver_options,
    mixer_options,
    sparval_options,
    expBeta_options,
    positivities_options,
    optimus_options,
    constraint_options = list("none"),
    sparsenessAlg = list(NA),
    num_samples = 10,
    search_type = c("random", "deterministic", "full")
) {
  search_type <- match.arg(search_type)
  
  # Step 1: Generate full options data frame
  options_df <- expand.grid(
    nsimlr = nsimlr_options,
    prescaling = prescaling_options,
    objectiver = objectiver_options,
    mixer = mixer_options,
    constraint = constraint_options,
    sparval = sparval_options,
    ebber = expBeta_options,
    pizzer = positivities_options,
    optimus = optimus_options,
    sparsenessAlg = sparsenessAlg,
    stringsAsFactors = FALSE
  )
  
  # Step 2: Subsample based on search_type
  if (search_type == "random") {
    options_df <- options_df[sample(nrow(options_df), num_samples), ]
  } else if (search_type == "deterministic") {
    options_df <- options_df[seq(1, nrow(options_df), length.out = num_samples), ]
  }
  
  return(options_df)
}


#' Perform SIMLR Grid Search
#'
#' This function performs a grid search over the parameter combinations provided by `options_df`.
#'
#' @param mats The input matrices for SIMLR.
#' @param regs The regularization options for SIMLR.
#' @param options_df A data frame of parameter combinations generated by `simlr.parameters`.
#' @param maxits The maximum number of iterations for SIMLR. Default is 100.
#' @param connectors a list ( length of projections or number of modalities )
#' that indicates which modalities should be paired with current modality
#' @param nperms The number of permutations for the significance test. Default is 1.
#' @param verbose The verbosity level. Default is 0.
#' @param FUN The function to use for the SIMLR evaluation. Default is `rvcoef`.
#' @return A list containing the best SIMLR result, its significance, and the parameters.
#' @export
simlr.search <- function(
    mats,
    regs,
    options_df,
    maxits = 100,
    connectors = NULL,
    nperms = 1,
    verbose = 0,
    FUN = rvcoef
) {
  myrbind.fill <- function(..., fill = NA) {
    args <- list(...)
    col_names <- unique(unlist(lapply(args, names)))
    result <- data.frame(matrix(nrow = 0, ncol = length(col_names)))
    names(result) <- col_names
    for (arg in args) {
      arg[, setdiff(col_names, names(arg))] <- fill
      result <- rbind(result, arg)
    }
    result
  }
  
  ssbont <- function() set.seed(as.integer(substr(as.character(Sys.time()), 22, 200)))
  # ssbont(i) <- function()  set.seed(as.integer(i))
  # Initialize results storage
  options_df_final <- NULL
  bestresult <- bestsig <- bestparams <- NA
  
  # Iterate over the options and evaluate the function
  cat(paste("Will search:", nrow(options_df), "parameter sets"))
  for (i in 1:nrow(options_df)) {
    if (i %% 10 == 0) cat(paste0("i ", i, " ..."))
    ssbont()
    nsimlr <- unlist(options_df$nsimlr[i])
    prescaling <- unlist(options_df$prescaling[i])
    objectiver <- unlist(options_df$objectiver[i])
    mixer <- unlist(options_df$mixer[i])
    constraint <- unlist(options_df$constraint[i])
    sparval <- unlist(options_df$sparval[i])
    ebber <- unlist(options_df$ebber[i])
    pizzer <- unlist(options_df$pizzer[i])
    optimus <- unlist(options_df$optimus[i])
    sparsenessAlgVal = unlist( options_df$sparsenessAlg[i] )
    
    if (is.character(sparval[1])) {
      parse_vec <- function(s) as.numeric(strsplit(gsub("rand", "", s), "x")[[1]])
      sparval <- parse_vec(sparval)
      sparval <- runif(sparval[1], sparval[2], sparval[3])
    }
    
    if (verbose > 3) {
      print(prescaling)
      print(objectiver)
      print(mixer)
      print(constraint)
      print(sparval)
      print(ebber)
      print(pizzer)
      print(optimus)
    }
    
    initu <- initializeSimlr(
      mats,
      nsimlr,
      jointReduction = TRUE,
      zeroUpper = FALSE,
      uAlgorithm = "pca",
      addNoise = 0
    )
    
    simlrX <- simlr.perm(
      mats,
      regs,
      iterations = maxits,
      randomSeed = 0,
      mixAlg = mixer,
      energyType = objectiver,
      orthogonalize = TRUE,
      scale = prescaling,
      sparsenessQuantiles = sparval,
      expBeta = ebber,
      positivities = pizzer,
      optimizationStyle = optimus,
      initialUMatrix = if ("lowrank" %in% prescaling) lowrankRowMatrix(initu, nsimlr * 2) else initu,
      constraint = constraint,
      connectors = connectors,
      verbose = verbose > 2,
      nperms = nperms,
      sparsenessAlg = sparsenessAlgVal,
      FUN = FUN
    )
    
    finalE <- sum(simlrX$significance[1, -c(1:2)])
    if (nperms > 4) {
      wtest <- which(simlrX$significance$perm == 'ttest')
      finalE <- sum(-log10(simlrX$significance[wtest, -c(1:2)]))
    }
    finalE <- finalE * 1.0 / length(mats) # Don't ask...
    
    parameters <- data.frame(
      nsimlr = nsimlr,
      objectiver = objectiver,
      mixer = mixer,
      ebber = ebber,
      optimus = optimus,
      constraint = constraint,
      final_energy = as.numeric(finalE)
    )
    
    prescaling <- vector_to_df(prescaling, 'prescaling')
    sparval <- vector_to_df(sparval, 'sparval')
    pizzer <- vector_to_df(pizzer, 'positivity')
    parameters <- cbind(parameters, prescaling, sparval, pizzer, simlrX$significance[1, -1])
    
    if (is.null(options_df_final)) {
      options_df_final <- parameters
    } else {
      options_df_final <- myrbind.fill(options_df_final, parameters)
    }
    
    if (nrow(options_df_final) >= 1) {
      rowsel <- 1:(nrow(options_df_final) - 1)
      if (nrow(options_df_final) == 1) {
        bestresult <- simlrX$simlr_result
        bestsig <- simlrX$significance
        bestparams <- parameters
      } else if (all(finalE > options_df_final$final_energy[rowsel])) {
        bestresult <- simlrX$simlr_result
        bestsig <- simlrX$significance
        bestparams <- parameters
        if (verbose > 0) {
          print(paste("improvement"))
          print(parameters)
          print(head(bestresult$v[[length(bestresult$v)]]))
        }
      }
    } else {
      bestresult <- bestsig <- bestparams <- NA
    }
  }
  
  if (verbose) {
    print(options_df_final[which.max(options_df_final$final_energy), ])
    cat("el finito\n")
  }
  
  outlist <- list(simlr_result = bestresult, significance = bestsig, parameters = bestparams, paramsearch=options_df_final )
  return(outlist)
}




#' L1 Normalize Columns of a Matrix
#'
#' This function scales the columns of a numeric matrix such that the sum of the
#' absolute values of each column (the L1 norm) is equal to 1. This is a common
#' preprocessing step for feature matrices.
#'
#' The function is designed to be highly efficient by using vectorized operations.
#' It also robustly handles columns that sum to zero to prevent division-by-zero errors.
#'
#' @param features A numeric matrix or a data frame that can be coerced into one.
#'   The normalization is applied column-wise.
#'
#' @return A matrix with the same dimensions as the input, where each column has
#'   been L1-normalized.
#'
#' @export
#' @examples
#' # Create a sample feature matrix
#' set.seed(123)
#' my_features <- matrix(rnorm(12, mean = 5), nrow = 4, ncol = 3)
#' print(my_features)
#' #>           [,1]     [,2]     [,3]
#' #> [1,] 4.4395244 5.487429 5.704929
#' #> [2,] 5.2325834 6.738325 5.558708
#' #> [3,] 3.4291247 5.575781 4.129288
#' #> [4,] 5.5060559 4.694612 4.261173
#'
#' # Check the column sums of absolute values before normalization
#' print(colSums(abs(my_features)))
#' #> [1] 18.60729 22.49615 19.65410
#'
#' # Apply L1 normalization
#' normalized_features <- l1_normalize_features(my_features)
#' print(normalized_features)
#' #>            [,1]      [,2]      [,3]
#' #> [1,] 0.23859089 0.2439247 0.2800889
#' #> [2,] 0.28121151 0.2995321 0.2891398
#' #> [3,] 0.18429107 0.2478546 0.2151676
#' #> [4,] 0.29590653 0.2086886 0.2221037
#'
#' # Confirm that the new column sums of absolute values are all 1
#' print(colSums(abs(normalized_features)))
#' #> [1] 1 1 1
l1_normalize_features <- function(features) {
  
  # --- 1. Calculate the L1 norm for each column ---
  # `colSums` is highly optimized for this task.
  col_l1_norms <- colSums(abs(features))
  
  # --- 2. Handle the zero-norm case ---
  # To prevent division by zero (which results in NaNs), we replace any
  # zero norms with 1. Dividing by 1 will not change the zero-column.
  col_l1_norms[col_l1_norms == 0] <- 1
  
  # --- 3. Normalize using a vectorized matrix operation ---
  # The `t()` function is used here for "recycling" to work correctly.
  # We divide each row of the transposed matrix by the norms vector,
  # then transpose it back to the original orientation.
  # This is much faster than a loop.
  normalized_matrix <- t(t(features) / col_l1_norms)
  
  return(normalized_matrix)
}

#' Apply simlr matrices to an existing data frame and combine the results
#'
#' This function takes a list of matrices, applies each matrix via matrix multiplication
#' to an existing data frame, and combines the resulting projections with the original data frame.
#'
#' @param existing_df An existing data frame to which the matrices will be applied.
#' @param matrices_list A list of matrices read from CSV files.
#' @param n_limit NULL or integer that can limit the number of projections
#' @param robust boolean
#' @param center boolean center the data before applying
#' @param scale boolean scale the data before applying
#' @param verbose boolean
#'
#' @return A list including (entry one) data frame with the original data frame combined with the projections (entry two) the new column names
#' @export
#' @examples
#' matrices_list <- list(
#'   matrix1 = matrix(rnorm(147 * 171), nrow = 147, ncol = 171),
#'   matrix2 = matrix(rnorm(147 * 156), nrow = 147, ncol = 156)
#' )
#' existing_df <- data.frame(matrix(rnorm(147 * 5), nrow = 147, ncol = 5))
#' # combined_df <- apply_simlr_matrices(existing_df, matrices_list)
apply_simlr_matrices <- function(existing_df, matrices_list, n_limit=NULL, robust=FALSE, center=FALSE, scale=FALSE, verbose=FALSE ) {
    
  replbind <- function(df1, df2) {
    # Find the common and unique columns
    common_cols <- intersect(names(df1), names(df2))
    unique_cols_df1 <- setdiff(names(df1), common_cols)
    unique_cols_df2 <- setdiff(names(df2), common_cols)
    
    # Replace values in common columns with those from df2
    if (length(common_cols) > 0) {
      for (col in common_cols) {
        df1[[col]] <- df2[[col]]
      }
    }
    
    # Bind the unique columns from both data frames
    if (length(unique_cols_df2) > 0) {
      result <- cbind(df1, df2[, unique_cols_df2, drop = FALSE])
    } else {
      result <- df1
    }
    
    return(result)
  }
  newnames=c()
  ct=0
  for (name in names(matrices_list)) {
    ct=ct+1
    if ( verbose ) print(name)
    # Ensure the matrix multiplication is valid
    locnames = rownames( matrices_list[[name]] )
    edfnames = colnames(existing_df) 
    inames = intersect( locnames, edfnames )
    if ( length(inames) > 0 ) {
      # Perform matrix multiplication
      imat = data.matrix(existing_df[,inames])
      if ( robust ) imat = robustMatrixTransform( imat )
      if ( center | scale ) imat=scale(imat,center=center,scale=scale)
      features = data.matrix(matrices_list[[name]][inames,])
      features = take_abs_unsigned( features )
      features = l1_normalize_features(features)
      projection <- as.data.frame( imat %*% features)
      ##################################################
      # Update column names to reflect the matrix name #
      colnames(projection) = paste0( name, colnames( matrices_list[[name]] ) )
      # Combine the projections with the existing data frame
      if ( !is.null(n_limit )  ) {
        projection=projection[,1:n_limit]
      }
      newnames=c(newnames,colnames(projection))
      
      existing_df <- replbind(existing_df, projection)
      if ( verbose ) {
        print( inames )
        print( colnames(projection) )
        print(tail(colnames(existing_df)))
      }
    } else {
      warning(paste("Number of columns in existing data frame does not match number of rows in matrix", name))
    }
  }
  
  return( list(extendeddf=existing_df, newcolnames=newnames))
}



#' Apply simlr matrices to an existing data frame and combine the results with DTI naming fix.
#'
#' This function takes a list of matrices, applies each matrix via matrix multiplication
#' to an existing data frame, and combines the resulting projections with the original data frame.
#' Handles an as yet uncharacterized issue with column names in prior versions of data processing.
#'
#' @param existing_df An existing data frame to which the matrices will be applied.
#' @param matrices_list A list of matrices read from CSV files.
#' @param n_limit NULL or integer that can limit the number of projections
#' @param robust boolean
#' @param center boolean center the data before applying
#' @param scale boolean scale the data before applying
#' @param verbose boolean
#'
#' @return A list including (entry one) data frame with the original data frame combined with the projections (entry two) the new column names
#' @export
apply_simlr_matrices_dtfix <- function(existing_df, matrices_list, n_limit = NULL, robust = FALSE, 
                                       center = FALSE, scale = FALSE, verbose = FALSE) {
  # Get column names for comparison
  existing_df_cols = colnames(existing_df)
  gg = grep("DTI_",existing_df_cols)
  existing_df_fix = existing_df
  dta_correspondence=FALSE
  dt_correspondence=FALSE
  matrices_list_fix = matrices_list
  if ( length(gg) > 0 ) {
    existing_df_cols = existing_df_cols[ gg ]
    # Shorten the names for comparison
    shortened_existing_df_cols = shorten_pymm_names(existing_df_cols)
    dt_cols=NULL
    dta_cols=NULL
    if ( "dt" %in% names(matrices_list) ) {
      rownames(matrices_list_fix$dt)=shorten_pymm_names( rownames(matrices_list$dt ) )
      dt_cols = rownames(matrices_list_fix$dt)
      shortened_dt_cols = shorten_pymm_names(dt_cols)
      dt_correspondence = sum(shortened_existing_df_cols %in% shortened_dt_cols) > sum(existing_df_cols %in% dt_cols)
    }
    if ( "dta" %in% names(matrices_list) ) {
      rownames(matrices_list_fix$dta)=shorten_pymm_names( rownames(matrices_list$dta ) )
      dta_cols = rownames(matrices_list_fix$dta)
      shortened_dta_cols = shorten_pymm_names(dta_cols)
      dta_correspondence = sum(shortened_existing_df_cols %in% shortened_dta_cols) > sum(existing_df_cols %in% dta_cols)
    } 
    if ( dt_correspondence || dta_correspondence ) {
      message("Shortened names improve dt correspondence. Applying shortened names...")
      colnames(existing_df_fix)[gg] = shortened_existing_df_cols
    } 
  }
  
  # Apply SIMLR matrices
  dd = apply_simlr_matrices(existing_df = existing_df_fix, matrices_list = matrices_list_fix, 
                            n_limit = n_limit, robust = robust, center = center, 
                            scale = scale, verbose = verbose)
  
  # Restore the original column names (if they were changed)
  if (dt_correspondence || dta_correspondence) {
    colnames(dd[[1]])[gg] = existing_df_cols
  }
  
  return(dd)
}



#' Get Quality Control (QC) Metric Names
#'
#' This function returns a vector of quality control (QC) metric names used in the \code{antspymm} package. 
#' These metrics include volume measures, reflection errors, PSNR (Peak Signal-to-Noise Ratio), CNR (Contrast-to-Noise Ratio), 
#' and various metrics related to motion correction in rsfMRI and DTI.
#'
#' @return A character vector containing the names of QC metrics.
#' @examples
#' qc_names <- antspymm_qc_names()
#' print(qc_names)
#' @export
antspymm_qc_names <- function() {
  zz <- c(
    'T1Hier_resnetGrade',
    "msk_vol", "T2Flair_msk_vol", "NM1_msk_vol", "NM2_msk_vol", "NM3_msk_vol", "NM4_msk_vol", "NM5_msk_vol", "DTI1_msk_vol", "DTI2_msk_vol", 
    "rsf1_msk_vol", "rsf2_msk_vol", "rsf3_msk_vol", "reflection_err", "T2Flair_reflection_err", "T2Flair_score_reflection_err", "NM1_reflection_err", 
    "NM1_score_reflection_err", "NM2_reflection_err", "NM2_score_reflection_err", "NM3_reflection_err", "NM3_score_reflection_err", 
    "NM4_reflection_err", "NM4_score_reflection_err", "NM5_reflection_err", "NM5_score_reflection_err", "DTI1_reflection_err", "DTI2_reflection_err", 
    "rsf1_reflection_err", "rsf2_reflection_err", "rsf3_reflection_errpsnr", "T2Flair_psnr", "NM1_psnr", "NM2_psnr", "NM3_psnr", "NM4_psnr", 
    "NM5_psnr", "DTI1_psnr", "DTI2_psnr", "rsf1_psnr", "rsf2_psnr", "rsf3_psnr", "T1Hier_evratio", "T2Flair_wmh_evr", "rsfMRI_fcnxpro134_bold_evr", 
    "rsfMRI_fcnxpro122_bold_evr", "rsfMRI_fcnxpro129_bold_evr", "NM2DMT_NM_evr", "T2Flair_flair_evr", "DTI_dti_fa_evr", "cnr", "T2Flair_cnr", 
    "NM1_cnr", "NM2_cnr", "NM3_cnr", "NM4_cnr", "NM5_cnr", "DTI1_cnr", "DTI2_cnr", "rsf1_cnr", "rsf2_cnr", "rsf3_cnr", 
    "rsfMRI_fcnxpro134_motion_corrected_mean", "rsfMRI_fcnxpro134_high_motion_count", "rsfMRI_fcnxpro134_high_motion_pct", 
    "rsfMRI_fcnxpro122_motion_corrected_mean", "rsfMRI_fcnxpro122_high_motion_count", "rsfMRI_fcnxpro122_high_motion_pct", 
    "rsfMRI_fcnxpro129_motion_corrected_mean", "rsfMRI_fcnxpro129_high_motion_count", "rsfMRI_fcnxpro129_high_motion_pct", 
    "DTI_dti_high_motion_count", "rsfMRI_fcnxpro134_minutes_original_data", "rsfMRI_fcnxpro134_minutes_censored_data", 
    "rsfMRI_fcnxpro122_minutes_original_data", "rsfMRI_fcnxpro122_minutes_censored_data", "rsfMRI_fcnxpro129_minutes_original_data", 
    "rsfMRI_fcnxpro129_minutes_censored_data"
  )
  zz <- zz[ -grep("score", zz) ]
  zz <- zz[ zz != "" ]
  return( unique( zz ) )
}



#' Impute missing data using GLM models
#'
#' @param dataframe A data frame containing the data to impute.
#' @param columns_to_impute A vector of column names to impute.
#' @param predictor_columns A vector of column names to use as predictors.
#' @param family A string specifying the GLM family (default is 'gaussian').
#' @return A data frame with imputed values.
#' @examples
#' set.seed(123)
#' df <- data.frame(
#'   age = c(25, 30, 35, NA, 45, 50, NA, 40, 35, NA),
#'   income = c(50000, 60000, 70000, 80000, 90000, 100000, 110000, NA, 120000, 130000),
#'   education = c(12, 16, 14, 12, NA, 18, 20, 16, 14, 12)
#' )
#' columns_to_impute <- c("age")
#' predictor_columns <- c( "income", "education")
#' imputed_data <- glm_impute(df, columns_to_impute, predictor_columns, family = 'gaussian')
#' print(imputed_data)
#' @export
glm_impute <- function(dataframe, columns_to_impute, predictor_columns, family = 'gaussian') {
  for (column in columns_to_impute) {
    # Create the formula for the GLM
    formula <- as.formula(paste(column, "~", paste(predictor_columns, collapse = "+")))
    
    # Identify rows where neither the target nor predictors are missing
    complete_cases <- complete.cases(dataframe[, c(column, predictor_columns)])
    
    # Fit the GLM model on the complete cases
    model <- glm(formula, data = dataframe[complete_cases, ], family = family)
    
    # Identify rows where the target is missing but predictors are available
    rows_to_impute <- is.na(dataframe[[column]]) & complete.cases(dataframe[, predictor_columns])
    
    # Predict the missing values using the fitted model
    predictions <- predict(model, newdata = dataframe[rows_to_impute, ])
    
    # Replace the missing values with the predictions
    dataframe[rows_to_impute, column] <- predictions
  }
  
  return(dataframe)
}

#' Impute missing SiMLR data in a specified column based on other columns
#'
#' @param dataframe A data frame containing the data to impute.
#' @param nms A vector of base column names.
#' @param vecnum A numeric value to append to the column names.
#' @param toimpute The base name of the target column to be imputed.
#' @param separator A string specifying the separator between the column name and feature.
#' @param family A string specifying the GLM family (default is 'gaussian').
#' @return A data frame with imputed values.
#' @examples
#' set.seed(123)
#' n=50
#' df <- data.frame(
#'   t1PC1 = rnorm(n),
#'   t1aPC1 = rnorm(n),
#'   dtPC1 = rnorm(n),
#'   dtaPC1 = rnorm(n),
#'   rsfPC1 = rnorm(n),
#'   perfPC1 = rnorm(n)
#' )
#' df[ sample(1:nrow(df),20),6 ]=NA
#' nms <- c("t1", "t1a", "dt", "dta", "rsf", "perf")
#' vecnum <- 1
#' toimpute <- "perf"
#' df = simlr_impute(df, nms, vecnum, toimpute, family = 'gaussian')
#' @export
simlr_impute <- function(dataframe, nms, vecnum, toimpute, separator="PC", family = 'gaussian') {
  # Create the list of predictor columns excluding the target column to be imputed
  predictor_columns <- as.vector(sapply(nms[nms != toimpute], function(x) paste0(x, paste0(separator, vecnum))))
  
  # Specify the target column to be imputed
  columns_to_impute <- paste0(toimpute, separator, vecnum)
  
  # Use the glm_impute function to impute missing values
  imputed_dataframe <- glm_impute(dataframe, columns_to_impute, predictor_columns, family)
  
  return(imputed_dataframe)
}


#' Visualize Permutation Test Results
#'
#' This function visualizes the results of a permutation test by plotting a histogram of the
#' permutation test statistics. A red dotted line indicates the location of the original unpermuted
#' test statistic.
#'
#' @param permutation_results A numeric vector of permutation test statistics.
#' @param original_stat A numeric value representing the original unpermuted test statistic.
#' @param stat_name A character string representing the name of the test statistic.
#' @param plot_title string for plot title
#' @param bin_width optional bin width for the histogram
#'
#' @return A ggplot object showing the histogram of permutation test statistics with the original
#' test statistic marked.
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' n_perms <- 1000
#' permutation_results <- rnorm(n_perms, mean = 0, sd = 1)
#' original_stat <- 2
#' visualize_permutation_test(permutation_results, original_stat, "Simulated Statistic")
#' }
visualize_permutation_test <- function(permutation_results, original_stat, stat_name, plot_title, bin_width=0.1 ) {
  # Create a data frame for plotting
  plot_data <- data.frame(statistic = permutation_results)
  if ( missing( plot_title ) ) plot_title = paste("Permutation Test Results for", stat_name)
  # Generate the plot
  # if ( missing( bin_width ) ) {
  #  p <- ggplot(plot_data, aes(x = statistic)) +
  #    geom_histogram( fill = "blue", color = "black", alpha = 0.7) 
  # } else  p <- ggplot(plot_data, aes(x = statistic)) +
  #  geom_histogram( binwidth = bin_width, fill = "blue", color = "black", alpha = 0.7) 
  #  p = p + geom_vline(xintercept = original_stat, color = "red", linetype = "dotted", linewidth = 1.2) +    labs(title = plot_title,
  #        x = paste(stat_name, "Statistic"),
  #       y = "Frequency") + theme_minimal()
  
  p <- ggpubr::gghistogram(plot_data, x = 'statistic', bins = 50, title=plot_title) +
    ggplot2::geom_vline(xintercept = original_stat, linetype = "dotted", color='red' )
  return( p )
}


#' Exploratory Clustering and Visualization
#'
#' This function performs automated clustering using PAM and silhouette method,
#' and visualizes the results using ggplot2 and ggdendro.
#'
#' @param data A data frame with numeric columns.
#' @param dotsne boolean
#' @param verbose boolean
#' @return A list containing the combined plot and the optimal k value.
#' @export
exploratory_visualization <- function(data, dotsne=FALSE, verbose=FALSE ) {
  if ( verbose ) print("clustering")
  # Find optimal k using pamk
  pamk_result <- fpc::pamk(scale(data), krange = 2:10)
  optimal_k <- pamk_result$nc
  
  # Perform PAM clustering with optimal k
  pam_cluster <- cluster::pam(scale(data), k = optimal_k)
  
  if ( verbose ) print("pairwise correlations")
  # Create plots
  p1 <- GGally::ggpairs(data, columns = 1:ncol(data), 
                        upper = list(continuous = "points"), 
                        lower = list(continuous = "cor"))
  
  if ( dotsne ) {
    if ( verbose ) print("tsne")
    tsne_data <- tsne::tsne(data, k = 2)
    tsne_data <- data.frame(X1 = tsne_data[, 1], X2 = tsne_data[, 2], cluster = pam_cluster$clustering)
    # fix for no visible binding for global variable NOTE
    X1 = X2 = cluster = NULL
    rm(list = c("X1", "X2", "cluster"))    
    p2 <- ggplot2::ggplot(tsne_data, 
                          ggplot2::aes(x = X1, y = X2, 
                                       color = factor(cluster))) +
      ggplot2::geom_point() +
      ggplot2::theme_minimal() + 
      ggplot2::ggtitle("TSNE projection")
  }
  
  if ( verbose ) print("dendrogram")
  data_dist <- stats::dist(scale(t(data)))
  data_cluster <- stats::hclust(data_dist, method = "ward.D2")
  p3 <- ggdendro::ggdendrogram(data_cluster, rotate = TRUE) +
    ggplot2::theme_minimal() + 
    ggplot2::ggtitle("Dendrogram")
  
  if ( verbose ) print("join plots")
  # Combine plots into a single page display
  if ( dotsne ) {
    p_combined <- p2 + patchwork::wrap_elements(GGally::ggmatrix_gtable(p1)) + p3
  } else p_combined <- patchwork::wrap_elements(GGally::ggmatrix_gtable(p1)) + p3
  # Return combined plot and optimal k
  list( plot = p_combined, optimal_k = optimal_k)
}



#' Generate Predictors from ANTsPyMM Imaging Data
#'
#' This function generates a list of variable names to be used as predictors
#' in a model, based antspymm tabular version of imaging data.
#' It filters and processes the data to create meaningful predictors. LRAVG
#'
#' @param demog A dataframe containing demographic and imaging data.
#' @param doasym boolean
#' @param return_colnames boolean
#' @return A dataframe with processed demographic and imaging data.
#' @examples
#' # predictors <- antspymm_predictors(demographic_data)
#' @export
#'
#' @importFrom rsvd rsvd
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
antspymm_predictors <- function( demog, doasym=FALSE, return_colnames=FALSE ) {
  badcaud=getNamesFromDataframe("bn_str_ca",demog)
  badcaud=badcaud[ -grep("deep",badcaud)]
  xcl=c("hier_id",'background','SNR','evr','mask','msk','smoothing','minutes', "RandBasis",'templateL1', 'upsampl', 'paramset', 'nc_wm', 'nc_csf', 'censor','bandpass', 'outlier', 'meanBold', 'dimensionality', 'spc', 'org','andwidth',
        'unclassified', 'cleanup', 'slice', badcaud, 'dimx', 'dimy', 'dimz','dimt', 'modality' )
  if ( doasym & return_colnames ) xcl=c(xcl,'left','right',"_l_","_r_")
  t1namesbst = getNamesFromDataframe( c("T1Hier",'brainstem','vol'), demog, exclusions=c("tissues","lobes"))[-1]
  testnames=c(
    getNamesFromDataframe( "T1w_" , demog, exclusions=xcl),
    getNamesFromDataframe( "mtl" , demog, exclusions=xcl),
    getNamesFromDataframe( "cerebellum" , demog, exclusions=c(xcl,"_cerebell")),
    getNamesFromDataframe( "T1Hier_" , demog, exclusions=c("hier_id","[.]1","[.]2","[.]3",'background','tissue','dktregions','T1Hier_resnetGrade','hemisphere','lobes','SNR','evr','area',xcl)),
    t1namesbst,
    getNamesFromDataframe( "rsfMRI_fcnxpro" , demog, exclusions=c("hier_id",'background','thk','area','vol','FD','dvars','ssnr','tsnr','motion','SNR','evr','_alff','falff_sd','falff_mean',xcl)),
    getNamesFromDataframe( "perf_" , demog, exclusions=c("hier_id",'background','thk','area','vol','FD','dvars','ssnr','tsnr','motion','SNR','evr','_alff','falff_sd','falff_mean',xcl)),
    getNamesFromDataframe( "DTI_" , demog, exclusions=c("hier_id",'background','thk','area','vol','motion','FD','dvars','ssnr','tsnr','SNR','evr','cnx','relcn',xcl)) )
  testnames = unique( testnames )
  testnames = intersect( testnames, colnames(demog))
  if ( return_colnames ) return( testnames )
  
  if ( FALSE ) {
    testnames = c(
      getNamesFromDataframe( "Asym" , demog ),
      getNamesFromDataframe( "LRAVG" , demog ) ) %>% unique()
    testnames = testnames[ -multigrep( c("DTI_relcn_LRAVG","DTI_relcn_Asym"), testnames ) ]
    # special LR avg for falff
    falffnames = getNamesFromDataframe( c("falff"), demog, exclusions=c('mean','sd','Unk'))
  }
  
  tempnames=colnames(demog)
  tempnames=gsub("Right","right",tempnames)
  tempnames=gsub("Left","left",tempnames)
  colnames(demog)=tempnames
  
  if ( doasym )
    demog=mapAsymVar( demog, 
                      testnames[ grep("_l_", testnames) ], '_l_', "_r_" )
  demog=mapLRAverageVar( demog, 
                         testnames[ grep("_l_", testnames) ], '_l_', "_r_" )
  if ( doasym )
    demog=mapAsymVar( demog, 
                      testnames[ grep("left", testnames) ] )
  demog=mapLRAverageVar( demog, 
                         testnames[ grep("left", testnames) ] )
  return( demog )
}



#' Determine the Variable Type Based on Its Name
#'
#' This function inspects the input character vector for specific patterns
#' indicating the type of variable (e.g., "T1", "rsfMRI", "DTI", "NM2") and
#' returns a corresponding string identifier for the first matched type.
#' If no known pattern is found, it returns `NA`.
#'
#' @param x A character vector containing names or identifiers to be checked
#'          against known patterns for variable types. It must be a character vector.
#'
#' @return A character string indicating the type of variable matched based
#'         on the predefined patterns ("T1", "rsfMRI", "DTI", "NM2DMT").
#'         Returns `NA` if no pattern matches.
#'
#' @examples
#' antspymm_vartype("This is a T1 weighted image")  # Returns "T1"
#' antspymm_vartype("Subject underwent rsfMRI")    # Returns "rsfMRI"
#' antspymm_vartype("DTI sequence")                # Returns "DTI"
#' antspymm_vartype("Analysis of NM2")             # Returns "NM2DMT"
#' antspymm_vartype("Unknown type")                # Returns NA
#'
#' @note This function only checks for the first occurrence of a pattern
#'       and does not account for multiple different patterns within the
#'       same input. The order of pattern checking is fixed and may affect
#'       the result if multiple patterns are present.
#'
#' @export
antspymm_vartype <- function(x) {
  # Validate input
  if (!is.character(x)) {
    stop("Input must be a character vector.")
  }
  
  # Define patterns and corresponding returns in a named list
  patterns <- list(
    T1 = "T1", rsfMRI = "rsfMRI", DTI = "DTI", NM2 = "NM2DMT", T2="T2Flair", 
    t1 = "T1", rsfmri = "rsfMRI", dti = "DTI", nm2 = "NM2DMT", t2="T2Flair", 
    t1 = "T1", rs = "rsfMRI", dt = "DTI", nm2 = "NM2DMT", t2="T2Flair", 
    perf='perf')
  
  # Iterate through the patterns
  for (pattern in names(patterns)) {
    if (any(grepl(pattern, x))) {
      return(patterns[[pattern]])
    }
  }
  
  # Default return if no pattern matched
  return(NA)
}



#' return nuisance variable strings
#' 
#' these strings can be used with getNamesFromDataFrame or multigrep to 
#' either include or exclude nuisance variables.
#' 
#' @export
antspymm_nuisance_names <-function(){
  xcl = c("snr_","bandp","_mean","censor","smooth","outlier","motion","FD","despik","_nc_","_evr","minut","left","right","paramset","_sd","upsampling","mhdist","RandBasis","templateL1")
  return( xcl )
}

#' shorter antspymm names
#' @param x string or strings 
#' @return string
#' @author Avants BB
#' 
#' @export
shorten_pymm_names <-function(x){
  
  shorten_nm_names <- function(voinames) {    
    voinames <- gsub("nm2dmt.nm.", "nm.", voinames, fixed = TRUE)
    voinames <- gsub(".avg.", ".iavg.", voinames, fixed = TRUE)
    voinames <- gsub("intmean", "iavg", voinames)
    voinames <- gsub("intsum", "isum", voinames)
    voinames <- gsub("volume", "vol", voinames)
    voinames <- gsub("substantianigra", "sn", voinames)    
    return(voinames)
  }
  
  xx=tolower(x)
  xx=gsub("_",".",xx)
  xx=gsub("..",'.',xx,fixed=TRUE)
  xx=gsub("..",'.',xx,fixed=TRUE)
  xx=gsub( "sagittal.stratum.include.inferior.longitidinal.fasciculus.and.inferior.fronto.occipital.fasciculus.","ilf.and.ifo",xx,fixed=TRUE)
  xx=gsub(".cres.stria.terminalis.can.not.be.resolved.with.current.resolution.","",xx,fixed=TRUE)
  xx=gsub("longitudinal.fasciculus",'l.fasc',xx,fixed=TRUE)
  xx=gsub("corona.radiata",'cor.rad',xx,fixed=TRUE)
  xx=gsub("central",'cent',xx,fixed=TRUE)
  xx=gsub("deep.cit168",'dp.',xx,fixed=TRUE)
  xx=gsub("cit168",'',xx,fixed=TRUE)
  xx=gsub(".include",'',xx,fixed=TRUE)
  xx=gsub("mtg.sn",'',xx,fixed=TRUE)
  xx=gsub("brainstem",'.bst',xx,fixed=TRUE)
  xx=gsub("rsfmri.",'rsf.',xx,fixed=TRUE)
  xx=gsub("dti.mean.fa.",'dti.fa.',xx,fixed=TRUE)
  xx=gsub("perf.cbf.mean.",'cbf.',xx,fixed=TRUE)
  xx=gsub(".jhu.icbm.labels.1mm",'',xx,fixed=TRUE)
  xx=gsub(".include.optic.radiation.",'',xx,fixed=TRUE)
  xx=gsub("..",'.',xx,fixed=TRUE)
  xx=gsub("..",'.',xx,fixed=TRUE)
  xx=gsub("cerebellar.peduncle",'cereb.ped',xx,fixed=TRUE)
  xx=gsub("anterior.limb.of.internal.capsule",'ant.int.cap',xx,fixed=TRUE)
  xx=gsub("posterior.limb.of.internal.capsule",'post.int.cap',xx,fixed=TRUE)
  xx=gsub("t1hier.",'t1.',xx,fixed=TRUE)
  xx=gsub("anterior",'ant',xx,fixed=TRUE)
  xx=gsub("posterior",'post',xx,fixed=TRUE)
  xx=gsub("inferior",'inf',xx,fixed=TRUE)
  xx=gsub("superior",'sup',xx,fixed=TRUE)
  xx=gsub("dktcortex",'.ctx',xx,fixed=TRUE)
  xx=gsub(".lravg",'',xx,fixed=TRUE)
  xx=gsub("dti.mean.fa",'dti.fa',xx,fixed=TRUE)
  xx=gsub("retrolenticular.part.of.internal","rent.int.cap",xx,fixed=TRUE)
  xx=gsub("iculus.could.be.a.part.of.ant.internal.capsule","",xx,fixed=TRUE)
  xx=gsub("iculus.could.be.a.part.of.ant.internal.capsule","",xx,fixed=TRUE)
  xx=gsub(".fronto.occipital.",".frnt.occ.",xx,fixed=TRUE)
  xx=gsub(".longitidinal.fasciculus.",".long.fasc.",xx,fixed=TRUE)
  xx=gsub(".longitidinal.fasciculus.",".long.fasc.",xx,fixed=TRUE)
  xx=gsub(".external.capsule",".ext.cap",xx,fixed=TRUE)
  xx=gsub("of.internal.capsule",".int.cap",xx,fixed=TRUE)
  xx=gsub("fornix.cres.stria.terminalis","fornix.",xx,fixed=TRUE)
  xx=gsub("capsule","",xx,fixed=TRUE)
  xx=gsub("and.inf.frnt.occ.fasciculus.","",xx,fixed=TRUE)
  xx=gsub("crossing.tract.a.part.of.mcp.","",xx,fixed=TRUE)
  xx=gsub("post.thalamic.radiation.optic.radiation","post.thalamic.radiation",xx,fixed=TRUE)
  xx=gsub("adjusted",'adj',xx,fixed=TRUE)
  xx=gsub("..",'.',xx,fixed=TRUE)
  xx=gsub("t1w.mean","t1vth",xx,fixed=TRUE)
  xx=gsub("fcnxpro129","p2",xx,fixed=TRUE)
  xx=gsub("fcnxpro134","p3",xx,fixed=TRUE)
  xx=gsub("fcnxpro122","p1",xx,fixed=TRUE)
  xx=shorten_nm_names(xx)
  #    for ( x in 1:length(xx) ) {
  #      xx[x]=substr(xx[x],0,40)
  #    }
  return(xx)
}



#' Interpret SiMLR Vector
#'
#' This function interprets a vector from SiMLR (similarity-driven multivariate linear reconstruction)
#' specifically focusing on a given variable (e.g., a specific principal component or cluster). It extracts and normalizes the vector associated 
#' with the specified SiMLR variable, sorts it to identify the top elements, and optionally filters out non-significant values. 
#' This function is useful for understanding the contribution of different features in the context of the SiMLR analysis. Assumes this input is generated by antspymm_simlr.
#'
#' @param simlrResult a specific v matrix out of SiMLR
#' @param simlrVariable A string specifying the variable within `simlrResult` to interpret. The variable 
#' name should include both an identifier (e.g., "PC" for principal component) and a numeric index.
#' @param n2show An integer specifying the number of top elements to show from the sorted, normalized vector. 
#' Defaults to 5. If `NULL` or greater than the length of the vector, all elements are shown.
#' @param shortnames boolean
#' @param return_dataframe boolean
#' @return A named vector of the top `n2show` elements (or all if `n2show` is `NULL` or too large), 
#' sorted in decreasing order of their absolute values. Elements are named according to their identifiers 
#' in `simlrMats` and filtered to exclude non-significant values (absolute value > 0).
#' @examples
#' # This example assumes you have SiMLR result `simlrResult`, matrices `simlrMats`, and you want to 
#' # interpret the first principal component "PC1".
#' # simlrResult <- list(v = list(PC = matrix(runif(20), ncol = 2)))
#' # simlrMats <- list(PC = matrix(runif(100), ncol = 10))
#' # simlrVariable <- "PC1"
#' # interpretedVector <- interpret_simlr_vector2(simlrResult$v[[1]] )
#' # print(interpretedVector)
#' @importFrom stringr str_match str_extract
#' @export
interpret_simlr_vector2 <- function( simlrResult, simlrVariable, n2show = 5, shortnames=TRUE, return_dataframe=FALSE ) {
  
  split_string_correctly <- function(input_string) {
    # Extract the leading alphabetic characters (possibly including numbers within the alphabetic segment)
    alpha_part <- str_match(input_string, "([A-Za-z0-9]+(?=[A-Za-z]+[0-9]+$))[A-Za-z]*")[,1]
    
    # Extract the numeric part at the end
    numeric_part <- str_extract(input_string, "[0-9]+$")
    
    c( alpha_part, numeric_part)
  }
  varparts = split_string_correctly( simlrVariable )
  varparts[1]=gsub("PC","",varparts[1])
  if ( shortnames ) {
    nmslist=shorten_pymm_names( rownames( simlrResult ) )
  } else {
    nmslist = rownames( simlrResult )
  }
  
  # Extract the vector for the given modality and region, and normalize it
  t1vec <- (simlrResult[, simlrVariable ])
  t1vec=t1vec/max(abs(t1vec))
  
  # Assign names to the vector elements from the names list
  names(t1vec) <- nmslist
  
  # Sort the vector in decreasing order and select the top 'n2show' elements
  # If 'n2show' is NULL or greater than the length of t1vec, use the length of t1vec
  n_items_to_show <- if (is.null(n2show)) length(t1vec) else min(c(n2show, length(t1vec)))
  t1vec_sorted <- head(t1vec[order(abs(t1vec), decreasing = TRUE)], n_items_to_show)
  if ( all(t1vec <= 0 ) ) t1vec_sorted=abs(t1vec_sorted)
  # Filter out non-significant values (absolute value > 0)
  t1vec_filtered <- t1vec_sorted[abs(t1vec_sorted) > 0]
  if ( return_dataframe ) {
    t1vec_filtered=data.frame( anat=names(t1vec_filtered), values=t1vec_filtered)
  }
  return(t1vec_filtered)
}


#' Update Residuals for SiMLR
#'
#' This function updates residuals for SiMLR by applying different covariate transformations.
#'
#' @param mats A list of matrices.
#' @param x An index to select a matrix from the list.
#' @param covariate A character string specifying the covariate transformation to apply.
#' @param blaster2 A data frame containing additional covariate data.
#' @param allnna A logical vector indicating non-missing data points.
#' @param n.comp An integer specifying the number of components.
#' @param opt A character string. If set to "opt", the function will print available covariate options.
#'
#' @return The residual matrix after applying the specified covariate transformation.
#' If `opt` is set to "opt", the function will print available covariate options and return NULL.
#'
#' @examples
#' \dontrun{
#' antspymm_simlr_update_residuals(mats, 1, "whiten", blaster2, allnna, 5)
#' antspymm_simlr_update_residuals(opt = "opt")
#' }
#' @export
antspymm_simlr_update_residuals <- function(mats, x, covariate, blaster2, allnna, n.comp, opt = NULL) {
  covariate_options <- c(
    "whiten", "lowrank", "robust", "center", "rank", "scale", "mean", "centerAndScale", "np",
    "formula such as T1Hier_resnetGrade + snr + EVR + psnr"
  )
  
  if (!is.null(opt) && opt == "opt") {
    print("Available covariate options:")
    print(covariate_options)
    return(invisible(NULL))
  }
  
  if (is.null(covariate)) return(mats[[x]])
  
  nc <- min(c(n.comp * 2, nrow(mats[[x]]) - 1))
  
  if (covariate == "whiten") {
    return(icawhiten(data.matrix(mats[[x]]), n.comp = nc))
  }
  if (covariate == "lowrank") {
    return(lowrankRowMatrix(data.matrix(mats[[x]]), nc))
  }
  if (covariate == "robust") {
    return(robustMatrixTransform(data.matrix(mats[[x]])))
  }
  if (covariate == "center") {
    return(scale(data.matrix(mats[[x]]), center = TRUE, scale = FALSE))
  }
  if (covariate == "rank") {
    return(rank_and_scale(data.matrix(mats[[x]])))
  }
  if (covariate == "scale") {
    return(scale(data.matrix(mats[[x]]), center = FALSE, scale = TRUE))
  }
  if (covariate == "centerAndScale") {
    return(scale(data.matrix(mats[[x]]), center = TRUE, scale = TRUE))
  }
  if (covariate == "np") {
    temp = data.matrix(mats[[x]])
    np = prod( dim( temp ) )
    return( temp * 1.0/( np ) )
  }
  if (covariate == "mean") {
    mymean <- rowMeans(data.matrix(mats[[x]]))
    covariate2 <- "mymean"
  } else {
    covariate2 <- covariate
  }
  
  formula <- as.formula(paste("data.matrix(mats[[", x, "]]) ~ ", covariate2))
  fit <- lm(formula, data = blaster2[allnna, ])
  residuals(fit)
}

#' Perform SiMLR Analysis on Multimodal ANTsPyMM Data
#'
#' This function processes multimodal data using SiMLR. It is designed
#' to be flexible, allowing for various preprocessing steps and analysis options. The analysis
#' can be adjusted through multiple parameters, offering control over the inclusion of certain
#' data types, permutation testing, and more.
#'
#' @param blaster A dataframe containing multimodal data for analysis.
#' @param select_training_boolean boolean vector to define which entries are in training data
#' @param connect_cog Vector of column names to be treated as a special target matrix;  often used for cognitive data and in a superivsed variant of simlr.  Exclude this argument if this is unclear.
#' @param energy The type of energy model to use for similarity analysis. Usually 'reg' or 'cca'.
#'              Other options include 'lrr', 'regression', 'base.pca', 'base.spca', 'base.rand.1', and 'base.rand.0'.
#' @param nsimlr Number of components.
#' @param constraint orthogonality constraint of the form constraintxFloatWeightEnergyxFloatWeightGrad where constraints is ortho, Stiefel or Grassmann or GrassmannInv
#' @param covariates any covariates to adjust training matrices. if covariates is set to 'mean' then the rowwise mean will be factored out of each matrix.  this can be a vector e.g. \code{c('center','scale','rank')}. pass the name opt to antspymm_simlr_update_residuals to have the function print the options.
#' @param myseed Seed for random number generation to ensure reproducibility. Defaults to 3.
#' @param doAsym integer 0 for FALSE, 1 for TRUE and 2 for separate matrices for asymm variables.
#' @param returnidps Logical indicating whether to return the intermediate processing steps' results. Defaults to FALSE.
#' @param restrictDFN Logical indicating whether to restrict analysis to default network features. Defaults to FALSE.
#' @param resnetGradeThresh image quality threshold (higher better).
#' @param doperm Logical indicating whether to perform permutation tests. Defaults to FALSE.  Will randomize image features in the training data and thus leads to "randomized" but still regularized projections.
#' @param exclusions vector of strings to exclude from predictors
#' @param inclusions vector of strings to include in predictors
#' @param sparseness vector or scalar value to set sparseness
#' @param iterations int value to set max iterations
#' @param path_modeling the result of a call to \code{simlr_path_models(n)}
#' @param sparsenessAlg NA is default otherwise basic, spmp or orthorank
#' @param verbose boolean
#' @return A list containing the results of the similarity analysis and related data.
#' @export
#' @examples
#' # Example usage:
#' # result <- antspymm_simlr(dataframe)
antspymm_simlr = function( blaster, select_training_boolean, connect_cog,  
                           energy, nsimlr, constraint, 
                           covariates='1', myseed=3,  doAsym=TRUE, returnidps=FALSE, restrictDFN=FALSE,
                           resnetGradeThresh=1.02, doperm=FALSE, 
                           exclusions=NULL, inclusions=NULL, sparseness=NULL, iterations=NULL, path_modeling=NULL, 
                           sparsenessAlg=NA, verbose=FALSE ) 
{
  if ( missing( nsimlr ) ) nsimlr = 5
  safegrep <- function(pattern, x, ...) {
    result <- grep(pattern, x, ...)
    if (length(result) == 0) {
      return(1:length(x))
    }
    return(result)
  }
  myenergies = c('cca','reg','lrr','regression',"base.pca" , "base.spca", "base.rand.1", "base.rand.0" )
  if ( !energy %in% myenergies ) {
    stop( paste0("energy must be one of ", paste(myenergies, collapse=", ")))
  }
  safeclean = function( pattern, x,fixed=FALSE, exclude=TRUE) {
    mysub=grep(pattern,x,fixed=fixed)
    if ( length(mysub) == 0 ) return( x )
    if ( exclude ) return( x[-mysub] ) else return( x[mysub] )
  }
  idps=antspymm_predictors(blaster,TRUE,TRUE)
  rsfnames = idps[ grepl("rsfMRI",idps) ]
  if ( length(rsfnames) > 0 ) rsfnames = rsfnames[ safegrep("_2_",rsfnames)]
  if ( !all(grepl("rsfMRI", rsfnames )) ) rsfnames=c()
  if ( !is.null(exclusions)) {
    for ( x in exclusions ) {
      idps=safeclean(x,idps)
      rsfnames=safeclean(x,rsfnames)
    }
  }
  if ( !is.null(inclusions)) {
    for ( x in inclusions ) {
      idps=safeclean(x,idps,exclude=FALSE)
      rsfnames=safeclean(x,rsfnames,exclude=FALSE)
    }
  }
  idps=idps[ -multigrep(antspymm_nuisance_names()[-3],idps)]
  if ( doAsym == 0 ) {
    idps=safeclean("Asym",idps)
  } else {
    idps=safeclean("Asymcit168",idps)
  }
  idps=safeclean("cleanup",idps)
  idps=safeclean("snseg",idps)
  idps=safeclean("_deep_",idps)
  idps=safeclean("peraf",idps)
  idps=safeclean("alff",idps)
  idps=safeclean("LRAVGcit168",idps)
  idps=safeclean("_l_",idps,fixed=TRUE)
  idps=safeclean("_r_",idps,fixed=TRUE)
  if ( restrictDFN ) {
    rsfnames = rsfnames[ safegrep("Default",rsfnames)]
  } else {
    #    rsfnames = rsfnames[ multigrep( c("imbic","TempPar"),rsfnames)]
  }
  perfnames = idps[ multigrep( c("perf_cbf_mean_"),idps,intersect=TRUE)]
  t1names = idps[ multigrep( c("T1Hier"),idps,intersect=TRUE)]
  dtnames = unique( c( 
    idps[ multigrep( c("mean_fa","DTI"),idps,intersect=TRUE)],
    idps[ multigrep( c("mean_md","DTI"),idps,intersect=TRUE)] ))
  
  t1asymnames=c()
  dtasymnames=c()
  pfasymnames=c()
  if ( doAsym == 2 ) {
    t1nms = t1names
    t1asymnames = t1nms[ grep("Asym",t1nms)]
    t1names = t1nms[ !( t1nms %in%  t1asymnames ) ]
    
    dtnms = dtnames
    dtasymnames = dtnms[ grep("Asym",dtnms)]
    dtnames = dtnames[ !( dtnames %in%  dtasymnames ) ]
    
    pfnms = perfnames
    pfasymnames = pfnms[ grep("Asym",pfnms)]
    perfnames = pfnms[ !( pfnms %in%  pfasymnames ) ]
  }
  
  idps=unique(t1names)
  idplist = list()
  idplist[["t1"]]=t1names
  if ( length(dtnames) > 0 ) {
    idps = c( idps, unique(dtnames) )
    idplist[["dt"]]=dtnames
  }
  
  if ( length(rsfnames) > 0 ) {
    idps = c( idps, unique(rsfnames) )
    idplist[["rsf"]]=rsfnames
  }
  if ( length(perfnames) > 0 ) {
    idps = c( idps, unique(perfnames) )
    idplist[["perf"]]=perfnames
  }
  if ( length(t1asymnames) > 0 ) {
    idps = c( idps, unique(t1asymnames) )
    idplist[["t1a"]]=t1asymnames
  }
  
  if ( length(dtasymnames) > 0 ) {
    idps = c( idps, unique(dtasymnames) )
    idplist[["dta"]]=dtasymnames
  }
  
  if ( length(pfasymnames) > 0 ) {
    idps = c( idps, unique(pfasymnames) )
    idplist[["pfa"]]=pfasymnames
  }
  if ( !missing( connect_cog ) ) { 
    idplist[["cg"]]=connect_cog
  }
  if ( verbose ) {
    print(names(idplist))
    print(sample(idps,10))
  }
  if ( returnidps ) return(idps)
  allnna=select_training_boolean[  blaster$T1Hier_resnetGrade >= resnetGradeThresh ]
  blaster2=blaster[  blaster$T1Hier_resnetGrade >= resnetGradeThresh, ]
  stopifnot( min(dim(blaster2)) > 3 )
  if ( verbose ) {
    print("dim( subsetdataframe)")
    print(dim(blaster2) )
  }
  #################################################
  nperms=0
  matsFull = list()
  mats = list()
  for ( kk in 1:length(idplist)) {
    matsFull[[ names(idplist)[kk] ]] = blaster[,idplist[[kk]]]
    mats[[ names(idplist)[kk] ]] = antsrimpute( blaster2[allnna,idplist[[kk]]] )
  }
  if ( verbose ) print("mats done")
  if ( doperm ) {
    nada=setSeedBasedOnTime()
    sss=sample( 1:nrow( matsFull[[1]]  ))
    for ( jj in 1:length( mats ) ) {
      ss=sample( 1:nrow( mats[[jj]]  ))
      mats[[jj]]=mats[[jj]][sample( 1:nrow( mats[[jj]]  )),]
    }
  }
  nms = names(mats)
  regs0 = list()
  rank_and_scale <- function(mat) {
    # Function to rank transform and scale a single column
    rank_and_scale_col <- function(col) {
      ranked_col <- rank(col, ties.method = "average") # Rank the column
      scaled_col <- 2 * ((ranked_col - min(ranked_col)) / (max(ranked_col) - min(ranked_col))) - 1 # Scale to range -1 to 1
      return(scaled_col)
    }
    
    # Apply the rank_and_scale_col function to each column of the matrix
    result <- apply(mat, 2, rank_and_scale_col)
    return(result)
  }
  
  if ( verbose) print("setting up regularization")
  for ( mycov in covariates ) {
    print(paste("adjust by:",mycov))
    for ( x in 1:length(mats)) {
      if ( verbose ) {
        if ( x == 1 ) print(paste("training n= ",nrow(mats[[x]])))
        cat(paste0(names(mats)[x],"..."))
      }
      mats[[x]]=antspymm_simlr_update_residuals( mats, x, mycov, blaster2, allnna, n.comp=nsimlr )
      mats[[x]]=data.matrix(mats[[x]])
    }}
  for ( x in 1:length(mats)) {
    mycor = cor( mats[[x]] )
    mycor[mycor < 0.8]=0
    regs0[[x]]=data.matrix(mycor)
  }
  names(regs0)=names(mats)
  regs = regs0 # regularizeSimlr( mats, fraction=0.05, sigma=rep(2.0,length(mats)) )
  if ( verbose ) print("regularizeSimlr done")
  names(regs0)=names(mats)
  names(regs)=names(mats)
  if ( !missing( connect_cog ) ) {
    regs[["cg"]] = Matrix::Matrix(regs0[["cg"]], sparse = TRUE) 
    print("regularize cg")
  }

  if (  grepl('base.rand',energy) ) {
    if ( verbose ) {
      print(paste("antsr_random_features begin",energy))
    }
    get_seed <- function(s) as.integer(sub(".*\\.", "", s))
    temp = antsr_random_features( mats, nsimlr, seed = get_seed(energy) )
    for ( k in 1:length(mats)) {
      rownames(temp[[k]]) = colnames(mats[[k]])
    }
    return( list( simlrX=list(v=temp) ))
  } else if ( energy == 'base.pca' ) {
    nsimlrmin = min(c(nsimlr,unlist(lapply( mats, ncol))))
    if ( verbose ) {
      print(paste("antsr_pca_features begin",energy,nsimlrmin))
    }
    if ( nsimlrmin < nsimlr ) {
      message(paste("dimensionally adjusted: nsimlr ",nsimlrmin))
    } else nsimlrmin=nsimlr
    return( list( simlrX=list(v=antsr_pca_features( mats, nsimlrmin ) ) ))
  } else if ( energy == 'base.spca' ) {
    nsimlrmin = min(c(nsimlr,unlist(lapply( mats, ncol))))
    if ( verbose ) {
      print(paste("antsr_spca_features begin",energy,nsimlrmin))
    }
    if ( nsimlrmin < nsimlr ) {
      message(paste("dimensionally adjusted: nsimlr ",nsimlrmin))
    } else nsimlrmin=nsimlr
    return( list( simlrX=list(v=antsr_spca_features( mats, nsimlrmin ) ) ))
  }
  
  #  if ( !doperm )
  #    for ( pp in 1:length(regs)) plot(image(regs[[pp]]))
  
  if ( verbose ) print("loop mat")
  for ( k in 1:length(mats)) {
    if ( ncol(mats[[k]]) != ncol(regs[[k]]) ) {
      regs[[k]]=Matrix::Matrix(regs0[[k]], sparse = TRUE) 
      msg=paste("regularization cols not equal",k,ncol(mats[[k]]),ncol(regs[[k]]),names(mats)[k])
      message(msg)
      # stop( )
    }
  }
  if ( verbose ) print("loopmatdone")
  ########### zzz ############
  myjr = T
  prescaling = c( 'center', 'np' )
  optimus = 'lineSearch'
  maxits = 1000
  if ( ! is.null( iterations ) ) maxits = iterations
  if ( verbose ) print( paste( "maxits",maxits) )
  ebber = 0.99
  pizzer = rep( "positive", length(mats) )
  objectiver='cca';mixer = 'pca'
  if ( energy %in% c('reg','regression') ) {
    objectiver='regression';mixer = 'ica'
    if ( missing( constraint ) )
      constraint='orthox0.1x0.1'
  } else {
    if ( missing( constraint ) )
      constraint='Grassmannx1000x1000'
  }
  if ( energy == 'lrr') {
    objectiver='lowRankRegression';mixer = 'pca'
  }
  if ( verbose ) print("sparseness begin")
  sparval = rep( 0.8, length( mats ))
  if ( ! is.null( sparseness ) ) {
    if ( length( sparseness ) == length(mats) ) {
      sparval = sparseness
    } else sparval = rep( sparseness[1], length( mats ))
    if ( verbose ) {
      print('sparseness')
      print(sparseness)
    }
  }
  
  if ( nsimlr < 1 ) {
    ctit=0
    for ( jj in 1:length(mats) ) {
      ctit=ctit+ncol(mats[[jj]])
      sparval[jj] = 1.0 - 20/ncol(mats[[jj]])
      if ( sparval[jj] < 0 ) sparval[jj] = 0.5
    }
    nsimlr = round( ctit * nsimlr )
    message(paste("nsimlr",nsimlr))
    #    print(paste("nsimlr",nsimlr))
    #    print(sparval)
  }
  
  if ( verbose ) {
    print("initu begin")
  }
  initu = initializeSimlr(
    mats,
    nsimlr,
    jointReduction = myjr,
    zeroUpper = FALSE,
    uAlgorithm = "pca",
    addNoise = 0 )
  if ( verbose ) print("initu done")
  
  # initu = initu[,(ncol(initu)-nsimlr):ncol(initu)]
  # initu = initu[,1:nsimlr]
  
  if ( ! missing( connect_cog ) ) {
    clist = list()
    inflammNums=which(names(mats)=='cg')
    for ( j in 1:length( mats ) ) clist[[j]] = inflammNums
    for ( j in inflammNums )
      clist[[j]] = (1:length(mats))[ -inflammNums ]
  } else clist=NULL
  
  if ( !is.null( path_modeling ) ) {
    clist = path_modeling
    if ( verbose ) {
      print('custom path modeling')
      print( clist )
    }
  }
  
  simlrX = simlr( mats, regs, 
                  iterations=maxits, 
                  verbose= !doperm,
                  randomSeed = myseed,
                  mixAlg=mixer,
                  energyType=objectiver,
                  scale = prescaling,
                  sparsenessQuantiles=sparval,
                  expBeta = ebber,
                  positivities = pizzer, 
                  connectors=clist,
                  constraint=constraint,
                  optimizationStyle=optimus,
                  sparsenessAlg=sparsenessAlg,
                  initialUMatrix=initu )
  for ( kk in 1:length(mats) ) {
    rownames(simlrX$v[[kk]])=idplist[[kk]]
    temp = simlrX$v[[kk]]
    if ( pizzer[kk] == 'positive' ) {
      #      for ( n in 1:ncol(temp)) temp[,n]=abs(temp[,n])/max(abs(temp[,n]))
      #      simlrX$v[[kk]]=eliminateNonUniqueColumns(temp)
    }
  }
  
  if ( verbose ) print('simlr done')
  #################
  nsimx=nsimlr
  nms = names( simlrX$v ) = names(mats)
  simmat = data.matrix(matsFull[[1]] )%*% abs( simlrX$v[[1]] )
  colnames( simmat ) = paste0(nms[1],colnames( simmat ))
  for ( j in 2:length(mats)) {
    if (names(mats)[j]=='cg' & pizzer[j] != 'positive' ) {
      temp = data.matrix(matsFull[[j]] ) %*% ( simlrX$v[[j]])
    } else temp = data.matrix(matsFull[[j]] ) %*% abs( simlrX$v[[j]] )
    colnames( temp ) = paste0(nms[j],colnames( temp ))
    simmat = cbind( simmat, temp )
  }
  blaster2sim = cbind( blaster, simmat )
  if ( verbose ) print('bound')
  nsim = ncol( simlrX$v[[1]] )
  simnames = colnames(simmat)
  kk=1
  nmats=1:length(matsFull)
  matsB=mats
  for ( kk in 1:length(mats)) matsB[[kk]]=data.matrix(matsB[[kk]])
  kk=length(mats)
  # temp = predictSimlr( matsB, simlrX, targetMatrix=kk, 
  #      sourceMatrices=nmats[nmats!=kk] )
  return( list( demog=blaster2sim, mats=matsFull, simnames=simnames, simlrX=simlrX, energy=energy ) )
  ################
}




#' Write a list of data frames to disk with SiMLR-specific naming convention
#'
#' This function writes each data frame in a list to a separate CSV file on disk,
#' using the names of each data frame to create unique filenames.
#'
#' @param data_list A list of data frames to write to disk.
#' @param file_prefix A character string to use as the prefix for the filenames.
#'
#' @return No return value, called for side effects.
#' @examples
#' mysim <- list(simlrX = list(v = list(
#'   data1 = data.frame(matrix(rnorm(147 * 171), nrow = 147, ncol = 171)),
#'   data2 = data.frame(matrix(rnorm(156 * 171), nrow = 156, ncol = 171))
#' )))
#' write_simlr_data_frames(mysim$simlrX$v, "output")
#' @export
write_simlr_data_frames <- function(data_list, file_prefix) {
  for (i in seq_along(data_list)) {
    # Generate a filename using the index
    file_name <- paste0(file_prefix, "_", names(data_list)[i], "_simlr.csv")
    
    # Write the data frame to disk
    write.csv(data_list[[i]], file_name, row.names = TRUE)
  }
}

#' Read a list of data frames from disk with SiMLR-specific naming convention
#'
#' This function reads a list of data frames from disk into a list,
#' assuming the files are named with a common prefix and the names of the data frames.
#' It converts the column named `X` to the row names of the read data frame.
#'
#' @param file_prefix A character string used as the prefix for the filenames.
#' @param data_names A character vector of names for the data frames.
#' @param verbose boolean
#'
#' @return A list of data frames read from disk with the column named `X` set as row names.
#' @examples
#' # data_names <- c("data1", "data2")
#' # data_list <- read_simlr_data_frames(file_prefix = "output", data_names = data_names)
#' # dim(data_list[[1]])
#' # dim(data_list[[2]])
#' @export
read_simlr_data_frames <- function(file_prefix, data_names, verbose=FALSE ) {
  data_list <- list()
  
  for (name in data_names) {
    # Generate the filename using the prefix and data names
    file_name <- paste0(file_prefix, "_", name, "_simlr.csv")
    
    # Read the data frame from disk
    if ( file.exists( file_name ) ) {
      df <- read.csv(file_name, row.names = 1)
      
      # Convert the column named `X` to row names, if it exists
      if ("X" %in% colnames(df)) {
        rownames(df) <- df$X
        df <- df[ , !colnames(df) %in% "X"]
      }
      
      # Store the data frame in the list
      data_list[[name]] <- df
    } else if ( verbose ) print(paste("missing",file_name))
  }
  
  return(data_list)
}


#' Interpret SiMLR Vector
#'
#' This function interprets a vector from SiMLR (similarity-driven multivariate linear reconstruction)
#' specifically focusing on a given variable (e.g., a specific principal component or cluster). It extracts and normalizes the vector associated 
#' with the specified SiMLR variable, sorts it to identify the top elements, and optionally filters out non-significant values. 
#' This function is useful for understanding the contribution of different features in the context of the SiMLR analysis.
#'
#' @param simlrResult A list containing SiMLR analysis results, which should include a matrix `v` 
#' representing vectors of interest (e.g., principal components).
#' @param simlrMats A list of matrices associated with SiMLR analysis, where each matrix corresponds 
#' to a different modality or data type analyzed by SiMLR.
#' @param simlrVariable A string specifying the variable within `simlrResult` to interpret. The variable 
#' name should include both an identifier (e.g., "PC" for principal component) and a numeric index.
#' @param n2show An integer specifying the number of top elements to show from the sorted, normalized vector. 
#' Defaults to 5. If `NULL` or greater than the length of the vector, all elements are shown.
#' @param shortnames boolean
#' @param return_dataframe boolean
#' @return A named vector of the top `n2show` elements (or all if `n2show` is `NULL` or too large), 
#' sorted in decreasing order of their absolute values. Elements are named according to their identifiers 
#' in `simlrMats` and filtered to exclude non-significant values (absolute value > 0).
#' @examples
#' # This example assumes you have SiMLR result `simlrResult`, matrices `simlrMats`, and you want to 
#' # interpret the first principal component "PC1".
#' # simlrResult <- list(v = list(PC = matrix(runif(20), ncol = 2)))
#' # simlrMats <- list(PC = matrix(runif(100), ncol = 10))
#' # simlrVariable <- "PC1"
#' # interpretedVector <- interpret_simlr_vector(simlrResult, simlrMats, simlrVariable)
#' # print(interpretedVector)
#' @importFrom stringr str_match str_extract
#' @export
interpret_simlr_vector <- function( simlrResult, simlrMats, simlrVariable, n2show = 5, shortnames=TRUE, return_dataframe=FALSE ) {
  
  split_string_correctly <- function(input_string) {
    # Extract the leading alphabetic characters (possibly including numbers within the alphabetic segment)
    alpha_part <- str_match(input_string, "([A-Za-z0-9]+(?=[A-Za-z]+[0-9]+$))[A-Za-z]*")[,1]
    
    # Extract the numeric part at the end
    numeric_part <- str_extract(input_string, "[0-9]+$")
    
    c( alpha_part, numeric_part)
  }
  varparts = split_string_correctly( simlrVariable )
  varparts[1]=gsub("PC","",varparts[1])
  nmslist=list()
  if ( shortnames ) {
    for ( k in 1:length(simlrMats) ) 
      nmslist[[names(simlrMats)[k]]]=shorten_pymm_names(colnames(simlrMats[[k]]))
  } else {
    for ( k in 1:length(simlrMats) ) 
      nmslist[[names(simlrMats)[k]]]=colnames(simlrMats[[k]])
  }
  
  # Extract the vector for the given modality and region, and normalize it
  t1vec <- abs(simlrResult$v[[varparts[1]]][, as.integer(varparts[2])])
  t1vec=t1vec/max(t1vec)
  
  # Assign names to the vector elements from the names list
  names(t1vec) <- nmslist[[varparts[1]]]
  
  # Sort the vector in decreasing order and select the top 'n2show' elements
  # If 'n2show' is NULL or greater than the length of t1vec, use the length of t1vec
  n_items_to_show <- if (is.null(n2show)) length(t1vec) else min(c(n2show, length(t1vec)))
  t1vec_sorted <- head(t1vec[order(t1vec, decreasing = TRUE)], n_items_to_show)
  
  # Filter out non-significant values (absolute value > 0)
  t1vec_filtered <- t1vec_sorted[abs(t1vec_sorted) > 0]
  if ( return_dataframe ) {
    t1vec_filtered=data.frame( anat=names(t1vec_filtered), values=t1vec_filtered)
  }
  return(t1vec_filtered)
}




#' Plot Features
#'
#' Create bar plots for each column in each data frame, showing only non-zero values.  Normalize each feature s.t. max is one.
#'
#' @param data_list A list of data frames.
#' @param take_abs boolean 
#' @param n_limit Integer, limit features to top n_limit with highest value
#'
#' @return A list of ggplot objects.
#'
#' @examples
#' \dontrun{
#' # Simulate data
#' set.seed(123)
#' feature_names <- paste("Feature", 1:10)
#' data_list <- list(
#'   df1 = data.frame(matrix(ifelse(runif(100) < 0.5, 0, runif(100)), nrow = 10)),
#'   df2 = data.frame(matrix(ifelse(runif(100) < 0.5, 0, runif(100)), nrow = 10))
#' )
#'
#' # Set rownames
#' rownames(data_list$df1) <- feature_names
#' rownames(data_list$df2) <- feature_names
#'
#' # Use the function
#' plots <- plot_features(data_list)
#'
#' # Display the plots
#' for (i in 1:length(plots)) {
#'   print(plots[[i]])
#' }
#' }
#' @export
plot_features <- function(data_list, take_abs = TRUE, n_limit = 12 ) {
  plots <- list()
  
  for (i in 1:length(data_list)) {
    df <- data_list[[i]]
    if (take_abs) df <- abs(df)
    df_name <- names(data_list)[i]
    
    for (j in 1:ncol(df)) {
      col_name <- colnames(df)[j]
      values <- df[, j]
      
      # Filter out zero values
      non_zero_values <- values[values != 0]
      non_zero_features <- rownames(df)[values != 0]
      non_zero_values <- non_zero_values / max(non_zero_values)
      
      # Select top n_limit features
      top_features <- head(data.frame(feature_names = non_zero_features, values = non_zero_values), n_limit)
      
      # Create the plot
      plot <- ggpubr::ggbarplot(
        top_features, x = "feature_names", y = "values", 
        main = paste('features !=0:', df_name, col_name), 
        xlab = "Feature", ylab = "Value", 
        rotate.x.text = 45, sort.val = "desc", fill = "lightblue") +
        ggpubr::theme_pubr() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      
      plots[[paste(df_name, col_name)]] <- plot
    }
  }
  
  return(plots)
}


#' Compute the Average Invariant Orthogonality Defect for Multiple Features
#'
#' This function calculates the average invariant orthogonality defect for a list of features, using the `invariant_orthogonality_defect` function. The orthogonality defect measures the deviation of feature vectors from orthogonality.
#'
#' @param p A list of matrices (or data frames), each representing a set of feature vectors.
#' 
#' @return A numeric value representing the average invariant orthogonality defect across all the provided feature matrices.
#' 
#' @details The function iterates over each matrix in the list `p` and applies the `invariant_orthogonality_defect` function to calculate the orthogonality defect for each matrix. The final result is the average orthogonality defect across all matrices.
#' 
#' @examples
#' # Example data: Two sets of feature matrices
#' feature1 <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
#' feature2 <- matrix(c(7, 8, 9, 10, 11, 12), nrow = 2)
#' feature_list <- list(feature1, feature2)
#' 
#' # Calculate the average orthogonality defect
#' avg_orthogonality_defect <- simlr_feature_orth(feature_list)
#' print(avg_orthogonality_defect)
#' 
#' @export
simlr_feature_orth <- function( p ) {
  oo = 0.0
  for ( k in 1:length(p)) oo = oo + invariant_orthogonality_defect( data.matrix( p[[k]] ))
  return( oo / length(p))
}






#' Multiview PCA
#'
#' Perform Multiview PCA on multiple datasets with an option for sparse PCA.
#'
#' @param views A list of data matrices for each view.
#' @param n_components Number of principal components to compute.
#' @param sparse vector of length views with values between zero and one
#' @param max_iter Maximum number of iterations for the optimization.
#' @param sparsenessAlg NA is default otherwise basic, spmp or orthorank
#' @param verbose Logical, whether to print information about energy and sparsity.
#' @return A list containing the common representation Z and transformation matrices W.
#' @examples
#' set.seed(123)
#' n_samples <- 100
#' n_features_1 <- 50
#' n_features_2 <- 60
#' n_features_3 <- 70
#' n_components <- 5
#' view1 <- matrix(rnorm(n_samples * n_features_1), nrow = n_samples, ncol = n_features_1)
#' view2 <- matrix(rnorm(n_samples * n_features_2), nrow = n_samples, ncol = n_features_2)
#' view3 <- matrix(rnorm(n_samples * n_features_3), nrow = n_samples, ncol = n_features_3)
#' result <- multiview_pca(list(view1, view2, view3), n_components, sparse = rep(0.5,3), 
#'  verbose = TRUE)
#' print(result)
#' @export
multiview_pca <- function(views, n_components, sparse = 0.5, max_iter = 100, sparsenessAlg='basic', verbose = FALSE) {
  # Validate alpha
  if ( any(sparse < 0) || any( sparse > 1) ) {
    stop("sparse must be between 0 and 1.")
  }
  
  n_views <- length(views)
  n_samples <- nrow(views[[1]])
  
  # Initialize Z (latent representation) with random values
  Z <- matrix(rnorm(n_samples * n_components), nrow = n_samples, ncol = n_components)
  
  # Initialize W_list: each W_i should have ncol(views[[i]]) rows and n_components columns
  W_list <- vector("list", n_views)
  for (i in seq_len(n_views)) {
    W_list[[i]] <- matrix(rnorm(ncol(views[[i]]) * n_components), nrow = ncol(views[[i]]), ncol = n_components)
    #    views[[i]]=views[[i]]/prod(dim(views[[i]]))
  }
  
  prev_Z <- Z
  tol <- 1e-6  # Tolerance for convergence
  
  # Iterative optimization
  for (iter in 1:max_iter) {
    # Fix W, solve for Z
    Z <- matrix(0, nrow = n_samples, ncol = n_components)
    for (i in seq_len(n_views)) {
      # Ensure matrix multiplication is conformable
      if (ncol(views[[i]]) == nrow(W_list[[i]])) {
        Z <- Z + views[[i]] %*% W_list[[i]]
      } else {
        stop("Non-conformable dimensions between views and W_list matrices.")
      }
    }
    Z <- Z / n_views
    
    # Fix Z, solve for W
    for (i in seq_len(n_views)) {
      # Non-sparse case: direct computation of W via least squares
      # Regularize Z'Z to ensure invertibility
      reg <- diag(1e-6, n_components)
      W_list[[i]] <- solve(t(Z) %*% Z + reg) %*% t(Z) %*% views[[i]]
      W_list[[i]] <- t(W_list[[i]])  # Transpose to match dimensions
      if ( sparse[i] > 0 & sparse[i] < 1) {
        for ( jj in 1:ncol(W_list[[i]]) ) {
          W_list[[i]] = orthogonalizeAndQSparsify( W_list[[i]], sparse[i], 'positive', 
                                                   sparsenessAlg=sparsenessAlg )
        }
      }
    }
    
    # Calculate energy (reconstruction error)
    energy <- 0
    for (i in seq_len(n_views)) {
      reconstruction <- Z %*% t(W_list[[i]])
      energy <- energy + sum((views[[i]] - reconstruction)^2)
    }
    
    # Print energy if verbose
    if (verbose) {
      cat(sprintf("Iteration %d: Energy = %.6f\n", iter, energy))
    }
    
    # Check for convergence
    if (max(abs(prev_Z - Z)) < tol) {
      message("Converged in ", iter, " iterations.")
      break
    }
    prev_Z <- Z
  }
  
  return(list(Z = Z, W = W_list))
}



#' Generate reproducible random feature projections for a list of voxel matrices
#'
#' This function applies random projection to each matrix in a list of voxel matrices.
#' It uses a fixed seed to ensure reproducibility across runs.
#'
#' @param voxmats A list of numeric matrices. Each matrix should have dimensions (subjects × voxels).
#' @param k Integer. Number of projection dimensions (features) to generate.
#' @param seed Integer. Random seed for reproducibility. Default is 42.
#'
#' @return A list of projection matrices, each with dimensions (voxels × k).
#' @export
antsr_random_features <- function(voxmats, k, seed = 42) {
  stopifnot(is.list(voxmats))
  stopifnot(all(sapply(voxmats, is.matrix)))
  set.seed(seed)
  plist = lapply(voxmats, function(m) {
    nvox <- ncol(m)
    projection_matrix <- 
    orthogonalizeAndQSparsify( matrix(rnorm(nvox * k), nrow = nvox, ncol = k), 0.8, positivity='positive' )
  })
  names(plist)=names(voxmats)
  for ( k in 1:length(voxmats)) {
    rownames(plist[[k]])=colnames(voxmats[[k]])
    prefix=paste0(names(voxmats)[k],"PC")
    prefix="PC"
    colnames(plist[[k]])=paste0(prefix,1:ncol(plist[[k]]))
  }
  return(plist)
}

#' Generate PCA-based feature projections for a list of voxel matrices
#'
#' This function applies principal component analysis (PCA) to each matrix in a list
#' of voxel matrices and returns a list of projection matrices (principal axes).
#'
#' @param voxmats A list of numeric matrices. Each matrix should have dimensions (subjects × voxels).
#' @param k Integer. Number of principal components to retain.
#'
#' @return A named list of projection matrices (voxels × k), one per input matrix.
#' @export
antsr_pca_features <- function(voxmats, k) {
  stopifnot(is.list(voxmats))
  stopifnot(all(sapply(voxmats, is.matrix)))

  plist <- lapply(voxmats, function(m) {
    max_components <- min(nrow(m), ncol(m))
    if (k > max_components) {
      warning(sprintf("Requested k = %d exceeds maximum possible components (%d) for a matrix with dimensions (%d × %d). Using k = %d instead.",
                      k, max_components, nrow(m), ncol(m), max_components))
      k_adj <- max_components
    } else {
      k_adj <- k
    }
    pca <- prcomp(m, center = TRUE, scale. = TRUE, rank. = k_adj)
    pca$rotation[, 1:k_adj, drop = FALSE]
  })

  names(plist) <- names(voxmats)
  return(plist)
}



#' Generate sparse PCA-based feature projections for a list of voxel matrices
#'
#' Applies sparse principal component analysis using the selected backend
#' ("elasticnet", "PMA", or "sparsepca") to each matrix in a list of subject × voxel data.
#'
#' @param voxmats A list of numeric matrices. Each matrix should be subjects × voxels.
#' @param k Integer. Number of components to retain.
#' @param method Character. Sparse PCA backend to use. One of "default", "elasticnet", "PMA", or "sparsepca".
#' @param para Sparsity control parameter(s). Interpretation depends on backend:
#'   - For "elasticnet": number of nonzero loadings per component (length-k vector).
#'   - For "PMA": L1 bound on loading vector (scalar or length-k).
#'   - For "sparsepca": ignored (uses built-in defaults).
#'   - For "default": length-k vector of sparsity parameters.
#' @return A named list of sparse projection matrices (voxels × k).
#' @export
antsr_spca_features <- function(voxmats, k, method = c( "default", "elasticnet", "PMA", "sparsepca"), para = NULL) {
  method <- match.arg(method)
  stopifnot(is.list(voxmats))
  stopifnot(all(sapply(voxmats, is.matrix)))

  if ( method == "default" ) {
    if ( is.null(para) ) {
      para <- rep(0.8, length(voxmats))  # Default sparsity
    } else if ( length(para) == 1 ) {
      para <- rep(para, length(voxmats))  # Replicate single value
    }
    loadings = antsr_pca_features( voxmats, k )
    for ( k in 1:length(loadings)) {
      loadings[[k]] = orthogonalizeAndQSparsify( loadings[[k]], para[k], positivity='positive' )
      }
    return(loadings)
    }

  plist <- lapply(voxmats, function(m) {
    max_k <- min(nrow(m), ncol(m))
    if (k > max_k) {
      warning(sprintf("Requested k = %d exceeds matrix rank (%d); reducing to k = %d", k, max_k, max_k))
      k_adj <- max_k
    } else {
      k_adj <- k
    }

    if (method == "elasticnet") {
      stopifnot(requireNamespace("elasticnet", quietly = TRUE))
      if (is.null(para)) {
        para <- rep(ceiling(ncol(m) / 2), k_adj)
      }
      stopifnot(length(para) == k_adj)
      m_scaled <- scale(m)
      gram_matrix <- cor(m_scaled)
      sfit <- elasticnet::spca(x = gram_matrix, K = k_adj, para = para,
                              type = "Gram", sparse = "varnum", trace = FALSE)
      loadings <- sfit$loadings[, 1:k_adj, drop = FALSE]
    } else if (method == "PMA") {
      stopifnot(requireNamespace("PMA", quietly = TRUE))
      if (is.null(para)) {
        para <- rep(2.0, k_adj)
      } else if (length(para) == 1) {
        para <- rep(para, k_adj)
      }
      stopifnot(length(para) == k_adj)

      loadings_list <- lapply(seq_len(k_adj), function(i) {
        comp <- PMA::SPC(x = scale(m), sumabsv = para[i], K = 1)
        comp$v[, 1]
      })
      loadings <- do.call(cbind, loadings_list)

    } else if (method == "sparsepca") {
      stopifnot(requireNamespace("sparsepca", quietly = TRUE))
      loadings <- tryCatch({
        sfit <- sparsepca::spca(X = scale(m), k = k_adj)
        if (is.null(sfit$rotation)) stop("rotation matrix is NULL")
        sfit$rotation[, 1:k_adj, drop = FALSE]
      }, error = function(e) {
        warning("Sparse PCA failed: ", conditionMessage(e))
        matrix(NA, nrow = ncol(m), ncol = k_adj)
      })
    } 
    rownames(loadings) <- colnames(m)
    colnames(loadings) <- paste0("PC", 1:ncol(loadings))
    loadings
  })

  names(plist) <- names(voxmats)
  return(plist)
}
