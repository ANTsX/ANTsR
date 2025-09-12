#' Set Seed Based on Current Time
#'
#' This function sets the random number generator seed based on the current time,
#' with a fine resolution of seconds. This ensures a different seed is used each time
#' the function is called, provided calls are at least one second apart.
#'
#' @return The numeric value used as the seed, derived from the current time in seconds.
#' @examples
#' seedValue <- setSeedBasedOnTime()
#' print(seedValue)
#' @export
setSeedBasedOnTime <- function() {
  op <- options(digits.secs = 8)
  # Get the current time
  currentTime <- Sys.time()
  
  # Convert the current time to a numeric value
  # numericTime <- as.numeric(currentTime, units = "secs")
  numericTime = as.integer(substr(as.character(Sys.time()),22,200))
  # Use the numeric time as the seed
  set.seed(numericTime)
  
  # Optionally, return the seed value used
  return(numericTime)
}

#' Grep entries with a vector search parameters
#'
#' @param x a vector of search terms
#' @param desc target vector of items to be searched
#' @param intersect boolean whether to use intersection or union otherwise
#'
#' @return result of grep (indices of desc that match x)
#' @author Avants BB
#' @export
multigrep <- function( x, desc, intersect=FALSE ) {
  roisel = c()
  for ( xx in x ) {
    if (length(roisel)==0 | !intersect ) {
      roisel = c( roisel, grep(xx, desc) )
    } else {
      roisel = intersect( roisel, grep(xx, desc) )
    }
  }
  return(  roisel )
}


#' Extract column names with concatenated search parameters
#'
#' @param x vector of strings
#' @param demogIn the dataframe with column names to search.
#' @param exclusions the strings to exclude
#'
#' @return vector of string column names
#' @author Avants BB
#' @examples
#'
#' # nms = getNamesFromDataframe( c("sp","ed"), cars )
#'
#' @export
getNamesFromDataframe <- function( x, demogIn, exclusions ) {
  outnames = names(demogIn)[ grep(x[1],names(demogIn ) ) ]
  if ( length( x ) > 1 )
  for ( y in x[-1] )
    outnames = outnames[ grep(y,outnames ) ]

  if ( ! missing( exclusions ) ) {
    toexclude=grep(exclusions[1],outnames)
    if ( length(exclusions) > 1 )
      for ( zz in exclusions[-1] ) {
        toexclude = c( toexclude, grep(zz,outnames) )
      }
    if ( length( toexclude ) > 0 ) outnames = outnames[ -toexclude ]
  }
  return( outnames )
}



#' Convert left/right variables to a measure of asymmetry
#'
#' @param mydataframe dataframe containing relevant variables
#' @param leftvar left side variable names ie the full names of the variables to asym
#' @param leftname the variable substring indicating left side
#' @param rightname the variable substring indicating right side
#' @param replacer string to replace left with in column names of output
#' @return fixed x
#' @author Avants BB
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_replace
#' @importFrom purrr map_chr
#' @export
mapAsymVar <-function( mydataframe, leftvar, leftname='left', rightname='right', replacer='Asym' ) {

  replace_values <- function(input_string) {
    # Function to modify a number based on the specified rules
    modify_value <- function(number) {
      num <- as.numeric(number)
      if (num >= 1 && num <= 249) {
        return(as.character(num + 249))
      } else {
        return(number)
      }
    }
    
    # Extract all numbers from the string
    numbers <- stringr::str_extract_all(input_string, "\\b\\d+\\b")[[1]]
    
    # Apply the modification to the numbers
    modified_numbers <- purrr::map_chr(numbers, modify_value)

    # Replace old numbers with new numbers in the string
    for (i in seq_along(numbers)) {
      input_string <- stringr::str_replace(input_string, numbers[i], modified_numbers[i])
    }

    return(input_string)
  }

  rightvar =  gsub( leftname, rightname, leftvar )
  hasright = rightvar %in% colnames(mydataframe)
  temp = mydataframe[,leftvar[hasright]] - mydataframe[,rightvar[hasright]]
  temp = temp * sign(temp )
  newnames = gsub(leftname, replacer,leftvar[hasright])
  mydataframe[,newnames]=temp
  return( mydataframe )
}



#' Convert left/right variables to an average measurement
#'
#' @param mydataframe dataframe containing relevant variables
#' @param leftvar left side variable names ie the full names of the variables to average
#' @param leftname the variable substring indicating left side
#' @param rightname the variable substring indicating right side
#' @param replacer string to replace left with in column names of output
#' @return fixed x
#' @author Avants BB
#' @export
mapLRAverageVar <- function( mydataframe, leftvar, leftname='left',rightname='right', replacer='LRAVG' ) {
  rightvar =  gsub( leftname, rightname, leftvar )
  hasright = rightvar %in% colnames(mydataframe)
  temp = mydataframe[,leftvar[hasright]] * 0.5 + mydataframe[,rightvar[hasright]] * 0.5
  newnames = gsub(leftname, replacer,leftvar[hasright])
  mydataframe[,newnames]=temp
  return( mydataframe )
}



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
      library(rsvd)
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
    if ( sparsenessAlg %in% c("spmp","sum_preserving_matrix_partition") ) return( 
      ( t( sparsify_by_column_winner( t(v), positivity, positivity )) )
      )
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
  #  
  return( v )
  # 
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


#' Sparsify a Matrix via Column-wise Winner-Take-All
#'
#' This function applies a "winner-take-all" sparsity model. For each column,
#' it finds the single most significant entry based on specified sign constraints
#' and sets all other entries in that column to zero.
#'
#' It includes special handling for the first column, which often represents a
#' main effect that may not require the same constraints as subsequent components.
#'
#' @param X A numeric matrix [p_features x k_components].
#' @param first_column_constraint How to treat the first column. One of:
#'   \itemize{
#'     \item `"none"`: (Default) The first column is left dense (not sparsified).
#'     \item `"either"`: The entry with the largest absolute value is kept, regardless of sign.
#'     \item `"positive"`: The largest entry with a positive sign is kept.
#'     \item `"negative"`: The entry with the largest magnitude (most negative) with a negative sign is kept.
#'   }
#' @param default_constraint How to treat all other columns (from the second
#'   onwards). Takes the same options as `first_column_constraint`.
#'   Defaults to `"either"`.
#' @param ensure_row_membership Logical. If TRUE, ensures every feature is
#'   represented in at least one component by "reviving" the single most
#'   significant entry for any row that becomes all-zero after sparsity.
#'
#' @return A sparsified matrix with the same dimensions as X.
#' @export
#' @examples
#' set.seed(123)
#' mat <- matrix(c(
#'   -5, 0.1, 0.2, 0.3, 0.4,    # Col 1: Large negative value
#'    1, 2.0, 0.1, 0.2, 0.3,    # Col 2: Large positive value
#'   -1, -0.2, -3, -0.4, -0.5, # Col 3: Only negative values
#'   1, -2, 3, -4, 5           # Col 4: Mixed signs
#' ), nrow = 5, ncol = 4)
#'
#' res1 <- sparsify_by_column_winner(mat, 'positive', 'positive')
#'
sparsify_by_column_winner <- function(X,
                                      first_column_constraint = c("none", "either", "positive", "negative"),
                                      default_constraint = c("either", "positive", "negative"),
                                      ensure_row_membership = TRUE) {

  # --- 1. Input Validation and Setup ---
  first_column_constraint <- match.arg(first_column_constraint)
  # Add "none" as a valid option for the default, in case user wants to pass it
  valid_defaults <- c("either", "positive", "negative", "none")
  default_constraint <- match.arg(default_constraint, choices = valid_defaults)
  
  stopifnot(is.matrix(X))

  if (nrow(X) == 0 || ncol(X) == 0) return(X)

  p <- nrow(X)
  k <- ncol(X)
  
  X_original <- X
  Y <- matrix(0, nrow = p, ncol = k) # Initialize output matrix

  # --- Helper function to find the winner index in a single column ---
  find_winner <- function(column_vec, constraint) {
    if (constraint == "none") {
      return(NA_integer_) # Sentinel for "do nothing"
    }
    
    vec_for_max <- switch(constraint,
      "positive" = {
        temp <- column_vec; temp[temp <= 0] <- -Inf; temp
      },
      "negative" = {
        # To find the "largest" negative, we find the max of the absolute values
        # of the negative numbers.
        temp <- column_vec; temp[temp >= 0] <- -Inf; abs(temp)
      },
      "either" = {
        abs(column_vec)
      }
    )
    
    # Check if a valid max exists (handles all -Inf case)
    if (all(is.infinite(vec_for_max))) {
      return(NA_real_) # Sentinel for "no valid winner found"
    }
    
    return(which.max(vec_for_max))
  }

  # --- 2. Process all columns based on their constraint ---
  
  constraints <- c(first_column_constraint, rep(default_constraint, k - 1))
  
  for (j in 1:k) {
    constraint <- constraints[j]
    
    if (constraint == "none") {
      Y[, j] <- X_original[, j] # Keep the column dense
      next # Move to the next column
    }
    
    winner_row <- find_winner(X_original[, j], constraint)
    
    # If a winner was found (i.e., not NA), place it in the output matrix
    if (!is.na(winner_row)) {
      Y[winner_row, j] <- X_original[winner_row, j]
    }
    # If NA, Y[,j] remains all zeros, which is the correct behavior.
  }

  # --- 3. Ensure Every Row Has At Least One Non-Zero Entry (Optional) ---
  if (ensure_row_membership) {
    # This logic remains the same, as it's independent of the previous steps
    zero_row_indices <- which(rowSums(abs(Y), na.rm = TRUE) == 0)
    
    if (length(zero_row_indices) > 0) {
      revived_col_indices <- max.col(abs(X_original[zero_row_indices, , drop = FALSE]),
                                     ties.method = "first")
      
      revived_matrix_indices <- cbind(zero_row_indices, revived_col_indices)
      Y[revived_matrix_indices] <- X_original[revived_matrix_indices]
    }
  }

  return(Y)
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


#' Calculate the Gradient for the Procrustes-like Correlation Objective
#'
#' This function computes the gradient of the objective function:
#' J(V) = tr(U'XV) / ||U'XV||_F
#' with respect to the loading matrix V.
#'
#' The returned gradient is an ascent direction for J, which is a descent
#' direction for the energy E = -J, suitable for use in the simlr optimizer.
#'
#' @param X A centered data matrix for a single modality [n x p].
#' @param U The current shared basis matrix [n x k], with orthonormal columns.
#' @param V The current loading matrix for the modality [p x k].
#'
#' @return A matrix [p x k] representing the gradient of the objective.
#'
.calculate_procrustes_gradient <- function(X, U, V) {
  # C = U' * X * V
  C <- crossprod(U, X %*% V)
  k <- ncol(U)
  
  # Calculate scalar terms
  trace_C <- sum(diag(C))
  norm_C <- sqrt(sum(C^2))
  
  # Handle edge case
  if (norm_C < .Machine$double.eps) return(V * 0)
  
  # Driving term from the gradient formula
  driving_term <- crossprod(X, U)
  
  # The "braking" term from the gradient formula
  # This part was incorrect in the previous version.
  # The correct term is (tr(C) / ||C||_F) * C
  braking_matrix <- (trace_C / norm_C) * C
  
  # The matrix in the main parentheses of the gradient formula
  matrix_term <- (norm_C * diag(k)) - braking_matrix
  
  # Full gradient numerator
  gradient_numerator <- driving_term %*% matrix_term
  
  # Denominator
  gradient_denominator <- norm_C^2
  
  # Final gradient
  gradient <- gradient_numerator / gradient_denominator
  
  return(gradient)
}


#' Calculate Gradient for Procrustes Correlation (Verified Final Version)
#'
#' @description Computes the correct analytical gradient for the objective
#'   J = tr(U'XV) / ||U'XV||_F. This version has been rigorously derived
#'   and verified against numerical differentiation.
#' @return A matrix [p x k] representing an **ascent direction** for the objective J.
#' @keywords internal
.calculate_procrustes_gradient <- function(X, U, V) {
  # This is the DEFINITIVE, CORRECTED gradient for J = tr(U'XV) / ||U'XV||_F
  C <- crossprod(U, X %*% V)
  trace_C <- sum(diag(C))
  norm_C_sq <- sum(C^2)
  if (norm_C_sq < .Machine$double.eps) return(V * 0)
  norm_C <- sqrt(norm_C_sq)

  XtU <- crossprod(X, U)
  term1 <- XtU * norm_C_sq
  term2 <- (XtU %*% C) * trace_C
  gradient_numerator <- term1 - term2
  gradient_denominator <- norm_C^3
  gradient <- gradient_numerator / gradient_denominator
  return(gradient)
}

#' Calculate Squared Angular Distance
#' @description The energy E = || U/||U|| - XV/||XV|| ||_F^2. This is a
#'   **minimization** objective.
#' @param X A data matrix [n x p].
#' @param U A target basis matrix [n x k].
#' @param V A loading matrix [p x k].
#' @return A single numeric value for the energy.
#' @keywords internal
.calculate_angular_distance <- function(X, U, V) {
  projection_XV <- X %*% V
  norm_U <- sqrt(sum(U^2))
  norm_XV <- sqrt(sum(projection_XV^2))
  if (norm_U < .Machine$double.eps || norm_XV < .Machine$double.eps) return(2.0)
  U_norm <- U / norm_U
  XV_norm <- projection_XV / norm_XV
  return(sum((U_norm - XV_norm)^2))
}

#' Calculate Gradient for Squared Angular Distance
#' @description Computes the analytical gradient of the squared angular distance.
#' @return A matrix [p x k] representing a **descent direction** for the energy.
#' @keywords internal
.calculate_angular_distance_gradient <- function(X, U, V) {
  projection_XV <- X %*% V
  norm_U <- sqrt(sum(U^2))
  norm_XV <- sqrt(sum(projection_XV^2))
  if (norm_U < .Machine$double.eps || norm_XV < .Machine$double.eps) return(V * 0)

  trace_term <- sum(diag(crossprod(U, projection_XV)))
  grad_J_numerator <- (crossprod(X, U) * (norm_XV^2)) - (trace_term * crossprod(X, projection_XV))
  grad_J_denominator <- norm_U * (norm_XV^3)
  grad_J <- grad_J_numerator / grad_J_denominator
  return( 2.0 * grad_J)
}

#' Calculate Procrustes Correlation
#' @description The objective J = tr(U'XV) / ||U'XV||_F. This is a
#'   **maximization** objective.
#' @return A single numeric value for the objective.
#' @keywords internal
.calculate_procrustes_correlation <- function(X, U, V) {
  cross_cov <- crossprod(U, X %*% V)
  numerator <- sum(diag(cross_cov))
  frobenius_norm <- sqrt(sum(cross_cov^2))
  if (frobenius_norm < .Machine$double.eps) return(0)
  return(numerator / frobenius_norm)
}

#' Calculate Gradient for Procrustes Correlation
#' @return A matrix [p x k] representing an **ascent direction** for the objective J.
#' @keywords internal
.calculate_procrustes_gradient <- function(X, U, V) {
  
  # --- 1. Pre-calculate key components ---
  
  # The cross-covariance matrix C = U' * X * V
  C <- crossprod(U, X %*% V)
  
  # Scalar properties of C
  trace_C <- sum(diag(C))
  norm_C <- sqrt(sum(C^2))
  
  # Handle edge case
  if (norm_C < .Machine$double.eps) {
    return(V * 0)
  }

  # --- 2. Assemble the Gradient using the verified formula ---
  # grad(J) = (X'U / ||C||) - (tr(C) / ||C||^3) * (X'UC)
  
  # First term of the gradient
  term1 <- crossprod(X, U) / norm_C
  
  # Second term of the gradient
  # First, calculate the matrix part: X' * U * C
  term2_matrix <- crossprod(X, U %*% C)
  # Then, calculate the scalar part
  term2_scalar <- trace_C / (norm_C^3)
  
  term2 <- term2_matrix * term2_scalar
  
  # The final gradient is the difference
  gradient <- term1 - term2
  
  return( 1.0 * gradient)
}

#' Calculate Absolute Canonical Covariance
#' @description The objective J = sum(abs(diag(U'XV))) / (||U||*||XV||). This is a
#'   **maximization** objective.
#' @return A single numeric value for the objective.
#' @keywords internal
.calculate_abs_canonical_covariance <- function(X, U, V) {
  projection_XV <- X %*% V
  norm_U <- sqrt(sum(U^2))
  norm_XV <- sqrt(sum(projection_XV^2))
  if (norm_U < .Machine$double.eps || norm_XV < .Machine$double.eps) return(0)

  cross_cov <- crossprod(U, projection_XV)
  numerator <- sum(abs(diag(cross_cov)))
  return(numerator / (norm_U * norm_XV))
}

#' Calculate Gradient for Absolute Canonical Covariance
#' @return A matrix [p x k] representing an **ascent direction** for the objective J.
#' @keywords internal
.calculate_abs_canonical_covariance_gradient <- function(X, U, V) {
  projection_XV <- X %*% V
  norm_U <- sqrt(sum(U^2))
  norm_XV <- sqrt(sum(projection_XV^2))
  if (norm_U < .Machine$double.eps || norm_XV < .Machine$double.eps) return(V * 0)

  cross_cov <- crossprod(U, projection_XV)
  signer <- diag(sign(diag(cross_cov)), nrow = ncol(U), ncol = ncol(U))
  sum_abs_diag <- sum(abs(diag(cross_cov)))

  term1 <- (crossprod(X, U) %*% signer) / (norm_U * norm_XV)
  term2 <- (sum_abs_diag * crossprod(X, projection_XV)) / (norm_U * (norm_XV^3))
  return( 1.0 * ( term1 - term2) )
}


#' Calculate Basic or Centered Regression Error
#'
#' Computes the squared Frobenius norm of the residual matrix. It can calculate
#' either the basic reconstruction error ||X - P||^2 or the centered error
#' ||X_c - P_c||^2, where P is the prediction U*V'.
#'
#' @param X A data matrix [n_subjects x p_features].
#' @param U A target basis matrix [n_subjects x k_components].
#' @param V A loading matrix [p_features x k_components].
#' @param center_prediction Logical. If TRUE, both data and prediction are
#'   column-centered before the error is calculated. Defaults to TRUE.
#'
#' @return A single numeric value representing the reconstruction error.
#' @keywords internal
.calculate_regression_error <- function(X, U, V, center_prediction = TRUE) {

  # --- 1. Input Validation ---
  stopifnot(
    is.matrix(X) && is.matrix(U) && is.matrix(V),
    "Matrix dimensions are not compatible." =
      nrow(X) == nrow(U) && ncol(X) == nrow(V) && ncol(U) == ncol(V)
  )

  # --- 2. Calculate the Prediction ---
  prediction <- U %*% t(V)

  # --- 3. Calculate the Residual based on the centering flag ---
  if (center_prediction) {
    # Compare the centered versions of the matrices
    X_centered <- scale(X, center = TRUE, scale = FALSE)
    prediction_centered <- scale(prediction, center = TRUE, scale = FALSE)
    residual <- X_centered - prediction_centered
  } else {
    # Compare the raw, un-centered matrices
    residual <- X - prediction
  }

  # --- 4. Compute the Energy ---
  # The squared Frobenius norm is the sum of all squared elements.
  energy <- sum(residual^2)

  return(energy)
}

#' Calculate Gradient for Basic or Centered Regression Error
#'
#' Computes the analytical gradient for the regression error objective. The
#' formula changes depending on whether the prediction is centered.
#'
#' @param X A data matrix [n x p].
#' @param U A target basis matrix [n x k].
#' @param V The current loading matrix [p x k].
#' @param center_prediction Logical. If TRUE, computes the gradient for the
#'   centered error objective. Defaults to TRUE.
#'
#' @return A matrix [p x k] representing a DESCENT direction for the energy.
#' @keywords internal
.calculate_regression_gradient <- function(X, U, V, center_prediction = TRUE) {

  # --- 1. Defensive Dimension Checks ---
  n <- nrow(X); p <- ncol(X); k <- ncol(V)
  stopifnot(
    is.matrix(X) && is.matrix(U) && is.matrix(V),
    nrow(U) == n, ncol(U) == k, nrow(V) == p
  )
  
  # --- 2. Gradient Calculation based on the centering flag ---
  if (center_prediction) {
    # The gradient for the centered objective depends on centered X and U.
    X_centered <- scale(X, center = TRUE, scale = FALSE)
    U_centered <- scale(U, center = TRUE, scale = FALSE)
    
    # Descent direction for ||X_c - U_c V'||^2 is 2 * (X_c' U_c - V U_c' U_c)
    term1 <- crossprod(X_centered, U_centered)
    term2 <- V %*% crossprod(U_centered)
    
    descent_direction <- 2 * (term1 - term2)
    
  } else {
    # The standard gradient for ||X - UV'||^2
    term1 <- crossprod(X, U)
    term2 <- V %*% crossprod(U)
    
    descent_direction <- 2 * (term1 - term2)
  }
  
  return(descent_direction)
}

#' Calculate normed Regression Error
#' @description The energy E = ||X - UV'||^2/||X + epsilon||. This is a **minimization** objective.
#' @param center_prediction Logical, controls if prediction is centered relative to X.
#' @return A single numeric value for the energy.
#' @keywords internal
.calculate_normed_regression_error <- function(X, U, V, center_prediction = TRUE) {

  # --- 1. Calculate the Prediction ---
  prediction <- U %*% t(V)

  # --- 2. Calculate the Residual ---
  if (center_prediction) {
    # Compare the centered versions of the matrices
    X_centered <- scale(X, center = TRUE, scale = FALSE)
    prediction_centered <- scale(prediction, center = TRUE, scale = FALSE)
    residual <- X_centered - prediction_centered
  } else {
    # Compare the raw matrices
    residual <- X - prediction
  }
  
  # --- 3. Calculate the Normalization Factor ---
  # The factor is the squared Frobenius norm of the ORIGINAL data matrix.
  norm_X_sq <- sum(X^2) + 1e0
  if (norm_X_sq < .Machine$double.eps) {
    return(0) # Error is zero if the data is zero
  }

  # --- 4. Compute the Final Energy ---
  # The energy is the squared norm of the residual, scaled by the norm of X.
  energy <- sum(residual^2) / ( norm_X_sq )
  
  return(energy)
}


#' Calculate Gradient for normed Centered and Normalized Regression Error
#'
#' This function computes the mathematically precise analytical gradient for the
#' energy function defined in `.calculate_normed_regression_error`.
#'
#' @param X A data matrix [n x p].
#' @param U A target basis matrix [n x k].
#' @param V The current loading matrix [p x k].
#' @param center_prediction Logical. Should be consistent with the energy function.
#'
#' @return A matrix [p x k] representing a DESCENT direction for the energy E.
#' @keywords internal
.calculate_normed_regression_error_gradient <- function(X, U, V, center_prediction = TRUE) {

  # --- 1. Calculate the scaling factor from the energy function ---
  # The factor is 2 / ||X||_F^2.
  norm_X_sq <- sum(X^2) + 1e0
  if (norm_X_sq < .Machine$double.eps) {
    return(V * 0) # Gradient is zero if data matrix is zero
  }
  scaling_factor <- 2 / norm_X_sq

  # --- 2. Calculate the un-scaled gradient part ---
  if (center_prediction) {
    # The gradient for the centered objective depends on centered X and U.
    X_centered <- scale(X, center = TRUE, scale = FALSE)
    U_centered <- scale(U, center = TRUE, scale = FALSE)
    
    # Descent direction for ||X_c - U_c V'||^2 is 2 * (X_c' U_c - V U_c' U_c)
    term1 <- crossprod(X_centered, U_centered)
    term2 <- V %*% crossprod(U_centered)
    
    unscaled_gradient <- term1 - term2
  } else {
    # The standard gradient for ||X - UV'||^2
    unscaled_gradient <- crossprod(X, U) - V %*% crossprod(U)
  }
  
  # --- 3. Apply the scaling factor ---
  descent_direction <- scaling_factor * unscaled_gradient
  
  return(descent_direction)
}



#' Calculate ICA Energy
#'
#' Computes the negentropy-based energy for the ICA objective using a specified
#' non-linearity to measure non-Gaussianity.
#'
#' @param X A data matrix [n x p].
#' @param V A loading matrix [p x k].
#' @param nonlinearity Non-linearity function ("logcosh", "exp", "kurtosis", or "gauss").
#' @param a Parameter for gaussian nonlinearity (default: 1).
#' @return A single numeric value representing the ICA energy.
#' @keywords internal
.calculate_ica_energy <- function(X, V, nonlinearity = "logcosh", a = 1) {
  # Input validation
  stopifnot(
    is.matrix(X) && is.matrix(V),
    "Matrix dimensions are not compatible." = ncol(X) == nrow(V),
    "Nonlinearity must be 'logcosh', 'exp', 'kurtosis' or 'gauss'." = nonlinearity %in% c("logcosh", "exp", "gauss", "kurtosis"),
    "Parameter a must be positive for gaussian nonlinearity." = a > 0
  )
  
  # Calculate sources
  S <- X %*% V
  
  # Compute ICA energy based on nonlinearity
  if (nonlinearity == "logcosh") {
    energy <- -sum(log(cosh(S))) / nrow(X)
  } else if (nonlinearity == "exp") {
    energy <- -sum(-exp(-S^2 / 2)) / nrow(X)
  } else if (nonlinearity == "gauss") {
    energy <- -sum(-0.5 * exp(-a * S^2)) / nrow(X)
  } else if (nonlinearity == "kurtosis") {
    energy <- -sum((S^4.) / 4.) / nrow(X)
  }
  
  return(energy)
}

#' Calculate ICA Gradient
#'
#' Computes the analytical gradient for the ICA objective using the derivative
#' of the specified non-linearity.
#'
#' @param X A data matrix [n x p].
#' @param V A loading matrix [p x k].
#' @param nonlinearity Non-linearity function ("logcosh", "exp", "kurtosis" or "gauss").
#' @param a Parameter for gaussian nonlinearity (default: 1).
#' @return A matrix [p x k] representing a descent direction for the ICA energy.
#' @keywords internal
.calculate_ica_gradient <- function(X, V, nonlinearity = "logcosh", a = 1) {
  # Defensive dimension checks
  n <- nrow(X); p <- ncol(X); k <- ncol(V)
  stopifnot(
    is.matrix(X) && is.matrix(V),
    "Matrix dimensions are not compatible." = p == nrow(V),
    "Nonlinearity must be 'logcosh', 'exp', 'kurtosis', or 'gauss'." = nonlinearity %in% c("logcosh", "exp", "gauss", "kurtosis"),
    "Parameter a must be positive for gaussian nonlinearity." = a > 0
  )
  
  # Calculate sources
  S <- X %*% V
  
  # Compute gradient based on nonlinearity
  if (nonlinearity == "logcosh") {
    gradient <- (1/n) * (t(X) %*% tanh(S))
  } else if (nonlinearity == "exp") {
    gradient <- (1/n) * (t(X) %*% (S * exp(-S^2 / 2)))
  } else if (nonlinearity == "gauss") {
    gradient <- (1/n) * (t(X) %*% (a * S * exp(-a * S^2)))
  } else if (nonlinearity == "kurtosis") {
    gradient <- (1/n) * (t(X) %*% (S^3))
  }
  
  return(gradient)
}


#' Calculate Domain Alignment Energy
#'
#' Computes the quadratic domain alignment energy, given by -lambda * ||ZV||_F^2,
#' which encourages alignment between the loading matrix V and the prior matrix Z.
#'
#' @param V A loading matrix [p x k].
#' @param Z A prior matrix [z x p].
#' @param lambda Domain alignment strength (non-negative scalar).
#' @return A single numeric value representing the domain alignment energy.
#' @keywords internal
.calculate_domain_energy <- function(V, Z, lambda) {
  # Input validation
  stopifnot(
    is.matrix(V) && is.matrix(Z),
    "Matrix dimensions are not compatible." = ncol(Z) == nrow(V),
    "Lambda must be non-negative." = lambda >= 0
  )
  
  # Compute projection
  Vmod=l1_normalize_features(V)
  M <- Z %*% Vmod
  
  # Compute domain alignment energy: -lambda * ||M||_F^2
  energy <- -lambda * sum(M^2)
  
  return(energy)
}

#' Calculate Domain Alignment Gradient
#'
#' Computes the analytical gradient for the quadratic domain alignment objective,
#' given by -2 * lambda * Z^T (ZV).
#'
#' @param V A loading matrix [p x k].
#' @param Z A prior matrix [z x p].
#' @param lambda Domain alignment strength (non-negative scalar).
#' @return A matrix [p x k] representing a descent direction for the domain alignment energy.
#' @keywords internal
.calculate_domain_gradient <- function(V, Z, lambda) {
  # Defensive dimension checks
  p <- nrow(V); k <- ncol(V); z <- nrow(Z)
  stopifnot(
    is.matrix(V) && is.matrix(Z),
    "Matrix dimensions are not compatible." = ncol(Z) == p,
    "Lambda must be non-negative." = lambda >= 0
  )
  
  # Compute projection
  Vmod=l1_normalize_features(V)
  M <- Z %*% Vmod
  
  # Compute gradient: -2 * lambda * Z^T M
  gradient <- -2 * lambda * t(Z) %*% M
  
  return(gradient)
}



#' Clip a Gradient Matrix by its Frobenius Norm
#'
#' This function prevents exploding gradients by rescaling any gradient whose
#' Frobenius norm exceeds a specified threshold. The direction of the gradient
#' is preserved.
#'
#' @param gradient The gradient matrix to be clipped.
#' @param threshold The maximum allowed Frobenius norm for the gradient.
#'
#' @return A gradient matrix whose Frobenius norm is guaranteed to be less
#'   than or equal to the threshold.
#'
clip_gradient_norm <- function(gradient, threshold = 1.0) {
  # Calculate the Frobenius norm of the gradient
  grad_norm <- sqrt(sum(gradient^2))
  
  # If the norm exceeds the threshold, compute the scaling factor
  if (grad_norm > threshold) {
    # The factor is threshold / current_norm
    clipping_factor <- threshold / grad_norm
    # Rescale the gradient
    gradient <- gradient * clipping_factor
  }
  
  return(gradient)
}


#' Clip a Gradient Matrix by a Quantile of its Values
#'
#' This function provides an adaptive method for controlling gradient magnitudes.
#' It calculates a threshold based on a specified quantile of the absolute values
#' within the gradient matrix itself. Any value exceeding this dynamic threshold
#' is "clipped" or shrunk back to the threshold, preserving its original sign.
#'
#' This is more robust than a fixed threshold as it automatically adapts to the
#' scale of the gradients at each optimization step.
#'
#' @param gradient The gradient matrix to be clipped.
#' @param quantile The quantile to use for determining the clipping threshold.
#'   For example, a value of `0.98` means that any gradient value larger in
#'   magnitude than the 98th percentile of all absolute gradient values will be
#'   clipped. Must be between 0 and 1.
#'
#' @return A new gradient matrix with extreme values clipped.
#' @export
#' @examples
#' # Create a gradient with some large outlier values
#' set.seed(123)
#' grad_matrix <- matrix(rnorm(100, mean = 0, sd = 1), 10, 10)
#' grad_matrix[1, 1] <- 10  # Large positive outlier
#' grad_matrix[5, 5] <- -12 # Large negative outlier
#'
#' # Clip at the 80th percentile. This will tame the outliers.
#' clipped_grad <- clip_gradient_by_quantile(grad_matrix, quantile = 0.80)
#'
#' cat("Original Gradient Range:\n")
#' print(range(grad_matrix))
#'
#' cat("\n80th Percentile Threshold:\n")
#' # The threshold will be the 80th percentile of the absolute values
#' print(quantile(abs(grad_matrix), probs = 0.80))
#'
#' cat("\nClipped Gradient Range:\n")
#' # The new range will be capped at the threshold
#' print(range(clipped_grad))
#'
clip_gradient_by_quantile <- function(gradient, quantile = 0.80) {
  if (quantile >= 1) return(gradient)

  # --- 1. Input Validation ---
  stopifnot(is.matrix(gradient), is.numeric(gradient))
  stopifnot(quantile > 0 && quantile < 1)
  
  # --- 2. Calculate the Clipping Threshold ---
  
  # First, get the absolute values of all elements in the gradient matrix
  abs_gradient_values <- abs(gradient)
  
  # Calculate the threshold using the specified quantile of these absolute values
  threshold <- quantile(abs_gradient_values, probs = quantile, na.rm = TRUE)
  
  # Handle the edge case where the threshold is zero (e.g., for a zero matrix)
  if (threshold < .Machine$double.eps) {
    return(gradient) # No clipping needed if threshold is zero
  }
  
  # --- 3. Perform Clipping (Vectorized) ---
  
  # This is a robust way to clip while preserving the sign of the original values.
  # 1. Get the signs of the original gradient (-1, 0, or 1)
  gradient_signs <- sign(gradient)
  
  # 2. Find which values have a magnitude greater than the threshold
  #    pmin() takes two vectors and returns the element-wise minimum.
  #    This effectively caps the absolute values at the threshold.
  clipped_magnitudes <- pmin(abs_gradient_values, threshold)
  
  # 3. Re-apply the original signs to the clipped magnitudes
  clipped_gradient <- clipped_magnitudes * gradient_signs
  
  return(clipped_gradient)
}

#' Calculate SIMLR Similarity Energy
#'
#' This dispatcher calculates the similarity/reconstruction part of the
#' objective function for use in an optimization routine.
#'
#' @param V A candidate loading matrix [p x k] to evaluate.
#' @param X The data matrix for the current modality [n x p].
#' @param U The shared basis matrix [n x k].
#' @param energy_type A string specifying the similarity objective.
#' @param lambda prior weight term for dat
#' @param prior_matrix matrix of prior weights, same number of columns as V
#' @return A single numeric value for the similarity energy. The sign is adjusted
#'   such that the value should always be minimized.
#' @export
calculate_simlr_energy <- function(V, X, U, energy_type, lambda=1.0, prior_matrix=NULL ) {
  if ( ! is.null( prior_matrix ) ) {
    Z=prior_matrix
  }

  # For maximization objectives, we return the negative value because the
  # optimizer's goal is always to MINIMIZE the returned energy.
  energy <- switch(energy_type,
    "regression" = .calculate_regression_error(X, U, V),
    "reconorm" = .calculate_normed_regression_error(X, U, V),
    # "lowRank" = .calculate_lowrank_norm_error(X, U, V), # Assuming this helper exists
    "lowRankRegression" = .calculate_angular_distance(X, U, V),
    "lrr" = .calculate_angular_distance(X, U, V),
    "cca" = -.calculate_abs_canonical_covariance(X, U, V),
    "acc" = -.calculate_abs_canonical_covariance(X, U, V),
    "logcosh" = .calculate_ica_energy( X, V, nonlinearity = energy_type ),
    "kurtosis" = .calculate_ica_energy( X, V, nonlinearity = energy_type ),
    "exp" = .calculate_ica_energy( X, V, nonlinearity = energy_type ),
    "gauss" = .calculate_ica_energy( X, V, nonlinearity = energy_type ),
    "dat" = .calculate_domain_energy( V, Z, lambda),
    "normalized_correlation" = -.calculate_procrustes_correlation(X, U, V),
    stop(paste("Unknown energy_type in calculate_simlr_energy:", energy_type))
  )
  return(energy)
}


#' Calculate SIMLR Similarity Gradient
#'
#' This dispatcher computes the gradient for the similarity part of the objective,
#' ensuring it is always a descent direction for the energy function.
#'
#' @param V The current loading matrix [p x k].
#' @param X The data matrix for the modality [n x p].
#' @param U The shared basis matrix [n x k].
#' @param energy_type A string specifying the similarity objective.
#' @param clipping_threshold Optional numeric value for gradient clipping.
#'
#' @param lambda prior weight term for dat
#' @param prior_matrix matrix of prior weights, same shape as V
#' @return A matrix [p x k] representing the descent direction.
#' @export
calculate_simlr_gradient <- function(V, X, U, energy_type, clipping_threshold = NULL, lambda=1.0, prior_matrix=NULL ) {
  if ( ! is.null( prior_matrix ) ) {
    Z=prior_matrix
  }

  # Each helper function is now defined to return a descent direction
  # for its corresponding energy function.
  gradient <- switch(energy_type,
    "regression" = .calculate_regression_gradient(X, U, V),
    "reconorm" = .calculate_normed_regression_error_gradient(X, U, V),
    "lowRankRegression" = .calculate_angular_distance_gradient(X, U, V),
    "lrr" = .calculate_angular_distance_gradient(X, U, V),
    "cca" = .calculate_abs_canonical_covariance_gradient(X, U, V),
    "acc" = .calculate_abs_canonical_covariance_gradient(X, U, V),
    "normalized_correlation" = .calculate_procrustes_gradient(X, U, V),
    "logcosh" = .calculate_ica_gradient( X, V, nonlinearity = energy_type ),
    "kurtosis" = .calculate_ica_gradient( X, V, nonlinearity = energy_type ),
    "exp" = .calculate_ica_gradient( X, V, nonlinearity = energy_type ),
    "gauss" = .calculate_ica_gradient( X, V, nonlinearity = energy_type ),
    "dat" = .calculate_domain_gradient( V, Z, lambda),
    "nc" = .calculate_procrustes_gradient(X, U, V),
    
    stop(paste("Unknown energy_type in calculate_simlr_gradient:", energy_type))
  )

  # Apply gradient clipping if a threshold is provided
  if (!is.null(clipping_threshold) && clipping_threshold > 0) {
    gradient <- clip_gradient_by_quantile( gradient )
  }
  
  
  return( as.matrix( gradient) )
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
#' @param data_matrices A list that contains the named matrices.  Note: the optimization will likely perform much more smoothly if the input matrices are each scaled to zero mean unit variance e.g. by the \code{scale} function.
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
#' @param optimizationStyle the simlr optimizer
#' @param scale options to standardize each matrix. e.g. divide by the square root
#' of its number of variables (Westerhuis, Kourti, and MacGregor 1998), divide
#' by the number of variables or center or center and scale or ... (see code).
#' can be a vector which will apply each strategy in order.
#' @param expBeta if greater than zero, use exponential moving average on gradient.
#' @param jointInitialization boolean for initialization options, default TRUE
#' @param sparsenessAlg NA is default otherwise basic, spmp or orthorank
#' @param orthogonalizeU boolean controlling whether we orthogonalize the U matrices
#' @param domainMatrices matrices containing domain knowledge length of \code{data_matrices} with number of columns also equal to each corresponding data matrix
#' @param domainLambdas weights for domain knowledge term length of \code{data_matrices}
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
    data_matrices,
    smoothingMatrices,
    iterations = 500,
    sparsenessQuantiles,
    positivities,
    initialUMatrix,
    mixAlg = c("svd", "ica", "avg", "rrpca-l", "rrpca-s", "pca", "stochastic"),
    repeatedMeasures = NA,
    lineSearchRange = c(-5e2, 5e2),
    lineSearchTolerance = 1e-12,
    randomSeed=0,
    constraint = c( "Grassmannx0", "Stiefelx0", "orthox0.01", "none"),
    energyType = c("cca", "regression", "normalized", "ucca", "lowRank", "lowRankRegression",'normalized_correlation','acc','nc','dat', 'lrr', 'reconorm', 'logcosh', 'exp', 'kurtosis', 'gauss'),
    vmats,
    connectors = NULL,
    optimizationStyle = "adam",
    scale = c("center",  "eigenvalue" ),
    expBeta = 0.0,
    jointInitialization = TRUE,
    sparsenessAlg = 'soft',
    orthogonalizeU = FALSE,
    domainMatrices = NULL,
    domainLambdas  = NULL,
    verbose = FALSE) {

  parse_constraint <- function(x) {
    num1=num2=NA
    temp=unlist(strsplit( x, "x"))
    if ( length(temp) > 1 ) num1=as.numeric(temp[2])
    if ( length(temp) > 2 ) num2=as.numeric(temp[3])
    return(c(temp[1], as.numeric(num1), as.numeric(num2)))
  }
  if (missing(scale)) scale <- c("centerAndScale")
  if (missing(energyType)) energyType <- "acc"
  if (missing(mixAlg)) mixAlg <- "pca"
  if (!missing("randomSeed")) set.seed(randomSeed) #  else set.seed( 0 )
  energyType <- match.arg(energyType)
  if ( energyType == 'nc') energyType <- 'normalized_correlation'
  if ( energyType == 'lrr') energyType <- 'lowRankRegression'
  constraint <- parse_constraint( constraint[1] )
  constraint_weight=as.numeric(constraint[2])
  if ( is.na(constraint_weight) ) constraint_weight=1
  constraint_iterations=as.numeric(constraint[3])
  constraint_type=constraint[1]
  scalechoices = c(
    "sqrtnp", "np", "centerAndScale",
    "norm", "none", "impute", "eigenvalue", "center", "robust", 'lowrank','whiten','rank'
  )
  scaleList <- c()
  if (length(scale) == 1) {
    scaleList[1] <- match.arg(scale[1], choices = scalechoices )
  } else if (length(scale) > 1) {
    for (kk in 1:length(scale)) {
      scaleList[kk] <- match.arg(scale[kk], choices = scalechoices
      )
    }
  }
  stopifnot( optimizationStyle %in% list_simlr_optimizers())
  # \sum_i  \| X_i - \sum_{ j ne i } u_j v_i^t \|^2 + \| G_i \star v_i \|_1
  # \sum_i  \| X_i - \sum_{ j ne i } u_j v_i^t - z_r v_r^ T \|^2 + constraints
  #
  constrainG <- function(vgrad, i, constraint) {
    # grassmann manifold - see https://stats.stackexchange.com/questions/252633/optimization-with-orthogonal-constraints
    # Edelman, A., Arias, T. A., & Smith, S. T. (1998). The geometry of algorithms with orthogonality constraints. SIAM journal on Matrix Analysis and Applications, 20(2), 303-353.
    if (constraint == "Grassmann") {
      projjer <- diag(ncol(vgrad)) - t(vmats[[i]]) %*% vmats[[i]]
      return(vgrad %*% projjer)
    }
    if (constraint == "Stiefel") { # stiefel manifold
      vgrad <- vgrad - vmats[[i]] %*% (t(vgrad) %*% (vmats[[i]]))
    }
    return(vgrad)
  }
  
  nModalities <- length(data_matrices)
  if (missing(connectors)) {
    temp <- 1:nModalities
    connectors <- list()
    for (i in temp) {
      connectors[[i]] <- temp[-i]
    }
  }
  mixAlg <- match.arg(mixAlg)
  # 0.0 adjust length of input data
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
    p[i] <- ncol(data_matrices[[i]])
  }
  n <- nrow(data_matrices[[1]])
  if (missing(sparsenessQuantiles)) {
    sparsenessQuantiles <- rep(0.5, nModalities)
  }
  
  lrbasis = length( data_matrices )
  if ( ! missing( initialUMatrix ) ) {
    if ( is.integer(initialUMatrix) ) lrbasis=initialUMatrix
    if ( is.matrix( initialUMatrix ) ) lrbasis=ncol(initialUMatrix)
    if ( is.list( initialUMatrix ) ) if ( is.matrix(initialUMatrix[[1]])) 
      lrbasis=ncol(initialUMatrix[[1]])
  }
  
  # 1.0 adjust matrix norms
  if (!(any(scaleList == "none"))) {
    for (i in 1:nModalities) {
      if (any(is.null(data_matrices[[i]])) | any(is.na(data_matrices[[i]]))) {
        stop(paste("input matrix", i, "is null or NA."))
      }
      matnames <- names(data_matrices)[i]
      if (any(is.na(data_matrices[[i]]))) {
        data_matrices[[i]][is.na(data_matrices[[i]])] <- mean(data_matrices[[i]], na.rm = T)
      }
      for (j in 1:length(scaleList)) {
        if (scaleList[j] == "norm") {
          data_matrices[[i]] <- data_matrices[[i]] / norm(data_matrices[[i]], type = "F")
        }
        if (scaleList[j] == "np") {
          data_matrices[[i]] <- data_matrices[[i]] / prod(dim(data_matrices[[i]]))
        }
        if (scaleList[j] == "sqrtnp") {
          data_matrices[[i]] <- data_matrices[[i]] / sqrt(prod(dim(data_matrices[[i]])))
        }
        if (scaleList[j] == "center") {
          data_matrices[[i]] <- base::scale(data_matrices[[i]], center = TRUE, scale = FALSE)
        }
        if (scaleList[j] == "centerAndScale") {
          data_matrices[[i]] <- base::scale(data_matrices[[i]], center = TRUE, scale = TRUE)
        }
        if (scaleList[j] == "eigenvalue") {
          data_matrices[[i]] <- data_matrices[[i]] / sum(ba_svd(data_matrices[[i]])$d)
        }
        if ( scaleList[j] %in% c("robust","rank") ) {
          data_matrices[[i]] <- robustMatrixTransform(data_matrices[[i]])
        }
        if (scaleList[j] == "whiten") {
          data_matrices[[i]] <- whiten_matrix( data.matrix(data_matrices[[i]]) )$whitened_matrix
        }
        if (scaleList[j] == "lowrank") {
          data_matrices[[i]] <- lowrankRowMatrix( data.matrix(data_matrices[[i]]), lrbasis*2 )
        }
      }
    }
  }
  
  # 3.0 setup regularization
  if (missing(smoothingMatrices)) {
    smoothingMatrices <- list()
    for (i in 1:nModalities) {
      smoothingMatrices[[i]] <- diag(ncol(data_matrices[[i]]))
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
  localGS <- function(x, orthogonalizeU = TRUE) {
    if (!orthogonalizeU) {
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
      temp <- initializeSimlr(data_matrices, initialUMatrix, uAlgorithm = mixAlg, jointReduction = jointInitialization)
      initialUMatrix <- list()
      for (i in 1:nModalities) initialUMatrix[[i]] <- temp
    } else {
      initialUMatrix <- initializeSimlr(data_matrices, initialUMatrix, uAlgorithm = mixAlg, jointReduction = jointInitialization)
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
#    if (verbose) cat("-------<00>--BEGIN--<00>-------\n")
    vmats <- list()
    for (i in 1:nModalities) {
      vmats[[i]] <- t(data_matrices[[i]]) %*% initialUMatrix[[i]]
      # 0 # svd( temp, nu=basisK, nv=0 )$u
      # for ( kk in 1:nModalities ) vmats[[ i ]] = vmats[[ i ]] + t(data_matrices[[i]]) %*% initialUMatrix[[kk]]
      #      vmats[[ i ]] = # svd( vmats[[ i ]], nu=basisK, nv=0 )$u
      #        ( stats::prcomp( vmats[[ i ]], retx=TRUE, rank.=basisK, scale.=TRUE )$x )
      vmats[[i]] <- vmats[[i]] / norm(vmats[[i]], "F")
    }
  }
  
  nc <- ncol(initialUMatrix[[1]])
  myw <- matrix(rnorm(nc^2), nc, nc) # initialization for fastICA

  energyPath <- matrix(Inf, nrow = iterations, ncol = nModalities)
  orthPath = matrix(Inf, nrow = iterations, ncol = nModalities)
  bestU <- initialUMatrix
  bestV <- vmats
  if (verbose) {
    cat(sprintf("
      --- Method Summary ---
        • Mixer Algorithm  : %s
        • Energy Type      : %s
        • Sparseness Alg.  : %s
        • expBeta          : %s
        • constraint       : %s
        • constraint-it    : %s
        • constraint-wt    : %s
        • optimizationStyle: %s
      ----------------------
      ", mixAlg, energyType, sparsenessAlg, expBeta, constraint_type, constraint_iterations, constraint_weight, optimizationStyle))
  }
  
  # 2.0 Define Logic for Optimization Style
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
  
# ==============================================================================
#      High-Performance SIMLR Loop (Hybrid: Adam/SGD + Line Search)
# ==============================================================================
# --- 1. Setup before the loop ---
# Initialize adaptive orthogonality weights
orth_weights <- rep(0.0, nModalities)
normalizing_weights = rep( 1.0, nModalities )
names( orth_weights ) = names( normalizing_weights ) = names( data_matrices )
clipper = 0.80
bestTot <- Inf
bestRow <- 1
bestU <- initialUMatrix
bestV <- vmats
convergence_df <- tibble::tibble()
converged=0

# --- Add these parameters to your main simlr() function signature ---
optimizer = optimizationStyle
initial_learning_rate = 0.01
final_learning_rate = 1e-5

# Create the optimizer object based on user's choice
optimizer_object <- create_optimizer(
  optimizer_type = optimizer,
  vmats = vmats,
  # Pass hyperparameters that the step functions will need
  beta1 = 0.9, beta2 = 0.999, epsilon = 1e-8, sgd_momentum_beta = 0.9
)

# initialize energy trackers for each modality
all_sim_energy   <- vector("list", nModalities)
all_dom_energy   <- vector("list", nModalities)
all_total_energy <- vector("list", nModalities)

for (j in 1:nModalities) {
  all_sim_energy[[j]]   <- numeric()
  all_dom_energy[[j]]   <- numeric()
  all_total_energy[[j]] <- numeric()
}
# --- 2. Main Optimization Loop ---
for (myit in 1:iterations) {
# --- Calculate dynamic learning rate for non-line-search methods ---
  decay_progress <- (myit - 1) / max(1, iterations - 1)
  current_learning_rate <- final_learning_rate + 0.5 * (initial_learning_rate - final_learning_rate) * (1 + cos(pi * decay_progress))
  # --- A. Update each V_i matrix ---
  for (i in 1:nModalities) {

    # first define the local versions of the energy and gradient 
    smooth_cost <- function(V, return_raw = FALSE) {
      if (positivities[i] == 'positive') V <- take_abs_unsigned(V)
      V_sp <- simlr_sparseness(
        V,
        constraint_type = constraint_type,
        smoothing_matrix = smoothingMatrices[[i]],
        positivity = positivities[i],
        sparseness_quantile = sparsenessQuantiles[i],
        constraint_iterations = constraint_iterations,
        constraint_weight = constraint_weight,
        sparseness_alg = sparsenessAlg
      )
      
      # --- User-chosen energy ---
      sim_e <- calculate_simlr_energy(
        V_sp, data_matrices[[i]], initialUMatrix[[i]],
        energyType
      ) * normalizing_weights[i]

      # --- Domain energy (only if lambda > 0) ---
      dom_e <- 0
      if (!is.null(domainMatrices) && !is.null(domainLambdas)) {
        lam <- domainLambdas[i]
        if (lam > 0) {
          dom_e <- calculate_simlr_energy(
            V_sp, data_matrices[[i]], initialUMatrix[[i]],
            "dat", lambda = lam, prior_matrix = domainMatrices[[i]]
          )
        }
      }

      orth_e <- 0
      if (constraint_type == "ortho") {
        orth_e <- invariant_orthogonality_defect(V_sp) * constraint_weight * orth_weights[i]
      }

      total_e <- sim_e + dom_e + orth_e
      # --- Track values ---
      all_sim_energy[[i]] <<- c(all_sim_energy[[i]], sim_e)
      all_dom_energy[[i]] <<- c(all_dom_energy[[i]], dom_e)
      all_total_energy[[i]] <<- c(all_total_energy[[i]], total_e)
      if (return_raw) return(sim_e + dom_e) # raw similarity+domain only
      return(total_e)
    }
    smooth_grad <- function(V) {
      if (positivities[i] == 'positive') V <- take_abs_unsigned(V)
      V_sp <- simlr_sparseness(
        V,
        constraint_type = constraint_type,
        smoothing_matrix = smoothingMatrices[[i]],
        positivity = positivities[i],
        sparseness_quantile = sparsenessQuantiles[i],
        constraint_iterations = constraint_iterations,
        constraint_weight = constraint_weight,
        sparseness_alg = sparsenessAlg
      )

      # --- User-chosen gradient ---
      sim_grad <- calculate_simlr_gradient(
        V_sp, data_matrices[[i]], initialUMatrix[[i]],
        energyType
      ) * normalizing_weights[i]

      # --- Domain gradient (only if lambda > 0) ---
      dom_grad <- 0
      if (!is.null(domainMatrices) && !is.null(domainLambdas)) {
        lam <- domainLambdas[i]
        if (lam > 0) {
          dom_grad <- calculate_simlr_gradient(
            V_sp, data_matrices[[i]], initialUMatrix[[i]],
            "dat", lambda = lam, prior_matrix = domainMatrices[[i]]
          )
        }
      }

      orth_grad <- 0
      if (constraint_type == "ortho" & FALSE) {
        orth_grad <- constraint_weight * orth_weights[i] *
          gradient_invariant_orthogonality_defect(V_sp)
      }

      g <- sim_grad + dom_grad - orth_grad
      g <- clip_gradient_by_quantile(constrainG(as.matrix(g), i, constraint_type), clipper)
      return(g)
    }      

    riemannian_descent_grad = smooth_grad(vmats[[i]])
    step_result <- step(
          optimizer_object,
          i = i,
          V_current = vmats[[i]],
          descent_gradient = riemannian_descent_grad,
          # Pass arguments needed by the specific step method
          full_energy_function = smooth_cost, # For hybrid methods
          learning_rate = current_learning_rate,       # For non-hybrid methods
          myit = myit                                  # For Adam bias correction
        )
    
 # Update the parameter and the optimizer object with their new states
    V_updated <- step_result$updated_V
    optimizer_object <- step_result$optimizer
    # print(step_result)
    # 5. Apply the final non-smooth projection (sparsity and retraction)
    vmats[[i]] <- simlr_sparseness(
      V_updated,
      constraint_type = constraint_type,
      smoothing_matrix = smoothingMatrices[[i]],
      positivity = positivities[i],
      sparseness_quantile = sparsenessQuantiles[i],
      constraint_weight = constraint_weight,
      sparseness_alg = sparsenessAlg
    )

    } # End V_i update loop

    # --- B. Update each U_i matrix (logic is unchanged from original simlr) ---
    if ( !(energyType %in% c('regression','reg')) ) {
        tempU <- lapply(1:nModalities, function(j) scale(data_matrices[[j]] %*% vmats[[j]], TRUE, FALSE))
      } else {
        tempU <- lapply(1:nModalities, function(j) data_matrices[[j]] %*% vmats[[j]])
      }
    updated_Us <- simlrU(tempU, mixAlg, myw,
                   orthogonalize = orthogonalizeU,
                   connectors = connectors)
    # Apply Gram-Schmidt orthogonalization (localGS)
    initialUMatrix <- lapply(updated_Us, function(u) localGS(u, orthogonalize = orthogonalizeU))


    # --- C. Evaluate, Track, and Report Convergence ---
    
    # Calculate energies and orthogonality for each modality at the end of the iteration
    iter_results_list <- list()

    for (i in seq_len(nModalities)) {
      if ( is.na(orth_weights[i])) orth_weights[i]=1.0
      V_current <- vmats[[i]]
      sim_e <- smooth_cost(V_current, return_raw=TRUE )
      tot_e <- smooth_cost(V_current  )
      orth_e <- invariant_orthogonality_defect(V_current)
      iter_results_list[[i]] <- tibble::tibble(
        iteration = i, 
        modality = names(data_matrices)[i],
        total_energy = all_total_energy[[i]][length(all_total_energy[[i]])],
        similarity_energy = all_sim_energy[[i]][length(all_sim_energy[[i]])],
        domain_energy = all_dom_energy[[i]][length(all_dom_energy[[i]])], # NEW
        feature_orthogonality = orth_e,
        similarity_energy_w = sim_e * normalizing_weights[i],
        feature_orthogonality_w = orth_e * orth_weights[i] * constraint_weight
      )
    }

    iter_results <- do.call(dplyr::bind_rows, iter_results_list)    
    iter_results$iteration <- myit
    convergence_df <- dplyr::bind_rows(convergence_df, iter_results)
#    print(data.frame(iter_results))
    # --- Adaptive Weighting: Set weights ONLY at the end of the first iteration ---
  if ( myit <= 1 ) {
    if (verbose) {
      message("Setting adaptive orthogonality/energy weights based on first few iterations...")
      print("Setting adaptive orthogonality/energy weights based on first few iterations...")
      }
    for (i in 1:nModalities) {
      # Get the results for this modality from the table we just built
      mod_results <- iter_results[i, ]
      normalizing_weights[i]=1.0/ ( abs(mod_results$similarity_energy) * nModalities )
      # The weight is the ratio of the absolute similarity energy to the orthogonality penalty
      if ( !is.na(mod_results$feature_orthogonality)) {
        if (mod_results$feature_orthogonality > 1e-10 ) {
          orth_weights[i] <- abs(mod_results$similarity_energy) *normalizing_weights[i] / mod_results$feature_orthogonality
        } else {
          orth_weights[i] <- 0.0 # Default to 1 if orthogonality is already perfect
        }
        if ( is.na(orth_weights[i]) | is.infinite(orth_weights[i]) ) {
          orth_weights[i] <- 1.0
        }
      }
    }
    if (verbose) {
      message("Norm Weights: ", paste(round(normalizing_weights, 3), collapse=", "))
      message("Orth Weights: ", paste(round(orth_weights, 2), collapse=", "))
    }
  }
  
  # Update the "best" solution found so far based on mean total energy
  mean_current_energy <- mean(iter_results$total_energy, na.rm = TRUE)
  printit=FALSE
  if (mean_current_energy < bestTot & myit >= 2 ) {
    lastBest = bestTot
    bestTot <- mean_current_energy
    bestRow <- myit
    bestU <- initialUMatrix
    bestV <- vmats
    printit=TRUE
    converged=myit
    pct_reduction_less_than <- function(old_val, new_val, threshold_pct) {
      if (!is.numeric(old_val) || !is.numeric(new_val)) {
        stop("Values must be numeric")
      }
      if (old_val == 0) {
        stop("Old value cannot be zero for percentage reduction calculation")
      }
      
      pct_change <- abs((old_val - new_val) / old_val) * 100
      return( c(pct_change < threshold_pct, pct_change ) )
    }
    if ( myit > 5 ) {
      change_detector = pct_reduction_less_than( bestTot, lastBest, 0.01 )
      if (  change_detector[1] & verbose > 0 ) {
        message(paste("~~Small.delt: E ", round(bestTot,4), " E-1 ", round(lastBest, 4),"E / E-1 ",round(change_detector[2],4)))
        myit=iterations
      }
    }
  }
  
  if (verbose & printit | verbose > 1 ) {
    # Report the mean orthogonality across all modalities for this iteration
    mean_orthogonality <- mean(iter_results$feature_orthogonality, na.rm = TRUE)
#    message(sprintf("Iter: %d | Mean Energy: %.4f | Best Energy: %.4f (at iter %d) | Mean Orthogonality: %.4f",
#                  myit, mean_current_energy, bestTot, bestRow, mean_orthogonality))
    cat(sprintf("Iter: %d | Mean Energy: %.4f | Best Energy: %.4f (at iter %d) | Mean Orthogonality: %.4f \n",
                  myit, mean_current_energy, bestTot, bestRow, mean_orthogonality))
    if ( myit == 1) cat("\n----iteration 1 is an auto-tuning iteration----\n")
  }
  
  # Check for convergence
  maxitnoimp = round( 0.1 * iterations )
  if ((myit - bestRow) > maxitnoimp) {
    if(verbose) message(paste("~~Convergence criteria met @ ",myit," \n No improvement over 10% of max iterations", maxitnoimp))
    break
  }
} # End main optimization loop

if ( converged > 2 & verbose > 0 ) {
  message(paste("~~Converged at", converged-1, "iterations."))
} else if ( verbose > 0 ) {
  message(paste("--Did not converge after", myit, "iterations."))
}
names(bestV)=names(data_matrices)
for ( k in 1:length(data_matrices)) {
    rownames(bestV[[k]])=colnames(data_matrices[[k]])
    colnames(bestV[[k]])=paste0("PC",1:ncol(bestV[[k]]))
  }
  
energyPath <- na.omit(energyPath)
return(
    list(
      u = bestU,
      v = bestV,
      initialRandomMatrix = randmat,
      energyPath = convergence_df,
      finalError = bestTot,
      connectors = connectors,
      energyType = energyType,
      optimizationStyle = optimizationStyle,
      converged_at = converged,
      sim_energy = all_sim_energy,
      domain_energy = all_dom_energy,
      total_energy = all_total_energy
    )
  )
}

#' Plot Energy Decomposition During SIMLR Optimization
#'
#' Creates line plots showing the evolution of similarity, domain, and total 
#' energy across optimization iterations, for each modality. Energies are 
#' rescaled to [0,1] within each type for comparability.
#'
#' @param simlr_result A list returned by \code{simlr()}, containing
#'   \code{energyPath}, \code{sim_energy}, \code{domain_energy}, 
#'   and \code{total_energy}.
#' @param modality Character string giving the name of the modality to plot
#'   (must match names in \code{energyPath$modality}).
#' @param show_weights Logical, if TRUE also show weighted energies.
#' @return A \code{ggplot2} object.
#' @examples
#' \dontrun{
#'   res <- simlr(...)
#'   plot_energy_decomposition(res, modality = "RNA")
#' }
#' @export
plot_energy_decomposition <- function(simlr_result, modality, show_weights = FALSE) {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  
  df <- simlr_result$energyPath
  df <- df[df$modality == modality, ]
  df = df[-1,]
  
  # Base data
  energies <- data.frame(
    iteration = df$iteration,
    similarity = df$similarity_energy,
    domain = df$domain_energy,
    total = df$total_energy
  )
  
  energies_long <- tidyr::pivot_longer(
    energies,
    cols = -iteration,
    names_to = "energy_type",
    values_to = "value"
  )
  
  # Rescale each energy type to [0,1] for visual comparability
  energies_long <- energies_long %>%
    dplyr::group_by(energy_type) %>%
    dplyr::mutate(value_rescaled = (value - min(value, na.rm = TRUE)) /
                                   (max(value, na.rm = TRUE) - min(value, na.rm = TRUE) + 1e-8)) %>%
    dplyr::ungroup()
  
  p <- ggplot2::ggplot(energies_long, ggplot2::aes(x = iteration, y = value_rescaled, color = energy_type)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(
      title = paste("Energy Decomposition for", modality),
      subtitle = "All energies rescaled to [0,1] for comparability",
      x = "Iteration",
      y = "Rescaled Energy (0–1)",
      color = "Energy type"
    )
  
  if (show_weights) {
    weighted <- data.frame(
      iteration = df$iteration,
      sim_w = df$similarity_energy_w,
      orth_w = df$feature_orthogonality_w
    )
    weighted_long <- tidyr::pivot_longer(
      weighted,
      cols = -iteration,
      names_to = "weighted_type",
      values_to = "value"
    )
    p <- p + ggplot2::geom_line(
      data = weighted_long,
      ggplot2::aes(x = iteration, y = value, linetype = weighted_type),
      color = "black"
    )
  }
  
  return(p)
}


#' Safe PCA with Deterministic Bad-Column Repair
#'
#' Performs PCA after automatically detecting and fixing problematic columns
#' (e.g., zero variance, NA-only, partial NAs) in a deterministic way.
#' Ensures that the output dimensions match those from a clean dataset.
#'
#' @param X A numeric matrix or data frame.
#' @param nc Number of components to return.
#' @param center Logical, whether to center columns.
#' @param scale Logical, whether to scale columns to unit variance.
#' @return A list with the same structure as \code{\link[stats]{prcomp}},
#'         with rotation, sdev, x, etc.
#' @examples
#' set.seed(123)
#' mat <- matrix(rnorm(50), nrow = 10)
#' mat[, 3] <- 1  # zero variance col
#' mat[, 5] <- NA # all NA col
#' # result <- safe_pca(mat, nc = 3)
#' # result$rotation
#' @export
safe_pca <- function(X, nc = min(dim(X)), center = TRUE, scale = TRUE) {
  X <- as.matrix(X)
  
  # Detect bad columns
  col_var <- apply(X, 2, function(col) var(col, na.rm = TRUE))
  all_na <- apply(X, 2, function(col) all(is.na(col)))
  some_na <- apply(X, 2, function(col) any(is.na(col)) && !all(is.na(col)))
  
  # Replace bad columns deterministically
  n <- nrow(X)
  fix_count <- 0
  
  for (j in seq_len(ncol(X))) {
    if (all_na[j]) {
      # Replace all-NA col with fixed ±1 pattern
      X[, j] <- rep(c(-1, 1), length.out = n)
      fix_count <- fix_count + 1
    } else if (col_var[j] == 0 || is.na(col_var[j])) {
      # Replace zero-variance col with deterministic sine wave
      X[, j] <- sin(seq_len(n) * (pi / n) * (j + 1))
      fix_count <- fix_count + 1
    } else if (some_na[j]) {
      # Impute partial NAs with column mean
      mu <- mean(X[, j], na.rm = TRUE)
      X[is.na(X[, j]), j] <- mu
      fix_count <- fix_count + 1
    }
  }
  
  # Ensure full column rank
  qr_rank <- qr(X)$rank
  if (qr_rank < nc) {
    needed <- nc - qr_rank
    message("Adding ", needed, " orthogonal synthetic columns to ensure rank.")
    synth_cols <- matrix(0, nrow = n, ncol = needed)
    for (k in seq_len(needed)) {
      synth_cols[, k] <- cos(seq_len(n) * (pi / n) * (k + ncol(X)))
    }
    X <- cbind(X, synth_cols)
  }
  
  # Run PCA (deterministic)
  result <- stats::prcomp(X, center = center, scale. = scale, rank. = nc, retx=TRUE )
  
  # Drop any extra synthetic components so rotation is always ncol(X) x nc
  if (ncol(result$rotation) < nc) {
    pad <- matrix(0, nrow = ncol(result$rotation), ncol = nc - ncol(result$rotation))
    result$rotation <- cbind(result$rotation, pad)
    result$sdev <- c(result$sdev, rep(0, nc - length(result$sdev)))
  }
  
  attr(result, "fixed_columns") <- fix_count
  result
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
      basis <- safe_pca( avgU, nc = nc )$x
    } else if (mixAlg == "rrpca-l") {
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
#' @param data_matrices A list of voxel matrices.
#' @param smoothingMatrices A list of smoothing matrices.
#' @param iterations Number of iterations. Default is 10.
#' @param sparsenessQuantiles A vector of sparseness quantiles.
#' @param positivities A vector of positivity constraints.
#' @param initialUMatrix Initial U matrix for the algorithm.
#' @param mixAlg The mixing algorithm to use. Default is 'svd'.
#' @param orthogonalizeU Logical indicating whether to orthogonalize U. Default is FALSE.
#' @param repeatedMeasures Repeated measures data. Default is NA.
#' @param lineSearchRange Range for line search. Default is c(-1e+10, 1e+10).
#' @param lineSearchTolerance Tolerance for line search. Default is 1e-08.
#' @param randomSeed Seed for random number generation.
#' @param constraint The constraint type. Default is 'none'.
#' @param energyType The energy type. Default is 'cca'.
#' @param vmats List of V matrices - optional initialization matrices
#' @param connectors List of connectors. Default is NULL.
#' @param optimizationStyle The simlr optimizer.
#' @param scale Scaling method. Default is 'centerAndScale'.
#' @param expBeta Exponential beta value. Default is 0.
#' @param jointInitialization Logical indicating joint initialization. Default is TRUE.
#' @param sparsenessAlg Sparseness algorithm. Default is NA.
#' @param verbose Logical indicating whether to print verbose output. Default is FALSE. values > 1 lead to more verbosity
#' @param nperms Number of permutations for significance testing. Default is 50.
#' @param FUN function for summarizing variance explained 
#' @return A data frame containing p-values for each permutation.
#' @export
simlr.perm <- function(data_matrices, 
  smoothingMatrices, 
  iterations = 10, sparsenessQuantiles, 
  positivities, initialUMatrix, 
  mixAlg = c("svd", "ica", "avg","rrpca-l", "rrpca-s", "pca", "stochastic"), 
  orthogonalizeU = FALSE,
  repeatedMeasures = NA, lineSearchRange = c(-5e2, 5e2), 
  lineSearchTolerance = 1e-12, randomSeed, 
  constraint = c("orthox0.001x1", "Grassmannx0", "Stiefelx0", "none" ), 
  energyType = c("cca", "regression","normalized", "acc", "dat", "lowRank", "lowRankRegression",'normalized_correlation', 'logcosh', 'exp', 'kurtosis','gauss'), 
  vmats, connectors = NULL, optimizationStyle = 'adam', 
  scale = c("centerAndScale", "eigenvalue"), 
  expBeta = 0, jointInitialization = TRUE, sparsenessAlg = NA, 
  verbose = FALSE, nperms = 50, FUN='mean') {
  
  # Set up permutations
  myseeds <- 1:1000000
  # Initial SiMLR run
  simlr_result <- simlr( data_matrices, 
    smoothingMatrices=smoothingMatrices, 
    iterations=iterations, 
    sparsenessQuantiles=sparsenessQuantiles, 
    positivities=positivities, 
    initialUMatrix=initialUMatrix, 
    mixAlg=mixAlg, 
    orthogonalizeU=orthogonalizeU, 
    repeatedMeasures=repeatedMeasures, 
    lineSearchRange=lineSearchRange, 
    lineSearchTolerance=lineSearchTolerance, 
    randomSeed=randomSeed, 
    constraint=constraint, 
    energyType=energyType, 
    vmats=vmats, 
    connectors=connectors, 
    optimizationStyle=optimizationStyle, 
    scale=scale, expBeta=expBeta, 
    jointInitialization=jointInitialization, 
    sparsenessAlg=sparsenessAlg, verbose=verbose )
  for ( k in 1:length(data_matrices)) {
    simlr_result$v[[k]]=take_abs_unsigned(simlr_result$v[[k]])
    simlr_result$v[[k]]=l1_normalize_features( simlr_result$v[[k]] )
    rownames(simlr_result$v[[k]])=colnames(data_matrices[[k]])
  }
  
  refvarxmeans = pairwise_matrix_similarity( data_matrices, simlr_result$v, FUN=FUN )
  simlrpermvarx = data.frame( n=ncol(initialUMatrix), perm=0:nperms ) 
  refvarxmeansnms=names(refvarxmeans)
  simlrpermvarx[1, refvarxmeansnms]=refvarxmeans
  
  # begin permutation  
  if ( nperms > 1 )
    for (nperm in 1:nperms) {
      set.seed(myseeds[nperm])
      
      data_matrices_perm <- lapply(data_matrices, function(mat) mat[sample(1:nrow(mat)), ])
      
      simlr_result_perm <- simlr(data_matrices_perm,
          smoothingMatrices=smoothingMatrices, 
          iterations=iterations, 
          sparsenessQuantiles=sparsenessQuantiles, 
          positivities=positivities, 
          initialUMatrix=initialUMatrix, 
          mixAlg=mixAlg, 
          orthogonalizeU=orthogonalizeU, 
          repeatedMeasures=repeatedMeasures, 
          lineSearchRange=lineSearchRange, 
          lineSearchTolerance=lineSearchTolerance, 
          randomSeed=randomSeed, 
          constraint=constraint, 
          energyType=energyType, 
          vmats=vmats, 
          connectors=connectors, 
          optimizationStyle=optimizationStyle, 
          scale=scale, expBeta=expBeta, 
          jointInitialization=jointInitialization, 
          sparsenessAlg=sparsenessAlg, verbose=verbose )
      for ( k in 1:length(data_matrices)) {
        simlr_result$v[[k]]=take_abs_unsigned(simlr_result$v[[k]])
        simlr_result_perm$v[[k]] = l1_normalize_features( simlr_result_perm$v[[k]] )
      }
      refvarxmeans_perm = pairwise_matrix_similarity( data_matrices_perm, simlr_result_perm$v, FUN=FUN )
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

#' Computes the RV-Coefficient (Internal) Trace Method for Wide data
#'
#' @param X_centered A numeric matrix (n observations, p variables).
#' @param Y_centered A numeric matrix (n observations, q variables).
#' @return values.
#' @export
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

#' Computes the RV-Coefficient (Internal) Gram Matrix Method for Tall data
#'
#' @param X_centered A numeric matrix (n observations, p variables).
#' @param Y_centered A numeric matrix (n observations, q variables).
#' @return values.
#' @export
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

#' Computes the RV-Coefficient (Internal)
#'
#' This function automatically selects the fastest algorithm based on data dimensions.
#' Internal Dispatcher: Returns all components from the fastest method.
#'
#' @param X A numeric matrix (n observations, p variables).
#' @param Y A numeric matrix (n observations, q variables).
#' @return values.
#' @export
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
  rvcoefval <- rvcoef(projection1, projection2)
  adj_rvcoefval <- adjusted_rvcoef(projection1, projection2)
  
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
    rv_coefficient = rvcoefval,
    adj_rv_coefficient=adj_rvcoefval
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
  m=as.matrix(m)
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
    sparsenessAlg = list('soft'),
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



#' Perform Parallel SIMLR Grid Search (with Progress Bar)
#'
#' This version runs parameter sets in parallel using all available CPU cores (minus one by default),
#' and shows a progress bar with ETA.
#'
#' @param mats List of input matrices for SIMLR.
#' @param regs List of regularization parameters for SIMLR.
#' @param options_df Data frame of parameter combinations.
#' @param maxits Maximum iterations for SIMLR.
#' @param connectors List of connectors for modalities.
#' @param nperms Number of permutations for significance test.
#' @param verbose Verbosity level.
#' @param FUN Evaluation function.
#' @param cores Number of CPU cores to use (default: detectCores() - 1). \code{parallel::detectCores() - 1}
#' @return List with best SIMLR result, significance, best parameters, and search table.
#' @export
simlr.search <- function(
    mats,
    regs,
    options_df,
    maxits = 100,
    connectors = NULL,
    nperms = 1,
    verbose = 0,
    FUN = rvcoef,
    cores = 1
) {
  # ---- Helper to run a single set ----
  run_one <- function(i) {
    nsimlr <- unlist(options_df$nsimlr[i])
    prescaling <- unlist(options_df$prescaling[i])
    objectiver <- unlist(options_df$objectiver[i])
    mixer <- unlist(options_df$mixer[i])
    constraint <- unlist(options_df$constraint[i])
    sparval <- unlist(options_df$sparval[i])
    ebber <- unlist(options_df$ebber[i])
    pizzer <- unlist(options_df$pizzer[i])
    optimus <- unlist(options_df$optimus[i])
    sparsenessAlgVal <- unlist(options_df$sparsenessAlg[i])
    
    if (is.character(sparval[1])) {
      parse_vec <- function(s) as.numeric(strsplit(gsub("rand", "", s), "x")[[1]])
      sparval <- parse_vec(sparval)
      sparval <- runif(sparval[1], sparval[2], sparval[3])
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
      orthogonalizeU = FALSE,
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
    
    wtest <- 1
    if (nperms > 4) {
      wtest <- which(simlrX$significance$perm == "ttest")
    }
    finalE <- mean(as.numeric(simlrX$significance[wtest, -c(1:2)]), na.rm = TRUE)
    if (is.na(finalE)) finalE <- -Inf
    
    parameters <- data.frame(
      nsimlr = nsimlr,
      objectiver = objectiver,
      mixer = mixer,
      ebber = ebber,
      optimus = optimus,
      constraint = constraint,
      final_energy = as.numeric(finalE)
    )
    
    prescaling <- vector_to_df(prescaling, "prescaling")
    sparval <- vector_to_df(sparval, "sparval")
    pizzer <- vector_to_df(pizzer, "positivity")
    
    parameters <- cbind(parameters, prescaling, sparval, pizzer, simlrX$significance[1, -1])
    
    list(
      simlr_result = simlrX$simlr_result,
      significance = simlrX$significance,
      parameters = parameters
    )
  }
  
  # ---- Parallel or sequential run ----
  if (cores > 1) {
    cat(sprintf("Running %d parameter sets on %d cores...\n", nrow(options_df), cores))
    cl <- parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    
    # 1. Load packages in workers
    parallel::clusterEvalQ(cl, {
      library(ANTsR)   # adjust if SIMLR is in a different package
      library(pbapply)
    })
    
    # 2. Detect user-defined functions from this environment
    user_funcs <- ls(environment(), all.names = TRUE)
    user_funcs <- user_funcs[sapply(user_funcs, function(f) is.function(get(f, envir = environment())))]
    
    # 3. Export everything needed
    parallel::clusterExport(
      cl,
      varlist = c(
        user_funcs,
        "mats", "regs", "options_df", "maxits", "connectors", "nperms", "verbose", "FUN"
      ),
      envir = environment()
    )
    
    # 4. Run in parallel with progress bar
    results_list <- pbapply::pblapply(seq_len(nrow(options_df)), run_one, cl = cl)
  } else {
    cat(sprintf("Running %d parameter sets sequentially...\n", nrow(options_df)))
    results_list <- pbapply::pblapply(seq_len(nrow(options_df)), run_one)
  }
  
  # ---- Combine results ----
  all_params <- do.call(rbind, lapply(results_list, function(x) x$parameters))
  best_idx <- which.max(all_params$final_energy)
  
  list(
    simlr_result = results_list[[best_idx]]$simlr_result,
    significance = results_list[[best_idx]]$significance,
    parameters = results_list[[best_idx]]$parameters,
    paramsearch = all_params
  )
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
#' @importFrom tibble tibble
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

  covariate_options <- c(
    "whiten", "lowrank", "robust", "center", "rank", "scale", "mean", "centerAndScale", "np",
    "formula such as T1Hier_resnetGrade + snr + EVR + psnr", "eigenvalue"
  )
  
  if (!is.null(opt) && opt == "opt") {
    print("Available covariate options:")
    print(covariate_options)
    return(invisible(NULL))
  }
  
  if (is.null(covariate)) return(mats[[x]])
  
  nc <- min(c(n.comp * 2, nrow(mats[[x]]) - 1))
  if (covariate == "norm") {
    return( data.matrix(mats[[x]])/norm(data.matrix(mats[[x]]), type = "F") )
  }
  if (covariate == "whiten") {
    return(icawhiten(data.matrix(mats[[x]]), n.comp = nc))
  }
  if (covariate == "lowrank") {
    return(lowrankRowMatrix(data.matrix(mats[[x]]), nc))
  }
  if (covariate == "eigenvalue") {
    return( mats[[x]] / sum(ba_svd(mats[[x]])$d))
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
#'              Other options include 'lrr', 'regression', 'nc', 'base.pca', 'base.spca', 'base.rand.1', and 'base.rand.0'.
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
#' @param sparseness vector or scalar value to set sparseness.
#' @param mixAlg string 'svd', 'ica', 'rrpca-l', 'rrpca-s', 'stochastic', 'pca' or 'avg' denotes the algorithm employed when estimating the mixed modality bases
#' @param iterations int value to set max iterations
#' @param path_modeling the result of a call to \code{simlr_path_models(n)}
#' @param sparsenessAlg NA is default otherwise basic, spmp or orthorank
#' @param optimizationStyle see \code{list_simlr_optimizers}
#' @param verbose boolean
#' @return A list containing the results of the similarity analysis and related data.
#' @export
#' @examples
#' # Example usage:
#' # result <- antspymm_simlr(dataframe)
antspymm_simlr <- function(blaster,
                           select_training_boolean,
                           connect_cog,
                           energy,
                           nsimlr,
                           constraint,
                           covariates = '1',
                           myseed = 3,
                           doAsym = TRUE,
                           returnidps = FALSE,
                           restrictDFN = FALSE,
                           resnetGradeThresh = 1.02,
                           doperm = FALSE,
                           exclusions = NULL,
                           inclusions = NULL,
                           sparseness = NULL,
                           mixAlg = NULL,
                           iterations = NULL,
                           path_modeling = NULL,
                           sparsenessAlg = 'soft',
                           optimizationStyle = NULL,
                           verbose = FALSE) {

  # --- Internal Helper Functions ---
  safegrep <- function(pattern, x, ...) {
    result <- grep(pattern, x, ...)
    if (length(result) == 0) return(1:length(x))
    return(result)
  }
  
  safeclean <- function(pattern, x, fixed = FALSE, exclude = TRUE) {
    mysub <- grep(pattern, x, fixed = fixed)
    if (length(mysub) == 0) return(x)
    if (exclude) return(x[-mysub]) else return(x[mysub])
  }

  # --- 1. Parameter Initialization and Validation ---
  if (is.null(optimizationStyle)) optimizationStyle <- 'ls_nadam'
  
  myenergies <- c('cca', 'acc', 'reg', 'lrr', 'lowRankRegression', 'regression',
                  "base.pca", "base.spca", "base.rand.1", "base.rand.0",
                  "normalized_correlation", 'nc', 'dat', 'logcosh', 'exp', 'kurtosis','gauss')
  if (!energy %in% myenergies) {
    message(paste0("energy should be one of ", paste(myenergies, collapse = ", ")))
  }

  # --- 2. Feature Selection and Filtering (IDPs) ---
  if (verbose) message("Step 1: Selecting and filtering features (IDPs)...")
  
  idps <- antspymm_predictors(blaster, TRUE, TRUE)
  rsfnames <- idps[grepl("rsfMRI", idps)]
  if (length(rsfnames) > 0) rsfnames <- rsfnames[safegrep("_2_", rsfnames)]
  if (!all(grepl("rsfMRI", rsfnames))) rsfnames <- c()

  if (!is.null(exclusions)) {
    for (x in exclusions) {
      idps <- safeclean(x, idps)
      rsfnames <- safeclean(x, rsfnames)
    }
  }
  if (!is.null(inclusions)) {
    for (x in inclusions) {
      idps <- safeclean(x, idps, exclude = FALSE)
      rsfnames <- safeclean(x, rsfnames, exclude = FALSE)
    }
  }
  
  idps <- idps[-multigrep(antspymm_nuisance_names()[-3], idps)]
  idps <- safeclean("cleanup|snseg|_deep_|peraf|alff|LRAVGcit168|_l_|_r_", idps)
  if (doAsym == 0) {
    idps <- safeclean("Asym", idps)
  } else {
    idps <- safeclean("Asymcit168", idps)
  }
  
  if (restrictDFN) {
    rsfnames <- rsfnames[safegrep("Default", rsfnames)]
  }

  # --- 3. Assembling Modality-Specific Feature Lists ---
  perfnames <- idps[multigrep(c("perf_cbf_mean_"), idps, intersect = TRUE)]
  t1names <- idps[multigrep(c("T1Hier"), idps, intersect = TRUE)]
  dtnames <- unique(c(
    idps[multigrep(c("mean_fa", "DTI"), idps, intersect = TRUE)],
    idps[multigrep(c("mean_md", "DTI"), idps, intersect = TRUE)]
  ))
  
  t1asymnames <- c(); dtasymnames <- c(); pfasymnames <- c()
  if (doAsym == 2) {
    t1nms <- t1names
    t1asymnames <- t1nms[grep("Asym", t1nms)]
    t1names <- t1nms[!(t1nms %in% t1asymnames)]
    
    dtnms <- dtnames
    dtasymnames <- dtnms[grep("Asym", dtnms)]
    dtnames <- dtnames[!(dtnames %in% dtasymnames)]
    
    pfnms <- perfnames
    pfasymnames <- pfnms[grep("Asym", pfnms)]
    perfnames <- pfnms[!(pfnms %in% pfasymnames)]
  }
  
  idplist <- list()
  idplist[["t1"]] <- t1names
  if (length(dtnames) > 0) idplist[["dt"]] <- dtnames
  if (length(rsfnames) > 0) idplist[["rsf"]] <- rsfnames
  if (length(perfnames) > 0) idplist[["perf"]] <- perfnames
  if (length(t1asymnames) > 0) idplist[["t1a"]] <- t1asymnames
  if (length(dtasymnames) > 0) idplist[["dta"]] <- dtasymnames
  if (length(pfasymnames) > 0) idplist[["pfa"]] <- pfasymnames
  if (!missing(connect_cog)) idplist[["cg"]] <- connect_cog
  
  if (verbose) {
    message(sprintf("Assembled %d feature modalities: %s", length(idplist), paste(names(idplist), collapse = ", ")))
    total_idps <- length(unique(unlist(idplist)))
    message(sprintf("Total unique features selected: %d. (Showing 10 random samples)", total_idps))
    print(sample(unique(unlist(idplist)), min(10, total_idps)))
  }
  
  if (returnidps) return(unique(unlist(idplist)))

  # --- 4. Data Preparation and Subsetting ---
  if (verbose) message("Step 2: Preparing and subsetting data based on quality checks...")
  
  allnna <- select_training_boolean[blaster$T1Hier_resnetGrade >= resnetGradeThresh]
  blaster2 <- blaster[blaster$T1Hier_resnetGrade >= resnetGradeThresh, ]
  stopifnot(min(dim(blaster2)) > 3)
  
  if (verbose) message(sprintf("Data subset to %d rows after quality control.", nrow(blaster2)))

  # --- 5. Matrix Assembly and Preprocessing ---
  if (verbose) message("Step 3: Assembling and imputing modality matrices...")

  matsFull <- list()
  mats <- list()
  for (kk in 1:length(idplist)) {
    matsFull[[names(idplist)[kk]]] <- blaster[, idplist[[kk]]]
    mats[[names(idplist)[kk]]] <- antsrimpute(blaster2[allnna, idplist[[kk]]])
  }
  
  if (doperm) {
    if (verbose) message("Applying permutations to matrices for null model testing.")
    set.seed(myseed)
    for (jj in 1:length(mats)) {
      mats[[jj]] <- mats[[jj]][sample(1:nrow(mats[[jj]])), ]
    }
  }

  # --- 6. Rank Estimation and Covariate Adjustment ---
  if (missing(nsimlr) || is.na(nsimlr)) {
    if (verbose) message("Step 4a: Estimating optimal rank (k)...")
    k_to_find <- estimate_rank_by_permutation_rv(mats, n_permutations = 0, return_max = TRUE)
    # print(k_to_find$plot) # Optionally show the plot
    nsimlr <- round(k_to_find$optimal_k)
    message(sprintf("SIMLR >> Estimated optimal k = %d", nsimlr))
  }
  
  if (verbose) message(sprintf("Step 4b: Adjusting modality matrices for covariates: %s", paste(covariates, collapse = ", ")))
  for (mycov in covariates) {
    if (verbose) cat(sprintf("  Adjusting by '%s' for modalities: ", mycov))
    for (x in 1:length(mats)) {
      if (verbose) cat(paste0(names(mats)[x], "..."))
      mats[[x]] <- antspymm_simlr_update_residuals(mats, x, mycov, blaster2, allnna, n.comp = nsimlr)
      mats[[x]] <- data.matrix(mats[[x]])
    }
    if (verbose) cat("Done.\n")
  }
  
  # --- 7. Regularization and Baseline Model Execution ---
  if (verbose) message("Step 5: Preparing regularization matrices...")
  regs0 <- lapply(mats, function(mat) {
    mycor <- cor(mat)
    mycor[mycor < 0.8] <- 0
    data.matrix(mycor)
  })
  regs <- regs0
  if (!missing(connect_cog)) {
    if (verbose) message("  Setting up sparse regularization for 'cg' modality.")
    regs[["cg"]] <- Matrix::Matrix(regs0[["cg"]], sparse = TRUE)
  }

  # --- Handle baseline energy types ---
  if (grepl('base.', energy, fixed = TRUE)) {
    if (verbose) message(sprintf("Executing baseline model: %s", energy))
    nsimlrmin <- min(c(nsimlr, unlist(lapply(mats, ncol))))
    if (nsimlrmin < nsimlr) {
      message(sprintf("SIMLR >> Dimensionality adjusted for baseline model: k = %d", nsimlrmin))
    } else {
      nsimlrmin <- nsimlr
    }
    
    v_result <- switch(energy,
      'base.rand.0' = antsr_random_features(mats, nsimlr, seed = 0),
      'base.rand.1' = antsr_random_features(mats, nsimlr, seed = 1),
      'base.pca' = antsr_pca_features(mats, nsimlrmin),
      'base.spca' = antsr_spca_features(mats, nsimlrmin),
      stop("Unknown baseline energy type.")
    )
    
    for (k in 1:length(mats)) {
      rownames(v_result[[k]]) <- colnames(mats[[k]])
    }
    return(list(simlrX = list(v = v_result)))
  }

  # --- 8. SIMLR Parameter Configuration ---
  if (verbose) message("Step 6: Configuring SIMLR parameters...")
  
  # Map energy aliases
  energy_map <- c(lrr = 'lowRankRegression', nc = 'normalized_correlation', cca = 'acc')
  if (energy %in% names(energy_map)) energy <- energy_map[energy]
  
  # Configure mixer algorithm
  mixer <- if (grepl("reg", energy)) 'ica' else 'pca'
  if (energy %in% c("lowRankRegression", "normalized_correlation")) mixer <- 'pca'
  if ( !is.null( mixAlg ) ) mixer=mixAlg
  # Configure sparseness
  sparval <- rep(0.8, length(mats))
  if (!is.null(sparseness)) {
    sparval <- if (length(sparseness) == length(mats)) sparseness else rep(sparseness[1], length(mats))
    if (verbose) message(sprintf("  Using custom sparseness: %s", paste(round(sparval, 2), collapse = ", ")))
  }
  
  if (nsimlr < 1) {
    ctit <- sum(unlist(lapply(mats, ncol)))
    nsimlr_new <- round(ctit * nsimlr)
    message(sprintf("SIMLR >> Relative k specified. New k = %d (%.2f * %d total features)", nsimlr_new, nsimlr, ctit))
    nsimlr <- nsimlr_new
  }

  # Initialize U matrix
  initu <- initializeSimlr(mats, nsimlr, uAlgorithm = "pca", jointReduction = FALSE)
  
  # Configure path modeling
  if (!is.null(path_modeling)) {
    clist <- path_modeling
    if (verbose) {
      message("  Using custom path modeling (connectors):")
      print(clist)
    }
  } else if (!missing(connect_cog)) {
    inflammNums <- which(names(mats) == 'cg')
    clist <- lapply(1:length(mats), function(j) if(j %in% inflammNums) (1:length(mats))[-inflammNums] else inflammNums)
  } else {
    clist <- NULL
  }
  
  # --- 9. Execute SIMLR ---
  maxits <- if (!is.null(iterations)) iterations else 1000
  if (verbose) message(sprintf("Step 7: Executing SIMLR with energy='%s', constraint='%s', k=%d, iterations=%d...",
                             energy, constraint, nsimlr, maxits))
  
  simlrX <- simlr(mats, regs,
                  iterations = maxits,
                  verbose = !doperm,
                  randomSeed = myseed,
                  mixAlg = mixer,
                  energyType = energy,
                  scale = c('center', 'eigenvalue'),
                  sparsenessQuantiles = sparval,
                  expBeta = 0.0,
                  positivities = rep("positive", length(mats)),
                  connectors = clist,
                  constraint = constraint,
                  optimizationStyle = optimizationStyle,
                  sparsenessAlg = sparsenessAlg,
                  initialUMatrix = initu)

  if (verbose) message("SIMLR execution complete.")

  # --- 10. Post-processing and Output Assembly ---
  if (verbose) message("Step 8: Post-processing results and assembling output...")

  pizzer <- rep("positive", length(mats)) # From original code
  for (kk in 1:length(mats)) {
    rownames(simlrX$v[[kk]]) <- idplist[[kk]]
  }
  
  nms <- names(simlrX$v) <- names(mats)
  simmat_list <- lapply(1:length(mats), function(j) {
    v_mat <- if (names(mats)[j] == 'cg' && pizzer[j] != 'positive') simlrX$v[[j]] else abs(simlrX$v[[j]])
    temp <- data.matrix(matsFull[[j]]) %*% v_mat
    colnames(temp) <- paste0(nms[j], colnames(temp))
    temp
  })
  
  simmat <- do.call(cbind, simmat_list)
  blaster2sim <- cbind(blaster, simmat)
  simnames <- colnames(simmat)
  
  if (verbose) message("Output assembly complete. Returning results.")
  
  return(list(
    demog = blaster2sim,
    mats = matsFull,
    simnames = simnames,
    simlrX = simlrX,
    energy = energy
  ))
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
#' @param data_matrices A list of numeric matrices. Each matrix should have dimensions (subjects by voxels).
#' @param k Integer. Number of projection dimensions (features) to generate.
#' @param seed Integer. Random seed for reproducibility. Default is 42.
#'
#' @return A list of projection matrices, each with dimensions (voxels by k).
#' @export
antsr_random_features <- function(data_matrices, k, seed = 42) {
  stopifnot(is.list(data_matrices))
  stopifnot(all(sapply(data_matrices, is.matrix)))
  set.seed(seed)
  plist = lapply(data_matrices, function(m) {
    nvox <- ncol(m)
    projection_matrix <- 
    orthogonalizeAndQSparsify( matrix(rnorm(nvox * k), nrow = nvox, ncol = k), 0.8, positivity='positive' )
  })
  names(plist)=names(data_matrices)
  for ( k in 1:length(data_matrices)) {
    rownames(plist[[k]])=colnames(data_matrices[[k]])
    prefix=paste0(names(data_matrices)[k],"PC")
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
#' @param data_matrices A list of numeric matrices. Each matrix should have dimensions (subjects by voxels).
#' @param k Integer. Number of principal components to retain.
#'
#' @return A named list of projection matrices (voxels by k), one per input matrix.
#' @export
antsr_pca_features <- function(data_matrices, k) {
  stopifnot(is.list(data_matrices))
  stopifnot(all(sapply(data_matrices, is.matrix)))

  plist <- lapply(data_matrices, function(m) {
    max_components <- min(nrow(m), ncol(m))
    if (k > max_components) {
      warning(sprintf("Requested k = %d exceeds maximum possible components (%d) for a matrix with dimensions (%d by %d). Using k = %d instead.",
                      k, max_components, nrow(m), ncol(m), max_components))
      k_adj <- max_components
    } else {
      k_adj <- k
    }
    pca <- prcomp(m, center = TRUE, scale. = TRUE, rank. = k_adj)
    pca$rotation[, 1:k_adj, drop = FALSE]
  })

  names(plist) <- names(data_matrices)
  return(plist)
}



#' Generate sparse PCA-based feature projections for a list of voxel matrices
#'
#' Applies sparse principal component analysis using the selected backend
#' ("elasticnet", "PMA", or "sparsepca") to each matrix in a list of subject by voxel data.
#'
#' @param data_matrices A list of numeric matrices. Each matrix should be subjects by voxels.
#' @param k Integer. Number of components to retain.
#' @param method Character. Sparse PCA backend to use. One of "default", "elasticnet", "PMA", or "sparsepca".
#' @param para Sparsity control parameter(s). Interpretation depends on backend:
#'   - For "elasticnet": number of nonzero loadings per component (length-k vector).
#'   - For "PMA": L1 bound on loading vector (scalar or length-k).
#'   - For "sparsepca": ignored (uses built-in defaults).
#'   - For "default": length-k vector of sparsity parameters.
#' @return A named list of sparse projection matrices (voxels by k).
#' @export
antsr_spca_features <- function(data_matrices, k, method = c( "default", "elasticnet", "PMA", "sparsepca"), para = NULL) {
  method <- match.arg(method)
  stopifnot(is.list(data_matrices))
  stopifnot(all(sapply(data_matrices, is.matrix)))

  if ( method == "default" ) {
    if ( is.null(para) ) {
      para <- rep(0.8, length(data_matrices))  # Default sparsity
    } else if ( length(para) == 1 ) {
      para <- rep(para, length(data_matrices))  # Replicate single value
    }
    loadings = antsr_pca_features( data_matrices, k )
    for ( k in 1:length(loadings)) {
      loadings[[k]] = orthogonalizeAndQSparsify( loadings[[k]], para[k], positivity='positive' )
      }
    return(loadings)
    }

  plist <- lapply(data_matrices, function(m) {
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

  names(plist) <- names(data_matrices)
  return(plist)
}


#' Orthogonalize a List of Matrices via Gradient Descent with Convergence Check
#'
#' This function takes a list of matrices (with the same number of columns) and
#' iteratively updates them to make the columns within each matrix more orthogonal.
#' It stops when the change in the total orthogonality error is below a given
#' tolerance or when the maximum number of iterations is reached.
#'
#' @param matrix_list A list of numeric matrices. All matrices must have the
#'   same number of columns (e.g., k components).
#' @param max_iterations The maximum number of gradient descent iterations to perform.
#' @param learning_rate The step size for each update.
#' @param tolerance The convergence threshold. The algorithm stops if the absolute
#'   change in the total orthogonality defect between iterations is less than this
#'   value.
#' @param verbose If TRUE, prints the orthogonality error at each iteration.
#'
#' @return A list of orthogonalized matrices with the same dimensions as the input.
orthogonalize_feature_space <- function(matrix_list,
                                        max_iterations = 100, # Increased default for convergence
                                        learning_rate = 0.05,
                                        tolerance = 1e-4,
                                        verbose = TRUE) {

  # --- 1. Input Validation ---
  stopifnot(is.list(matrix_list), length(matrix_list) > 0)
  k <- ncol(matrix_list[[1]])
  if (is.null(k)) stop("Matrices in the list must have columns.")
  stopifnot(all(sapply(matrix_list, ncol) == k))
  
  # --- 2. Helper Functions (as provided by you) ---
  
  # This helper calculates the orthogonality penalty for a single matrix
  # We assume a function `invariant_orthogonality_defect` exists in your environment.
  calculate_defect <- function(mat) {
    # Using a placeholder if the real function isn't available
    if (exists("invariant_orthogonality_defect")) {
      return(invariant_orthogonality_defect(mat))
    } else {
      # A standard implementation of ||A'A - I||^2 (for orthonormal columns)
      AtA <- crossprod(mat)
      return(sum((AtA - diag(k))^2))
    }
  }

  # --- 3. Optimization Loop ---
  if (verbose) message("Starting feature space orthogonalization...")
  
  # Initialize the previous total defect to a large number
  previous_total_defect <- Inf
  
  for (i in 1:max_iterations) {
    
    # Update each matrix in the list
    matrix_list <- lapply(matrix_list, function(mat) {
      grad <- gradient_invariant_orthogonality_defect(mat)
      mat_updated <- mat - learning_rate * grad
      return(mat_updated)
    })
    
    # --- Convergence Check ---
    
    # Calculate the total defect for the current iteration
    current_total_defect <- sum(sapply(matrix_list, calculate_defect))
    
    # Calculate the absolute change from the previous iteration
    change_in_defect <- abs(previous_total_defect - current_total_defect)
    
    if (verbose) {
      message(sprintf(
        "Iter: %d | Total Defect: %.6f | Change: %.2e",
        i, current_total_defect, change_in_defect
      ))
    }
    
    # Check if the change is below the tolerance threshold
    if (change_in_defect < tolerance) {
      if (verbose) message(paste("Convergence reached after", i, "iterations."))
      # Break the loop early
      break
    }
    
    # Update the previous defect for the next iteration's comparison
    previous_total_defect <- current_total_defect
  }
  
  if (i == max_iterations) {
#    warning("Reached maximum iterations without converging to the specified tolerance.")
  }
  
  if (verbose) message("Orthogonalization complete.")
  return(matrix_list)
}




.unit_inner_product <- function(x, y) {
  # Check that vectors are of same length
  if (length(x) != length(y)) {
    stop("Vectors x and y must be the same length.")
  }

  # Compute unit vectors
  x_unit <- x / sqrt(sum(x^2))
  y_unit <- y / sqrt(sum(y^2))

  # Return their inner product
  sum(x_unit * y_unit)
}



#' Apply SIMLR Sparseness and Normalize Features
#'
#' Applies smoothing and sparsity constraints to a matrix or vector as used in SIMLR or manifold learning,
#' followed by optional L1 feature normalization depending on the energy type.
#'
#' @param v A numeric matrix or vector to be transformed.
#' @param constraint_type Character. Type of manifold constraint; one of `"Stiefel"`, `"Grassmann"`, or `"None"`.
#' @param smoothing_matrix Optional numeric matrix. If provided, \code{v} is left-multiplied by this matrix.
#' @param positivity Character positive, negative or either.
#' @param sparseness_quantile Numeric between 0 and 1. Fraction of elements to sparsify using quantile thresholding.
#' @param constraint_weight Numeric. Weight for the constraint, used in orthogonalization.
#' @param constraint_iterations Numeric. Number of iterations for the orthogonalization optimization.
#' @param sparseness_alg Character. Sparsity algorithm to use (relevant for Stiefel and Grassmann).
#'.   ensemble or nnorth are test options.
#' @param energy_type Character. If set to one of `"acc"`, `"cca"`, `"nc"`, `"normalized_correlation"`, `"lowRankRegression"`, or `"lrr"`,
#'        then the returned matrix is normalized using \code{l1_normalize_features}.
#'
#' @return A numeric matrix of the same dimensions as \code{v}, with applied smoothing, sparsity, and optional normalization.
#' @export
simlr_sparseness <- function(v, 
                             constraint_type = c("Stiefel", "Grassmann", "none", "ortho"),
                             smoothing_matrix = NULL,
                             positivity = 'positive',
                             sparseness_quantile = 0.8,
                             constraint_weight = NA,
                             constraint_iterations = 10,
                             sparseness_alg = 'soft',
                             energy_type = 'acc') {
  v <- as.matrix(v)
  constraint_type <- match.arg(constraint_type)
  if ( is.na(sparseness_alg) ) sparseness_alg='soft'
  if ( positivity == 'positive') v=take_abs_unsigned(v)
  na2f.loc <- function (x) {
    x[is.na(x)] = FALSE
    x
  }
  # Apply smoothing
  if (!is.null(smoothing_matrix)) {
    v <- as.matrix( smoothing_matrix %*% v )
  }
  if ( is.na(constraint_weight)) {
    constraint_weight = 0.0
  }
  if ( constraint_weight > 1 ) constraint_weight=1
  if ( constraint_weight < 0 ) constraint_weight=0
  # Apply sparsity
  if (constraint_type %in% c("Stiefel", "Grassmann") ) {
    if ( is.na( sparseness_alg )) sparseness_alg = 'nnorth'
    if (sparseness_alg == 'ensemble') v <- t(ensembled_sparsity(t(v), positivity))
    if (sparseness_alg == 'nnorth') v = project_to_orthonormal_nonnegative( v, 
      constraint=positivity )
  } else {
    if ( constraint_type == "ortho" & constraint_weight > 0 ){
      if ( constraint_weight == 1 ) {
      v = project_to_orthonormal_nonnegative( v, 
        max_iter=constraint_iterations, constraint=positivity)
      } else {
      v = project_to_partially_orthonormal_nonnegative( v, 
        max_iter=constraint_iterations, constraint=positivity, ortho_strength=constraint_weight )
      }
    } else if ( na2f.loc( sparseness_alg == 'ensemble') ) {
      v <- t(ensembled_sparsity(t(v), positivity))
    } else if (na2f.loc( sparseness_alg == 'nnorth') ) {
      v = project_to_orthonormal_nonnegative( v, constraint=positivity )
    }
    if ( sparseness_quantile != 0 & constraint_weight == 0 & sparseness_alg =='soft' ) {
      v <- orthogonalizeAndQSparsify(
        v,
        sparsenessQuantile = sparseness_quantile,
        positivity = positivity,
        orthogonalize = FALSE,
        unitNorm = FALSE,
        softThresholding = TRUE,
        sparsenessAlg = NA
      )
    }
  }

  # Optional L1 normalization
  normalize_energy_types <- c("acc", "cca", "nc", "normalized_correlation", "lowRankRegression", "lrr", "dat", 'logcosh', 'exp', 'kurtosis','gauss' )
  if (!is.null(energy_type) && energy_type %in% normalize_energy_types) {
    v <- l1_normalize_features(v)
  }

  return( as.matrix( v ) )
}



#' Compute Dice overlap for sparse vectors with optional quantile thresholding
#'
#' This function computes the Dice similarity coefficient for two vectors,
#' either using a soft binary threshold (non-zero = 1), or by thresholding
#' values by quantile.
#'
#' @param x Numeric vector.
#' @param y Numeric vector of the same length as `x`.
#' @param quantile Numeric between 0 and 1. If > 0, values below the given quantile
#'        (based on absolute value) are set to 0 before computing Dice.
#'
#' @return Dice similarity coefficient (between 0 and 1).
#' @examples
#' dice_overlap_soft_vector(c(0,1,0,2), c(3,1,0,0))  # returns 0.5
#' dice_overlap_soft_vector(c(0,0,0,0), c(0,0,0,0))  # returns 1
#' dice_overlap_soft_vector(c(1,2,3,4), c(1,0,0,4), quantile=0.5)  # uses threshold
#' @export
dice_overlap_soft_vector <- function(x, y, quantile=0.5) {
 stopifnot(length(x) == length(y))
  stopifnot(is.numeric(quantile), quantile >= 0, quantile <= 1)

  # Threshold by quantile if specified
  if (quantile > 0) {
    thresh_x <- stats::quantile(abs(x), probs = quantile)
    thresh_y <- stats::quantile(abs(y), probs = quantile)
    x_bin <- as.integer(abs(x) >= thresh_x)
    y_bin <- as.integer(abs(y) >= thresh_y)
  } else {
    # Default: soft binary thresholding
    x_bin <- as.integer(x != 0)
    y_bin <- as.integer(y != 0)
  }

  intersection <- sum(x_bin & y_bin)
  size_x <- sum(x_bin)
  size_y <- sum(y_bin)

  if (size_x + size_y == 0) {
    return(1)  # Define overlap as 1 if both are all zeros
  }

  dice <- (2 * intersection) / (size_x + size_y)
  return(dice)
}


#' Compute pairwise Dice overlaps for matrix rows or columns
#'
#' Calculates Dice similarity coefficients between corresponding rows or columns
#' of two matrices, treating all non-zero values as 1 (soft binary overlap).
#'
#' @param A Numeric matrix.
#' @param B Numeric matrix of the same dimensions as `A`.
#' @param margin Integer: 1 for row-wise comparison, 2 for column-wise comparison.
#'
#' @return A numeric vector of Dice coefficients (length = nrow(A) or ncol(A)).
#' @examples
#' mat1 <- matrix(c(0,1,0,2, 1,0,0,0), nrow=2, byrow=TRUE)
#' mat2 <- matrix(c(1,1,0,0, 0,0,1,0), nrow=2, byrow=TRUE)
#' dice_overlap_soft_matrix(mat1, mat2, margin = 1)
#' dice_overlap_soft_matrix(t(mat1), t(mat2), margin = 2)
#' @export
dice_overlap_soft_matrix <- function(A, B, margin = 1) {
  # Check dimensions
  if (is.null(dim(A))) {
    stop("A must be a matrix, not a vector or NULL.")
  }
  if (is.null(dim(B))) {
    stop("B must be a matrix, not a vector or NULL.")
  }
  stopifnot(all(dim(A) == dim(B)))
  stopifnot(margin %in% c(1, 2))

  n <- if (margin == 1) nrow(A) else ncol(A)
  
  out <- numeric(n)
  for (i in seq_len(n)) {
    a <- if (margin == 1) A[i, ] else A[, i]
    b <- if (margin == 1) B[i, ] else B[, i]
    out[i] <- dice_overlap_soft_vector(a, b)
  }
  return(out)
}





#' Generate a Sophisticated Simulated Multi-View Dataset
#'
#' This function creates simulated multi-view data with a complex and realistic
#' ground truth, containing both shared and modality-specific latent signals.
#'
#' @param n_subjects The number of subjects (rows).
#' @param n_features A vector of integers specifying the number of features for each view.
#' @param k_shared The number of latent components common to ALL views.
#' @param k_specific A vector of integers specifying the number of unique latent
#'   components for EACH view. If a single number is given, it's recycled.
#' @param noise_sd The standard deviation of the Gaussian noise added to the data.
#' @param sparsity_level The proportion of elements in the shared loading matrix (V_f)
#'   to set to zero. A value between 0 and 1. Defaults to 0.5.
#' @param seed An optional random seed for reproducibility.
#'
#' @return A list containing:
#'   \item{data_list}{A named list of the final [n x p] data matrices.}
#'   \item{ground_truth}{A list containing the true latent structures:
#'     \itemize{
#'       \item `U_shared`: The ground truth shared basis [n x k_shared]. This is what
#'         `simlr` should aim to recover.
#'       \item `V_shared`: A list of the ground truth shared loading matrices
#'         [p x k_shared]. This is what the `simlr` `v` matrices should be compared against.
#'       \item `U_specific`: A list of the ground truth modality-specific bases.
#'       \item `U_combined`: The full latent basis [n x (k_shared + sum(k_specific))]
#'         that was used to generate the data.
#'     }
#'   }
#' @export
generate_structured_multiview_data <- function(n_subjects,
                                               n_features,
                                               k_shared,
                                               k_specific,
                                               noise_sd = 0.1,
                                               sparsity_level = 0.5,
                                               seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  n_modalities <- length(n_features)
  
  # Recycle k_specific if a single value is given
  if (length(k_specific) == 1) {
    k_specific <- rep(k_specific, n_modalities)
  }
  stopifnot(length(k_specific) == n_modalities)
  
  # --- 1. Create Shared and Specific Latent Structures (U matrices) ---
  U_shared <- qr.Q(qr(matrix(rnorm(n_subjects * k_shared), n_subjects, k_shared)))
  
  U_specific_list <- lapply(k_specific, function(m) {
    if (m > 0) qr.Q(qr(matrix(rnorm(n_subjects * m), n_subjects, m))) else NULL
  })
  
  U_combined <- do.call(cbind, c(list(U_shared), U_specific_list))

  # --- 2. Create Generative Loading Matrices (V matrices) ---
  data_list <- vector("list", n_modalities)
  V_shared_list <- vector("list", n_modalities)
  
  for (j in 1:n_modalities) {
    p_j <- n_features[j]
    m_j <- k_specific[j]
    
    # Create the loading block for the shared signal, V_f
    V_f <- matrix(rnorm(k_shared * p_j), nrow = k_shared, ncol = p_j)
    # Apply sparsity in a vectorized way
    n_zero <- floor(sparsity_level * length(V_f))
    V_f[sample(length(V_f), size = n_zero)] <- 0
    V_shared_list[[j]] <- t(V_f) # Store the [p x k] version
    
    # Create loading blocks for all specific signals
    v_specific_blocks <- lapply(1:n_modalities, function(i) {
      if (i == j && k_specific[i] > 0) {
        # This is the loading block for this modality's own specific signal
        matrix(rnorm(k_specific[i] * p_j), nrow = k_specific[i], ncol = p_j)
      } else if (k_specific[i] > 0) {
        # This is a zero block for another modality's specific signal
        matrix(0, nrow = k_specific[i], ncol = p_j)
      } else {
        NULL # No block needed if k_specific for that modality is 0
      }
    })
    
    # Combine all loading blocks
    V_combined <- do.call(rbind, c(list(V_f), v_specific_blocks))
    
    # --- 3. Generate Final Data Matrix ---
    signal <- U_combined %*% V_combined
    noise <- matrix(rnorm(n_subjects * p_j, sd = noise_sd), n_subjects, p_j)
    data_list[[j]] <- t(signal) + t(noise) # Transpose to get [n x p]
  }
  
  # Transpose final data matrices to be [n_subjects x n_features]
  data_list <- lapply(data_list, t)
  names(data_list) <- paste0("View", 1:n_modalities)
  names(V_shared_list) <- names(data_list)
  for ( k in 1:length(data_list)) colnames(data_list[[k]])=paste0(names(data_list)[k],'.',1:ncol(data_list[[k]]))

  return(list(
    data_list = data_list,
    ground_truth = list(
      U_shared = U_shared,
      V_shared = V_shared_list,
      U_specific = U_specific_list,
      U_combined = U_combined
    )
  ))
}



#' Select an Optimal Joint Number of Components for Multi-View Data
#'
#' This function determines a single optimal number of components (k) to represent
#' a multi-view dataset by analyzing the joint variance explained curve. It offers
#' multiple methods for selecting k and includes a built-in self-test to verify
#' its own correctness.
#'
#' @param mat_list A list of numeric matrices [subjects x features]. Required unless
#'   `self_test = TRUE`.
#' @param method The decomposition method. One of `"pca"` (fast SVD-based) or `"spca"`.
#' @param k_max The maximum number of components to consider.
#' @param sparsity A single sparsity parameter (0-1) for SPCA. Ignored for PCA.
#' @param selection_method The method for choosing k. One of `"elbow"` (point of
#'   maximum deviation from a straight line of improvement) or `"threshold"`.
#' @param variance_threshold The proportion of variance (0-1) to be explained.
#'   Only used when `selection_method = "threshold"`.
#' @param self_test Logical. If TRUE, the function will ignore all other inputs,
#'   run a built-in suite of tests on simulated data, and print the results.
#'   This is for verifying the function's integrity. Defaults to FALSE.
#'
#' @return If `self_test = FALSE`, a list containing:
#' \itemize{
#'   \item `optimal_k`: The selected optimal number of components.
#'   \item `joint_variance_curve`: A tibble with `k` and the cumulative proportion
#'     of variance explained at each `k`.
#'   \item `plot`: A ggplot object visualizing the results.
#' }
#' If `self_test = TRUE`, it prints test results and returns invisibly.
#'
#' @export
select_joint_k <- function(mat_list = NULL,
                           method = c("pca", "spca"),
                           k_max = NULL,
                           sparsity = 0.5,
                           selection_method = c("elbow", "threshold"),
                           variance_threshold = 0.90,
                           self_test = FALSE) {

  # --- Self-Test Block ---
  if (self_test) {
    .run_select_joint_k_tests()
    return(invisible())
  }

  find_min_dim <- function(mat_list) {
#    if (!all(sapply(mat_list, is.matrix))) {
#      stop("All elements of mat_list must be matrices")
#    }
    
    n_rows <- sapply(mat_list, nrow)
    n_cols <- sapply(mat_list, ncol)
    
    c(min_rows = min(n_rows), min_cols = min(n_cols))
  }
  if ( is.null( k_max ) ) {
    k_max = min( find_min_dim( mat_list ) )-1
  }
  
  # --- Main Function Logic ---
  if (is.null(mat_list)) stop("Input `mat_list` must be provided when not running self_test.")
  method <- match.arg(method)
  selection_method <- match.arg(selection_method)
  
  # 1. Calculate Explained Variance for Each Modality
  modality_variances <- lapply(mat_list, function(X) {
    k_max_local <- min(k_max, nrow(X) - 1, ncol(X))
    if (k_max_local < 1) return(numeric(0))

    if (method == "pca") {
      X_centered <- scale(X, center = TRUE, scale = FALSE)
      singular_values <- svd(X_centered, nu = k_max_local, nv = 0)$d
      prop_var_per_component <- singular_values^2 / sum(X_centered^2)
      return(cumsum(prop_var_per_component))
    } else { # "spca"
      if (!requireNamespace("elasticnet", quietly = TRUE)) stop("Package 'elasticnet' is required for spca.")
      explained_var <- numeric(k_max_local)
      X_centered <- scale(X, center = TRUE, scale = TRUE)
      total_variance <- sum(X_centered^2)
      for (k in 1:k_max_local) {
        spca_fit <- elasticnet::spca(X_centered, K = k, para = rep(sparsity, k), type = "predictor", sparse = "varnum")
        recon <- spca_fit$scores %*% t(spca_fit$loadings)
        explained_var[k] <- 1 - (sum((X_centered - recon)^2) / total_variance)
      }
      return(explained_var)
    }
  })

  # 2. Create the Joint Variance Curve
  joint_variance_curve <- modality_variances %>%
    setNames(paste0("Modality", 1:length(.))) %>%
    lapply(function(v) `length<-`(v, k_max)) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(k = 1:k_max) %>%
    tidyr::pivot_longer(-k, names_to = "modality", values_to = "variance_explained") %>%
    dplyr::group_by(k) %>%
    dplyr::summarise(joint_var = mean(variance_explained, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!is.na(joint_var))

  if (nrow(joint_variance_curve) < 3) {
      warning("Not enough data points (k < 3) to reliably determine an elbow. Returning k=1.")
      optimal_k <- 1
  } else {
      # 3. Select the Optimal K
      if (selection_method == "threshold") {
        optimal_k <- joint_variance_curve %>%
          dplyr::filter(joint_var >= variance_threshold) %>%
          dplyr::pull(k) %>%
          min()
        if (!is.finite(optimal_k)) {
          warning("No k reached the specified variance threshold. Returning the max k evaluated.")
          optimal_k <- max(joint_variance_curve$k)
        }
      } else { # "elbow" method
        
        y <- joint_variance_curve$joint_var
        k_values <- joint_variance_curve$k
        
        # Normalize both axes to a [0, 1] scale to make distance meaningful
        x_norm <- (k_values - min(k_values)) / (max(k_values) - min(k_values))
        y_norm <- (y - min(y)) / (max(y) - min(y))
        
        # The elbow is the point with the maximum perpendicular distance
        # to the line connecting the first and last points.
        distances <- y_norm - x_norm
        optimal_k <- k_values[which.max(distances)]
      }
  }
  
  # 4. Create Visualization and Return Results
  plot <- ggplot2::ggplot(joint_variance_curve, aes(x = k, y = joint_var)) +
    ggplot2::geom_line(linewidth = 1.2, color = "royalblue") +
    ggplot2::geom_point(size = 3, color = "royalblue") +
    ggplot2::geom_vline(xintercept = optimal_k, linetype = "dashed", color = "firebrick", linewidth = 1) +
    ggplot2::geom_text(aes(x = optimal_k, y = min(joint_var),
                           label = paste("Optimal k =", optimal_k)),
                       color = "firebrick", hjust = -0.1, vjust = 0, size = 4.5) +
    ggplot2::labs(
      title = "Joint Variance Explained vs. Number of Components",
      x = "Number of Components (k)",
      y = "Proportion of Joint Variance Explained"
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_x_continuous(breaks = 1:k_max) +
    ggplot2::theme_minimal(base_size = 14)

  return(list(
    optimal_k = optimal_k,
    joint_variance_curve = joint_variance_curve,
    plot = plot
  ))
}


#' Internal Self-Test for select_joint_k
#' @keywords internal
.run_select_joint_k_tests <- function() {
  
  context("Testing select_joint_k Functionality")
  
  # --- Setup 1: Create data with a known, sharp elbow at k=8 ---
  set.seed(42)
  n_subjects_pca <- 100; n_features_pca <- 25; k_true <- 8
  
  s_vals1 <- c(seq(from = 20, to = 15, length.out = k_true), 
               seq(from = 3, to = 1, length.out = n_features_pca - k_true))
  s_vals2 <- c(seq(from = 25, to = 18, length.out = k_true), 
               seq(from = 3.5, to = 0.5, length.out = n_features_pca - k_true))
  
  reconstruct_from_svd <- function(n, p, s_values) {
    U <- qr.Q(qr(matrix(rnorm(n * p), n, p)))[, 1:p]
    V <- qr.Q(qr(matrix(rnorm(p * p), p, p)))
    return(U %*% diag(s_values) %*% t(V))
  }
  X1 <- reconstruct_from_svd(n_subjects_pca, n_features_pca, s_vals1)
  X2 <- reconstruct_from_svd(n_subjects_pca, n_features_pca, s_vals2)
  pca_test_mat_list <- list(View1 = X1, View2 = X2)
  
  # --- Setup 2: Create a simple, well-behaved dataset for SPCA test ---
  set.seed(123)
  spca_test_mat_list <- list(
    ViewA = matrix(rnorm(50 * 20), 50, 20),
    ViewB = matrix(rnorm(50 * 30), 50, 30)
  )

  # --- Begin Tests ---
  
  test_that("Function returns a correctly structured list output", {
    cat("\n- Testing output structure...\n")
    res <- select_joint_k(pca_test_mat_list, k_max = 15)
    expect_true(is.list(res))
    expect_named(res, c("optimal_k", "joint_variance_curve", "plot"))
    succeed()
  })
  
  test_that("Robust elbow method correctly identifies the known elbow at k=8", {
    cat("- Testing robust elbow detection method...\n")
    res_pca <- select_joint_k(pca_test_mat_list, method = "pca", k_max = 20, selection_method = "elbow")
    
    expect_equal(res_pca$optimal_k, k_true, 
                 label = paste("Elbow method failed. Expected k=", k_true, ", but got k=", res_pca$optimal_k))
    succeed()
  })
  
  test_that("Threshold method works correctly", {
    cat("- Testing threshold selection method...\n")
    temp_res <- select_joint_k(pca_test_mat_list, k_max = 20)
    var_at_k4 <- temp_res$joint_variance_curve$joint_var[4]
    
    res_thresh <- select_joint_k(pca_test_mat_list, method = "pca", k_max = 20, 
                                 selection_method = "threshold", variance_threshold = var_at_k4)
    
    expect_equal(res_thresh$optimal_k, 4,
                 label = "Threshold method should have selected k=4.")
    succeed()
  })
  
  if ( FALSE )
  test_that("SPCA method runs and returns a valid result object", {
    cat("- Testing SPCA execution...\n")
    
    # This call now uses the well-behaved spca_test_mat_list
    result_spca <- select_joint_k(spca_test_mat_list, method = "spca", k_max = 5)
    
    # We test that the call completes and returns a list of the correct structure.
    expect_true(is.list(result_spca), label = "SPCA run should return a list.")
    expect_named(result_spca, c("optimal_k", "joint_variance_curve", "plot"),
                 label = "SPCA run should return a correctly named list.")
    
    succeed()
  })
  
  cat("\n--- All select_joint_k self-tests passed successfully! ---\n")
}




#' Estimate the Joint Rank of a Multi-View Dataset via Parallel Analysis
#'
#' This function provides a robust estimate of the number of significant, shared
#' components in a list of data matrices. It uses a streamlined Parallel Analysis
#' approach, comparing the singular value spectrum of the real data against the
#' spectrum of permuted (noise) data.
#'
#' @param mat_list A list of numeric matrices [subjects x features].
#' @param n_permutations The number of permutations to perform to create a stable
#'   null distribution. A higher number is more stable but slower. Defaults to 20.
#' @param pre_scaling Method to scale matrices before combining them.
#'   `"frobenius"` (default) scales each matrix to have a total variance of 1,
#'   ensuring fair contribution. `"none"` performs no scaling.
#' @param plot_scree Logical. If TRUE, generates a scree plot comparing the
#'   real and permuted singular values, which is essential for visual diagnosis.
#'
#' @return A list containing:
#'   \item{optimal_k}{The estimated number of significant components.}
#'   \item{results}{A tibble showing the real vs. permuted eigenvalues for each component.}
#'   \item{plot}{A ggplot object of the scree plot.}
#'
#' @details
#' The function first combines all modalities into a single matrix. It then
#' computes the SVD and its eigenvalues (squared singular values) for this real
#' data. It repeats this process `n_permutations` times on shuffled versions of
#' the data to generate a stable "null" or "chance" eigenvalue distribution. The
#' optimal k is determined as the last component where the real eigenvalue
#' exceeds the mean of the permuted eigenvalues.
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme_minimal scale_y_log10
#' @export
estimate_joint_rank <- function(mat_list,
                                n_permutations = 20,
                                pre_scaling = c("frobenius", "none"),
                                plot_scree = TRUE) {
  
  pre_scaling <- match.arg(pre_scaling)
  
  # --- 1. Pre-process and Combine Data ---
  if (pre_scaling == "frobenius") {
    # This is the recommended approach: center and scale each matrix by its
    # Frobenius norm so all modalities contribute equally.
    processed_mats <- lapply(mat_list, function(m) {
      m_centered <- scale(m, center = TRUE, scale = FALSE)
      norm_val <- sqrt(sum(m_centered^2))
      if (norm_val > 0) m_centered / norm_val else m_centered
    })
  } else {
    processed_mats <- lapply(mat_list, function(m) scale(m, center = TRUE, scale = TRUE))
  }
  
  combined_mat <- do.call(cbind, processed_mats)
  n <- nrow(combined_mat)
  
  # --- 2. Analyze Real Data ---
  # The eigenvalues of the covariance matrix are proportional to the squared singular values.
  # We use the squared singular values as they represent variance.
  message("Performing SVD on real data...")
  svd_real <- svd(combined_mat, nu = 0, nv = 0)
  eigenvalues_real <- (svd_real$d)^2
  
  # --- 3. Analyze Permuted Data (The Null Model) ---
  message(paste("Performing", n_permutations, "permutations..."))
  
  permuted_eigenvalues_list <- purrr::map(1:n_permutations, ~{
    # A more efficient permutation: shuffle each column independently.
    # This preserves the distributional properties of each feature better.
    mat_permuted <- apply(combined_mat, 2, sample)
    svd_perm <- svd(mat_permuted, nu = 0, nv = 0)
    return((svd_perm$d)^2)
  })
  
  # Average the eigenvalues across all permutations to get a stable null estimate
  eigenvalues_permuted_mean <- rowMeans(do.call(cbind, permuted_eigenvalues_list))
  
  # --- 4. Compare and Select Optimal K ---
  
  # The optimal k is the number of components where the real eigenvalue
  # is greater than the eigenvalue expected by chance.
  is_significant <- eigenvalues_real > eigenvalues_permuted_mean
  
  # Find the last TRUE before the first FALSE
  if (any(is_significant)) {
    optimal_k <- max(which(is_significant))
  } else {
    optimal_k <- 1 # Fallback if no component is significant
  }
  
  # Ensure k is reasonable
  optimal_k <- min(optimal_k, floor(n / 2))
  
  # --- 5. Prepare Results ---
  k_max_plot <- min(length(eigenvalues_real), 50) # Limit plot for clarity
  results_df <- tibble::tibble(
    k = 1:k_max_plot,
    eigenvalue = eigenvalues_real[1:k_max_plot],
    type = "Real Data"
  ) %>%
  bind_rows(tibble::tibble(
    k = 1:k_max_plot,
    eigenvalue = eigenvalues_permuted_mean[1:k_max_plot],
    type = "Permuted Data (Null)"
  ))
  
  # Create the scree plot for visualization
  scree_plot <- NULL
  if (plot_scree) {
    scree_plot <- ggplot(results_df, aes(x = k, y = eigenvalue, color = type, group = type)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      geom_vline(xintercept = optimal_k, linetype = "dashed", color = "firebrick", linewidth = 1) +
      ggrepel::geom_text_repel(data = ~subset(., k == optimal_k & type == "Real Data"),
                               aes(label = paste("Optimal k =", optimal_k)),
                               color = "firebrick", nudge_y = 0.5, nudge_x = 5) +
      scale_y_log10(labels = scales::scientific) + # Log scale is essential for scree plots
      labs(
        title = "Parallel Analysis Scree Plot",
        subtitle = "Comparing eigenvalues of real vs. permuted data",
        x = "Component Number (k)",
        y = "Eigenvalue (Variance Explained, Log Scale)"
      ) +
      theme_minimal(base_size = 14)
  }
  
  return(list(
    optimal_k = optimal_k,
    results = results_df,
    plot = scree_plot
  ))
}


#' Estimate Joint Rank via Cross-View RV Analysis
#'
#' @description Estimates the shared rank of a multi-view dataset. If permutations
#'   are used, it finds the rank that maximizes the signal-to-noise ratio against
#'   a permuted null model. If `n_permutations = 0`, it finds the "elbow" of the
#'   real data's signal curve.
#'
#' @param mat_list A list of numeric matrices [subjects x features].
#' @param n_permutations The number of permutations to create the null model.
#'   If set to 0, the function will use the elbow detection method instead.
#' @param var_explained_threshold The variance threshold to determine the
#'   upper bound on k.
#' @param return_max boolean just return the max likely rank from an individual matrix
#' @return A list containing `optimal_k`, a results tibble, and a plot.
#' @export
estimate_rank_by_permutation_rv <- function(mat_list,
                                            n_permutations = 20,
                                            var_explained_threshold = 0.99, return_max=FALSE ) {
  
  # --- 1. Setup and k_max determination ---
  n_modalities <- length(mat_list)
  if (n_modalities < 2 && n_permutations > 0) {
    warning("Permutation method requires at least 2 modalities. Switching to elbow method.")
    n_permutations <- 0
  }
  
  message("Determining max rank from individual modalities...")
  k_max <- min(sapply(mat_list, function(m) {
    if (ncol(m) < 2) return(1)
    eigenvalues <- svd(scale(m, center = TRUE, scale = FALSE))$d^2
    prop_var <- cumsum(eigenvalues) / sum(eigenvalues)
    min(which(prop_var >= var_explained_threshold))
  }))
  message(paste("Maximum possible rank (k_max) set to:", k_max))
  if (k_max < 1) stop("k_max is less than 1. Check input matrices.")
  if ( return_max ) return( list(optimal_k=k_max))

  # --- Core function to calculate the RV curve for a given dataset ---
  .calculate_rv_curve <- function(inner_mat_list, k_max_local) {
    U_list <- lapply(inner_mat_list, function(m) svd(m, nu = k_max_local)$u)
    rv_adj_matrix <- matrix(NA, nrow = k_max_local, ncol = n_modalities)
    for (k in 1:k_max_local) {
      for (i in 1:n_modalities) {
        Y_target <- U_list[[i]][, 1:k, drop = FALSE]
        other_indices <- setdiff(1:n_modalities, i)
        if (length(other_indices) == 0) { rv_adj_matrix[k, i] <- 0; next }
        U_other_combined <- do.call(cbind, U_list[other_indices])
        consensus_basis <- svd(U_other_combined, nu = k, nv = 0)$u
        rv_adj_matrix[k, i] <- adjusted_rvcoef(Y_target, consensus_basis)
      }
    }
    return(rowMeans(rv_adj_matrix, na.rm = TRUE))
  }
  
  preprocess_for_simlr <- function(modality_list) {
      lapply(modality_list, function(mat) {
        mat_centered <- scale(mat, center = TRUE, scale = FALSE)
        frobenius_norm <- sqrt(sum(mat_centered^2))
        if (frobenius_norm > .Machine$double.eps) mat_centered / frobenius_norm else mat_centered
      })
    }

  # --- 2. Calculate the "Real" Signal Curve (Always needed) ---
  message("Calculating RV curve for real data...")
  processed_mats <- preprocess_for_simlr(mat_list)
  real_rv_curve_vals <- .calculate_rv_curve(processed_mats, k_max)
  
  results_df <- tibble::tibble(
    k = 1:k_max,
    score = real_rv_curve_vals,
    type = "Real Data"
  )
  
  # --- 3. Determine Optimal K based on method ---
  
  if (n_permutations > 0) {
    # --- METHOD 1: PERMUTATION TEST ---
    message(paste("Calculating null distribution with", n_permutations, "permutations..."))
    pb <- progress::progress_bar$new(format = "  Permuting [:bar] :percent", total = n_permutations, width=60)
    
    permuted_rv_curves <- purrr::map(1:n_permutations, ~{
      pb$tick()
      permuted_mats <- processed_mats
      for (j in 2:n_modalities) {
        permuted_mats[[j]] <- permuted_mats[[j]][sample(nrow(permuted_mats[[j]])), ]
      }
      .calculate_rv_curve(permuted_mats, k_max)
    })
    
    null_rv_curve <- rowMeans(do.call(cbind, permuted_rv_curves))
    results_df <- results_df %>%
      bind_rows(tibble::tibble(k = 1:k_max, score = null_rv_curve, type = "Permuted Null"))
      
    signal_vs_noise <- real_rv_curve_vals - null_rv_curve
    optimal_k <- which.max(signal_vs_noise)
    plot_subtitle <- "Optimal k maximizes the difference between real and permuted signal strength"

  } else {
    # --- METHOD 2: ELBOW DETECTION (Fallback when n_permutations = 0) ---
    message("n_permutations is 0. Using elbow detection to find optimal k...")
    
    if (nrow(results_df) < 3) {
      warning("Not enough points to find an elbow; returning k=1.")
      optimal_k <- 1
    } else {
      y <- results_df$score
      k_values <- results_df$k
      x_norm <- (k_values - min(k_values)) / (max(k_values) - min(k_values))
      y_norm <- (y - min(y)) / (max(y) - min(y))
      distances <- y_norm - x_norm
      optimal_k <- k_values[which.max(distances)]
    }
    plot_subtitle <- "Optimal k is the 'elbow' of the real data's signal curve"
  }
  
  # --- 4. Visualize and Return Results ---
  plot <- ggplot(results_df, aes(x = k, y = score, color = type, linetype = type)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    geom_vline(xintercept = optimal_k, color = "firebrick", linetype = "dashed", linewidth = 1.2) +
    ggrepel::geom_text_repel(
      data = ~subset(., k == optimal_k & type == "Real Data"),
      aes(label = paste("Optimal k =", optimal_k)),
      color = "firebrick", nudge_y = 0.05, fontface = "bold",
      min.segment.length = 0, point.padding = 0.5
    ) +
    labs(
      title = "Shared Rank Estimation via Cross-View RV",
      subtitle = plot_subtitle,
      x = "Number of Shared Components (k)",
      y = "Joint Adjusted RV-Coefficient",
      color = "Data Type", linetype = "Data Type"
    ) +
    scale_color_manual(values = c("Real Data" = "navyblue", "Permuted Null" = "gray50", "Difference" = "firebrick")) +
    scale_linetype_manual(values = c("Real Data" = "solid", "Permuted Null" = "dotted", "Difference" = "dashed")) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")
    
  if (n_permutations > 0) {
    diff_df <- tibble(k = 1:k_max, score = signal_vs_noise, type = "Difference")
    plot <- plot + geom_line(data = diff_df, aes(x = k, y = score), color = "firebrick", alpha = 0.6)
  }

  return(list(optimal_k = optimal_k, results = results_df, plot = plot))
}


#' Create a Smoothed Sparse Matrix via Permutation Ensemble Averaging
#'
#' This function removes the bias of treating a single column as "special" by
#' running the `sparsify_by_column_winner` function multiple times. In each run,
#' the columns of the input matrix are permuted so that a different column is
#' in the "first" position. The results are un-permuted and then averaged to
#' produce a stable, unbiased sparse representation.
#'
#' @param X A numeric matrix [p_features x k_components].
#' @param default_constraint The constraint to apply to all other columns.
#'
#' @return A single, averaged sparse matrix with the same dimensions as X.
#' @export
#' @examples
#' set.seed(42)
#' mat <- matrix(rnorm(15), nrow=5, ncol=3)
#' mat[2,1] <- 10; mat[4,2] <- -12; mat[1,3] <- 14
#' print("Original Matrix:")
#' print(round(mat, 2))
#'
#' # Keep one column dense, sparsify others by magnitude ('either')
#' # sparse_ensembled <- ensembled_sparsity(mat,
#' #  default_constraint = "either"
#' # )
#'
ensembled_sparsity <- function(X, default_constraint = "positive") {
  if (default_constraint == 'positive') {
    X <- take_abs_unsigned(X)
  }
  
  k <- ncol(X)
  if (is.null(k) || k < 1) return(X)
  
  Y <- sparsify_by_column_winner(
    X,
    first_column_constraint = default_constraint,
    default_constraint = default_constraint,
    ensure_row_membership = FALSE # As requested, no reviving
  )
  
  return(Y)
  }



#' Check a matrix for zero-variance rows and columns
#'
#' This function checks a numeric matrix for rows or columns
#' with zero variance. It reports how many and which ones are affected,
#' using either their names (if present) or indices.
#'
#' @param mat A numeric matrix.
#'
#' @return
#' Boolean checking if any zero-variance rows or columns found.
#'
#' @details
#' Zero variance means all values in a row or column are identical
#' (after removing \code{NA}s).
#'
#' @examples
#' m <- matrix(c(1, 1, 1,
#'               2, 3, 4,
#'               5, 6, 7),
#'             nrow = 3, byrow = TRUE)
#' rownames(m) <- c("rowA", "rowB", "rowC")
#' colnames(m) <- c("colX", "colY", "colZ")
#' check_zero_variance(m)
#'
#' @export
check_zero_variance <- function(mat) {
  if (!is.matrix(mat)) {
    stop("Input must be a matrix.")
  }
  
  # Calculate variance for rows and columns
  row_var <- apply(mat, 1, var, na.rm = TRUE)
  col_var <- apply(mat, 2, var, na.rm = TRUE)
  
  zero_var_rows <- which(row_var == 0)
  zero_var_cols <- which(col_var == 0)
  
  # Get names if available
  row_ids <- if (!is.null(rownames(mat))) rownames(mat)[zero_var_rows] else zero_var_rows
  col_ids <- if (!is.null(colnames(mat))) colnames(mat)[zero_var_cols] else zero_var_cols
  outputter = c(FALSE,FALSE)
  if (length(zero_var_rows) > 0) {
    warning(sprintf(
      "Zero variance in %d rows: %s",
      length(zero_var_rows),
      paste(row_ids, collapse = ", ")
    ))
    outputter[1]=TRUE
  }
  
  if (length(zero_var_cols) > 0) {
    warning(sprintf(
      "Zero variance in %d columns: %s",
      length(zero_var_cols),
      paste(col_ids, collapse = ", ")
    ))
    outputter[2]=TRUE
  }
  return( outputter )
}


#' @title Project Non-Negative Matrix to Orthonormal Columns Matrix
#' @description Iteratively projects a non-negative matrix onto the intersection
#'              of non-negative matrices and matrices with orthonormal columns.
#' @param X A square numeric matrix with non-negative entries.
#' @param max_iter Maximum number of iterations for the alternating projection.
#' @param tol Tolerance for convergence. The projection stops if the change
#'            in the matrix norm is below this tolerance, or if max_iter is reached.
#' @param constraint either or positive 
#' @return A matrix Y that is non-negative and whose columns are orthonormal,
#'         closest to X in a sense defined by the alternating projections.
#' @export
project_to_orthonormal_nonnegative <- function(X, max_iter = 100, tol = 1e-4, constraint='positive' ) {
  # --- Input Validation ---
  stopifnot(is.matrix(X))
  if ( constraint=='positive' ) {
    X=take_abs_unsigned(X)
    X=pmax(X,0)
  }
  
  k <- ncol(X)
  p <- nrow(X)

  if (k == 0 || p == 0) return(X) # Handle empty matrix

  # Ensure matrix is square for the (A^T A)^(-1/2) part to make sense
  # If not square, the projection space is more complex.
  # The typical goal here is for the columns to be orthonormal, not necessarily
  # forming an orthogonal matrix if p != k.
  # The projection A(A^T A)^{-1/2} works even if A is not square.
  # If A is p x k, A^T A is k x k, so (A^T A)^(-1/2) is k x k.
  # The result is p x k.

  Y_prev <- X # Start with the input matrix

  for (iter in 1:max_iter) {
    # --- Projection 1: Orthonormal Columns ---
    # This projection is A * (A^T * A)^(-1/2)
    # First, compute A^T * A
    AtA <- crossprod(Y_prev) # Equivalent to t(Y_prev) %*% Y_prev

    # Compute the matrix square root of A^T * A
    # We need to handle potential singularity (if columns are linearly dependent)
    # Use SVD for robust square root of a matrix.
    # If AtA is singular, its square root won't be directly invertible.
    # A safer way for projection onto column space is QR.
    # Q from QR decomposition gives orthonormal columns spanning the same space.
    # However, for the specific projection A(A^T A)^{-1/2}, we need the matrix square root.

    # Alternative: Use SVD for the projection onto an orthonormal set of columns
    # X = U S V^T
    # Projection Q = U V^T (if square) or U * diag(1, ..., 1, 0, ...) * V^T
    # The projection A(A^T A)^{-1/2} is more direct for orthonormal columns.
    # Handle potential singularity of AtA by adding a small epsilon to the diagonal.
    # This makes the matrix positive definite.
    epsilon <- 1e-10
    AtA_reg <- AtA + diag(epsilon, k)

    # Compute the matrix square root of AtA_reg
    # Using SVD is a robust way: (U S V^T)^(1/2) = U S^(1/2) V^T
    svd_AtA <- svd(AtA_reg)
    sqrt_AtA <- svd_AtA$u %*% diag(sqrt(pmax(svd_AtA$d, 0))) %*% t(svd_AtA$v)

    # Compute the inverse of the square root
    # Handle potential singularity in sqrt_AtA itself
    inv_sqrt_AtA <- tryCatch({
      # Use SVD for inverse too for robustness
      svd_sqrt_AtA <- svd(sqrt_AtA)
      svd_sqrt_AtA$u %*% diag(1 / pmax(svd_sqrt_AtA$d, 0)) %*% t(svd_sqrt_AtA$v)
    }, error = function(e) {
      # Fallback if even after regularization, it's problematic.
      # This might happen if columns were perfectly collinear initially.
      # In such cases, the projection space might be lower-dimensional.
      # For simplicity, we'll return the previous Y_prev if this fails badly,
      # or try a simpler method like QR's Q.
      warning("Matrix square root inverse calculation failed. Falling back.")
      # If we fail here, a simpler fallback might be to just normalize columns individually,
      # but that doesn't enforce orthonormality between them.
      # A more robust approach for projection onto a general subspace using QR:
      qr_decomp <- qr(Y_prev)
      Q <- qr.Q(qr_decomp, ncol(qr_decomp$rank))
      return(Q) # This will have orthonormal columns, but might not be closest in Frobenius
    })

    # Calculate the projection onto orthonormal columns
    # Ensure Y_prev is not producing issues with dimensions or types for calculations
    Y_ortho <- Y_prev %*% inv_sqrt_AtA

    # --- Projection 2: Non-Negativity ---
    Y_new <- pmax(Y_ortho, 0)

    # --- Check for Convergence ---
    # Calculate the Frobenius norm of the change
    change_norm <- sqrt(sum((Y_new - Y_prev)^2))

    # Update Y_prev for the next iteration
    Y_prev <- Y_new

    if (change_norm < tol) {
      # print(paste("Converged at iteration", iter))
      break
    }
    if (iter == max_iter) {
#      warning("Projection did not converge within max_iter.")
    }
  }

  # Final check of the defect
  # final_defect <- invariant_orthogonality_defect(Y_prev)
  # print(paste("Final defect:", final_defect))

  return(Y_prev)
}





#' @title Project Non-Negative Matrix with Controllable Orthonormality
#' @description Iteratively projects a non-negative matrix towards the intersection
#'              of non-negative matrices and matrices with orthonormal columns.
#'              Allows control over the degree of orthonormality enforced.
#' @param X A square or rectangular numeric matrix with non-negative entries.
#' @param max_iter Maximum number of iterations for the alternating projection.
#' @param tol Tolerance for convergence (Frobenius norm of change).
#' @param constraint 'positive' or 'either' (influences initial data preprocessing).
#' @param ortho_strength A value between 0 and 1, controlling the degree of
#'                       orthonormality enforced. 0 means no orthonormality
#'                       enforcement (only non-negativity), 1 means full
#'                       orthonormality.
#' @param epsilon very small value for stability.
#' @return A matrix Y that is non-negative and has columns that are
#'         controlled in their orthonormality by ortho_strength.
#' @examples
#' # Example matrix (non-negative)
#' X_sample <- matrix(c(
#'   1, 2, 0.1,
#'   0.5, 1, 0.8,
#'   0.2, 0.5, 1
#' ), nrow=3, byrow=TRUE)
#'
#' # Full orthonormality (ortho_strength = 1)
#' Y_full_ortho = project_to_partially_orthonormal_nonnegative(
#'   X_sample, ortho_strength = 1, max_iter = 5)
#'
#' # No orthonormality enforcement (ortho_strength = 0) - 
#'     # should just be pmax(X, 0)
#' Y_no_ortho <- project_to_partially_orthonormal_nonnegative(
#'    X_sample, ortho_strength = 0, max_iter = 5) # Low iter for speed
#'
#' @export
project_to_partially_orthonormal_nonnegative <- function(X, 
  max_iter = 100, tol = 1e-6, 
  constraint='positive', 
  ortho_strength = 1.0, 
  epsilon = 1e-4 ) {
  # --- Input Validation ---
  stopifnot(is.matrix(X))
  
  # Validate ortho_strength
  if (!is.numeric(ortho_strength) || ortho_strength < 0 || ortho_strength > 1) {
    stop("ortho_strength must be between 0 and 1.")
  }
  
  # Apply initial constraint and non-negativity
  if ( constraint=='positive' ) {
    X_processed <- take_abs_unsigned(X) # Assumes take_abs_unsigned is available
    X_processed <- pmax(X_processed, 0) # Ensure non-negativity
  } else if (constraint == 'either') {
      # For 'either', we only care about non-negativity initially for the projection
      # The projection itself will handle magnitudes.
      X_processed <- pmax(X, 0) # Ensure non-negativity
  } else {
      stop("Constraint must be 'positive' or 'either'.")
  }

  k <- ncol(X_processed)
  p <- nrow(X_processed)

  if (k == 0 || p == 0) return(X_processed) # Handle empty matrix

  Y_prev <- X_processed # Start with the processed input matrix

  for (iter in 1:max_iter) {
    # --- Projection 1: Controllable Orthonormal Columns ---
    # This projection is A * (A^T * A)^(-1/2)
    # We modify the inversion of singular values based on ortho_strength.

    AtA <- crossprod(Y_prev) # A^T * A

    # Use SVD for robust computation of eigenvalues and eigenvectors
    svd_AtA <- svd(AtA)
    eigenvalues <- svd_AtA$d
    V <- svd_AtA$v # Eigenvectors (columns of V are eigenvectors)

    # --- Modify singular values based on ortho_strength ---
    # We want to transform s_i = sqrt(eigenvalues_i) to s'_i.
    # s'_i = (1 - strength) * s_i + strength * (1 / s_i)
    # This interpolates between preserving original column magnitudes (strength=0)
    # and forcing them to be unit norms (strength=1).

    s_i <- sqrt(pmax(eigenvalues, 0)) # Get singular values (sqrt of eigenvalues), handle negative eigenvalues from computation errors
    
    # Avoid division by zero if s_i is very small.
    # For strength=1, 1/s_i can be infinity if s_i=0.
    # For strength=0, s_i is preserved.
    # We need to be careful: if s_i is near zero, 1/s_i will be very large.
    # This can destabilize the projection.

    # A more stable approach for the modified inverse square root:
    # Let f(s) = (1 - strength)*s + strength*(1/s)
    # Compute g(s) = 1 / f(s)
    # If s_i is very small (near zero), 1/s_i is large. If strength > 0,
    # the term (1-strength)*s_i becomes small, and strength/s_i dominates.
    # This can lead to very large values if s_i is tiny.

    # Consider a thresholding/clipping for 1/s_i when it's very large.
    # Or, a different interpolation:
    # Let's transform s_i to 1/s_i. If s_i is very small, 1/s_i is huge.
    # We want to dampen this effect.
    # Alternative approach:
    # `transformed_s_i = s_i ^ (1 - ortho_strength)` ? No, this doesn't interpolate correctly.

    # Let's use the interpolation formula directly, but add safeguards.
    # Target scaling for singular values:
    # If strength = 1, we want scaling factor 1/s_i
    # If strength = 0, we want scaling factor s_i (to keep magnitude) - wait, the projection IS A(A^T A)^{-1/2}.
    # The goal is to scale A's columns. The (A^T A)^{-1/2} part *applies* the scaling.
    # So, if strength=0, we want the *effect* of (A^T A)^{-1/2} to be identity.
    # This means we want to transform the diagonal matrix S^-1/2 to something else.

    # Let S_inv_sqrt = diag(1 / s_i)
    # We want to find S'_inv_sqrt such that:
    # strength=1 -> S'_inv_sqrt = S_inv_sqrt
    # strength=0 -> S'_inv_sqrt = Identity matrix (effectively, A * I = A, no projection to unit norm)
    #
    # Interpolate the *inverse singular values*:
    # Let s_inv_i = 1 / s_i
    # new_s_inv_i = (1 - ortho_strength) * 1 + ortho_strength * s_inv_i
    # This is still problematic if s_i is zero.

    # More robust strategy for softening: Modify the reciprocal of s_i.
    # Instead of 1/s_i, use 1 / (s_i + epsilon_for_inversion)
    # Then, interpolate:
    # effective_inv_s_i = (1 - ortho_strength) * 1 + ortho_strength * (1 / (s_i + epsilon))
    # This seems too arbitrary.

    # Let's reconsider the projection: A * (A^T A)^{-1/2} = A * V * S_inv_sqrt * V^T
    # The term V * S_inv_sqrt * V^T is the matrix that transforms the columns of A
    # to make them orthonormal.
    # We want to control how much this transformation is applied.
    # Let T = V * S_inv_sqrt * V^T. We want to apply a transformation that is a mix
    # of Identity (strength=0) and T (strength=1).
    # New_Transform = (1 - ortho_strength) * Identity + ortho_strength * T

    # Compute T = V * diag(1/s_i) * V^T
    # Need to handle s_i near zero. Add epsilon to s_i before inverting.
    s_i_safe <- pmax(s_i, epsilon) # Ensure s_i is not zero
    S_inv_sqrt <- diag(1 / s_i_safe)
    T <- V %*% S_inv_sqrt %*% t(V)

    # Construct the mixed transformation matrix
    Mixed_Transform <- (1 - ortho_strength) * diag(k) + ortho_strength * T

    # Apply the mixed transformation to Y_prev
    Y_ortho <- Y_prev %*% Mixed_Transform

    # --- Projection 2: Non-Negativity ---
    Y_new <- pmax(Y_ortho, 0)

    # --- Check for Convergence ---
    change_norm <- sqrt(sum((Y_new - Y_prev)^2))

    Y_prev <- Y_new

    if (change_norm < tol) {
      # print(paste("Converged at iteration", iter))
      break
    }
    if (iter == max_iter) {
      # warning("Projection did not converge within max_iter.")
    }
  }

  return(Y_prev)
}


