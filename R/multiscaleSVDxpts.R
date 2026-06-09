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


#' Robust SVD with Automatic rsvd Fallback
#'
#' This function attempts a standard SVD using base R's \code{svd()}.
#' If that fails (typically due to ill-conditioning or non-convergence),
#' it automatically falls back to a randomized SVD via the \code{rsvd} package.
#'
#' Optionally, the matrix can be scaled by its maximum absolute value to
#' improve numerical stability before decomposition.
#'
#' @param x A numeric matrix.
#' @param nu Number of left singular vectors to compute.
#'           Default: \code{min(nrow(x), ncol(x))}.
#' @param nv Number of right singular vectors to compute.
#'           Default: \code{min(nrow(x), ncol(x))}.
#' @param dividebymax Logical. If TRUE, scale \code{x} by its max absolute value
#'        before SVD. Default: FALSE.
#' @param NA2Noise boolean replaces NA values with small noise values
#'
#' @return A list with components \code{u}, \code{d}, and \code{v}, matching
#'         the structure of base R's \code{svd()} output.
#'
#' @examples
#' mat <- matrix(rnorm(100 * 50), 100, 50)
#' sv <- ba_svd(mat, nu = 10, nv = 0)
#' @export
ba_svd <- function(x,
                   nu = min(nrow(x), ncol(x)),
                   nv = min(nrow(x), ncol(x)),
                   dividebymax = FALSE, 
                   NA2Noise = TRUE) {

  if (!is.matrix(x)) stop("Input `x` must be a matrix.")
  
  orig_ncol <- ncol(x)
  col_names <- colnames(x)

  # 1. Handle NA/Inf early
  if (NA2Noise) {
    if (anyNA(x) || any(is.infinite(x))) {
      warning("Input contains NA/Inf --- replacing with small random noise.")
      bad <- which(!is.finite(x))
      x[bad] <- rnorm(length(bad), mean = 0, sd = 1e-6)
    }
  }

  # 2. Identify and Remove Zero-Variance Columns
  # We use matrixStats or apply for speed; here checking if min == max
  col_min <- apply(x, 2, min)
  col_max <- apply(x, 2, max)
  keep_cols <- col_min != col_max
  
  if (sum(keep_cols) == 0) {
    # If all columns are constant, the SVD is simple: 
    # one non-zero singular value if the constant is non-zero.
    # But for simplicity and safety in these pipelines, we can just 
    # use the base svd on the original matrix if it's very small or constant.
    return(svd(x, nu = nu, nv = nv))
  }
  
  x_working <- x[, keep_cols, drop = FALSE]

  # 3. Optional stability scaling
  if (dividebymax) {
    mx <- max(abs(x_working))
    if (mx > 0) x_working <- x_working / mx
  }

  # 4. Perform SVD (Attempt base, then rsvd)
  # Adjust nu/nv for the reduced dimensions to avoid subscript out of bounds
  safe_nu <- min(nu, nrow(x_working), ncol(x_working))
  safe_nv <- min(nv, nrow(x_working), ncol(x_working))

  svd_out <- try(svd(x_working, nu = safe_nu, nv = safe_nv), silent = TRUE)

  if (inherits(svd_out, "try-error")) {
    message("Base svd() failed; switching to randomized SVD (rsvd).")
    if (!requireNamespace("rsvd", quietly = TRUE)) stop("Package 'rsvd' required.")
    
    r <- rsvd::rsvd(x_working, k = max(safe_nu, safe_nv))
    svd_out <- list(
      u = if (safe_nu > 0) r$u[, seq_len(safe_nu), drop = FALSE] else matrix(0, nrow(x), 0),
      d = r$d,
      v = if (safe_nv > 0) r$v[, seq_len(safe_nv), drop = FALSE] else matrix(0, ncol(x_working), 0)
    )
  }
  return(svd_out)
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
#' @references Maggioni, M., et al. "Multiscale data geometry."
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
    x = myijmat[, 3], symmetric = TRUE,
    dims = c(ncol(x), ncol(x))
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
#' @references Maggioni, M., et al. "Multiscale data geometry."
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
#' @references Maggioni, M., et al. "Multiscale data geometry."
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
    x <- fast_whiten(x)
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
    v_var <- var(v[, vv], na.rm = TRUE)
    if (!is.finite(v_var)) v_var <- 0
    if (v_var > epsval) {
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
#' @param V The current loading matrix for the modality [p x k].
#' @param Z Prior matrix
#' @param lambda weight
#'
#' @return A matrix [p x k] representing the gradient of the objective.
#'
.calculate_domain_energy <- function(V, Z, lambda) {
  # Input validation
  stopifnot(
#    is.matrix(V) && is.matrix(Z),
    "Matrix dimensions are not compatible." = ncol(Z) == nrow(V),
    "Lambda must be non-negative." = lambda >= 0
  )
  
  # Compute projection
  Z=data.matrix(Z)
#  Vmod=l1_normalize_features(V)
#  Vmod <- apply(V, 2, function(col) col / sqrt(sum(col^2)))  # L2 normalize columns
#  Z <- apply(Z, 2, function(col) col / sqrt(sum(col^2)))  # L2 normalize columns
  M <- Z %*% V
  
  # Compute domain alignment energy: -lambda * ||M||_F^2
  energy <- -lambda * sum(M^2) / prod( c(nrow(Z), ncol(V)))
  
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
#    is.matrix(V) && is.matrix(Z),
    "Matrix dimensions are not compatible." = ncol(Z) == p,
    "Lambda must be non-negative." = lambda >= 0
  )
  
  # Compute projection
  Z=data.matrix(Z)
#  Vmod=l1_normalize_features(V)
#  Vmod <- apply(V, 2, function(col) col / sqrt(sum(col^2)))  # L2 normalize columns
#  Z <- apply(Z, 2, function(col) col / sqrt(sum(col^2)))  # L2 normalize columns
  M <- Z %*% V
  
  # Compute gradient: -2 * lambda * Z^T M
  gradient <- 2 / prod( c(nrow(Z), ncol(V))) * lambda * t(Z) %*% M
  
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



#' Calculate SiMLR energy for optimization
#' Find Optimal SiMLR Initializer
#'
#' This function generates multiple random candidate U matrices, evaluates their
#' associated energy (without gradient descent), and returns the best-scoring
#' initialization. A seed parameter ensures reproducibility across runs.
#'
#' @param data_matrices A list of modality-specific data matrices.
#' @param n_init Number of random initializations to try (default = 10).
#' @param basisK Number of basis components (columns in U).
#' @param energyType Energy function to evaluate.
#' @param domainMatrices Optional list of domain priors (same length as data_matrices).
#' @param domainLambdas Optional vector of domain weights.
#' @param verbose Logical, whether to print progress.
#' @param seed Optional numeric seed for reproducibility (default = NULL, no seed set).
#'
#' @return A list with elements:
#'   \describe{
#'     \item{bestU}{List of U matrices (one per modality) for the best initialization.}
#'     \item{bestV}{List of corresponding V matrices.}
#'     \item{bestEnergy}{Mean energy for the selected initialization.}
#'   }
#' @export
optimal_simlr_initializer <- function(data_matrices,
                                     n_init = 10,
                                     basisK,
                                     energyType = "acc",
                                     domainMatrices = NULL,
                                     domainLambdas = NULL,
                                     verbose = TRUE,
                                     seed = NULL) {
  nModalities <- length(data_matrices)

  # Set random seed if provided
  if (!is.null(seed)) {
    if (!is.numeric(seed) || seed < 0 || seed != as.integer(seed)) {
      stop("seed must be a non-negative integer or NULL")
    }
    set.seed(seed)
    if (verbose) {
      message(sprintf("Using seed: %d", seed))
    }
  }

  bestEnergy <- Inf
  bestU <- NULL
  bestV <- NULL

  for (trial in seq_len(n_init)) {
    # --- 1. Generate random U matrices ---
    if ( trial == 1 ) {
      U = initializeSimlr(data_matrices, basisK, uAlgorithm = "pca", jointReduction = TRUE)
    } else {
      U = matrix(rnorm(nrow(data_matrices[[1]]) * basisK), nrow(data_matrices[[1]]), basisK)
    }
    U = U / norm(U, "F")
    # --- 2. Build corresponding V matrices ---
    V_list <- vector("list", nModalities)
    for (i in seq_len(nModalities)) {
      V_list[[i]] <- t(data_matrices[[i]]) %*% U
      V_list[[i]] <- V_list[[i]] / norm(V_list[[i]], "F")
    }

    # --- 3. Compute energy for this initialization ---
    total_energy <- 0
    for (i in seq_len(nModalities)) {
      sim_e <- calculate_simlr_energy(
        V_list[[i]], data_matrices[[i]], U,
        energyType
      )

      dom_e <- 0
      if (!is.null(domainMatrices) && !is.null(domainLambdas)) {
        lam <- domainLambdas[i]
        if (lam > 0) {
          dom_e <- calculate_simlr_energy(
            V_list[[i]], data_matrices[[i]], U,
            "dat", lambda = lam, prior_matrix = domainMatrices[[i]]
          )
        }
      }

      total_energy <- total_energy + sim_e + dom_e
    }

    mean_energy <- total_energy / nModalities

    if (verbose) {
      message(sprintf("Trial %d/%d | Mean energy = %.4f",
                      trial, n_init, mean_energy))
    }

    # --- 4. Keep the best initialization ---
    if (mean_energy < bestEnergy) {
      bestEnergy <- mean_energy
      bestU <- U
      bestV <- V_list
    }
  }

  if (verbose) {
    message(sprintf("[OK] Best initialization found with mean energy = %.4f", bestEnergy))
  }

  return(list(bestU = bestU, bestV = bestV, bestEnergy = bestEnergy))
}



#' Transform a matrix by standard preprocessing
#'
#' Applies centering, scaling, and/or normalization transforms
#' to prepare data for optimization or learning-rate estimation.
#'
#' @param X Numeric matrix.
#' @param method Character string specifying the transformation:
#'   \itemize{
#'     \item `"none"`: no transformation
#'     \item `"center"`: subtract column means
#'     \item `"zscore"`: center and scale columns by their standard deviations
#'     \item `"minmax"`: rescale columns to [0, 1]
#'     \item `"l2"`: normalize each column to unit L2 norm, then multiply by sqrt(p*k)
#'     \item `"frob"`: normalize entire matrix to unit Frobenius norm, then multiply by sqrt(p*k)
#'     \item `"frob_zscore"`: z-score columns, then normalize Frobenius norm,
#'           then multiply by sqrt(p*k)
#'   }
#'
#' @return A list with:
#'   \itemize{
#'     \item \code{Xs}: transformed matrix
#'     \item \code{params}: list of learned transform parameters
#'   }
#' @export
transform_matrix <- function(X, method = c("none", "center", "zscore", "minmax", "l2", "frob", "frob_zscore")) {
  method <- match.arg(method)
  stopifnot(is.matrix(X))

  params <- list(method = method)
  Xs <- X
  scale_factor <- sqrt(prod(dim(X)))  # dimension-based scaling factor

  if (method == "none") {
    return(list(Xs = X, params = params))
  }

  if (method == "center") {
    mu <- colMeans(X)
    Xs <- sweep(X, 2, mu, "-")
    params$mu <- mu
  }

  if (method == "zscore") {
    mu <- colMeans(X)
    sdv <- apply(X, 2, sd)
    sdv[sdv == 0] <- 1
    Xs <- sweep(X, 2, mu, "-")
    Xs <- sweep(Xs, 2, sdv, "/")
    params$mu <- mu
    params$sdv <- sdv
  }

  if (method == "minmax") {
    minv <- apply(X, 2, min)
    maxv <- apply(X, 2, max)
    rng <- maxv - minv
    rng[rng == 0] <- 1
    Xs <- sweep(X, 2, minv, "-")
    Xs <- sweep(Xs, 2, rng, "/")
    params$minv <- minv
    params$maxv <- maxv
  }

  if (method == "l2") {
    l2 <- sqrt(colSums(X^2))
    l2[l2 == 0] <- 1
    Xs <- sweep(X, 2, l2, "/")
    Xs <- Xs * scale_factor
    params$l2 <- l2
    params$scale_factor <- scale_factor
  }

  if (method == "frob") {
    frob_norm <- sqrt(sum(X^2))
    if (frob_norm < 1e-12) frob_norm <- 1
    Xs <- X / frob_norm * scale_factor
    params$frob_norm <- frob_norm
    params$scale_factor <- scale_factor
  }

  if (method == "frob_zscore") {
    mu <- colMeans(X)
    sdv <- apply(X, 2, sd)
    sdv[sdv == 0] <- 1
    Xs <- sweep(X, 2, mu, "-")
    Xs <- sweep(Xs, 2, sdv, "/")
    frob_norm <- sqrt(sum(Xs^2))
    if (frob_norm < 1e-12) frob_norm <- 1
    Xs <- Xs / frob_norm * scale_factor
    params$mu <- mu
    params$sdv <- sdv
    params$frob_norm <- frob_norm
    params$scale_factor <- scale_factor
  }

  colnames(Xs) <- colnames(X)
  rownames(Xs) <- rownames(X)

  list(Xs = Xs, params = params)
}


#' Undo a previously applied matrix transform
#'
#' @param Xs Transformed matrix.
#' @param params List of parameters returned by \code{transform_matrix()}.
#'
#' @return The untransformed (original scale) matrix.
#' @export
undo_transform_matrix <- function(Xs, params) {
  stopifnot(is.matrix(Xs), is.list(params))
  method <- params$method
  Xrec <- Xs

  if (method == "none") return(Xrec)

  if (method == "center") {
    Xrec <- sweep(Xs, 2, params$mu, "+")
  }

  if (method == "zscore") {
    Xrec <- sweep(Xs, 2, params$sdv, "*")
    Xrec <- sweep(Xrec, 2, params$mu, "+")
  }

  if (method == "minmax") {
    rng <- params$maxv - params$minv
    rng[rng == 0] <- 1
    Xrec <- sweep(Xs, 2, rng, "*")
    Xrec <- sweep(Xrec, 2, params$minv, "+")
  }

  if (method == "l2") {
    Xrec <- Xs / params$scale_factor
    Xrec <- sweep(Xrec, 2, params$l2, "*")
  }

  if (method == "frob") {
    Xrec <- Xs / params$scale_factor * params$frob_norm
  }

  if (method == "frob_zscore") {
    Xrec <- Xs / params$scale_factor * params$frob_norm
    Xrec <- sweep(Xrec, 2, params$sdv, "*")
    Xrec <- sweep(Xrec, 2, params$mu, "+")
  }

  colnames(Xrec) <- colnames(Xs)
  rownames(Xrec) <- rownames(Xs)

  Xrec
}


#' Apply a learned transform to a new matrix
#'
#' Applies the same centering, scaling, and normalization learned from a training matrix.
#'
#' @param Xnew New data matrix to transform.
#' @param params Parameters returned by \code{transform_matrix()}.
#'
#' @return Transformed matrix using the learned parameters.
#' @export
apply_transform_matrix <- function(Xnew, params) {
  stopifnot(is.matrix(Xnew), is.list(params))
  method <- params$method
  Xs <- Xnew

  if (method == "none") return(Xs)

  if (method == "center") {
    Xs <- sweep(Xs, 2, params$mu, "-")
  }

  if (method == "zscore") {
    Xs <- sweep(Xs, 2, params$mu, "-")
    Xs <- sweep(Xs, 2, params$sdv, "/")
  }

  if (method == "minmax") {
    rng <- params$maxv - params$minv
    rng[rng == 0] <- 1
    Xs <- sweep(Xs, 2, params$minv, "-")
    Xs <- sweep(Xs, 2, rng, "/")
  }

  if (method == "l2") {
    Xs <- sweep(Xs, 2, params$l2, "/")
    Xs <- Xs * (params$scale_factor + 1e-12)
  }

  if (method == "frob") {
    Xs <- Xs / (params$frob_norm + 1e-12) * (params$scale_factor + 1e-12)
  }

  if (method == "frob_zscore") {
    Xs <- sweep(Xs, 2, params$mu, "-")
    Xs <- sweep(Xs, 2, params$sdv, "/")
    Xs <- Xs / (params$frob_norm + 1e-12) * (params$scale_factor + 1e-12)
  }

  colnames(Xs) <- colnames(Xnew)
  rownames(Xs) <- rownames(Xnew)

  Xs
}



#' Estimate Robust Learning Rate for NSA-Flow
#'
#' @description
#' General-purpose estimator for an initial learning rate in \code{nsa_flow()},
#' supporting multiple automatic strategies including the original Brent line search.
#'
#' @param Y0 Numeric matrix, initial point.
#' @param X0 Numeric matrix, target matrix.
#' @param w Weighting parameter between fidelity and orthogonality.
#' @param retraction Retraction method used by nsa_flow.
#' @param nsa_energy Optional function(Y) to compute NSA energy; if NULL, uses default.
#' @param apply_nonneg Logical; if TRUE, applies nonnegativity.
#' @param method Character; learning rate estimation method:
#'   one of c("brent", "grid", "armijo", "golden", "adaptive").
#'   "default" returns fixed values (0.001 if nonneg, else 1.0).
#' @param search_range Numeric vector of length 2; log10(alpha) search range
#' @param verbose Logical; if TRUE, prints diagnostics.
#' @param plot Logical; if TRUE, plots energy vs learning rate (if applicable).
#'
#' @return A list with:
#'   \itemize{
#'     \item \code{estimated_learning_rate}: numeric, chosen value.
#'     \item \code{search_df}: data frame with log10(alpha), alpha, and energy (if available).
#'     \item \code{plot}: ggplot object (if \code{plot=TRUE}), otherwise NULL.
#'   }
#' @export
estimate_learning_rate_nsa <- function(
  Y0, X0,
  w = 0.5,
  retraction = "soft_polar",
  nsa_energy = NULL,
  apply_nonneg = TRUE,
  method = c("brent", "grid", "armijo", "golden", "adaptive", "default" ),
  search_range = c(-8.0,2.0),
  verbose = TRUE,
  plot = TRUE
) {
  method <- match.arg(method)
  stopifnot(is.matrix(Y0), is.matrix(X0))
  if (method == "default" & apply_nonneg) {
    return(list(
      estimated_learning_rate = 0.001,
      search_df = NULL,
      plot = NULL
    ))
  } else if (method == "default" & !apply_nonneg) {
    return(list(
      estimated_learning_rate = 1.0,
      search_df = NULL,
      plot = NULL
    ))
  }

  # --- Local helper (defect penalty) ---
  defect_fast <- function(V) {
    norm2 <- sum(V^2)
    if (norm2 <= 1e-12) return(0)
    S <- crossprod(V)
    diagS <- diag(S)
    off_f2 <- sum(S * S) - sum(diagS^2)
    off_f2 / norm2^2
  }

  # --- Default energy function if not provided ---
  if (is.null(nsa_energy)) {
    nsa_energy <- function(V) {
      Vp <- nsa_flow_retract_auto(V, w, retraction)
      if (apply_nonneg) Vp <- pmax(Vp, 0)
      0.5 * sum((Vp - X0)^2) + 0.1 * defect_fast(Vp)
    }
  }

  # --- Gradient direction ---
  grad <- Y0 - X0
  grad_norm <- sqrt(sum(grad^2)) + 1e-12
  grad_dir <- grad # / grad_norm

  # --- Energy along line ---
  line_obj <- function(alpha) {
    Y_trial <- Y0 + alpha * grad_dir
    nsa_energy(Y_trial)
  }
  lowr=search_range[1]
  upr=search_range[2]
  # --- Method 1: Brent (original) ---
  if (method == "brent") {
    opt_result <- optim(par = 0, fn = function(log_alpha) line_obj(10^log_alpha),
                        method = "Brent", lower = lowr, upper = upr)
    best_log_alpha <- opt_result$par
    best_alpha <- 10^best_log_alpha
  }

  # --- Method 2: Grid search ---
  if (method == "grid") {
    trial_logs <- seq(lowr, upr, length.out = 100)
    trial_alphas <- 10^trial_logs
    energies <- sapply(trial_alphas, line_obj)
    best_idx <- which.min(energies)
    best_alpha <- trial_alphas[best_idx]
  }

  # --- Method 3: Armijo backtracking line search ---
  if (method == "armijo") {
    alpha <- 1.0
    c <- 1e-4
    rho <- 0.5
    E0 <- line_obj(0)
    grad_dot <- sum(grad_dir * grad)
    while (line_obj(alpha) > E0 - c * alpha * grad_dot) {
      alpha <- alpha * rho
      if (alpha < 1e-5) break
    }
    best_alpha <- alpha
  }

  # --- Method 4: Golden-section search ---
  if (method == "golden") {
    phi <- (1 + sqrt(5)) / 2
    a <- 10^lowr; b <- 10^upr
    tol <- 1e-5
    while ((b - a) > tol) {
      c1 <- b - (b - a) / phi
      c2 <- a + (b - a) / phi
      if (line_obj(c1) < line_obj(c2)) b <- c2 else a <- c1
    }
    best_alpha <- (a + b) / 2
  }

  # --- Method 5: Adaptive curvature-based rule ---
  if (method == "adaptive") {
    f0 <- line_obj(0)
    f1 <- line_obj(1e-2)
    curvature <- abs(f1 - f0) / 1e-2
    best_alpha <- 1 / (1 + curvature)
    best_alpha <- max(min(best_alpha, 10), 1e-5)
  }

  # --- Diagnostics ---
  if (verbose)
    cat(sprintf("[%s] Estimated learning rate: %.3e\n", method, best_alpha))

  # --- Optional diagnostic curve ---
  lr_plot <- NULL
  search_df <- NULL
  if (plot && method %in% c("brent", "grid", "golden", "adaptive")) {
    trial_logs <- seq(-5, 2, length.out = 60)
    trial_alphas <- 10^trial_logs
    trial_energies <- sapply(trial_alphas, line_obj)
    search_df <- data.frame(log10_lr = trial_logs, learning_rate = trial_alphas,
                            energy = trial_energies)
    lr_plot <- ggplot2::ggplot(search_df, ggplot2::aes(x = learning_rate, y = energy)) +
      ggplot2::geom_line(color = "#0072B2", size = 1.2) +
      ggplot2::geom_vline(xintercept = best_alpha, color = "red", linetype = "dashed") +
      ggplot2::scale_x_log10() +
      ggplot2::labs(title = paste("Learning Rate Estimation (", method, ")", sep = ""),
                    subtitle = sprintf("Estimated alpha = %.3e", best_alpha),
                    x = "Learning Rate (alpha, log scale)", y = "Energy") +
      ggplot2::theme_minimal(base_size = 14)
  }

  list(
    estimated_learning_rate = best_alpha,
    search_df = search_df,
    plot = lr_plot
  )
}


#' Retraction onto the Stiefel Manifold (soft-polar multiplicative, with adaptive inv-sqrt)
#'
#' @param Y_cand Candidate matrix.
#' @param w_retract Weight for soft retraction.
#' @param retraction_type Type of retraction ('polar', 'soft_polar', or 'none').
#' @param eps_rf Small numeric value for stability.
#' @param inv_method Method for inverse square root.
#' @param ns_iter Number of iterations for Newton-Schulz.
#' @param eig_thresh Threshold for eigen-decomposition.
#' @param diag_thresh Threshold for diagonal check.
#' @param verbose Verbose output.
#' @export
nsa_flow_retract_auto <- function(Y_cand, w_retract = 1.0, retraction_type = c("polar", "soft_polar", "none"),
                             eps_rf = 1e-8, inv_method = "diag", ns_iter = 1L,
                             eig_thresh = 128L, diag_thresh = 8192L, verbose = FALSE) {
  retraction_type <- match.arg(retraction_type)

  # trivial
  if (is.null(retraction_type) || retraction_type == "none") return(Y_cand)
  if (!is.matrix(Y_cand)) stop("Y_cand must be a matrix")

  p <- nrow(Y_cand); k <- ncol(Y_cand)
  normY <- sqrt(sum(Y_cand^2))

  # Prepare Gram matrix (k x k)
  # We add eps_rf to the diagonal inside the inv_sqrt routine for stability

  # --- polar exact (full) ---
  if (retraction_type == "polar") {
    # economy SVD for efficiency on wide/tall shapes
    s <- svd(Y_cand, nu = min(p, k), nv = min(p, k))
    Ytilde <- s$u %*% t(s$v)
    # enforce exact orthonormal columns (polar)
    if (!is.null(normY) && normY > 1e-12) {
      # optional: preserve Frobenius norm of input as historically done
      cur_norm <- sqrt(sum(Ytilde^2))
      if (cur_norm > 0) Ytilde <- Ytilde / cur_norm * normY
    }
    return(Ytilde)
  }

  # --- soft_polar (multiplicative) ---
  if (retraction_type == "soft_polar") {
    # We use the multiplicative soft-polar operator:
    #   Ytilde = Y_cand %*% ( (1-w) I + w * (YtY + eps_rf I)^{-1/2} )
    # Choose an efficient way to compute the inverse sqrt of the small k x k Gram matrix.
    # For moderate k we use eigendecomposition; for larger k we use Newton-Schulz or diag fallback.
    # If k is extremely large relative to p, computing T on k x k may be worse than SVD on (p x p) route.
    use_additive_svd_fallback <- FALSE

    # Heuristic: if k is very large compared to p and k is huge, prefer SVD-based soft additive fallback
    if (k > diag_thresh && p < k) {
      use_additive_svd_fallback <- TRUE
    }
    use_additive_svd_fallback <- TRUE
    if (!use_additive_svd_fallback) {
      # compute T = (YtY + eps_rf I)^{-1/2} using adaptive routine
      YtY <- crossprod(Y_cand)
      T <- inv_sqrt_sym_adaptive(YtY, epsilon = eps_rf, method = inv_method,
                                 ns_iter = ns_iter, eig_thresh = eig_thresh, verbose = verbose)
      # multiplicative soft polar
      T_w <- (1 - w_retract) * diag(k) + w_retract * T
      Ytilde <- Y_cand %*% T_w

      # Optional: preserve Frobenius norm (matches previous code / practical desire)
      if (!is.null(normY) && normY > 1e-12) {
        cur_norm <- sqrt(sum(Ytilde^2))
        if (cur_norm > 0) Ytilde <- Ytilde / cur_norm * normY
      }

      # Ensure numeric symmetry / cleanup
      return(as.matrix(Ytilde))
    } else {
      # Fallback: for extremely wide problems compute economy SVD and perform additive soft-SVD blend
      # (this avoids expensive k x k inv-sqrt when k >> p)
      s <- svd(Y_cand, nu = min(p, k), nv = min(p, k))
      Q <- s$u %*% t(s$v)
      Ytilde <- (1 - w_retract) * Y_cand + w_retract * Q
      if (!is.null(normY) && normY > 1e-12) {
        cur_norm <- sqrt(sum(Ytilde^2))
        if (cur_norm > 0) Ytilde <- Ytilde / cur_norm * normY
      }
      return(as.matrix(Ytilde))
    }
  }

  stop("unsupported retraction_type in nsa_flow_retract_auto()")
}



#' @title NSA-Flow Optimization
#'
#' @description
#' Performs optimization to balance fidelity to a target matrix and orthogonality
#' of the solution matrix using a weighted objective function. The function supports multiple retraction methods and includes robust convergence checks.  These constraints provide global control over column-wise sparseness by projecting the matrix onto the approximate Stiefel manifold.
#'
#' @param Y0 Numeric matrix of size \code{p x k}, the initial guess for the solution.
#' @param X0 Numeric matrix of size \code{p x k}, the target matrix for fidelity.
#'   If \code{NULL}, initialized as \code{pmax(Y0, 0)} with a small perturbation added to \code{Y0}.
#' @param w Numeric scalar in \code{[0,1]}, weighting the trade-off between fidelity
#'   (1 - w) and orthogonality (w). Default is 0.5.
#' @param retraction Character string specifying the retraction method to enforce
#'   orthogonality constraints.
#' @param max_iter Integer, maximum number of iterations. Default is 100.
#' @param tol Numeric, tolerance for convergence based on relative gradient norm
#'   and energy stability. Default is 1e-6.
#' @param verbose Logical, if \code{TRUE}, prints iteration details. Default is \code{FALSE}.
#' @param seed Integer, random seed for reproducibility. If \code{NULL}, no seed is set.
#'   Default is 42.
#' @param apply_nonneg Logical, if \code{TRUE}, enforces non-negativity on the solution
#'   after retraction. Default is \code{TRUE}.
#' @param optimizer Character string, optimization algorithm to use. The "fast" option 
#'   will select the best option based on whether simplified = TRUE or FALSE. 
#'   otherwise, pass the names of optimizers supported by \code{create_optimizer()} 
#'   as seen in \code{list_simlr_optimizers()}. Default is "fast".
#' @param initial_learning_rate Numeric, initial learning rate for the optimizer.
#'   Default is 1e-3 for non-neg and 1 for unconstrained.  Otherwise, you can use \code{estimate_learning_rate_nsa()} to find a robust value.
#'.  pass a string one of c("brent", "grid", "armijo", "golden", "adaptive") to engage this method.  
#' @param record_every Integer, frequency of recording iteration metrics.
#'   Default is 1 (record every iteration).
#' @param window_size Integer, size of the window for energy stability convergence check.
#'   Default is 5.
#' @param c1_armijo Numeric, Armijo condition constant for line search.
#' @param simplified Logical, if \code{TRUE}, uses the simplified objective
#'   \deqn{\min_U (1 - w) \frac{1}{2} ||U - Z||_F^2 + w \frac{1}{2} ||U^\top U - I_k||_F^2}.
#'   If \code{FALSE}, uses the invariant defect objective. Default is \code{FALSE}.
#' @param plot Logical, if \code{TRUE}, generates a ggplot of fidelity and orthogonality
#'   traces with dual axes. Default is \code{FALSE}.
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{Y}: Numeric matrix, the best solution found (lowest total energy).
#'     \item \code{traces}: Data frame with columns \code{iter}, \code{time},
#'           \code{fidelity}, \code{orthogonality}, and \code{total_energy}
#'           for recorded iterations.
#'     \item \code{final_iter}: Integer, number of iterations performed.
#'     \item \code{plot}: ggplot object of the optimization trace
#'           (if \code{plot = TRUE}), otherwise \code{NULL}.
#'     \item \code{best_total_energy}: Numeric, the lowest total energy achieved.
#'   }
#'
#' @details
#' The function minimizes a weighted objective combining fidelity to \code{X0} and
#' orthogonality of \code{Y}, defined as:
#' \deqn{E(Y) = (1 - w) * ||Y - X0||_F^2 / (2 * p * k) + w * defect(Y)}
#' where \code{defect(Y)} measures orthogonality deviation.
#'
#' The optimization uses a Riemannian gradient descent approach with optional
#' retraction to enforce orthogonality constraints. Convergence is checked via
#' relative gradient norm and energy stability over a window of iterations.
#'
#' @examples
#' set.seed(123)
#' Y0 <- matrix(runif(20), 5, 4)
#' X0 <- matrix(runif(20), 5, 4)
#' # The original function relies on helper functions not shown here, such as:
#' # create_optimizer, step, inv_sqrt_sym, symm, and invariant_orthogonality_defect.
#' # The following example is conceptual:
#' # result <- nsa_flow(Y0, X0, w = 0.0, max_iter = 10, verbose = TRUE, plot = TRUE)
#' # print(result$plot)
#' # print(result$traces)
#'
#' @import ggplot2
#' @import reshape2
#' @export
nsa_flow <- function(
  Y0, X0 = NULL, w = 0.5,
  retraction = c(  "soft_polar", "polar",   "none" ),
  max_iter = 500, tol = 1e-5, verbose = FALSE, seed = 42,
  apply_nonneg = TRUE, optimizer = "fast",
  initial_learning_rate = 'brent',
  record_every = 1, window_size = 5, c1_armijo=1e-6,
  simplified = FALSE,
  plot = FALSE
) {
  # --- Input validation ---
  stopifnot(is.matrix(Y0), is.numeric(Y0))
  stopifnot(is.null(X0) || (is.matrix(X0) && is.numeric(X0)))
  stopifnot(is.numeric(w), w >= 0, w <= 1)
  if ( is.na( max_iter ) ) max_iter = 1
  stopifnot(is.integer(max_iter) || max_iter == as.integer(max_iter), max_iter > 0)
  stopifnot(is.numeric(tol), tol > 0)
  stopifnot(is.logical(verbose))
  stopifnot(is.null(seed) || is.numeric(seed))
  stopifnot(is.logical(apply_nonneg))
  stopifnot(is.character(optimizer))
  stopifnot(is.integer(record_every) || record_every == as.integer(record_every), record_every > 0)
  stopifnot(is.integer(window_size) || window_size == as.integer(window_size), window_size > 0)
  stopifnot(is.logical(plot))
  stopifnot(!all(Y0==0))
  # Compute slope of recent energies via simple linear regression
  compute_energy_slope <- function(energies) {
    n <- length(energies)
    if (n < 2) return(Inf)
    t <- seq_len(n)
    fit <- stats::lm(energies ~ t)
    as.numeric(coef(fit)[2])  # slope
  }
  if ( optimizer == "fast" ) {
    if ( simplified ) {
      optimizer <- "lars"
    } else {
      optimizer <- "lars"
    }
    if ( verbose ) cat("Selected optimizer:", optimizer, "\n")
  }

  # --- Fast helper functions ---
  # Fast defect (used in energy, tracking, and d0)
  defect_fast <- function(V) {
    norm2 <- sum(V^2)
    if (norm2 <= 1e-12) return(0)
    S <- crossprod(V)
    diagS <- diag(S)
    off_f2 <- sum(S * S) - sum(diagS^2)  # ||S - diag(diag(S))||_F^2, no temp matrices
    off_f2 / norm2^2
  }

  # Fast ortho terms (used in gradients; optional c_orth scaling)
  compute_ortho_terms <- function(Y, c_orth = 1, simplified = FALSE ) {
    norm2 <- sum(Y^2)
    if (norm2 <= 1e-12 || c_orth <= 0) {
      return(list(grad_orth = matrix(0, nrow(Y), ncol(Y)), defect = 0, norm2 = norm2))
    }
    S <- crossprod(Y)  # Once!
    diagS <- diag(S)
    off_f2 <- sum(S * S) - sum(diagS^2)
    defect <- off_f2 / norm2^2
    # term1 = Y %*% (S - diag(diagS)) / norm2^2, without creating (S - diag(diagS))
    Y_S <- Y %*% S
    Y_diag_scale <- sweep(Y, 2, diagS, "*")  # Columns of Y scaled by diagS
    term1 <- (Y_S - Y_diag_scale) / norm2^2
    term2 <- (defect / norm2) * Y
    if ( simplified ) {
      grad_orth <- - c_orth * 2 * Y %*% (S - diag(ncol(Y)))
      } else {
      grad_orth <- c_orth * (term1 - term2)
      }
    list(grad_orth = grad_orth, defect = defect, norm2 = norm2)
  }

  # If missing, define symm explicitly (cheap O(k^2))
  symm <- function(A) (A + t(A)) / 2

  # --- Reproducibility ---
  if (!is.null(seed)) {
    set.seed(seed)
    RNGkind("Mersenne-Twister", "Inversion")
  }
  p <- nrow(Y0)
  k <- ncol(Y0)
  # --- Initialize X0 if missing ---
  if (is.null(X0)) {
    if ( apply_nonneg ) X0 <- pmax(Y0, 0) else X0 = Y0
    perturb_scale <- sqrt(sum(Y0^2)) / sqrt(length(Y0)) * 0.05
    Y0 <- Y0 + matrix(rnorm(p * k, sd = perturb_scale), p, k)
    if (verbose) cat("Added perturbation to Y0\n")
  } else {
    if ( apply_nonneg ) X0 <- pmax(X0, 0)
    if (nrow(X0) != p || ncol(X0) != k) stop("X0 must have same dimensions as Y0")
  }
#  X0=transform_matrix(X0, method = "frob")$Xs
#  Y0=transform_matrix(Y0, method = "frob")$Xs
  retraction <- match.arg(retraction)
  Y <- Y0
  # --- Compute initial scales ---
  g0 <- 0.5 * sum((Y0 - X0)^2) / (p * k)
  if (g0 < 1e-8) g0 <- 1e-8
  d0 <- defect_fast(Y0)  # Fast!
  if (d0 < 1e-8) d0 <- 1e-8
  # --- Weighting terms ---
  fid_eta <- (1 - w) / (g0 * p * k)
  c_orth <- 4 * w / d0
  fid_eta_pt5 <- (1 - 0.5) / (g0 * p * k)
  c_orth_pt5 <- 4 * 0.5 / d0
  trace <- list()
  recent_energies <- numeric(0)
  t0 <- Sys.time()
  # --- Track best solution ---
  best_Y <- Y
  best_total_energy <- Inf
  # --- Compute initial gradient for relative norm tolerance ---
  grad_fid_init <- fid_eta * (Y - X0) * (-1.0)
  ortho_init <- compute_ortho_terms(Y, c_orth, simplified=simplified)
  grad_orth_init <- ortho_init$grad_orth
  if (c_orth > 0) {
    sym_term_orth_init <- symm(crossprod(Y, grad_orth_init))  # t(Y) %*% = crossprod(Y, .)
    rgrad_orth_init <- grad_orth_init - Y %*% sym_term_orth_init
  } else {
    rgrad_orth_init <- grad_orth_init
  }
  rgrad_init <- grad_fid_init + rgrad_orth_init
  init_grad_norm <- sqrt(sum(rgrad_init^2)) + 1e-8
  # --- Adaptive learning rate ---
  lr <- initial_learning_rate
  # --- Full energy function ---
  nsa_energy <- function(Vp) {
    # --- Retraction ---
    Vp <- nsa_flow_retract_auto(Vp, w, retraction)
    # --- Optional non-negativity ---
    Vp <- if (apply_nonneg) pmax(Vp, 0) else Vp
    e <- 0.5 * fid_eta * sum((Vp - X0)^2)
    if (c_orth > 0) {
      norm2_V <- sum(Vp^2)
      if (norm2_V > 0) {
        defect <- defect_fast(Vp)  # Fast!
        e <- e + 0.25 * c_orth * defect
      }
    }
    e
  }

  nsa_energy_pt5 <- function(Vp) {
    # --- Retraction ---
    Vp <- nsa_flow_retract_auto(Vp, 0.5, retraction)
    # --- Optional non-negativity ---
    Vp <- if (apply_nonneg) pmax(Vp, 0) else Vp
    e <- 0.5 * fid_eta_pt5 * sum((Vp - X0)^2)
    if (c_orth_pt5 > 0) {
      norm2_V <- sum(Vp^2)
      if (norm2_V > 0) {
        defect <- defect_fast(Vp)  # Fast!
        e <- e + 0.25 * c_orth_pt5 * defect
        }
      }
    e
  }

  # --- Optimizer initialization ---
    if (is.null(initial_learning_rate) || is.na(initial_learning_rate)) {
        initial_learning_rate <- "brent"
    }
    if (is.character(initial_learning_rate)) {
        if (verbose) 
            cat("Estimating robust initial learning rate using optim()...\n")
        lr_res <- estimate_learning_rate_nsa(Y0, X0, w = w, 
            retraction = retraction, nsa_energy = nsa_energy_pt5, 
            apply_nonneg = apply_nonneg, method = initial_learning_rate, 
            verbose = verbose, plot = FALSE)
        initial_learning_rate <- lr_res$estimated_learning_rate
        lr <- initial_learning_rate
        if (verbose) 
            cat(paste("Estimated initial learning rate:", initial_learning_rate, "\n"))
    }
  opt <- create_optimizer( optimizer, vmats = list(Y), learning_rate = lr )
  for (it in seq_len(max_iter)) {
    # --- Fidelity descent dir ---
    grad_fid <- fid_eta * (Y - X0) * (-1.0)

    # --- Orthogonality descent dir ---
    ortho_terms <- compute_ortho_terms(Y, c_orth, simplified=simplified)
    grad_orth <- ortho_terms$grad_orth

    # --- Riemannian projection ---
    if (c_orth > 0) {
      sym_term_orth <- symm(crossprod(Y, grad_orth))
      rgrad_orth <- grad_orth - Y %*% sym_term_orth
    } else {
      rgrad_orth <- grad_orth
    }

    # --- Combined descent dir ---
    rgrad <- grad_fid + rgrad_orth

    # --- Check for NaN/Inf in descent dir ---
    if (any(is.na(rgrad)) || any(is.infinite(rgrad))) {
      if (verbose) cat("NaN or Inf detected in descent dir. Stopping optimization.\n")
      break
    }

    # --- Optimizer step ---
    step_result <- step(
      opt, i = 1, V_current = Y,
      descent_gradient = rgrad,
      learning_rate = lr,
      full_energy_function = nsa_energy,
      myit = it
    )
    Y <- step_result$updated_V
    opt <- step_result$optimizer

    # --- Backtracking line search ---
    current_energy <- nsa_energy(Y)
    alpha <- 1
    count <- 0
    max_bt_count = 20
    retracted_in_loop <- FALSE  # Track whether retraction already applied

    while (current_energy > best_total_energy && count < max_bt_count) {
      alpha <- alpha * 0.5
      Y <- best_Y + alpha * (step_result$updated_V - best_Y)
      current_energy <- nsa_energy(Y)
      if (current_energy < best_total_energy) {
        Y <- nsa_flow_retract_auto(Y, w, retraction)
        retracted_in_loop <- TRUE
        break
      }
      count <- count + 1
    }

    # --- Handle backtracking outcomes ---
    if (count == max_bt_count) {
      # Backtracking failed -> revert
      Y <- best_Y
      retracted_in_loop <- TRUE  # previous Y was already retracted
      if (verbose) cat("Backtracking failed to reduce energy; reverting to previous Y.\n")
    }

    # --- Adaptive learning rate adjustment ---
    if (count > 2) {
      lr <- lr * 0.95
#      if (verbose) cat(sprintf("Reducing learning rate to %.6e due to backtracking\n", lr))
    } else if (count == 0 && it %% 5 == 0) {
      lr <- lr * 1.01
#      if (verbose) cat(sprintf("Increasing learning rate to %.6e\n", lr))
    }

    # --- Retraction safeguard ---
    if (!retracted_in_loop) {
      Y <- nsa_flow_retract_auto(Y, w, retraction)
    }

    # --- Optional nonnegativity constraint ---
    Y <- if (apply_nonneg) pmax(Y, 0) else Y

    # --- Check for NaN/Inf in Y ---
    if (any(is.na(Y)) || any(is.infinite(Y))) {
      if (verbose) cat("NaN or Inf detected in Y. Stopping optimization.\n")
      break
    }

    # --- Energy and orthogonality tracking (reuse from ortho_terms) ---
    fidelity <- 0.5 * fid_eta * sum((Y - X0)^2)  # Direct, fast
    orthogonality <- defect_fast(Y)  # Exact post-retraction
    total_energy <- fidelity + 0.25 * c_orth * orthogonality
    dt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

    # --- Update best solution ---
    if (total_energy < best_total_energy) {
      best_total_energy <- total_energy
      best_Y <- Y
      best_Y_iteration <- it
    }

    # --- Append to recent energies ---
    recent_energies <- c(recent_energies, total_energy)
    if (length(recent_energies) > window_size) {
      recent_energies <- recent_energies[-1]
    }

    if (it %% record_every == 0) {
      trace[[length(trace) + 1]] <- list(
        iter = it, time = dt,
        fidelity = fidelity,
        orthogonality = orthogonality,
        total_energy = total_energy
      )
    }

    # --- Verbose output ---
    if (verbose) {
      cat(sprintf("[Iter %3d] Total Energy: %.6e | Fidelity: %.6e | Orthogonality: %.6e | Time: %.2fs\n",
                  it, total_energy, fidelity, orthogonality, dt))
    }

    # --- Convergence checks ---
    if (it > 1) {

      # (1) Relative gradient-norm convergence
      grad_norm <- sqrt(sum(rgrad^2))
      rel_grad_norm <- grad_norm / init_grad_norm
      if (rel_grad_norm < tol) {
        if (verbose)
          cat(sprintf("Converged at iteration %d (relative gradient norm < %.2e)\n", it, tol))
        break
      }

      # (2) Energy stability over window (slope-based)
      if (length(recent_energies) == window_size) {

        slope <- compute_energy_slope(recent_energies)
        avg_e <- mean(recent_energies)
        rel_slope <- abs(slope) / (abs(avg_e) + 1e-12)

        if (rel_slope < tol) {
          if (verbose)
            cat(sprintf("Converged at iteration %d (energy slope < %.2e)\n", it, tol))
          break
        }
      }
    }
  }
  trace_df <- do.call(rbind, lapply(trace, as.data.frame))
  # --- Optional plot with dual axes ---
  energy_plot <- NULL
  if (plot && !is.null(trace_df) && nrow(trace_df) > 0) {
    max_fid <- max(trace_df$fidelity, na.rm = TRUE)
    max_orth <- max(trace_df$orthogonality, na.rm = TRUE)
    ratio <- if (max_orth > 0) max_fid / max_orth else 1
    energy_plot <- ggplot2::ggplot(trace_df, ggplot2::aes(x = iter)) +
      ggplot2::geom_line(ggplot2::aes(y = fidelity, color = "Fidelity"), size = 1.2) +
      ggplot2::geom_point(ggplot2::aes(y = fidelity, color = "Fidelity"), size = 1.5, alpha = 0.7) +
      ggplot2::geom_line(ggplot2::aes(y = orthogonality * ratio, color = "Orthogonality"), size = 1.2) +
      ggplot2::geom_point(ggplot2::aes(y = orthogonality * ratio, color = "Orthogonality"), size = 1.5, alpha = 0.7) +
      ggplot2::scale_y_continuous(name = "Fidelity Energy",
                                  sec.axis = ggplot2::sec_axis(~ . / ratio, name = "Orthogonality Defect")) +
      ggplot2::scale_color_manual(values = c("Fidelity" = "#1f78b4", "Orthogonality" = "#33a02c")) +
      ggplot2::labs(title = paste("NSA-Flow Optimization Trace: ", retraction),
                    subtitle = "Fidelity and Orthogonality Terms (Dual Scales)",
                    x = "Iteration", color = "Term") +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
                     plot.subtitle = ggplot2::element_text(hjust = 0.5),
                     legend.position = "top",
                     panel.grid.major = ggplot2::element_line(color = "gray80"),
                     panel.grid.minor = ggplot2::element_line(color = "gray90"),
                     axis.title.y.left = ggplot2::element_text(color = "#1f78b4"),
                     axis.text.y.left = ggplot2::element_text(color = "#1f78b4"),
                     axis.title.y.right = ggplot2::element_text(color = "#33a02c"),
                     axis.text.y.right = ggplot2::element_text(color = "#33a02c"))
  }
  rownames(best_Y) <- rownames(Y0)
  colnames(best_Y) <- colnames(Y0)
  list(
    Y = best_Y,
    traces = trace_df,
    final_iter = if(is.null(trace_df)) 0 else nrow(trace_df),
    plot = energy_plot,
    best_total_energy = best_total_energy, 
    best_Y_iteration = best_Y_iteration,
    target = X0
  )
}


#' NSA-Flow Algorithm Flowchart
#'
#' Generates a DiagrammeR flowchart summarizing the full NSA-Flow optimization pipeline, 
#' including initialization, gradient computation, Riemannian projection, descent, 
#' retraction, non-negativity, and convergence checks.
#'
#' @param node_fill_color Color for node fill (default: "lightblue").
#' @param node_font_color Color for node text (default: "black").
#' @param edge_color Color for edges (default: "black").
#' @param font_name Font for node labels (default: "Helvetica").
#' @param node_shape Node shape (default: "rectangle").
#' @param graph_rankdir Graph layout direction (default: "TB" = top to bottom).
#' @param fontsize Font size (default: 10).
#'
#' @return A DiagrammeR graph object.
#' @export
nsa_flow_flowchart <- function(
  node_fill_color = "lightblue",
  node_font_color = "black",
  edge_color = "black",
  font_name = "Helvetica",
  node_shape = "rectangle",
  graph_rankdir = "TB",
  fontsize = 10
) {
  if (!requireNamespace("DiagrammeR", quietly = TRUE)) stop("Install DiagrammeR")
  
  # Define the graph specification using grViz
  graph_spec <- paste0("
digraph nsa_flow_algorithm {
  graph [rankdir = ", graph_rankdir, ", fontsize = ", fontsize, ", splines = ortho]
  node [shape = ", node_shape, ", style = filled, fillcolor = ", node_fill_color, ", 
        fontcolor = ", node_font_color, ", fontname = ", font_name, ", fontsize = ", fontsize, "]
  edge [color = ", edge_color, ", fontsize = ", fontsize, "]

  # Main nodes
  A [label = 'Initialize Y0\\n(Random, Orthogonal, or SVD-based)']
  B [label = 'Set Parameters\\n(w, retraction, optimizer)']
  C [label = 'Compute Euclidean Gradient\\ngradF(Y) = (1-w)gradg(Y) + wgrad_srcn(Y)']
  D [label = 'Project to Tangent Space\\nRiemannian grad_R F']
  E [label = 'Descent Step\\nZ = Y - alpha * grad_R F']
  F [label = 'Choose Retraction\\n(polar, QR, soft, soft-polar, soft-QR, none)']
  G [label = 'Apply Retraction\\nMap Z -> M (Stiefel manifold)']
  H [label = 'Proximal Projection\\nP+(Y) = max(Y, 0)']
  I [label = 'Check Convergence\\n(DeltaEnergy < tol or ||grad|| < tol)']
  J [label = 'Update Y and Record\\n(iter, energy, orth, neg)']
  K [label = 'Output Final Y*\\nand Diagnostic Traces']

  # Edges
  A -> B [label = 'Setup']
  B -> C [label = 'Start Iteration']
  C -> D [label = 'Project']
  D -> E [label = 'Descend']
  E -> F [label = 'Select Retraction']
  F -> G [label = 'Retract']
  G -> H [label = 'Project Nonneg']
  H -> I [label = 'Evaluate']
  I -> J [label = 'Not Converged', style = 'dashed']
  J -> C [label = 'Next Iteration']
  I -> K [label = 'Converged']

  # Subgraph for retraction options
  subgraph cluster_retraction {
    style = dashed
    color = gray
    label = 'Retraction Options'
    F1 [label = 'Polar\\nY~ = Y (YTY)^{-1/2}', fillcolor = lightyellow]
    F2 [label = 'QR\\nY~ = Q sign(diag(R))', fillcolor = lightyellow]
    F3 [label = 'Soft\\nY~ = (1-lambda)Y + lambdaQ', fillcolor = lightyellow]
    F4 [label = 'Soft-Polar\\nY~ = Y[(1-lambda)I + lambda(YTY)^{-1/2}]', fillcolor = lightyellow]
    F5 [label = 'None\\n(No retraction)', fillcolor = lightyellow]
    F -> F1 [style = invis]
    F -> F2 [style = invis]
    F -> F3 [style = invis]
    F -> F4 [style = invis]
    F -> F5 [style = invis]
  }
}
")
  
  # Create and return the DiagrammeR graph
  grViz(graph_spec)
}




#' Generate NSA-Flow FA Flowchart Diagram
#'
#' This function creates a flowchart diagram representing the NSA-Flow FA process
#' using the DiagrammeR package. The diagram illustrates the steps involved in the algorithm, including
#' data input, regularization, iteration, convergence checks, and output.
#'
#' @details
#' The diagram is generated using Graphviz syntax via \code{DiagrammeR::grViz}. It visualizes the flow
#' from input data through various processing steps to the final output. Ensure the DiagrammeR package
#' is installed to use this function.
#'
#' @return A DiagrammeR graph object representing the flowchart.
#'
#' @examples
#' \dontrun{
#' library(DiagrammeR)
#' nsa_flow_fa_diagram()
#' }
#'
#' @export
nsa_flow_fa_diagram <- function() {
  DiagrammeR::grViz("
digraph NSA_Flow_FA {
  graph [rankdir=TB, fontsize=12, nodesep=0.3, ranksep=0.4]

  # Nodes
  Data [label='Input Data (X) or R', shape=box]
  RegR [label='Regularize R', shape=box]
  InitComm [label='Initialize Communalities (h^2)', shape=box]
  BuildRmod [label='Build R_mod with h^2 on Diagonal', shape=box]
  PowerIter [label='Power Iteration: Orthonormal Basis & Loadings', shape=box]
  NSAFlow [label='NSA-Flow Regularization (w)', shape=box]
  EnergyPre [label='Compute Energy Pre-Rotation', shape=box]
  Rotate [label='Optional Rotation (varimax/promax/oblimin)', shape=box]
  EnergyPost [label='Compute Energy Post-Rotation', shape=box]
  UpdateBest [label='Update Best Loadings if Improved', shape=box]
  UpdateH2 [label='Damped Update of h^2', shape=box]
  ConvergeCheck [label='Convergence Check (Deltaenergy & Deltah^2)', shape=diamond]
  FactorScores [label='Compute Factor Scores (optional)', shape=box]
  Output [label='Output: Best Loadings, h^2, Energy Trace, etc.', shape=box]

  # Edges
  Data -> RegR -> InitComm -> BuildRmod -> PowerIter -> NSAFlow -> EnergyPre -> Rotate -> EnergyPost -> UpdateBest -> UpdateH2 -> ConvergeCheck
  ConvergeCheck -> BuildRmod [label='No']
  ConvergeCheck -> FactorScores [label='Yes']
  FactorScores -> Output
}
")
}


#' NSA-Flow Sparse PCA (stable + efficient)
#'
#' Performs sparse PCA using a proximal-gradient scheme with optional NSA-flow
#' proximal regularization. This implementation preserves the stable behavior
#' of the original `sparse_pca_imp()` while improving efficiency by:
#'  - precomputing X'X, using it for gradient/energy,
#'  - in-place trial updates to reduce allocations,
#'  - retaining Armijo backtracking and adaptive LR scheduling.
#'
#' @param X numeric matrix (n x p), rows = observations, cols = variables
#' @param k integer > 0 and <= min(n,p), number of components
#' @param lambda non-negative numeric, L1 proximal weight
#' @param alpha positive numeric, initial step size
#' @param max_iter integer, maximum iterations
#' @param proximal_type character, "basic" or "nsa_flow"
#' @param w_pca positive numeric, weight for PCA fidelity
#' @param nsa_w numeric in [0,1], weight parameter passed to nsa_flow
#' @param apply_soft_thresh_in_nns logical (passed through if used)
#' @param tol numeric, tolerance for relative parameter change convergence
#' @param retraction retraction function or identifier (passed to nsa_flow)
#' @param grad_tol numeric, gradient-norm tolerance for convergence
#' @param nsa_flow_fn optional, nsa_flow function to use (default: nsa_flow)
#' @param verbose logical, print iteration diagnostics
#'
#' @return list with components:
#'   \item{Y}{best p x k loading matrix found}
#'   \item{energy_trace}{vector of per-iteration energy/explained-variance records}
#'   \item{final_iter}{last iteration index}
#'   \item{best_energy}{best energy attained}
#'   \item{converged}{logical}
#'   \item{no_improve_count}{internal counter at exit}
#'   \item{lr_reductions}{number of LR reductions performed}
#'   \item{expl_var_ratio}{explained variance ratio for best_Y}
#'
#' @export
nsa_flow_pca <- function(X, k,
                         lambda = 0.1,
                         alpha = 0.0001,
                         max_iter = 100,
                         proximal_type = c("basic", "nsa_flow"),
                         w_pca = 1.0, nsa_w = 0.5,
                         apply_soft_thresh_in_nns = FALSE,
                         tol = 1e-6, retraction = def_ret,
                         grad_tol = 1e-4, nsa_flow_fn = nsa_flow_autograd, verbose = FALSE) {
  # --- argument checks ---
  if (!is.matrix(X) || any(!is.finite(X))) stop("X must be a finite numeric matrix")
  n <- nrow(X); p <- ncol(X)
  if (k <= 0 || k > min(n, p)) stop("k must be positive and not exceed min(n, p)")
  if (lambda < 0) stop("lambda must be non-negative")
  if (alpha <= 0) stop("alpha must be positive")
  if (w_pca <= 0) stop("w_pca must be positive")
  if (nsa_w < 0 || nsa_w > 1) stop("nsa_w must be in [0,1]")
  proximal_type <- match.arg(proximal_type)

  # Preserve previous behavior: when using nsa_flow proximal with nonzero nsa_w,
  # disable the L1 lambda to avoid double regularization (as in your original impl).
  if (nsa_w > 0 && proximal_type == "nsa_flow") {
    lambda <- 0.0
    if (verbose) cat("nsa_w > 0 and proximal_type == 'nsa_flow' -> setting lambda = 0\n")
  }

  # --- center (no scaling) ---
  Xc <- scale(X, center = TRUE, scale = FALSE)
  # precompute XtX and total_var
  XtX <- crossprod(Xc)           # p x p
  total_var <- sum(diag(XtX / n))
  if (!is.finite(total_var) || total_var <= 0) stop("Input matrix X has zero or non-finite variance")

  # --- SVD init: Y is p x k ---
  set.seed(1234)
  Y = matrix(rnorm(p * k), nrow = p, ncol = k)
#  sv <- svd(Xc, nu = 0, nv = k)
#  Y <- sv$v
#  if (ncol(Y) != k) stop("SVD initialization did not produce k columns")

  # bookkeeping
  energy_trace <- numeric(max_iter)
  best_Y <- Y
  best_energy <- Inf
  alpha_init <- alpha
  max_grad_norm <- 100.0
  bt_max <- 20
  bt_shrink <- 0.5
  armijo_c <- 1e-4

  # adaptive scheduler params
  patience <- 10
  min_delta <- 1e-8
  lr_reduction_factor <- 0.5
  max_lr_reductions <- 3
  min_iters_before_stop <- 10

  no_improve_count <- 0
  lr_reductions <- 0
  converged <- FALSE

  # Helper: Frobenius norm (if frob() not available)
  .frob <- function(A) sqrt(sum(A * A))

  # energy function using precomputed XtX (numerically stable)
  energy_of <- function(M) {
    # tr(M' XtX M) / n  == sum(diag(t(M) %*% XtX %*% M)) / n
    tr_val <- sum(diag(t(M) %*% XtX %*% M)) / n
    fid_term <- w_pca * (-0.5 * tr_val)
    prox_term <- lambda * sum(abs(M))
    fid_term + prox_term
  }

  for (iter in seq_len(max_iter)) {
    t_start <- Sys.time()
    if (proximal_type != "nsa_flow") Y <- qr.Q(qr(Y))
    # Euclidean gradient: - (XtX %*% Y) / n scaled by w_pca
    grad_p <- - (XtX %*% Y) / n   # p x k
    eu_grad <- w_pca * grad_p

    if (any(!is.finite(eu_grad))) stop("Non-finite Euclidean gradient at iteration ", iter)

    # gradient clipping
    gnorm <- .frob(eu_grad)
    if (gnorm > max_grad_norm) eu_grad <- eu_grad * (max_grad_norm / gnorm)

    # Riemannian projection (none here, we keep euclidean descent direction)
    rgrad <- eu_grad
    rgrad_norm <- .frob(rgrad)

    # Backtracking line search (Armijo)
    alpha <- alpha_init

    # In-place trial update: Z starts as copy of Y, but we replace contents (minimize allocs)
    Z <- Y
    Z[] <- Z - alpha * rgrad
    energy_old <- energy_of(Y)
    energy_new <- energy_of(Z)
    dir_deriv <- sum(eu_grad * (-rgrad))

    bt <- 0
    while ((!is.finite(energy_new) || energy_new > energy_old + armijo_c * alpha * dir_deriv) && bt < bt_max) {
      alpha <- alpha * bt_shrink
      Z[] <- Y - alpha * rgrad
      energy_new <- energy_of(Z)
      bt <- bt + 1
    }
    if (!is.finite(energy_new)) stop("Non-finite energy after backtracking at iteration ", iter)

    Y_ret <- Z

    # Proximal step
    thresh <- alpha * lambda
    if (proximal_type == "basic") {
      # keep your simlr_sparseness call (assumed defined elsewhere)
      Y_new <- simlr_sparseness(Y_ret, 'none', positivity = 'positive', sparseness_quantile = 0.8)
    } else if (proximal_type == "nsa_flow") {
      # call nsa_flow; we assume it takes arguments (Y0, X0=NULL, w=..., retraction=...)
      # use X0 = NULL to indicate proximal-only processing of Y_ret
      prox_res <- nsa_flow_fn( Y_ret, w=nsa_w )
      if (!is.list(prox_res) || is.null(prox_res$Y)) stop("nsa_flow returned unexpected result")
      Y_new <- prox_res$Y %>% apply( 2, function(x) (x - min(x)) / (max(x) - min(x)))
    } else {
      stop("unknown proximal_type")
    }
    if (any(!is.finite(Y_new))) stop("Non-finite proximal step at iteration ", iter)

    # compute energy and stats
    energy <- energy_of(Y_new)

    # lazy orthonormalize (to compute true explained variance and to keep iterates stable)
    if (k == 1) {
      Q <- Y_new / sqrt(sum(Y_new^2))
    } else {
      qr_decomp <- qr(Y_new)
      Q <- qr.Q(qr_decomp)
    }

    # explained variance ratio
    tr_val <- sum(diag(t(Q) %*% XtX %*% Q)) / n
    expl_var_ratio <- tr_val / total_var

    # record energy trace (negative explained var like original)
    energy_trace[iter] <- -expl_var_ratio

    # check for improvement
    improved <- FALSE
    if (energy < best_energy ) {
      best_energy <- energy
      best_Y <- Y_new
      improved <- TRUE
    }

    if (improved) {
      no_improve_count <- 0
    } else {
      no_improve_count <- no_improve_count + 1
    }

    # Adaptive scheduler: reduce LR when plateaued for `patience` iters
    if (no_improve_count >= patience && lr_reductions < max_lr_reductions) {
      alpha_init <- max(alpha_init * lr_reduction_factor, 1e-12)
      lr_reductions <- lr_reductions + 1
      if (verbose) {
        cat(sprintf("No improvement for %d iters -> reducing alpha_init by factor %.3f to %.3e (lr_reductions=%d)\n",
                    patience, lr_reduction_factor, alpha_init, lr_reductions))
      }
      no_improve_count <- 0  # reset after reducing LR
    }

    # Early stop if plateaued after exhausting LR reductions
    stop_due_to_plateau <- (no_improve_count >= patience && lr_reductions >= max_lr_reductions)

    if (verbose) {
      sp_lvl <- if (exists("sparsity_level")) tryCatch(sparsity_level(Y_new), error = function(e) NA) else NA
      cat(sprintf("Iter %3d | Energy: %12.6e | Sparsity: %s | ExplVar: %.4f | rgrad_norm: %.4e | bt: %2d | alpha: %.3e | t: %.2fs\n",
                  iter, energy, format(sp_lvl), expl_var_ratio, rgrad_norm, bt, alpha, as.numeric(Sys.time() - t_start, units = "secs")))
      if (!improved) cat(sprintf("  (no_improve_count=%d, lr_reductions=%d)\n", no_improve_count, lr_reductions))
    }

    # Convergence diagnostics
    if (iter > 1) {
      rel_energy_change <- abs(energy_trace[iter] - energy_trace[iter - 1]) / (abs(energy_trace[iter - 1]) + 1e-12)
      grad_ok <- (rgrad_norm < grad_tol)
      delta_Y <- .frob(Y_new - Y) / (.frob(Y) + 1e-12)
      delta_ok <- (delta_Y < tol)

      converged_condition <- (iter > min_iters_before_stop) && (rel_energy_change < tol) && (grad_ok || delta_ok)

      if (verbose) {
        cat(sprintf("  DeltaEnergy: %.2e | GradNorm: %.2e | DeltaY: %.2e | ConvergedCond: %s\n",
                    rel_energy_change, rgrad_norm, delta_Y, ifelse(converged_condition, "[OK]", "x")))
      }

      if (converged_condition) {
        converged <- TRUE
        if (verbose) cat("Convergence achieved at iteration", iter, "\n")
        break
      }
    }

    if (stop_due_to_plateau) {
      if (verbose) cat("Stopping early due to plateau (no improvement after LR reductions)\n")
      break
    }

    # set iterate for next step
    Y <- Y_new
  } # end iter loop

  energy_trace <- energy_trace[seq_len(iter)]

  # Final orthogonalization for returned best_Y
  if (k == 1) {
    Q <- best_Y / sqrt(sum(best_Y^2))
  } else {
    qr_decomp <- qr(best_Y)
    Q <- qr.Q(qr_decomp)
  }
  tr_val <- sum(diag(t(Q) %*% XtX %*% Q)) / n
  expl_var_ratio <- tr_val / total_var

  list(
    Y = best_Y,
    energy_trace = energy_trace,
    final_iter = iter,
    best_energy = best_energy,
    converged = converged,
    no_improve_count = no_improve_count,
    lr_reductions = lr_reductions,
    expl_var_ratio = expl_var_ratio
  )
}



#' NSA-Flow Sparse PCA or FA
#'
#' Optimize either a PCA objective (default) or a Factor Analysis (FA) objective
#' in a single call.  Armijo backtracking, optional NSA-flow proximal,
#' adaptive LR scheduling, and optional rotation every iteration.
#'
#' @inheritParams nsa_flow_pca
#' @param objective character, either "pca" (default) or "fa".
#'        If "fa", the function optimizes a FA-like loss: reconstructing the
#'        sample covariance R with Y Y^T + diag(psi) where psi >= 0 is set
#'        to the diagonal of R - Y Y^T (clipped to a small positive eps).
#' @param rotate character; one of "none","varimax","promax","oblimin".
#'        If not "none", the loadings are rotated every iteration (keeps shapes).
#' @param nsa_flow_args list, optional additional args forwarded to nsa_flow_fn()
#'
#' @return list with components:
#'   Y, energy_trace, final_iter, best_energy, converged,
#'   no_improve_count, lr_reductions, expl_var_ratio
#'
#' @export
nsa_flow_pca_fa <- function(
  X, k,
  lambda = 0.1,
  alpha = 0.01,
  max_iter = 100,
  proximal_type = c("basic", "nsa_flow"),
  w_pca = 1.0,
  tol = 1e-6,
  grad_tol = 1e-4, 
  nsa_flow_fn = NULL, 
  verbose = FALSE,
  objective = c("pca","fa"),
  rotate = c("none","varimax","promax","oblimin"),
  nsa_flow_args = list()
) {
  # --- arg checks & defaults -------------------------------------------------
  proximal_type <- match.arg(proximal_type)
  objective <- match.arg(objective)
  rotate <- match.arg(rotate)
  if (!is.matrix(X) || any(!is.finite(X))) stop("X must be a finite numeric matrix")
  n <- nrow(X); p <- ncol(X)
  if (k <= 0 || k > min(n, p)) stop("k must be positive and not exceed min(n, p)")
  if (lambda < 0) stop("lambda must be non-negative")
  if (alpha <= 0) stop("alpha must be positive")
  if (w_pca <= 0) stop("w_pca must be positive")
  if (is.null(nsa_flow_fn) && proximal_type == "nsa_flow") {
    # lazy require: user must have provided nsa_flow_fn (e.g., nsa_flow_autograd)
    stop("nsa_flow_fn must be provided when proximal_type == 'nsa_flow'")
  }
  # If NSA proximal used, we follow previous convention: disable L1 lambda to avoid double regularization
  if (proximal_type == "nsa_flow") {
    lambda <- 0.0
    if (verbose) message("proximal_type == 'nsa_flow' -> lambda set to 0")
  }
  # --- center X (no scaling) and precompute XtX / R ---------------------------
  Xc <- scale(X, center = TRUE, scale = FALSE)
  XtX <- crossprod(Xc) # p x p
  total_var <- sum(diag(XtX / n))
  if (!is.finite(total_var) || total_var <= 0) stop("Input X has zero or non-finite variance")
  # sample covariance/correlation matrix used for FA objective
  Rmat <- XtX / n
  # --- initialization -------------------------------------------------------
  set.seed(1234)
  Y <- matrix(rnorm(p * k, sd = 0.1), nrow = p, ncol = k)
  energy_trace <- numeric(max_iter)
  best_Y <- Y
  best_energy <- Inf
  alpha_init <- alpha
  max_grad_norm <- 100.0
  bt_max <- 20
  bt_shrink <- 0.5
  armijo_c <- 1e-4
  # adaptive scheduler
  patience <- 10
  lr_reduction_factor <- 0.5
  max_lr_reductions <- 3
  min_iters_before_stop <- 10
  no_improve_count <- 0
  lr_reductions <- 0
  converged <- FALSE
  .frob <- function(A) sqrt(sum(A * A))
  # Small numeric eps
  eps_diag <- 1e-8
  # energy_of depending on objective ------------------------------------------
  energy_of <- function(M) {
    # M is p x k (loadings)
    if (objective == "pca") {
      # use negative explained variance (consistent with your original) as "energy"
      # tr(M' XtX M)/n gives explained variance by columns in M
      tr_val <- sum(diag(t(M) %*% XtX %*% M)) / n
      fid_term <- w_pca * (-0.5 * tr_val)
#      prox_term <- lambda * sum(abs(M))
      fid_term #+ prox_term
    } else { # "fa"
      # FA objective: || R - (M M^T + diag(psi)) ||_F^2
      # choose psi = diag(R - M M^T) clipped to >= eps_diag
      recon_cross <- M %*% t(M)
      diag_err <- diag(Rmat - recon_cross)
      psi <- pmax(as.numeric(diag_err), eps_diag)
      # build recon matrix
      recon <- recon_cross + diag(psi, nrow = p, ncol = p)
      # squared Frobenius error (normalized)
      diff_mat <- Rmat - recon
      loss <- mean(diff_mat * diff_mat)
    }
  }
  # helper: explained variance ratio for reporting (PCA style)
  explained_ratio_of <- function(M) {
    # orthonormalize columns (lazy) and compute explained variance fraction
    if (k == 1) {
      Q <- M / sqrt(sum(M^2) + 1e-12)
    } else {
      qr_decomp <- qr(M)
      Q <- qr.Q(qr_decomp)
    }
    tr_val <- sum(diag(t(Q) %*% XtX %*% Q)) / n
    tr_val / total_var
  }
  # --- main optimization loop -----------------------------------------------
  for (iter in seq_len(max_iter)) {
    t_start <- Sys.time()
    # optional normalization for stability (when not using NSA proximal)
    if (proximal_type != "nsa_flow") {
      if (k == 1) {
        Y <- Y / sqrt(sum(Y^2) + 1e-12)
      } else {
        qr_decomp <- qr(Y)
        Q <- qr.Q(qr_decomp)
        # keep first k columns (qr.Q may return >k columns)
        if (ncol(Q) >= k) Y <- Q[, seq_len(k), drop = FALSE]
      }
    }
    # Euclidean gradient for PCA objective: - (XtX %*% Y) / n * w_pca
    # For FA objective, gradient is more complex; approximate by differentiating
    # the FA loss w.r.t. M: grad = -4 * (R - M M^T - diag(psi)) %*% M
    if (objective == "pca") {
      grad_p <- - (XtX %*% Y) / n
      eu_grad <- w_pca * grad_p
    } else {
      # FA gradient (exact for full Frobenius): grad = -4 * (R - recon) %*% M
      recon_cross <- Y %*% t(Y)
      diag_err <- diag(Rmat - recon_cross)
      psi <- pmax(as.numeric(diag_err), eps_diag)
      recon <- recon_cross + diag(psi, nrow = p, ncol = p)
      diff_mat <- Rmat - recon
      eu_grad <- -4 * (diff_mat %*% Y)
      # note: no separate w_pca multiplicative factor for FA branch (could be added)
    }
    if (any(!is.finite(eu_grad))) stop("Non-finite Euclidean gradient at iteration ", iter)
    # gradient clipping
    gnorm <- .frob(eu_grad)
    if (gnorm > max_grad_norm) eu_grad <- eu_grad * (max_grad_norm / gnorm)
    rgrad <- eu_grad
    rgrad_norm <- .frob(rgrad)
    # Backtracking Armijo line search (in-place)
    alpha <- alpha_init
    Z <- Y
    Z[] <- Z - alpha * rgrad
    energy_old <- energy_of(Y)
    energy_new <- energy_of(Z)
    dir_deriv <- sum(eu_grad * (-rgrad)) # directional derivative proxy
    bt <- 0
    while ((!is.finite(energy_new) || energy_new > energy_old + armijo_c * alpha * dir_deriv) && bt < bt_max) {
      alpha <- alpha * bt_shrink
      Z[] <- Y - alpha * rgrad
      energy_new <- energy_of(Z)
      bt <- bt + 1
    }
    if (!is.finite(energy_new)) stop("Non-finite energy after backtracking at iteration ", iter)
    Y_ret <- Z

    if (rotate != "none" ) {

      if (!requireNamespace("psych", quietly = TRUE)) {
        warning("psych package required for rotation; skipping rotation")
      } else {
        # call chosen rotation (varimax/promax/oblimin). psych::promax returns list with loadings
        if (rotate == "varimax") {
          rot_res <- tryCatch(stats::varimax(Y_ret, normalize = FALSE), error = function(e) NULL)
        } else if (rotate == "promax") {
          rot_res <- tryCatch(stats::promax(Y_ret), error = function(e) NULL)
        } else if (rotate == "oblimin") {
          rot_res <- tryCatch(GPArotation::oblimin(Y_ret, normalize = FALSE), error = function(e) NULL)
        } else rot_res <- NULL
        if (!is.null(rot_res) && !is.null(rot_res$loadings)) {
          # psych rotations may return a "loadings" object; coerce to matrix
          Y_ret <- as.matrix(rot_res$loadings)
          # ensure correct dims
          if (ncol(Y_ret) < k) {
            # pad with small noise columns
            need <- k - ncol(Y_ret)
            Y_ret <- cbind(Y_ret, matrix(rnorm(p * need, sd = 1e-6), nrow = p, ncol = need))
          } else if (ncol(Y_ret) > k) {
            Y_ret <- Y_ret[, seq_len(k), drop = FALSE]
          }
        }
      }
    }


    # Proximal step: either simple sparsity prox or call nsa_flow_fn
    if (proximal_type == "basic") {
      # simple soft-threshold (L1 prox). preserve positive scale if requested
      thresh <- alpha * lambda
      if (thresh > 0) {
        Y_new <- sign(Y_ret) * pmax(abs(Y_ret) - thresh, 0)
      } else {
        Y_new <- Y_ret
      }
    } else {
      # nsa_flow proximal: we call the provided function with Y_ret as Y0
      # allow forwarding extra args via nsa_flow_args list
      prox_call_args <- c(list(Y0 = Y_ret ), nsa_flow_args)
      prox_res <- tryCatch(do.call(nsa_flow_fn, prox_call_args), error = function(e) {
        stop("nsa_flow_fn failed: ", conditionMessage(e))
      })
      if (!is.list(prox_res) || is.null(prox_res$Y)) stop("nsa_flow_fn returned unexpected result")
      Y_new <- prox_res$Y
      # optional scaling of columns into [0,1] if original code did that
      # but preserve sign/structure; keep as-is to avoid destroying FA structure
    }
    if (any(!is.finite(Y_new))) stop("Non-finite proximal step at iteration ", iter)
    # optional rotation every iteration
    # compute energy and stats
    energy <- energy_of(Y_new)
    expl_var_ratio <- explained_ratio_of(Y_new)
    # record energy trace (PCA branch: negative explained variance; FA branch: FA loss)
    energy_trace[iter] <- energy
    # check improvement and bookkeeping
    improved <- FALSE
    if (energy < best_energy) {
      best_energy <- energy
      best_Y <- Y_new
      improved <- TRUE
    } else { # reset
#      energy = best_energy
#      Y_new = best_Y
    }
    if (improved) {
      no_improve_count <- 0
    } else {
      no_improve_count <- no_improve_count + 1
    }
    # Adaptive scheduler: reduce LR when plateaued
    if (no_improve_count >= patience && lr_reductions < max_lr_reductions) {
      alpha_init <- max(alpha_init * lr_reduction_factor, 1e-12)
      lr_reductions <- lr_reductions + 1
      if (verbose) message(sprintf("No improvement for %d iters -> alpha_init -> %.3e (lr_reductions=%d)",
                                   patience, alpha_init, lr_reductions))
      no_improve_count <- 0
    }
    stop_due_to_plateau <- (no_improve_count >= patience && lr_reductions >= max_lr_reductions)
    if (verbose) {
      cat(sprintf("Iter %3d | Energy: %12.6e | ExplVar: %.4f | GradNorm: %.4e | bt: %2d | alpha: %.3e | t: %.2fs\n",
                  iter, energy, expl_var_ratio, rgrad_norm, bt, alpha, as.numeric(difftime(Sys.time(), t_start, units = "secs"))))
      if (!improved) cat(sprintf(" (no_improve_count=%d, lr_reductions=%d)\n", no_improve_count, lr_reductions))
    }
    # convergence checks
    if (iter > 1) {
      rel_energy_change <- abs(energy_trace[iter] - energy_trace[iter - 1]) / (abs(energy_trace[iter - 1]) + 1e-12)
      grad_ok <- (rgrad_norm < grad_tol)
      delta_Y <- .frob(Y_new - Y) / (.frob(Y) + 1e-12)
      delta_ok <- (delta_Y < tol)
      converged_condition <- (iter > min_iters_before_stop) && (rel_energy_change < tol) && (grad_ok || delta_ok)
      if (verbose) {
        cat(sprintf(" DeltaEnergy: %.2e | DeltaY: %.2e | ConvergedCond: %s\n",
                    rel_energy_change, delta_Y, ifelse(converged_condition, "[OK]", "x")))
      }
      if (converged_condition) {
        converged <- TRUE
        if (verbose) message("Convergence achieved at iteration ", iter)
        break
      }
    }
    if (stop_due_to_plateau) {
      if (verbose) message("Stopping early due to plateau")
      break
    }
    # next iterate
    Y <- Y_new
  } # end iter
  energy_trace <- energy_trace[seq_len(iter)]
  # Final orthonormalization for reported best_Y (for explained variance calc)
  if (k == 1) {
    Q <- best_Y / sqrt(sum(best_Y^2) + 1e-12)
  } else {
    qr_decomp <- qr(best_Y)
    Q <- qr.Q(qr_decomp)
    if (ncol(Q) >= k) Q <- Q[, seq_len(k), drop = FALSE]
  }
  tr_val <- sum(diag(t(Q) %*% XtX %*% Q)) / n
  expl_var_ratio <- tr_val / total_var
  # --- construct "loadings" with rotation and names ---------------------
  loadings <- best_Y
  if (rotate != "none" && requireNamespace("psych", quietly = TRUE)) {
    rot_fun <- switch(rotate,
                      varimax = stats::varimax,
                      promax = stats::promax,
                      oblimin = GPArotation::oblimin,
                      NULL)
    if (!is.null(rot_fun)) {
      rot_res <- tryCatch(rot_fun(loadings), error = function(e) NULL)
      if (!is.null(rot_res$loadings)) loadings <- as.matrix(rot_res$loadings)
    }
  }
  if (!is.null(colnames(X))) rownames(loadings) <- colnames(X)
  colnames(loadings) <- paste0("PA", seq_len(k))
  # --- compute communalities --------------------------------------------
  posthoc <- posthoc_fa_summary(X, loadings = loadings, method = "NSA")
  list(
    loadings = loadings,
    scores = posthoc$scores,
    communalities = posthoc$communalities,
    uniqueness = posthoc$uniqueness,
    variance_per_factor = posthoc$variance_per_factor,
    variance_explained = posthoc$variance_explained,
    energy_trace = energy_trace,
    final_iter = iter,
    best_energy = best_energy,
    converged = converged,
    no_improve_count = no_improve_count,
    lr_reductions = lr_reductions,
    expl_var_ratio = expl_var_ratio
  )
}



#' Perform Proximal Gradient Optimization for PCA or Factor Analysis
#'
#' This function implements a proximal gradient descent algorithm for either
#' Principal Component Analysis (PCA) or Factor Analysis (FA), with support for
#' sparsity via L1 regularization or custom proximal operators (e.g., NSA flow).
#' For PCA, it maximizes explained variance; for FA, it minimizes the least-squares
#' reconstruction error of the covariance matrix using an alternating optimization
#' over loadings and uniquenesses. Initialization uses SVD, and optional rotation
#' is applied at the end.
#'
#' @param X Numeric matrix of data (n rows observations, p columns variables).
#' @param k Integer number of components/factors (1 <= k <= min(n, p)).
#' @param lambda Non-negative regularization parameter for L1 sparsity (default 0.1).
#' @param alpha Positive initial step size for gradient descent (default 0.01).
#' @param max_iter Maximum number of iterations (default 100).
#' @param proximal_type Type of proximal operator: "basic" for soft-thresholding or "nsa_flow" for custom (default "basic").
#' @param w_pca Positive weight for PCA objective (default 1.0).
#' @param tol Tolerance for convergence based on relative change (default 1e-6).
#' @param grad_tol Tolerance for gradient norm (default 1e-4).
#' @param nsa_flow_fn Function for NSA proximal operator (required if proximal_type = "nsa_flow").
#' @param verbose Logical; if TRUE, print iteration progress (default FALSE).
#' @param objective Objective: "pca" or "fa" (default "pca").
#' @param rotate Rotation method: "none", "varimax", "promax", or "oblimin" (default "none").
#' @param nsa_flow_args List of additional arguments for nsa_flow_fn (default list()).
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{Y}: Optimized loadings matrix (unrotated).
#'   \item \code{loadings}: Final loadings (possibly rotated).
#'   \item \code{communalities}: Vector of communalities for each variable.
#'   \item \code{energy_trace}: Vector of energy values per iteration.
#'   \item \code{final_iter}: Number of iterations performed.
#'   \item \code{best_energy}: Best (lowest) energy achieved.
#'   \item \code{converged}: Logical indicating convergence.
#'   \item \code{no_improve_count}: Count of iterations without improvement.
#'   \item \code{lr_reductions}: Number of learning rate reductions.
#'   \item \code{expl_var_ratio}: Explained variance ratio (PCA-style).
#' }
#'
#' @examples
#' \dontrun{
#' data(iris)
#' X <- as.matrix(iris[, 1:4])
#' res <- nsa_flow_pca_fa_2(X, k = 2, objective = "fa", verbose = TRUE)
#' print(res$loadings)
#' }
#'
#' @export
nsa_flow_pca_fa_2 <- function(
  X, k,
  lambda = 0.1,
  alpha = 0.01,
  max_iter = 100,
  proximal_type = c("basic", "nsa_flow"),
  w_pca = 1.0,
  tol = 1e-6,
  grad_tol = 1e-4,
  nsa_flow_fn = NULL,
  verbose = FALSE,
  objective = c("pca","fa"),
  rotate = c("none","varimax","promax","oblimin"),
  nsa_flow_args = list()
) {
  alternate_inner_iters = 3  # New arg: number of inner alternating iterations for FA
  # --- arg checks & defaults -------------------------------------------------
  proximal_type <- match.arg(proximal_type)
  objective <- match.arg(objective)
  rotate <- match.arg(rotate)
  if (!is.matrix(X) || any(!is.finite(X))) stop("X must be a finite numeric matrix")
  n <- nrow(X); p <- ncol(X)
  if (k <= 0 || k > min(n, p)) stop("k must be positive and not exceed min(n, p)")
  if (lambda < 0) stop("lambda must be non-negative")
  if (alpha <= 0) stop("alpha must be positive")
  if (w_pca <= 0) stop("w_pca must be positive")
  if (is.null(nsa_flow_fn) && proximal_type == "nsa_flow") {
    stop("nsa_flow_fn must be provided when proximal_type == 'nsa_flow'")
  }
  # If NSA proximal used, disable L1 lambda
  if (proximal_type == "nsa_flow") {
    lambda <- 0.0
    if (verbose) message("proximal_type == 'nsa_flow' -> lambda set to 0")
  }
  # --- center X (no scaling) and precompute XtX / R ---------------------------
  Xc <- scale(X, center = TRUE, scale = FALSE)
  XtX <- crossprod(Xc) # p x p
  total_var <- sum(diag(XtX / n))
  if (!is.finite(total_var) || total_var <= 0) stop("Input X has zero or non-finite variance")
  # sample covariance/correlation matrix used for FA objective
  Rmat <- XtX / n
  # --- initialization -------------------------------------------------------
  # Improved: Use SVD for initialization (PCA-like for both objectives)
  svd_res <- ba_svd(Xc, nu = 0, nv = k)
  Y <- svd_res$v %*% diag(svd_res$d[1:k]) / sqrt(n)  # Scaled to match covariance approximation
  # For FA, optionally reduce communalities or add noise, but keep simple
  energy_trace <- numeric(max_iter)
  best_Y <- Y
  best_energy <- Inf
  alpha_init <- alpha
  max_grad_norm <- 100.0
  bt_max <- 20
  bt_shrink <- 0.5
  armijo_c <- 1e-4
  # adaptive scheduler
  patience <- 10
  lr_reduction_factor <- 0.5
  max_lr_reductions <- 3
  min_iters_before_stop <- 10
  no_improve_count <- 0
  lr_reductions <- 0
  converged <- FALSE
  .frob <- function(A) sqrt(sum(A * A))
  # Small numeric eps
  eps_diag <- 1e-8
  # energy_of depending on objective ------------------------------------------
  energy_of <- function(M, psi = NULL) {
    # M is p x k (loadings), psi optional for FA
    if (objective == "pca") {
      tr_val <- sum(diag(t(M) %*% XtX %*% M)) / n
      fid_term <- w_pca * (-0.5 * tr_val)
      fid_term
    } else { # "fa"
      recon_cross <- M %*% t(M)
      if (is.null(psi)) {
        diag_err <- diag(Rmat - recon_cross)
        psi <- pmax(as.numeric(diag_err), eps_diag)
      }
      recon <- recon_cross + diag(psi, nrow = p, ncol = p)
      diff_mat <- Rmat - recon
      mean(diff_mat * diff_mat)
    }
  }
  # helper: explained variance ratio for reporting (PCA style)
  explained_ratio_of <- function(M) {
    if (k == 1) {
      Q <- M / sqrt(sum(M^2) + 1e-12)
    } else {
      qr_decomp <- qr(M)
      Q <- qr.Q(qr_decomp)
    }
    tr_val <- sum(diag(t(Q) %*% XtX %*% Q)) / n
    tr_val / total_var
  }
  # --- main optimization loop -----------------------------------------------
  for (iter in seq_len(max_iter)) {
    t_start <- Sys.time()
    # optional normalization for stability (only for PCA, not FA)
    if (objective == "pca" && proximal_type != "nsa_flow") {
      if (k == 1) {
        Y <- Y / sqrt(sum(Y^2) + 1e-12)
      } else {
        qr_decomp <- qr(Y)
        Q <- qr.Q(qr_decomp)
        if (ncol(Q) >= k) Y <- Q[, seq_len(k), drop = FALSE]
      }
    }
    # For FA, initialize psi from current Y
    if (objective == "fa") {
      recon_cross <- Y %*% t(Y)
      diag_err <- diag(Rmat - recon_cross)
      psi <- pmax(as.numeric(diag_err), eps_diag)
    }
    # Inner alternating loop for FA (explicit over M and psi)
    for (inner in seq_len(ifelse(objective == "fa", alternate_inner_iters, 1))) {
      # Euclidean gradient
      if (objective == "pca") {
        grad_p <- - (XtX %*% Y) / n
        eu_grad <- w_pca * grad_p
      } else {
        # FA gradient with fixed psi: -4 * (R - recon) %*% Y, recon = Y Y^T + diag(psi)
        recon_cross <- Y %*% t(Y)
        recon <- recon_cross + diag(psi, nrow = p, ncol = p)
        diff_mat <- Rmat - recon
        eu_grad <- -4 * (diff_mat %*% Y)
      }
      if (any(!is.finite(eu_grad))) stop("Non-finite Euclidean gradient at iteration ", iter)
      # gradient clipping
      gnorm <- .frob(eu_grad)
      if (gnorm > max_grad_norm) eu_grad <- eu_grad * (max_grad_norm / gnorm)
      rgrad <- eu_grad
      rgrad_norm <- .frob(rgrad)
      # Backtracking Armijo line search
      alpha <- alpha_init
      Z <- Y
      Z[] <- Z - alpha * rgrad
      energy_old <- energy_of(Y, psi = if (objective == "fa") psi else NULL)
      energy_new <- energy_of(Z, psi = if (objective == "fa") psi else NULL)
      dir_deriv <- sum(eu_grad * (-rgrad))
      bt <- 0
      while ((!is.finite(energy_new) || energy_new > energy_old + armijo_c * alpha * dir_deriv) && bt < bt_max) {
        alpha <- alpha * bt_shrink
        Z[] <- Y - alpha * rgrad
        energy_new <- energy_of(Z, psi = if (objective == "fa") psi else NULL)
        bt <- bt + 1
      }
      if (!is.finite(energy_new)) stop("Non-finite energy after backtracking at iteration ", iter)
      Y_ret <- Z
      if (rotate != "none" && requireNamespace("psych", quietly = TRUE)) {
        rot_fun <- switch(rotate,
                          varimax = stats::varimax,
                          promax = stats::promax,
                          oblimin = GPArotation::oblimin,
                          NULL)
        if (!is.null(rot_fun)) {
          rot_res <- tryCatch(rot_fun(Y_ret), error = function(e) NULL)
          if (!is.null(rot_res$loadings)) Y_ret <- as.matrix(rot_res$loadings)
        }
      }

      # Proximal step
      if (proximal_type == "basic") {
        thresh <- alpha * lambda
        if (thresh > 0) {
          Y_new <- sign(Y_ret) * pmax(abs(Y_ret) - thresh, 0)
        } else {
          Y_new <- Y_ret
        }
      } else {
        prox_call_args <- c(list(Y0 = Y_ret), nsa_flow_args)
        prox_res <- tryCatch(do.call(nsa_flow_fn, prox_call_args), error = function(e) {
          stop("nsa_flow_fn failed: ", conditionMessage(e))
        })
        if (!is.list(prox_res) || is.null(prox_res$Y)) stop("nsa_flow_fn returned unexpected result")
        Y_new <- prox_res$Y
      }
      if (any(!is.finite(Y_new))) stop("Non-finite proximal step at iteration ", iter)
      # For FA, update psi explicitly from new Y (closed-form)
      if (objective == "fa") {
        recon_cross <- Y_new %*% t(Y_new)
        diag_err <- diag(Rmat - recon_cross)
        psi <- pmax(as.numeric(diag_err), eps_diag)
      }
      # Update Y for next inner iter
      Y <- Y_new
    }  # end inner
    # compute energy and stats (with final psi for FA)
    energy <- energy_of(Y, psi = if (objective == "fa") psi else NULL)
    expl_var_ratio <- explained_ratio_of(Y)
    # record energy trace
    energy_trace[iter] <- energy
    # check improvement and bookkeeping
    improved <- FALSE
    if (energy < best_energy) {
      best_energy <- energy
      best_Y <- Y
      improved <- TRUE
    }
    if (improved) {
      no_improve_count <- 0
    } else {
      no_improve_count <- no_improve_count + 1
    }
    # Adaptive scheduler
    if (no_improve_count >= patience && lr_reductions < max_lr_reductions) {
      alpha_init <- max(alpha_init * lr_reduction_factor, 1e-12)
      lr_reductions <- lr_reductions + 1
      if (verbose) message(sprintf("No improvement for %d iters -> alpha_init -> %.3e (lr_reductions=%d)",
                                   patience, alpha_init, lr_reductions))
      no_improve_count <- 0
    }
    stop_due_to_plateau <- (no_improve_count >= patience && lr_reductions >= max_lr_reductions)
    if (verbose) {
      cat(sprintf("Iter %3d | Energy: %12.6e | ExplVar: %.4f | GradNorm: %.4e | bt: %2d | alpha: %.3e | t: %.2fs\n",
                  iter, energy, expl_var_ratio, rgrad_norm, bt, alpha, as.numeric(difftime(Sys.time(), t_start, units = "secs"))))
      if (!improved) cat(sprintf(" (no_improve_count=%d, lr_reductions=%d)\n", no_improve_count, lr_reductions))
    }
    # convergence checks
    if (iter > 1) {
      rel_energy_change <- abs(energy_trace[iter] - energy_trace[iter - 1]) / (abs(energy_trace[iter - 1]) + 1e-12)
      grad_ok <- (rgrad_norm < grad_tol)
      delta_Y <- .frob(Y - best_Y) / (.frob(best_Y) + 1e-12)  # Compare to best
      delta_ok <- (delta_Y < tol)
      converged_condition <- (iter > min_iters_before_stop) && (rel_energy_change < tol) && (grad_ok || delta_ok)
      if (verbose) {
        cat(sprintf(" DeltaEnergy: %.2e | DeltaY: %.2e | ConvergedCond: %s\n",
                    rel_energy_change, delta_Y, ifelse(converged_condition, "[OK]", "x")))
      }
      if (converged_condition) {
        converged <- TRUE
        if (verbose) message("Convergence achieved at iteration ", iter)
        break
      }
    }
    if (stop_due_to_plateau) {
      if (verbose) message("Stopping early due to plateau")
      break
    }
  } # end iter
  energy_trace <- energy_trace[seq_len(iter)]
  # Final orthonormalization for reported best_Y (for explained variance calc, PCA-style)
  if (k == 1) {
    Q <- best_Y / sqrt(sum(best_Y^2) + 1e-12)
  } else {
    qr_decomp <- qr(best_Y)
    Q <- qr.Q(qr_decomp)
    if (ncol(Q) >= k) Q <- Q[, seq_len(k), drop = FALSE]
  }
  tr_val <- sum(diag(t(Q) %*% XtX %*% Q)) / n
  expl_var_ratio <- tr_val / total_var
  # --- construct "loadings" with rotation and names ---------------------
  loadings <- best_Y
  if (rotate != "none" && requireNamespace("psych", quietly = TRUE)) {
    rot_fun <- switch(rotate,
                      varimax = stats::varimax,
                      promax = stats::promax,
                      oblimin = GPArotation::oblimin,
                      NULL)
    if (!is.null(rot_fun)) {
      rot_res <- tryCatch(rot_fun(loadings), error = function(e) NULL)
      if (!is.null(rot_res$loadings)) loadings <- as.matrix(rot_res$loadings)
    }
  }
  if (!is.null(colnames(X))) rownames(loadings) <- colnames(X)
  colnames(loadings) <- paste0("PA", seq_len(k))
  posthoc <- posthoc_fa_summary(X, loadings = loadings, method = "NSA")
  list(
    loadings = loadings,
    scores = posthoc$scores,
    communalities = posthoc$communalities,
    uniqueness = posthoc$uniqueness,
    variance_per_factor = posthoc$variance_per_factor,
    variance_explained = posthoc$variance_explained,
    energy_trace = energy_trace,
    final_iter = iter,
    best_energy = best_energy,
    converged = converged,
    no_improve_count = no_improve_count,
    lr_reductions = lr_reductions,
    expl_var_ratio = expl_var_ratio
  )
}



#' Compute Communalities from Factor Scores
#'
#' This function calculates the communality for each variable in the dataset,
#' defined as the proportion of variance explained by the factor scores (R^2 from
#' multiple linear regression of each variable on the scores). It provides an
#' empirical estimate of how much each variable's variance is accounted for by
#' the common factors. The function assumes the data is centered or standardized
#' as appropriate for factor analysis.
#'
#' @param data Numeric matrix or data.frame with n rows (observations) and p
#'   columns (variables).
#' @param scores Numeric matrix of factor scores with n rows and k columns
#'   (factors).
#'
#' @return A named numeric vector of length p containing the communalities for
#'   each variable, named by the column names of \code{data} (if available).
#'
#' @details
#' For each variable, a linear model is fit: \code{lm(data[, i] ~ scores)}, and
#' the R^2 is extracted. This aligns with model-based communalities in factor
#' analysis for large samples but may slightly overestimate due to in-sample
#' fitting. For efficiency with large p, consider matrix-based alternatives, but
#' this loop-based approach is simple and leverages base R.
#'
#' @examples
#' \dontrun{
#' # Synthetic data example
#' set.seed(42)
#' n <- 100; p <- 5; k <- 2
#' scores <- matrix(rnorm(n * k), n, k)
#' loadings <- matrix(runif(p * k, 0.5, 0.8), p, k)
#' data <- scores %*% t(loadings) + matrix(rnorm(n * p, sd = 0.5), n, p)
#' colnames(data) <- paste0("Var", 1:p)
#' comm <- compute_communalities(data, scores)
#' print(comm)
#' }
#'
#' @export
compute_communalities <- function(data, scores) {
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("data must be a matrix or data.frame")
  }
  if (!is.matrix(scores)) {
    scores <- as.matrix(scores)
  }
  if (nrow(data) != nrow(scores)) {
    stop("data and scores must have the same number of rows")
  }
  if (ncol(scores) == 0) {
    stop("scores must have at least one column")
  }
  vec <- sapply(1:ncol(data), function(i) {
    lm_fit <- lm(data[, i] ~ scores)
    summary(lm_fit)$r.squared
  })
  names(vec) <- colnames(data)
  vec
}




#' Compute Variance Explained per Factor
#'
#' This function calculates the variance explained by each factor in a factor analysis model.
#'
#' @param data A numeric matrix or data frame of observed variables (n x p).
#' @param scores A numeric matrix of factor scores (n x k).
#' @param method A string indicating the factor analysis method (e.g., "PAF").
#'
#' @return A data frame with the variance explained by each factor.
#'
#' @examples
#' # data(mtcars)
#' # fa_result <- factanal(mtcars, factors = 2)
#' # scores <- fa_result$scores
#' # compute_variance_explained_per_factor(mtcars, scores)
#'
#' @export
compute_variance_explained_per_factor <- function(data, scores, method = "PAF") {
  # data: n x p matrix/data.frame of observed variables
  # scores: n x k matrix of factor scores
  # method: string indicating the factor analysis method (e.g., "PAF")
  
  p <- ncol(data)
  k <- ncol(scores)
  
  variances <- sapply(1:k, function(j) {
    r2s <- sapply(1:p, function(i) {
      lm_fit <- lm(data[, i] ~ scores[, j])
      summary(lm_fit)$r.squared
    })
    mean(r2s)
  })
  
  data.frame(
    Factor = 1:k,
    Variance = variances,
    Method = method
  )
}



#' Post-hoc Factor Analysis Summary: Scores, Communalities, and Variance Explained
#'
#' Given data and factor loadings, this function computes factor scores, communalities,
#' uniquenesses, and per-factor variance explained using linear modeling.
#' It provides a convenient post-hoc summary of a factor analysis solution.
#'
#' @param data Numeric matrix or data.frame of observed variables (n x p).
#' @param loadings Numeric matrix of factor loadings (p x k).
#' @param scores Optional numeric matrix of factor scores (n x k). If NULL scores are computed by least squares regression.
#' @param method Character string describing the extraction method (e.g., "PAF", "NSA-Flow", "ML").
#'
#' @return A list with components:
#' \describe{
#'   \item{scores}{Matrix of factor scores (n x k).}
#'   \item{communalities}{Named numeric vector of communalities for each variable.}
#'   \item{uniquenesses}{Named numeric vector of uniquenesses (1 - communality).}
#'   \item{variance_per_factor}{Data frame with variance explained per factor.}
#'   \item{total_variance_explained}{Total proportion of variance explained across factors.}
#'   \item{reconstructed}{Reconstructed data from scores and loadings.}
#'   \item{residuals}{Matrix of residuals: \code{data - reconstructed}.}
#' }
#'
#' @examples
#' set.seed(42)
#' n <- 100; p <- 6; k <- 2
#' scores_true <- matrix(rnorm(n * k), n, k)
#' loadings <- matrix(runif(p * k, 0.5, 0.8), p, k)
#' data <- scores_true %*% t(loadings) + matrix(rnorm(n * p, sd = 0.5), n, p)
#' colnames(data) <- paste0("Var", 1:p)
#'
#' results <- posthoc_fa_summary(data, loadings, method = "PAF")
#' str(results)
#'
#' @export
posthoc_fa_summary <- function(data, loadings, scores = NULL, method = "PAF") {
  # Ensure inputs are matrices
  if (!is.matrix(data)) data <- as.matrix(data)
  if (!is.matrix(loadings)) loadings <- as.matrix(loadings)

  # Number of observations, variables, factors
  n <- nrow(data)
  p <- ncol(data)
  k <- ncol(loadings)


  compute_R_hat <- function(loadings, uniqueness) {
    L <- as.matrix(loadings)
    p <- nrow(L)
    
    Psi <- diag(as.vector(uniqueness))
    R_hat_cov <- L %*% t(L) + Psi
    
    # force symmetry
    R_hat_cov <- (R_hat_cov + t(R_hat_cov)) / 2
    
    # standardize to correlation
    sd_vec <- sqrt(diag(R_hat_cov))
    R_hat_corr <- R_hat_cov / (sd_vec %*% t(sd_vec))
    
    # force diagonal to 1 (just in case)
    diag(R_hat_corr) <- 1
    
    R_hat_corr
  }
  # Compute factor scores via regression method (Thomson scores)
  compute_factor_scores_lm <- function(data, loadings) {
    data <- as.matrix(data)
    loadings <- as.matrix(loadings)

    # Compute factor score coefficients using regression formula:
    # scores = data %*% (L (L' L)^-1)
    # Equivalent to least squares estimates of factors predicting variables
    inv_mat <- tryCatch(solve(t(loadings) %*% loadings), error = function(e) NULL)
    if (is.null(inv_mat)) stop("Loadings matrix is singular, cannot compute factor scores.")

    weights <- loadings %*% inv_mat  # p x k times k x k = p x k
    scores <- data %*% weights       # n x p times p x k = n x k
    colnames(scores) <- paste0("Factor", seq_len(ncol(loadings)))

    scores
  }

  # Compute or use provided scores
  if (is.null(scores)) {
    scores <- compute_factor_scores_lm(data, loadings)
  } else {
    if (!is.matrix(scores)) scores <- as.matrix(scores)
    if (ncol(scores) != k || nrow(scores) != n) {
      stop("Provided scores have incompatible dimensions.")
    }
  }

  # Compute communalities (R^2 from regressing each variable on all factors)
  communalities <- sapply(seq_len(p), function(i) {
    fit <- lm(data[, i] ~ scores)
    summary(fit)$r.squared
  })
  names(communalities) <- colnames(data)

  # Uniquenesses = 1 - communalities
  uniquenesses <- 1 - communalities

  # Variance explained by each factor (mean R^2 across variables)
  variance_per_factor <- sapply(seq_len(k), function(j) {
    r2s <- sapply(seq_len(p), function(i) {
      fit <- lm(data[, i] ~ scores[, j])
      summary(fit)$r.squared
    })
    mean(r2s)
  })
  variance_per_factor <- data.frame(
    Factor = paste0("Factor", seq_len(k)),
    Variance = variance_per_factor,
    Method = method,
    row.names = NULL
  )

  total_variance_explained <- mean(communalities)

  # Reconstruct data from factor scores and loadings
  reconstructed <- scores %*% t(loadings)
  colnames(reconstructed) <- colnames(data)

  residuals <- data - reconstructed

  # Compute R_hat (assuming orthogonal factors here; optionally pass Phi)
  rhat <- compute_R_hat(loadings, uniquenesses)

  list(
    scores = scores,
    communalities = communalities,
    uniquenesses = uniquenesses,
    variance_per_factor = variance_per_factor,
    total_variance_explained = total_variance_explained,
    reconstructed = reconstructed,
    residuals = residuals,
    R_hat = rhat
  )
}


#' Extend an existing SIMLR embedding with new modality blocks
#'
#' Rebuilds the projected SIMLR feature space from an existing SIMLR result,
#' adds one or more new modality blocks from the projected data, constructs
#' block-specific adjacency structures, chooses a joint initialization rank,
#' and runs \code{simlr.perm()} on the combined blocks.
#'
#' This implementation is intentionally strict:
#' - input validation is explicit
#' - preprocessing is deterministic
#' - constant / all-NA columns are dropped by default rather than jittered
#' - the input \code{simlr_result} is not mutated
#' - diagnostics are returned for auditability
#'
#' @param pymm Input multimodal object passed through to
#'   \code{apply_simlr_matrices_dtfix()}.
#' @param simlr_result Existing SIMLR result object with a \code{$v} element.
#' @param new_modalities Named list mapping modality names to projected-data
#'   column names, e.g. \code{list(pet = c("petPC1","petPC2"))}.
#' @param mode How to construct existing blocks from the current SIMLR
#'   projection. One of \code{"concatenate"}, \code{"split"}, or \code{"auto"}.
#' @param split_prefixes Character vector of prefixes used when
#'   \code{mode = "split"}.
#' @param adjacency_type Type of within-block adjacency to build. Currently
#'   \code{"feature_correlation"} is supported.
#' @param cor_threshold Nonnegative threshold applied to absolute correlations
#'   when building adjacency matrices.
#' @param use_abs_correlation Logical; if \code{TRUE}, threshold on absolute
#'   correlation magnitude.
#' @param k_new Optional rank specification for new modalities. May be
#'   \code{NULL}, a single positive integer, or a named numeric vector keyed by
#'   new modality names.
#' @param k_method Method for automatic rank selection when \code{k_new} is not
#'   supplied for a modality. One of \code{"cumulative"} or \code{"elbow"}.
#' @param cumvar_threshold Cumulative variance threshold used when
#'   \code{k_method = "cumulative"}.
#' @param min_k Minimum admissible latent dimension.
#' @param joint_k_policy Policy used to combine per-modality recommended ranks
#'   into a single initialization rank. One of \code{"max"}, \code{"median"},
#'   or \code{"min"}.
#' @param preprocess Policy for invalid columns. One of \code{"drop"} or
#'   \code{"zero"}.
#' @param verbose Logical; emit progress messages.
#' @param ... Additional arguments forwarded to \code{simlr.perm()}.
#'
#' @return A list with:
#' \describe{
#'   \item{updated_simlr_result}{Copy of \code{simlr_result} with any returned
#'     modality-specific \code{$v} entries merged in, without mutating input.}
#'   \item{simlr_permutations}{Result from \code{simlr.perm()}.}
#'   \item{blocks}{Named list of processed block matrices used for fitting.}
#'   \item{adjacency}{Named list of adjacency matrices.}
#'   \item{projected_data}{Projected data returned by
#'     \code{apply_simlr_matrices_dtfix()}.}
#'   \item{k_new_used}{Named integer vector of per-new-modality ranks used.}
#'   \item{joint_k}{Joint initialization rank passed to
#'     \code{initializeSimlr()}.}
#'   \item{diagnostics}{List of validation and preprocessing diagnostics.}
#' }
#'
#' @export
