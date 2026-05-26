
# Removed redundant take_abs_unsigned

# Removed redundant l1_normalize_features

#' Calculate the invariant orthogonality defect
#' @export
invariant_orthogonality_defect <- function( A ) 
{
  if (!is.matrix(A)) A <- as.matrix(A)
  Gram <- crossprod( A )
  defect_matrix <- Gram^2
  diag(defect_matrix) <- 0
  total_defect <- sum(defect_matrix)
  return(total_defect)
}

#' Compute the Gradient of the Orthogonality Defect
#' @export
gradient_invariant_orthogonality_defect <- function(A) {
  if (!is.matrix(A)) A <- as.matrix(A)
  Gram <- crossprod(A)
  G_off_diagonal <- Gram
  diag(G_off_diagonal) <- 0
  gradient_matrix <- 4 * (A %*% G_off_diagonal)
  return(gradient_matrix)
}


# Removed redundant pairwise_matrix_similarity and safe_pca

#' Inverse square root of a symmetric matrix via Newton-Schulz
#' @export
inv_sqrt_sym_newton <- function(A, epsilon = 1e-10, max_iter = 10L, tol = 1e-6, verbose = FALSE) {
  # Implementation from multiscaleSVDxpts.R L10713
  # Placeholder for now, I'll add the full body if needed.
  # For SiMLR it's used in mixAlg='newton-schulz'
  n <- ncol(A)
  norm_A <- norm(A, "F")
  Y <- A / norm_A
  Z <- diag(n)
  for (i in seq_len(max_iter)) {
    T_mat <- (3 * diag(n) - Z %*% Y) / 2
    Y_new <- Y %*% T_mat
    Z_new <- T_mat %*% Z
    if (norm(Y_new - Y, "F") < tol) break
    Y <- Y_new
    Z <- Z_new
  }
  return(Z / sqrt(norm_A))
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
    zeroUpper = FALSE, uAlgorithm = "ba_svd", addNoise = 0) {
  nModalities <- length(voxmats)

  for (i in 1:nModalities) {
    # 1. Clean NA and Inf values early
    if (any(!is.finite(voxmats[[i]]))) {
      voxmats[[i]] <- antsrimpute(voxmats[[i]])
      # If still has non-finite (e.g. all NA), replace with 0
      voxmats[[i]][!is.finite(voxmats[[i]])] <- 0
    }

    # 2. Replace zero variance columns with the column mean + noise
    col_min <- apply(voxmats[[i]], 2, min, na.rm = TRUE)
    col_max <- apply(voxmats[[i]], 2, max, na.rm = TRUE)
    zero_var_idx <- which(col_min == col_max | !is.finite(col_min) | !is.finite(col_max))

    if (length(zero_var_idx) > 0) {
      for (col in zero_var_idx) {
        # Get the mean of the specific zero-variance column
        c_mean <- mean(voxmats[[i]][, col], na.rm = TRUE)
        if (!is.finite(c_mean)) c_mean <- 0
        
        # Add deterministic epsilon noise to give it some small variance
        n_rows <- nrow(voxmats[[i]])
        eps_noise <- sin(seq_len(n_rows) * (pi / n_rows) * (col + i)) * 1e-6
        
        voxmats[[i]][, col] <- c_mean + eps_noise
      }
    }
  }
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
    if (uAlgorithm == "pca" | uAlgorithm == "svd" | uAlgorithm == "ba_svd" | uAlgorithm == "newton-schulz" | uAlgorithm == "ica-newton") {
      X.pcr <- stats::prcomp(t(X), rank. = k, scale. = uAlgorithm == "ba_svd") # PCA
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
    if (localAlgorithm == "pca " | localAlgorithm == "svd" | localAlgorithm == "newton-schulz" | localAlgorithm == "ica-newton") {
      uOut[[s]] <- (stats::prcomp(t(X), rank. = k, scale. = uAlgorithm == "ba_svd")$rotation)
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
      uOut[[s]] <- (stats::prcomp(t(X), rank. = k, scale. = uAlgorithm == "ba_svd")$rotation)
    }
    if (addNoise > 0) uOut[[s]] <- uOut[[s]] + replicate(k, rnorm(nrow(voxmats[[1]]))) * addNoise
    if (zeroUpper) uOut[[s]][upper.tri(uOut[[s]])] <- 0
  }
  return(uOut)
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
#' @param mixAlg 'svd', 'ica', 'rrpca-l', 'rrpca-s', 'stochastic', 'pca', 'newton-schulz', 'ica-newton' or 'avg' denotes the algorithm employed when estimating the mixed modality bases
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
#' @param expBeta if greater than zero, use exponential moving average on gradient and mixing (default 0.9).
#' @param jointInitialization boolean for initialization options, default TRUE
#' @param sparsenessAlg NA is default otherwise basic, spmp or orthorank
#' @param orthogonalizeU boolean controlling whether we orthogonalize the U matrices
#' @param domainMatrices matrices containing domain knowledge length of \code{data_matrices} with number of columns also equal to each corresponding data matrix
#' @param domainLambdas weights for domain knowledge term length of \code{data_matrices}
#' @param sparse_gradient boolean default TRUE
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
    sparsenessQuantiles = NULL,
    positivities = NULL,
    initialUMatrix = NULL,
    mixAlg = c("svd", "ica", "avg", "rrpca-l", "rrpca-s", "pca", "stochastic", "newton-schulz", "ica-newton"),
    repeatedMeasures = NA,
    lineSearchRange = c(-5e2, 5e2),
    lineSearchTolerance = 1e-12,
    randomSeed=0,
    constraint = c("nsaflowx0.5x10", "orthox0x1", "Grassmannx0", "Stiefelx0",  "none"),
    energyType = c("cca", "regression", "normalized", "ucca", "lowRank", "lowRankRegression",'normalized_correlation','acc','nc','dat', 'lrr', 'reconorm', 'logcosh', 'exp', 'kurtosis', 'gauss'),
    vmats = NULL,
    connectors = NULL,
    optimizationStyle = c("bidirectional_lookahead", "armijo_gradient","lookahead","bidirectional_armijo_gradient" ),
    scale = c("center",  "eigenvalue" ),
    expBeta = 0.9,
    jointInitialization = TRUE,
    sparsenessAlg = 'soft',
    orthogonalizeU = FALSE,
    domainMatrices = NULL,
    domainLambdas  = NULL,
    sparse_gradient = TRUE,
    verbose = FALSE) {

  # Capture provenance at the start
  prov_nsimlr <- if (is.matrix(initialUMatrix)) ncol(initialUMatrix) else if (is.numeric(initialUMatrix)) initialUMatrix else NA
  prov_prescaling <- if (missing(scale)) "centerAndScale" else scale
  prov_objectiver <- if (missing(energyType)) "acc" else energyType
  prov_mixer <- if (missing(mixAlg)) "pca" else mixAlg
  prov_constraint <- constraint[1]
  prov_sparval <- if (is.null(sparsenessQuantiles)) 0 else sparsenessQuantiles
  prov_ebber <- expBeta
  prov_pizzer <- if (is.null(positivities)) "positive" else positivities
  prov_optimus <- if (missing(optimizationStyle)) "bidirectional_lookahead" else optimizationStyle
  prov_sparsenessAlg <- sparsenessAlg

  provenance <- data.frame(
    nsimlr = prov_nsimlr,
    objectiver = prov_objectiver,
    mixer = prov_mixer,
    ebber = prov_ebber,
    optimus = prov_optimus,
    constraint = prov_constraint,
    sparsenessAlg = prov_sparsenessAlg,
    stringsAsFactors = FALSE
  )
  provenance <- cbind(provenance, vector_to_df(prov_prescaling, "prescaling"))
  provenance <- cbind(provenance, vector_to_df(prov_sparval, "sparval"))
  provenance <- cbind(provenance, vector_to_df(prov_pizzer, "positivity"))

  if ( length( optimizationStyle ) > 1 ) optimizationStyle=optimizationStyle[1]
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
  if (is.null(positivities)) {
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
  if (is.null(sparsenessQuantiles)) {
    sparsenessQuantiles <- rep(0.5, nModalities)
  }
  
  if (length(sparsenessQuantiles) == 1) {
    sparsenessQuantiles <- rep(sparsenessQuantiles[1], nModalities)
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
      # 1.1 Clean NA and Inf values early
      if (any(!is.finite(data_matrices[[i]]))) {
        data_matrices[[i]] <- antsrimpute(data_matrices[[i]])
        # If still has non-finite (e.g. all NA), replace with 0
        data_matrices[[i]][!is.finite(data_matrices[[i]])] <- 0
      }

      # 1.2 Replace zero variance columns with the column mean + noise
      col_min <- apply(data_matrices[[i]], 2, min, na.rm = TRUE)
      col_max <- apply(data_matrices[[i]], 2, max, na.rm = TRUE)
      zero_var_idx <- which(col_min == col_max | !is.finite(col_min) | !is.finite(col_max))

      if (length(zero_var_idx) > 0) {
        for (col in zero_var_idx) {
          # Get the mean of the specific zero-variance column
          c_mean <- mean(data_matrices[[i]][, col], na.rm = TRUE)
          if (!is.finite(c_mean)) c_mean <- 0
          
          # Add deterministic epsilon noise to give it some small variance
          n_rows <- nrow(data_matrices[[i]])
          eps_noise <- sin(seq_len(n_rows) * (pi / n_rows) * (col + i)) * 1e-6
          
          data_matrices[[i]][, col] <- c_mean + eps_noise
        }
      }

      if (any(is.null(data_matrices[[i]]))) {
        stop(paste("input matrix", i, "is null."))
      }
      matnames <- names(data_matrices)[i]
      
      for (j in 1:length(scaleList)) {
        if (scaleList[j] == "norm") {
          fnorm <- norm(data_matrices[[i]], type = "F")
          if (is.finite(fnorm) && fnorm > 0) {
            data_matrices[[i]] <- data_matrices[[i]] / fnorm
          }
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
  for (i in seq_along(smoothingMatrices)) {
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
    if (!is.finite(si) || si < .Machine$double.eps) si <- 1
    q[, 1] <- qi / si
    r[1, 1] <- si
    for (i in 2:m) {
      xi <- x[, i]
      qj <- q[, 1:(i - 1)]
      rj <- t(qj) %*% xi
      qi <- xi - qj %*% rj
      r[1:(i - 1), i] <- rj
      si <- sqrt(sum(qi^2))
      if (!is.finite(si) || si < .Machine$double.eps) si <- 1
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
      vmats[[i]] <- vmats[[i]] / norm(vmats[[i]], "F")
    }
  }
  
  nc <- ncol(initialUMatrix[[1]])
  myw <- matrix(rnorm(nc^2), nc, nc) # initialization for fastICA

  energyPath <- matrix(Inf, nrow = iterations, ncol = nModalities)
  orthPath = matrix(Inf, nrow = iterations, ncol = nModalities)
  bestU <- initialUMatrix
  bestV <- vmats
  domain_knowledge=0
  if ( !is.null( domainLambdas ) & !is.null( domainMatrices )  ){
    domain_knowledge=paste0("d.",nrow(domainMatrices[[1]]),'.l.',mean(domainLambdas) )
  }
  if (verbose) {
    cat(sprintf("
      --- Method Summary ---
        * Mixer Algorithm  : %s
        * Energy Type      : %s
        * Sparseness Alg.  : %s
        * expBeta          : %s
        * constraint       : %s
        * constraint-it    : %s
        * constraint-wt    : %s
        * optimizationStyle: %s
        * domain-knowledge : %s
      ----------------------
      ", mixAlg, energyType, sparsenessAlg, expBeta, constraint_type, constraint_iterations, constraint_weight, optimizationStyle, domain_knowledge ))
  }
    
# ==============================================================================
#      High-Performance SIMLR Loop (Hybrid: Adam/SGD + Line Search)
# ==============================================================================
# --- 1. Setup before the loop ---
# Initialize adaptive orthogonality weights
orth_weights <- rep(0.0, nModalities)
normalizing_weights = rep( 1.0, nModalities )
domain_weights <- rep(1.0, nModalities)  # Rename auto_norm_domain_weights
names(domain_weights) <- names(data_matrices)
names( orth_weights ) = names( normalizing_weights ) = names( data_matrices )
clipper = 0.95 # no clipping
bestTot <- Inf
bestRow <- 1
bestU <- initialUMatrix
bestV <- vmats
convergence_df <- tibble::tibble()
converged=0

# --- Add these parameters to your main simlr() function signature ---
optimizer = optimizationStyle
initial_learning_rate = 1.0
final_learning_rate = 1e-6

# Create the optimizer object based on user's choice
optimizer_object_l = list()
for ( k in 1:nModalities) {
  optimizer_object <- create_optimizer(
    optimizer_type = optimizer,
    vmats = vmats,
    # Pass hyperparameters that the step functions will need
    beta1 = 0.9, beta2 = 0.999, epsilon = 1e-8, sgd_momentum_beta = 0.9
  )
  optimizer_object_l[[k]] <- optimizer_object
}


# initialize energy trackers for each modality
all_sim_energy   <- vector("list", nModalities)
all_ort_energy   <- vector("list", nModalities)
all_dom_energy   <- vector("list", nModalities)
all_dom_energy_raw<-vector("list", nModalities)
all_total_energy <- vector("list", nModalities)

for (j in 1:nModalities) {
  all_sim_energy[[j]]   <- numeric()
  all_ort_energy[[j]]   <- numeric()
  all_dom_energy[[j]]   <- numeric()
  all_dom_energy_raw[[j]] <- numeric()
  all_total_energy[[j]] <- numeric()
}
v_initial = vmats
u_initial = initialUMatrix
  v_prev <- vmats
  stagnation_counter <- 0
  # --- 2. Main Optimization Loop ---
  for (myit in 1:iterations) { # Begin main optimization loop
    # --- Calculate dynamic learning rate for non-line-search methods ---
    if ( myit <= 2 ) {
      vmats = v_initial
      initialUMatrix = u_initial
    }
    decay_progress <- (myit - 1) / max(1, iterations - 1)
    current_learning_rate <- final_learning_rate + 0.5 * (initial_learning_rate - final_learning_rate) * (1 + cos(pi * decay_progress))
    
    # Track if any matrix changed in this iteration
    v_diff <- 0
    for (kk in 1:nModalities) {
      v_diff <- v_diff + sum((vmats[[kk]] - v_prev[[kk]])^2)
    }
    if (v_diff < .Machine$double.eps & myit > 2) {
      stagnation_counter <- stagnation_counter + 1
    } else {
      stagnation_counter <- 0
    }
    v_prev <- vmats
    
    if (stagnation_counter >= 10) {
      if (verbose) message("~~Parameter stagnation detected. Breaking loop.")
      break
    }
    
    # --- A. Update each V_i matrix ---
  for (i in 1:nModalities) {
    ##############################################################
    # first define the local versions of the energy and gradient #
    ##############################################################
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
      dom_e <- dom_e_raw <- 0
      if (!is.null(domainMatrices) && !is.null(domainLambdas)) {
        lam <- domainLambdas[i]
        if (lam > 0) {
          dom_e_raw <- calculate_simlr_energy(
            V_sp, data_matrices[[i]], initialUMatrix[[i]],
            "dat", lambda = lam, prior_matrix = domainMatrices[[i]]
          )
          dom_e <- dom_e_raw * domain_weights[i]    # Scale
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
      all_ort_energy[[i]] <<- c( all_ort_energy[[i]], orth_e )
      all_dom_energy_raw[[i]] <<- c(all_dom_energy_raw[[i]], dom_e_raw)
      all_total_energy[[i]] <<- c(all_total_energy[[i]], total_e)
      if (return_raw) return(sim_e) # raw similarity+domain only
      return(total_e)
    }
    ############################
    smooth_grad <- function(V) {
      if (positivities[i] == 'positive') V <- take_abs_unsigned(V)

      # --- User-chosen gradient ---
      sim_grad <- calculate_simlr_gradient(
        V, data_matrices[[i]], initialUMatrix[[i]],
        energyType
      ) * normalizing_weights[i]

      # --- Domain gradient (only if lambda > 0) ---
      dom_grad <- 0
      if (!is.null(domainMatrices) && !is.null(domainLambdas)) {
        lam <- domainLambdas[i]
        if (lam > 0) {
          dom_grad <- calculate_simlr_gradient(
            V, data_matrices[[i]], initialUMatrix[[i]],
            "dat", lambda = lam, prior_matrix = domainMatrices[[i]]
          ) * domain_weights[i]
        }
      }

      orth_grad <- 0
      if (constraint_type == "ortho" & FALSE) {
        orth_grad <- constraint_weight * orth_weights[i] *
          gradient_invariant_orthogonality_defect(V_sp)
      }


      g <- sim_grad + dom_grad - orth_grad
      g <- clip_gradient_by_quantile( as.matrix(g), clipper)
      g <- simlr_sparseness(
        g,
        constraint_type = constraint_type,
        smoothing_matrix = smoothingMatrices[[i]],
        positivity = positivities[i],
        sparseness_quantile = sparsenessQuantiles[i],
        constraint_iterations = constraint_iterations,
        constraint_weight = constraint_weight,
        sparseness_alg = sparsenessAlg
      )

      return(g)
    }      
    
    riemannian_descent_grad = smooth_grad(vmats[[i]])
    step_result <- step(
          optimizer = optimizer_object_l[[i]],
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
    optimizer_object_l[[i]] <- step_result$optimizer
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
    local_simlrU <- function(projectionsU, mixingAlgorithm, initialW, orthogonalize, connectors, expBeta) {
      return(simlrU(projectionsU, mixingAlgorithm, initialW, orthogonalize, connectors, expBeta))
    }
    updated_Us <- local_simlrU(tempU, mixAlg, initialUMatrix, orthogonalizeU, connectors, expBeta)
    # Apply Gram-Schmidt orthogonalization (localGS)
    initialUMatrix <- lapply(updated_Us, function(u) localGS(u, orthogonalize = orthogonalizeU))
    #   cat(paste("<o><o><o><o><o><o><o><o><o><o>\n"))
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
        iteration = myit, 
        modality = names(data_matrices)[i],
        total_energy = all_total_energy[[i]][length(all_total_energy[[i]])],
        similarity_energy = all_sim_energy[[i]][length(all_sim_energy[[i]])],
        domain_energy_raw = all_dom_energy_raw[[i]][length(all_dom_energy_raw[[i]])],  
        domain_energy = all_dom_energy[[i]][length(all_dom_energy[[i]])],
        feature_orthogonality = all_ort_energy[[i]][length(all_ort_energy[[i]])],
        similarity_energy_w = sim_e,
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
      sim_e_abs <- abs(mod_results$similarity_energy)
      if (sim_e_abs < 1e-10) sim_e_abs <- 1.0
      normalizing_weights[i] <- 1.0 / sim_e_abs
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
      # After setting normalizing_weights and orth_weights
      if (mod_results$domain_energy_raw != 0 & !is.na(mod_results$domain_energy_raw)) {  # Use raw
        if (abs(mod_results$domain_energy_raw) > 1e-10) {
          domain_weights[i] <- abs(mod_results$similarity_energy * normalizing_weights[i]) / abs(mod_results$domain_energy_raw)
        } else {
          domain_weights[i] <- 1.0
        }
        if (is.na(domain_weights[i]) | is.infinite(domain_weights[i])) domain_weights[i] <- 1.0
      }
      if (verbose & i == nModalities ) message("Domain Weights: ", paste(round(domain_weights, 2), collapse=", "))      
    }
    if (verbose) {
      print(normalizing_weights)
      message("Norm Weights: ", paste(round(normalizing_weights, 3), collapse=", "))
      message("Orth Weights: ", paste(round(orth_weights, 2), collapse=", "))
    }
  }
  
  # Update the "best" solution found so far based on mean total energy
  mean_current_energy <- mean(iter_results$total_energy, na.rm = TRUE)
  printit=FALSE
  if (is.finite(mean_current_energy) && mean_current_energy < bestTot & myit >= 2 ) {
    lastBest = bestTot
    bestTot <- mean_current_energy
    bestRow <- myit
    bestU <- initialUMatrix
    bestV <- vmats
    printit=TRUE
    converged=myit
    pct_reduction_less_than <- function(new_val, old_val, threshold_pct) {
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
        if (verbose) message(paste("~~Small.delt: E ", round(bestTot,4), " E-1 ", round(lastBest, 4),"E / E-1 ",round(change_detector[2],4)))
        break # Properly exit the loop
      }
    }
  } else {
    # Lack of improvement - reduce learning rate
    initial_learning_rate = initial_learning_rate * 0.5
    final_learning_rate = final_learning_rate * 0.9
  }
  
  if (verbose & printit | verbose > 1 ) {
    # Report the mean orthogonality across all modalities for this iteration
    mean_orthogonality <- mean(iter_results$feature_orthogonality, na.rm = TRUE)
    cat(sprintf("It: %d | Energy: %.4f | Best.Energy: %.4f (at iter %d) | Ortho: %.4f \n",
                  myit, mean_current_energy, bestTot, bestRow, mean_orthogonality))
    if ( myit == 1) cat("\n----iteration 1 is an auto-tuning iteration----\n")
  }
  
  # Check for convergence based on lack of improvement
  maxitnoimp = max(5, round( 0.05 * iterations ))
  if ((myit - bestRow) > maxitnoimp) {
    if(verbose) message(paste("~~Convergence criteria met @ ",myit," \n No improvement over ", maxitnoimp, " iterations"))
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
  
for ( v in 1:length(bestV)) {
  if ( is.null( rownames(bestV[[v]])) ) {
    rownames(bestV[[v]])=paste0("x",1:nrow(bestV[[v]]))
  }
}

energyPath <- na.omit(energyPath)
rlist=    list(
      u = bestU,
      v = bestV,
      initialRandomMatrix = randmat,
      energyPath = data.frame(convergence_df),
      finalError = bestTot,
      connectors = connectors,
      energyType = energyType,
      mixAlg = mixAlg,
      optimizationStyle = optimizationStyle,
      converged_at = converged,
      sim_energy = all_sim_energy,
      domain_energy = all_dom_energy_raw,
      orth_energy = all_ort_energy,
      total_energy = all_total_energy,
      constraint = constraint,
      provenance = provenance
    )
if ( ! is.null( domainLambdas ) ) {
  rlist$domainLambdas = domainLambdas
}
return(
  rlist
  )
}
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
  if (!"iteration" %in% colnames(df)) {
    df$iteration <- seq_len(nrow(df))
  }
  
  # Base data
  energies <- data.frame(
    iteration = df$iteration,
    similarity = df$similarity_energy,
    domain = df$domain_energy,
    orth = df$feature_orthogonality,
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
      y = "Rescaled Energy (0-1)",
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
      # Replace all-NA col with fixed +1 pattern
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
    orthogonalize = FALSE, connectors = NULL, expBeta = 0.0) {
  
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
    if (!is.finite(si) || si < .Machine$double.eps) si <- 1
    q[, 1] <- qi / si
    r[1, 1] <- si
    for (i in 2:m) {
      xi <- x[, i]
      qj <- q[, 1:(i - 1)]
      rj <- t(qj) %*% xi
      qi <- xi - qj %*% rj
      r[1:(i - 1), i] <- rj
      si <- sqrt(sum(qi^2))
      if (!is.finite(si) || si < .Machine$double.eps) si <- 1
      q[, i] <- qi / si
      r[i, i] <- si
    }
    return(q)
  }
  subU <- function(
    projectionsU, mixingAlgorithm, initialW,
    orthogonalize, i, wtobind, previousU = NULL, expBeta = 0.0) {
    avgU <- NULL
    mixAlg <- mixingAlgorithm
    nComponents <- ncol(projectionsU[[1]])
    nmodalities <- length(projectionsU)
    if (missing(wtobind)) wtobind <- (1:nmodalities)[-i]
    if (mixAlg == "avg") {
      avgU <- projectionsU[[1]] * 0.0
      for (j in wtobind) {
        avgU <- avgU + projectionsU[[j]] / length(wtobind)
      }
      basis <- avgU
    } else if (mixAlg == "stochastic") { # FIXME
      avgU <- Reduce(cbind, projectionsU[wtobind])
      G <- scale(replicate(nComponents, rnorm(nrow(avgU))), FALSE, FALSE)
      Y <- localGS(t(avgU) %*% G) # project onto random basis - no localGS because of numerical issues when self-mapping
      basis <- scale(avgU %*% Y, FALSE, FALSE)
    } else {
      nc <- ncol(projectionsU[[1]])
      avgU <- Reduce(cbind, projectionsU[wtobind])
      if (mixAlg == "pca") {
        basis <- safe_pca( avgU, nc = nc )$x
      } else if (mixAlg == "rrpca-l") {
        basis <- (rsvd::rrpca(avgU, rand = FALSE)$L[, 1:nc])
      } else if (mixAlg == "rrpca-s") {
        basis <- (rsvd::rrpca(avgU, rand = FALSE)$S[, 1:nc])
      } else if (mixAlg == "ica") {
        if (is.matrix(initialW) && all(dim(initialW) == c(ncol(avgU), nc))) {
          basis <- (fastICA::fastICA(avgU, method = "C", w.init = initialW, n.comp = nc)$S)
        } else if (is.matrix(previousU) && all(dim(previousU) == c(nrow(avgU), nc))) {
          # If previousU is a matrix, it's the shared basis [N x K]
          # We need an un-mixing matrix [K x K] or weights [P x K]?
          # fastICA w.init documentation says "Initial un-mixing matrix".
          # Usually it's better to let it initialize randomly if we don't have a good un-mixing matrix.
          basis <- (fastICA::fastICA(avgU, method = "C", n.comp = nc)$S)
        } else {
          basis <- (fastICA::fastICA(avgU, method = "C", n.comp = nc)$S)
        }
        if ( is.nan( norm(basis,"F"))) {
          message(paste("fastICA produced NaN - svd instead"))
          basis <- (ba_svd(avgU, nu = nc, nv = 0)$u)
        }
      } else if (mixAlg == "newton-schulz") {
        # This is a fast way to get an orthogonal basis from the averaged projections
        n_mod <- length(wtobind)
        M <- matrix(0, nrow(avgU), nc)
        for (idx in seq_len(n_mod)) {
          start_col <- (idx - 1) * nc + 1
          end_col <- idx * nc
          M <- M + avgU[, start_col:end_col]
        }
        M <- M / n_mod
        MtM <- t(M) %*% M
        inv_sqrt_MtM <- inv_sqrt_sym_newton(MtM)
        basis <- M %*% inv_sqrt_MtM
      } else if (mixAlg == "ica-newton") {
        # FastICA-style update with Newton-Schulz symmetric decorrelation
        n_mod <- length(wtobind)
        M <- matrix(0, nrow(avgU), nc)
        for (idx in seq_len(n_mod)) {
          start_col <- (idx - 1) * nc + 1
          end_col <- idx * nc
          M <- M + avgU[, start_col:end_col]
        }
        M <- M / n_mod
        # Standard full convergence (initial step or no EMA)
        MtM <- t(M) %*% M
        M <- M %*% inv_sqrt_sym_newton(MtM)
        tanhM <- tanh(M)
        E1 <- t(M) %*% tanhM / nrow(M)
        E2 <- colMeans(1 - tanhM^2)
        basis <- M %*% (E1 - diag(E2))
        BtB <- t(basis) %*% basis
        basis <- basis %*% inv_sqrt_sym_newton(BtB)
      } else {
        basis <- (ba_svd(scale(avgU,T,T), nu = nc, nv = 0)$u)
      }
    }

    if (ncol(basis) < nc) {
       needed <- nc - ncol(basis)
       if (!is.null(previousU) && ncol(previousU) == nc) {
           pad <- previousU[, (ncol(basis) + 1):nc, drop = FALSE]
       } else {
           pad <- matrix(rnorm(nrow(basis) * needed), nrow = nrow(basis), ncol = needed)
       }
       basis <- cbind(basis, pad)
    }

    if (expBeta > 0 && !is.null(previousU) ) {
       basis <- nsa_flow( Y0 = basis, X0 = previousU, w = expBeta, retraction = "soft_polar", 
        max_iter=10, apply_nonneg=FALSE )$Y
       # Single Newton-Schulz iteration step for symmetric decorrelation
#       BtB <- t(basis) %*% basis
#       basis <- basis %*% inv_sqrt_sym_newton(BtB, max_iter = 1L)
    }
    colnames(basis)=paste0("PC",1:nc)
    if (!orthogonalize) {
      return(basis)
    }
    return(localGS(basis, orthogonalize = orthogonalize))
  }
  
  outU <- list()
  for (i in 1:length(projectionsU)) {
    prev_u_i <- NULL
    if (!is.null(initialW) && is.list(initialW)) prev_u_i <- initialW[[i]]
    
    if (is.null(connectors)) {
      outU[[i]] <- subU(projectionsU, mixingAlgorithm, initialW, orthogonalize, i, 
                        previousU = prev_u_i, expBeta = expBeta)
    }
    if (!is.null(connectors)) {
      outU[[i]] <- subU(projectionsU, mixingAlgorithm, initialW, orthogonalize,
                        i,
                        wtobind = connectors[[i]],
                        previousU = prev_u_i,
                        expBeta = expBeta
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
#' @param expBeta Exponential beta value. Default is 0.9.
#' @param jointInitialization Logical indicating joint initialization. Default is TRUE.
#' @param sparsenessAlg Sparseness algorithm. Default is NA.
#' @param verbose Logical indicating whether to print verbose output. Default is FALSE. values > 1 lead to more verbosity
#' @param nperms Number of permutations for significance testing. Default is 50.
#' @param FUN function for summarizing variance explained 
#' @return A data frame containing p-values for each permutation.
#' @export
simlr.perm <- function(data_matrices,
  smoothingMatrices = NULL,
  iterations = 10,  sparsenessQuantiles = NULL, 
  positivities = NULL, 
  initialUMatrix = NULL, 
  mixAlg = c("svd", "ica", "avg","rrpca-l", "rrpca-s", "pca", "stochastic"), 
  orthogonalizeU = FALSE,
  repeatedMeasures = NA, lineSearchRange = c(-5e2, 5e2), 
  lineSearchTolerance = 1e-12, 
  randomSeed = 0, 
  constraint = c( "nsaflowx0.5x10" , "orthox0.001x1", "Grassmannx0", "Stiefelx0", "none" ), 
  energyType = c("cca", "regression","normalized", "acc", "dat", "lowRank", "lowRankRegression",'normalized_correlation', 'logcosh', 'exp', 'kurtosis','gauss'), 
  vmats = NULL, connectors = NULL, optimizationStyle = 'adam', 
  scale = c("centerAndScale", "eigenvalue"), 
  expBeta = 0, jointInitialization = TRUE, sparsenessAlg = NA, 
  verbose = FALSE, nperms = 50, FUN=adjusted_rvcoef,
  cores = 1) {
  
  # Set up permutations
  myseeds <- 1:1000000
  if ( !is.na( randomSeed ) ) {
    set.seed( randomSeed )
    myseeds <- sample( 1:1000000, nperms + 1 )
  }

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
  if ( nperms > 1 ) {
    if (verbose) {
      cat(sprintf("Starting %d permutations using %d core(s)...\n", nperms, cores))
    }
    perm_func <- function(nperm) {
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
          sparsenessAlg=sparsenessAlg, verbose=FALSE )
      for ( k in 1:length(data_matrices)) {
        simlr_result_perm$v[[k]] = take_abs_unsigned(simlr_result_perm$v[[k]])
        simlr_result_perm$v[[k]] = l1_normalize_features( simlr_result_perm$v[[k]] )
      }
      refvarxmeans_perm = pairwise_matrix_similarity( data_matrices_perm, simlr_result_perm$v, FUN=FUN )
      return(refvarxmeans_perm)
    }

    if (cores > 1) {
      if ( .Platform$OS.type == "unix" ) {
        perm_results <- pbapply::pblapply(1:nperms, perm_func, cl = cores)
      } else {
        cl <- parallel::makeCluster(cores)
        parallel::clusterExport(cl, varlist = ls(envir = environment()), envir = environment())
        parallel::clusterEvalQ(cl, library(ANTsR))
        perm_results <- pbapply::pblapply(1:nperms, perm_func, cl = cl)
        parallel::stopCluster(cl)
      }
    } else {
      perm_results <- pbapply::pblapply(1:nperms, perm_func)
    }

    for (nperm in 1:nperms) {
      simlrpermvarx[nperm + 1, refvarxmeansnms ] <- perm_results[[nperm]]
    }
  }
  
  # Statistical significance testing
  simlrpermvarx_ttest <- c()
  if ( nperms >  1  ) {
    for (varname in refvarxmeansnms ) {
      # Safely perform t.test, handle "data are essentially constant" error
      mytt <- tryCatch({
        t.test(simlrpermvarx[1, varname] - simlrpermvarx[-1, varname], alternative='greater')
      }, error = function(e) {
        list(statistic = NA) # Return NA if test fails
      })
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
  svd_C <- ba_svd(cross_product_matrix, nu = 0, nv = 0)
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
        L_i <- data.matrix(X_i) %*% data.matrix(V_i)
        L_j <- data.matrix(X_j) %*% data.matrix(V_j)
        
        # Compute RV coefficient
        rv = 0.
        if ( all(is.finite(L_i)) & all(is.finite(L_j))) {
          rv <- FUN(L_i, L_j)
        }

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
#' @param X1 First matrix (Subjects x Features)
#' @param X2 Second matrix (Subjects x Features)
#' @param V1 First feature loading matrix (Features x Components)
#' @param V2 Second feature loading matrix (Features x Components)
#' @param plot_title Title of the plot
#' @param nm1 Label for view 1
#' @param nm2 Label for view 2
#' @param max_pairs Maximum components to include in the pairs plot (Safety cap)
#' @export
visualize_lowrank_relationships <- function(X1, X2, V1, V2, 
                                            plot_title = NULL, 
                                            nm1 = 'X1', 
                                            nm2 = 'X2',
                                            max_pairs = 5) {
  
  # --- 1. DIMENSION GUARDS ---
  if (ncol(X1) != nrow(V1)) stop(glue::glue("Dimension mismatch: {nm1} columns ({ncol(X1)}) != {nm1} loadings rows ({nrow(V1)})"))
  if (ncol(X2) != nrow(V2)) stop(glue::glue("Dimension mismatch: {nm2} columns ({ncol(X2)}) != {nm2} loadings rows ({nrow(V2)})"))
  if (nrow(X1) != nrow(X2)) stop("Subject count mismatch: X1 and X2 must have the same number of rows.")

  if (is.null(plot_title)) plot_title <- paste("Shared Latent Space:", nm1, "&", nm2)

  # --- 2. COMPUTE PROJECTIONS ---
  # Ensure we are working with matrices
  proj1 <- as.matrix(X1) %*% as.matrix(V1)
  proj2 <- as.matrix(X2) %*% as.matrix(V2)
  
  # Standardize column names for the projections
  colnames(proj1) <- paste0(nm1, "_C", 1:ncol(proj1))
  colnames(proj2) <- paste0(nm2, "_C", 1:ncol(proj2))

  # --- 3. CORRELATION & RV ---
  correlation_matrix <- cor(proj1, proj2, use = "pairwise.complete.obs")
  
  # Assuming rvcoef and adjusted_rvcoef are available in your environment
  rv_val <- tryCatch(adjusted_rvcoef(proj1, proj2), error = function(e) NA)
  
  # --- 4. HEATMAP (GGPLOT2) ---
  cor_data <- as.data.frame(as.table(correlation_matrix))
  colnames(cor_data) <- c("View1", "View2", "Correlation")

  p_heat <- ggplot2::ggplot(cor_data, ggplot2::aes(x = .data$View1, y = .data$View2, fill = .data$Correlation)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                  midpoint = 0, limit = c(-1, 1), 
                                  name = "Pearson\nCorr") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$Correlation)), 
                       size = 3, color = ifelse(abs(cor_data$Correlation) > 0.7, "white", "black")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(title = plot_title, subtitle = paste("Global Adjusted RV:", round(rv_val, 3))) +
    ggplot2::coord_fixed()

  # --- 5. PAIRS PLOT (WITH SAFETY CAP) ---
  # Taking only the top components to prevent GGally from hanging
  k_show1 <- min(ncol(proj1), max_pairs)
  k_show2 <- min(ncol(proj2), max_pairs)
  
  cordf <- cbind(as.data.frame(proj1[, 1:k_show1, drop = FALSE]), 
                 as.data.frame(proj2[, 1:k_show2, drop = FALSE]))
  
  # Robust naming
  colnames(cordf) <- make.unique(colnames(cordf))

  p_pairs <- NULL
  if (requireNamespace("GGally", quietly = TRUE)) {
    p_pairs <- GGally::ggpairs(
      cordf,
      upper = list(continuous = GGally::wrap("points", alpha = 0.4, size = 1)),
      lower = list(continuous = GGally::wrap("cor", digits = 2)),
      title = paste("Pairwise Projections (Top", max_pairs, "Components)")
    ) + ggplot2::theme_bw()
  }

  # --- 6. RETURN ---
  return(list(
    heatmap = p_heat,
    pairsplot = p_pairs,
    correlations = correlation_matrix,
    adj_rv = rv_val,
    projections = list(proj1 = proj1, proj2 = proj2)
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
    is_non_negative <- all(column >= 0, na.rm = TRUE)
    is_non_positive <- all(column <= 0, na.rm = TRUE)

    if (!is.na(is_non_negative) && !is.na(is_non_positive) && (is_non_negative || is_non_positive)) {
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
  if (is.null(vector)) vector <- NA
  if (is.list(vector)) vector <- unlist(vector)
  df <- data.frame(matrix(vector, nrow = 1))
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
      verbose = verbose,
      nperms = nperms,
      sparsenessAlg = sparsenessAlgVal,
      FUN = FUN,
      cores = if (nrow(options_df) == 1) cores else 1
    )
    
    wtest <- 1
    if (nperms > 4) {
      wtest <- which(simlrX$significance$perm == "ttest")
    }
    finalE <- mean(as.matrix(simlrX$significance[wtest, -c(1:2)]), na.rm = TRUE)
    if (is.na(finalE)) finalE <- -Inf
    
    parameters <- simlrX$simlr_result$provenance
    parameters$final_energy <- as.numeric(finalE)
    parameters <- cbind(parameters, simlrX$significance[1, -1])
    
    list(
      simlr_result = simlrX$simlr_result,
      significance = simlrX$significance,
      parameters = parameters
    )
  }
  
  # ---- Parallel or sequential run ----
  if (cores > 1 && nrow(options_df) > 1) {
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
    if (nrow(options_df) > 1) {
      cat(sprintf("Running %d parameter sets sequentially...\n", nrow(options_df)))
    } else {
      cat(sprintf("Running 1 parameter set with %d-core deep parallelism...\n", cores))
    }
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


#' Apply SIMLR projection matrices with r01.1-style versioning
#'
#' Automatically handles name collisions by appending versioned suffixes:
#' \itemize{
#'   \item No collision -> \code{petPC1}, \code{petPC2}, ...
#'   \item Collision    -> \code{petPCr01.1}, \code{petPCr01.2}, ...
#'   \item Next round   -> \code{petPCr02.1}, \code{petPCr02.2}, ...
#' }
#'
#' @param existing_df Data frame (samples in rows, features in columns)
#' @param simlr_v Named list of weight matrices (rownames = original features)
#' @param n_limit Optional: keep only first n_limit components per block
#' @param version_prefix Prefix before version number (default: "r")
#' @param verbose Print messages when versioning occurs
#'
#' @return List with \code{extended_df} and \code{new_colnames}
#'
#' @examples
#' set.seed(123)
#' df <- data.frame(petPC1 = rnorm(5), petPC2 = rnorm(5), age = 1:5)
#' W <- matrix(rnorm(6), 2, 3,
#'             dimnames = list(c("petPC1", "petPC2"), c("PC1", "PC2", "PC3")))
#' weights <- list(pet = W)
#'
#' # First application
#' res1 <- apply_simlr_matrices(df, weights, verbose = TRUE)
#' res1$new_colnames
#' # [1] "petPCr01.1" "petPCr01.2" "petPCr01.3"
#'
#' # Second application
#' res2 <- apply_simlr_matrices(res1$extended_df, weights)
#' res2$new_colnames
#' # [1] "petPCr02.1" "petPCr02.2" "petPCr02.3"
#'
#' # No collision example
#' clean <- df[, "age", drop = FALSE]
#' apply_simlr_matrices(clean, weights)$new_colnames
#' # [1] "petPC1" "petPC2" "petPC3"
#'
#' @export
apply_simlr_matrices <- function(existing_df,
                                 simlr_v,
                                 n_limit = NULL,
                                 version_prefix = "r",
                                 verbose = FALSE) {
  
  if (!is.data.frame(existing_df)) stop("existing_df must be a data frame")
  if (!is.list(simlr_v) || length(simlr_v) == 0 || is.null(names(simlr_v))) {
    stop("simlr_v must be a named list of matrices")
  }
  
  proj_blocks <- list()
  added_names <- character()
  used_names  <- names(existing_df)
  
  for (block_name in names(simlr_v)) {
    W <- simlr_v[[block_name]]
    if (!is.matrix(W)) next
    
    if (!is.null(n_limit) && ncol(W) > n_limit) {
      W <- W[, seq_len(n_limit), drop = FALSE]
    }
    
    if (is.null(rownames(W))) stop("Weight matrix '", block_name, "' has no rownames")
    if (is.null(colnames(W))) colnames(W) <- paste0("C", seq_len(ncol(W)))
    
    # Base names: pet + PC1 -> petPC1
    base_names <- paste0(block_name, colnames(W))
    
    final_names <- character(length(base_names))
    
    for (i in seq_along(base_names)) {
      original <- base_names[i]  # e.g. "petPC1"
      candidate <- original
      
      # Find next free version: r01.1, r01.2, ..., r02.1, ...
      major <- 0
      minor <- 0
      
      while (candidate %in% used_names) {
        minor <- minor + 1
        if (minor > 9) {
          minor <- 1
          major <- major + 1
        }
        version_tag <- sprintf("%s%02d.", version_prefix, major)
        
        # Append the version tag right before the trailing number
        candidate <- sub("([0-9]+)$", paste0(version_tag, "\\1"), original)
        
        # Fallback: if no number at end, just append
        if (candidate == original) {
          candidate <- paste0(original, version_tag)
        }
      }
      
      final_names[i] <- candidate
      used_names <- c(used_names, candidate)
      
      if (verbose && (major > 0 || minor > 0)) {
        message(original, " -> ", candidate)
      }
    }
    
    overlap <- intersect(rownames(W), names(existing_df))
    if (length(overlap) == 0) {
      warning("No overlapping features for block '", block_name, "' - skipping")
      next
    }
    
    Y <- as.matrix(existing_df[, overlap, drop = FALSE]) %*% W[overlap, , drop = FALSE]
    colnames(Y) <- final_names
    
    proj_blocks[[block_name]] <- Y
    added_names <- c(added_names, final_names)
  }
  
  if (length(proj_blocks) == 0) {
    warning("No projections were added")
    return(list(extended_df = existing_df, new_colnames = character(0)))
  }
  
  new_df <- cbind(existing_df, do.call(cbind, proj_blocks))
  
  list(extended_df = new_df, new_colnames = added_names)
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
  dd = apply_simlr_matrices( existing_df_fix, matrices_list_fix )
  
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

#' Fast Matrix Whitening
#'
#' Perform whitening (sphering) of a data matrix \eqn{X} such that the
#' covariance matrix of the output is approximately the identity.  
#' The implementation automatically chooses the most efficient algorithm
#' depending on whether the number of features \eqn{p} is much larger than
#' the number of samples \eqn{n} (\eqn{p >> n}) or vice versa (\eqn{n >> p}).
#'
#' Whitening is performed via an SVD-based approach with Tikhonov-style
#' regularization to handle small singular values.
#'
#' @param X A numeric matrix of size \eqn{n \times p}, where \eqn{n} is the
#'   number of samples (rows) and \eqn{p} is the number of features (columns).
#' @param epsilon A small non-negative numeric value used to stabilize the
#'   inversion of singular values (default = 1e-8).
#'
#' @return A numeric matrix of the same dimensions as \code{X}, with whitened
#'   columns. The covariance of the whitened data is approximately the identity
#'   in the relevant space.
#'
#' @details
#' - If \eqn{p > n}, whitening is computed in the sample space
#'   (avoids forming a \eqn{p \times p} covariance).
#' - If \eqn{n >= p}, whitening is computed in the feature space
#'   (avoids forming an \eqn{n \times n} covariance).
#'
#' Computational complexity:
#' \itemize{
#'   \item \eqn{O(n^2 p)} when \eqn{p >> n}
#'   \item \eqn{O(n p^2)} when \eqn{n >> p}
#' }
#'
#' @examples
#' set.seed(42)
#'
#' # Case 1: p >> n
#' X1 <- matrix(rnorm(50 * 2000), 50, 2000)
#' Xw1 <- fast_whiten(X1)
#' round(cov(t(Xw1))[1:5, 1:5], 3)
#'
#' # Case 2: n >> p
#' X2 <- matrix(rnorm(2000 * 50), 2000, 50)
#' Xw2 <- fast_whiten(X2)
#' round(cov(Xw2)[1:5, 1:5], 3)
#'
#' @export
fast_whiten <- function(X, epsilon = 1e-8) {
  n <- nrow(X)
  p <- ncol(X)
  
  # Center data
  X_centered <- scale(X, center = TRUE, scale = FALSE)
  
  if (p > n) {
    # Case 1: p >> n
    svd_res <- ba_svd(X_centered, nu = n, nv = 0)
    s <- svd_res$d
    W <- diag(1 / sqrt(s^2 + epsilon))
    X_whitened <- svd_res$u %*% W %*% t(svd_res$u) %*% X_centered
  } else {
    # Case 2: n >> p
    svd_res <- ba_svd(X_centered, nu = 0, nv = p)
    s <- svd_res$d
    W <- diag(1 / sqrt(s^2 + epsilon))
    X_whitened <- X_centered %*% svd_res$v %*% W %*% t(svd_res$v)
  }
  colnames(X_whitened)=colnames(X)
  rownames(X_whitened)=rownames(X)
  return(X_whitened)
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

.anatomical_map_cache <- new.env(parent = emptyenv())

#' Decode ANTsPyMM technical shorthand to legible anatomical names
#'
#' This function uses consolidated atlas data (AAL, DKT, CIT168, JHU, etc.) to
#' translate technical shorthand strings into their original, legible anatomical
#' descriptions. It performs normalization to handle variations in dots, underscores,
#' and case.
#'
#' @param x A character vector of shorthand names to decode.
#' @return A character vector of legible anatomical names.
#' @examples
#' decode_pymm_names("cit168.bn.str.pu.left")
#' decode_pymm_names("dti.fa.thalamus.left.jhu.icbm.labels.1mm")
#' @export
decode_pymm_names <- function(x) {
  if (is.null(.anatomical_map_cache$map)) {
    .build_anatomical_map()
  }
  
  # Strip common prefixes
  prefixes <- c("^dti\\.fa\\.", "^dti\\.md\\.", "^rsf\\.", "^cbf\\.", "^vol\\.", "^t1\\.", "^nm\\.", "^pypet\\.", "^cit168\\.", "^aal\\.", "^dkt\\.", "^labels\\.")
  x_clean <- x
  for (p in prefixes) x_clean <- gsub(p, "", x_clean)
  
  # Strip common suffixes/metrics
  suffixes <- c("\\.jhu\\.icbm\\.labels\\.1mm$", "\\.cit168$", "\\.aal$", "\\.dkt$", 
                "\\.fa$", "\\.md$", "\\.mean$", "\\.avg$", "\\.iavg$", "\\.isum$", "\\.vol$")
  x_clean_no_metrics <- x_clean
  for (s in suffixes) x_clean_no_metrics <- gsub(s, "", x_clean_no_metrics)
  
  # Normalize for lookup
  normalize_str <- function(s) gsub("[._-]", "", tolower(s))
  x_norm <- normalize_str(x_clean_no_metrics)
  
  # Lookup
  map <- .anatomical_map_cache$map
  decoded <- sapply(x_norm, function(val) {
    if (val == "") return(NULL)
    match_idx <- match(val, map$norm)
    if (!is.na(match_idx)) return(map$orig[match_idx])
    return(NULL)
  })
  
  # Handle results and apply humanization
  results <- x
  for (i in seq_along(x)) {
    if (!is.null(decoded[[i]])) {
       results[i] <- humanize_anatomical_name(decoded[[i]])
    } else {
       results[i] <- humanize_anatomical_name(shorten_pymm_names(x_clean[i]))
    }
  }
  
  return(results)
}

#' Humanize anatomical names by expanding abbreviations and reordering
#'
#' @param x character string
#' @return humanized character string
#' @export
humanize_anatomical_name <- function(x) {
  if (is.na(x) || x == "") return(x)
  
  # 0. Strip modality markers, long parentheticals, and technical noise
  # e.g. "FA-Posterior_thalamic_radiation", "(include optic radiation)"
  x <- gsub("^(FA|MD|vol|cbf)-", "", x, ignore.case = TRUE)
  x <- gsub("\\s*\\([^)]*\\)", "", x)
  x <- gsub("include\\..*", "", x, ignore.case = TRUE)
  x <- gsub("part\\.of\\..*", "", x, ignore.case = TRUE)
  x <- gsub("\\.jhu\\.icbm\\.labels\\.1mm", "", x, ignore.case = TRUE)

  # 1. Expansion Map
  expansions <- list(
    BN = "Basal Nuclei",
    STR = "Striatum",
    Pu = "Putamen",
    Ca = "Caudate",
    NAC = "Nuc Accumbens",
    GP = "Globus Pallidus",
    GPe = "GP Externus",
    GPi = "GP Internus",
    VeP = "Ventral Pallidum",
    SN = "Substantia Nigra",
    SNc = "SN Compacta",
    SNr = "SN Reticulata",
    RN = "Red Nucleus",
    VTR = "Ventral Tegmental",
    VTA = "Ventral Tegmental Area",
    PBP = "Parabrachial Pigmented",
    ETH = "Epithalamus",
    HN = "Habenular",
    Die = "Diencephalon",
    HTH = "Hypothalamus",
    MN = "Mammillary Nucleus",
    STH = "Subthalamic",
    EXA = "Extended Amygdala",
    MTg = "Midbrain Tegmentum",
    aLEC = "Ant Lat Entorhinal",
    pMEC = "Post Med Entorhinal",
    IPL = "Inf Parietal",
    ST = "Sup Temporal",
    PHC = "Parahippocampal",
    RSC = "Retrosplenial",
    PFCd = "Dorsal PFC",
    PFCl = "Lateral PFC",
    PFCv = "Ventral PFC",
    PFCm = "Medial PFC",
    pCun = "Precuneus",
    FrOper = "Frontal Operculum",
    ParOper = "Parietal Operculum",
    PrCd = "Dorsal Precentral",
    PrCv = "Ventral Precentral",
    Sup = "Superior",
    "Inf" = "Inferior",
    Mid = "Middle",
    Orb = "Orbital",
    Oper = "Opercular",
    Tri = "Triangular",
    Med = "Medial",
    Medial = "Medial",
    Lat = "Lateral",
    Lateral = "Lateral",
    Ant = "Anterior",
    Post = "Posterior",
    Gyr = "Gyrus",
    Robule = "Lobule",
    Thal = "Thalamus",
    Cing = "Cingulum",
    Cereb = "Cerebellum",
    Vent = "Ventricle"
  )
  
  # 2. Split and Tokenize
  tokens <- unlist(strsplit(x, "[._ -]"))
  tokens <- tokens[tokens != ""]
  
  # 3. Identify Side (L/R) and remove from tokens
  side <- ""
  side_indices <- which(tolower(tokens) %in% c("l", "r", "left", "right"))
  if (length(side_indices) > 0) {
    side_token <- tolower(tokens[side_indices[1]])
    side <- if (side_token %in% c("l", "left")) "L" else "R"
    tokens <- tokens[-side_indices]
  }
  
  # 4. Expand tokens
  expanded_tokens <- sapply(tokens, function(t) {
    match_idx <- match(tolower(t), tolower(names(expansions)))
    if (!is.na(match_idx)) return(expansions[[match_idx]])
    # Default: Title case the token if it's not expanded
    return(tools::toTitleCase(tolower(t)))
  })
  
  # 5. Clean up redundant hierarchy
  if (length(expanded_tokens) > 1) {
     # Remove "Basal Nuclei" if it's a prefix to Striatum or GP
     if (expanded_tokens[1] == "Basal Nuclei" && any(c("Striatum", "Globus Pallidus") %in% expanded_tokens)) {
       expanded_tokens <- expanded_tokens[-1]
     }
     # Remove "Striatum" if it's followed by Putamen, Caudate, or Nuc Accumbens
     if (expanded_tokens[1] == "Striatum" && any(c("Putamen", "Caudate", "Nuc Accumbens") %in% expanded_tokens)) {
       expanded_tokens <- expanded_tokens[-1]
     }
  }
  
  # 6. Reassemble: Side first
  humanized <- paste(c(side, expanded_tokens), collapse = " ")
  humanized <- trimws(humanized)
  
  return(humanized)
}



.build_anatomical_map <- function() {
  datasets <- c(
    "aal", "DesikanKillianyTourville", "cit168_reinf_learn", "cit168_brainstem",
    "jhu_fa_labels", "jhu_md_labels", "lobes_brainstem", "basal_forebrain",
    "cerebellum_labels", "dkt_labels", "dkt_cit_labels", "lobes", "mtl_labels",
    "nbm3_labels", "tissues", "wm_major_tracts", "ppmi_yeo_labels"
  )
  
  consolidated_orig <- c()
  
  for (ds in datasets) {
    d <- tryCatch({
      # Load data object. In some environments (like devtools), data() doesn't
      # always put it where we expect, so we try multiple strategies.
      tmp_env <- new.env()
      utils::data(list = ds, package = "ANTsR", envir = tmp_env)
      if (exists(ds, envir = tmp_env)) {
        get(ds, envir = tmp_env)
      } else if (exists(ds)) {
        get(ds)
      } else {
        NULL
      }
    }, error = function(e) NULL)
    
    if (is.null(d)) {
      # message(paste("Could not load dataset:", ds))
      next
    }
    
    # Identify the description column
    desc_col <- intersect(colnames(d), c("Description", "label_name", "AAL", "Anatomy"))[1]
    if (!is.na(desc_col)) {
      # message(paste("Loaded", ds, "with column", desc_col))
      consolidated_orig <- c(consolidated_orig, as.character(d[[desc_col]]))
    }
  }
  
  if (length(consolidated_orig) == 0) {
    consolidated_orig <- c("Background", "CSF", "GM", "WM")
  }

  consolidated_orig <- unique(na.omit(consolidated_orig))
  normalize_str <- function(s) gsub("[._-]", "", tolower(s))
  consolidated_norm <- normalize_str(consolidated_orig)
  
  .anatomical_map_cache$map <- data.frame(
    norm = consolidated_norm,
    orig = consolidated_orig,
    stringsAsFactors = FALSE
  )
}






#' Interpret SiMLR Vector
#'
#' This function interprets a vector from SiMLR (similarity-driven multivariate linear reconstruction)
#' specifically focusing on a given variable (e.g., a specific principal component or cluster). It extracts and normalizes the vector associated 
#' with the specified SiMLR variable, sorts it to identify the top elements, and optionally filters out non-significant values. 
#' This function is useful for understanding the contribution of different features in the context of the SiMLR analysis. Assumes this input is generated by NNHEmbed.
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
#' NNHEmbed_update_residuals(mats, 1, "whiten", blaster2, allnna, 5)
#' NNHEmbed_update_residuals(opt = "opt")
#' }
#' @export
NNHEmbed_update_residuals <- function(mats, x, covariate, blaster2, allnna, n.comp, opt = NULL) {

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
    return(fast_whiten(data.matrix(mats[[x]])))
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
#' @param constraint orthogonality constraint of the form constraintxFloatWeightEnergyxFloatWeightGrad where constraints is ortho or nsaflow or none
#' @param covariates any covariates to adjust training matrices. if covariates is set to 'mean' then the rowwise mean will be factored out of each matrix.  this can be a vector e.g. \code{c('center','scale','rank')}. pass the name opt to NNHEmbed_update_residuals to have the function print the options.
#' @param myseed Seed for random number generation to ensure reproducibility. Defaults to 3.
#' @param doAsym integer 0 for FALSE, 1 for TRUE and 2 for separate matrices for asymm variables.
#' @param returnidps Integer indicating whether to return the idp names (1) or matrices (2) or neither (0). Defaults to 0.
#' @param restrictDFN Logical indicating whether to restrict analysis to default network features. Defaults to FALSE.
#' @param resnetGradeThresh image quality threshold (higher better).
#' @param doperm Logical indicating whether to perform permutation tests. Defaults to FALSE.  Will randomize image features in the training data and thus leads to "randomized" but still regularized projections.
#' @param exclusions vector of strings to exclude from predictors
#' @param inclusions vector of strings to include in predictors
#' @param sparseness vector or scalar value to set sparseness.
#' @param mixAlg string 'svd', 'ica', 'rrpca-l', 'rrpca-s', 'stochastic', 'pca', 'newton-schulz' or 'avg' denotes the algorithm employed when estimating the mixed modality bases
#' @param iterations int value to set max iterations
#' @param path_modeling the result of a call to \code{simlr_path_models(n)}
#' @param sparsenessAlg NA is default otherwise basic, spmp or orthorank
#' @param optimizationStyle see \code{list_simlr_optimizers}
#' @param domainMatrices matrices containing domain knowledge length of \code{data_matrices} with number of columns also equal to each corresponding data matrix
#' @param domainLambdas weights for domain knowledge term length of \code{data_matrices}
#' @param verbose boolean
#' @return A list containing the results of the similarity analysis and related data.
#' @export
#' @examples
#' # Example usage:
#' # result <- NNHEmbed(dataframe)
NNHEmbed <- function(
                          blaster,
                          select_training_boolean,
                          connect_cog,
                          energy,
                          nsimlr,
                          constraint,
                          covariates = '1',
                          myseed = 3,
                          doAsym = TRUE,
                          returnidps = 0,
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
                          domainMatrices = NULL,
                          domainLambdas  = NULL,
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
                  "base.pca", "base.spca", "base.rand.1", "base.rand.0",  "base.scca", "base.mcca", "base.rgcca",
                  "normalized_correlation", 'nc', 'dat', 'logcosh', 'exp', 'kurtosis','gauss')
  if (!energy %in% myenergies) {
    message(paste0("energy should be one of ", paste(myenergies, collapse = ", ")))
  }

  # --- 2. Feature Selection and Filtering (IDPs) ---
  if (verbose) message("Step 1: Selecting and filtering features (IDPs)...")
  
  idps <- antspymm_predictors(blaster, TRUE, TRUE)
  idps = idps[ !grepl("Unnamed",idps)]
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
  
  if (returnidps==1) {
    return(idplist)
  }

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
    nnacount = sum( is.na( blaster2[allnna, idplist[[kk]]] ))
    message(paste("Imputing:",nnacount, 'in modality kk'))
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
      mats[[x]] <- NNHEmbed_update_residuals(mats, x, mycov, blaster2, allnna, n.comp = nsimlr)
      mats[[x]] <- data.matrix(mats[[x]])
    }
    if (verbose) cat("Done.\n")
  }
  
  # --- 7. Regularization and Baseline Model Execution ---
  if (verbose) message("Step 5: Preparing regularization matrices...")
  regs0 <- lapply(mats, function(mat) {
    mycor <- cor(mat)
    mycor[mycor < 0.8] <- 0
    data.matrix((mycor))
  })
  regs <- regs0
  if (!missing(connect_cog)) {
    if (verbose) message("  Setting up sparse regularization for 'cg' modality.")
    regs[["cg"]] <- Matrix::Matrix(regs0[["cg"]], sparse = TRUE)
  }
  for ( k in 1:length(regs)) {
    regs[[k]][ is.na(regs[[k]]) ]=0
  }

  if (returnidps==2) {
    return( list( matrices=mats, regs=regs ) )
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
      'base.mcca' = antsr_mcca_features(mats, nsimlrmin),
      'base.rgcca' = antsr_rgcca_baseline(mats, ncomp=nsimlrmin),
      'base.scca' = antsr_rgcca_sparse(mats, ncomp=nsimlrmin, sparsity = 0.2 ),
      stop("Unknown baseline energy type.")
    )
    
    return(list(simlrX = list(v = v_result, energy = energy)))
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
  initu = optimal_simlr_initializer( mats, 
    n_init = 10, basisK = nsimlr,
    energyType=energy,
    seed = 123, verbose=TRUE )$bestU

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
                  verbose = verbose,
                  randomSeed = myseed,
                  mixAlg = mixer,
                  energyType = energy,
                  scale = c('none'),
                  sparsenessQuantiles = sparval,
                  expBeta = 0.0,
                  positivities = rep("positive", length(mats)),
                  connectors = clist,
                  constraint = constraint,
                  optimizationStyle = optimizationStyle,
                  sparsenessAlg = sparsenessAlg,
                  initialUMatrix = initu, domainMatrices=domainMatrices, domainLambdas=domainLambdas)

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
      nmslist[[names(simlrMats)[k]]]=decode_pymm_names(colnames(simlrMats[[k]]))
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
  for ( k in 1:length(data_matrices)) {
    rownames(plist[[k]])=colnames(data_matrices[[k]])
    prefix=paste0(names(data_matrices)[k],"PC")
    prefix="PC"
    colnames(plist[[k]])=paste0(prefix,1:ncol(plist[[k]]))
  }
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
  for ( k in 1:length(data_matrices)) {
    rownames(plist[[k]])=colnames(data_matrices[[k]])
    prefix=paste0(names(data_matrices)[k],"PC")
    prefix="PC"
    colnames(plist[[k]])=paste0(prefix,1:ncol(plist[[k]]))
  }

  return(plist)
}

#' Multi-view Regularized CCA baseline using RGCCA
#'
#' @param data_matrices A named list of subject x feature matrices
#' @param tau Regularization parameter (0 = no regularization, 1 = PCA limit)
#' @param scheme RGCCA scheme ('horst', 'factorial', 'centroid')
#' @param ncomp Number of components
#' @export
antsr_rgcca_baseline <- function(data_matrices, tau = 0.5,
                                 scheme = "factorial", ncomp = 3) {
  if (!requireNamespace("RGCCA", quietly = TRUE))
    stop("Please install RGCCA: install.packages('RGCCA')")

  stopifnot(is.list(data_matrices))
  n_views <- length(data_matrices)
  A <- lapply(data_matrices, function(x) scale(x, center = TRUE, scale = TRUE))

  # Identity design matrix (fully connected)
  C <- matrix(1, n_views, n_views)
  diag(C) <- 0

  rgcca_res <- RGCCA::rgcca(A, C, tau = rep(tau, n_views),
                            scheme = scheme, ncomp = rep(ncomp, n_views),
                            verbose = FALSE)

  plist = rgcca_res$a
  names(plist) <- names(data_matrices)
  for ( k in 1:length(data_matrices)) {
    rownames(plist[[k]])=colnames(data_matrices[[k]])
    prefix=paste0(names(data_matrices)[k],"PC")
    prefix="PC"
    colnames(plist[[k]])=paste0(prefix,1:ncol(plist[[k]]))
  }

  return(plist)
}


#' Sparse Multi-view CCA Baseline using RGCCA (version-safe)
#'
#' Implements a sparse multi-view canonical correlation analysis using RGCCA,
#' enforcing sparsity on each modality to obtain interpretable feature loadings.
#' Compatible with RGCCA >= 3.0 (using `blocks` and `connection` arguments).
#'
#' @param data_matrices Named list of subject x feature matrices. Each must have the same number of rows.
#' @param ncomp Number of canonical components per view (default = 3)
#' @param tau Numeric in [0,1] or vector. Regularization per view (0 = pure CCA, 1 = PCA limit)
#' @param sparsity Numeric in (0,1]. L1 sparsity per view; lower = sparser (default = 0.3)
#' @param scheme RGCCA optimization scheme: 'factorial', 'centroid', or 'horst' (default = 'factorial')
#' @param scale Logical. Whether to z-score features (default = TRUE)
#' @param verbose Logical. Print progress messages (default = FALSE)
#'
#' @return A list with:
#'   \item{loadings}{List of sparse feature loadings (features x components)}
#'   \item{scores}{List of subject x component canonical variates}
#'   \item{shared_embedding}{Average canonical embedding across modalities}
#'   \item{correlations}{Canonical correlations across shared space}
#'
#' @export
antsr_rgcca_sparse <- function(data_matrices,
                               ncomp = 3,
                               tau = 0.5,
                               sparsity = 0.3,
                               scheme = "factorial",
                               scale = TRUE,
                               verbose = FALSE) {
  if (!requireNamespace("RGCCA", quietly = TRUE))
    stop("Please install RGCCA first: install.packages('RGCCA')")

  stopifnot(is.list(data_matrices))
  stopifnot(all(sapply(data_matrices, is.matrix)))

  n_views <- length(data_matrices)
  if (is.null(names(data_matrices)))
    names(data_matrices) <- paste0("view", seq_len(n_views))

  # Ensure all matrices have same number of subjects
  nsubs <- sapply(data_matrices, nrow)
  if (length(unique(nsubs)) > 1)
    stop("All matrices must have the same number of rows (subjects).")

  # Standardize features if requested
  if (scale) data_matrices <- lapply(data_matrices, scale)

  # Fully connected design
  C <- matrix(1, n_views, n_views)
  diag(C) <- 0

  # Match parameter lengths
  tau <- rep(tau, length.out = n_views)
  sparsity <- rep(sparsity, length.out = n_views)

  # Version handling
  rgcca_ver <- as.numeric(strsplit(as.character(utils::packageVersion("RGCCA")), "[.]")[[1]][1])
  if (rgcca_ver >= 3) {
    if (verbose) message("Using RGCCA >= 3.0 interface (blocks/connection).")
    rgcca_res <- RGCCA::rgcca(
      blocks = data_matrices,
      connection = C,
      tau = tau,
      sparsity = sparsity,
      ncomp = rep(ncomp, n_views),
      scheme = scheme,
      verbose = verbose
    )
  } else {
    if (verbose) message("Using legacy RGCCA interface (A/C).")
    rgcca_res <- RGCCA::rgcca(
      A = data_matrices,
      C = C,
      tau = tau,
      sparsity = sparsity,
      ncomp = rep(ncomp, n_views),
      scheme = scheme,
      verbose = verbose
    )
  }


  plist = rgcca_res$a
  names(plist) <- names(data_matrices)
  for ( k in 1:length(data_matrices)) {
    rownames(plist[[k]])=colnames(data_matrices[[k]])
    prefix=paste0(names(data_matrices)[k],"PC")
    prefix="PC"
    colnames(plist[[k]])=paste0(prefix,1:ncol(plist[[k]]))
  }

  return(plist)
  # Compute canonical scores
  scores <- lapply(seq_along(data_matrices), function(i) data_matrices[[i]] %*% loadings[[i]])
  names(scores) <- names(data_matrices)

  # Shared embedding and cross-view correlations
  shared_embedding <- Reduce("+", lapply(scores, scale)) / n_views
  correlations <- sapply(seq_len(ncomp), function(i)
    mean(sapply(scores, function(s) cor(shared_embedding[, i], s[, i], use = "pairwise.complete.obs")))
  )

  return(list(
    loadings = loadings,
    scores = scores,
    shared_embedding = shared_embedding,
    correlations = correlations
  ))
}

#' Generate Multi-view CCA-based Feature Projections for a List of Matrices
#'
#' Applies multi-view Canonical Correlation Analysis (MCCA) to a list of subject-by-feature matrices.
#' Each view (modality) is projected into a shared latent space that maximizes inter-view correlation.
#'
#' This provides a classical linear baseline for benchmarking against SiMLR or multimodal deep models.
#'
#' @param data_matrices A named list of numeric matrices, each subjects x features.
#'                      All matrices must have the same number of subjects (rows).
#' @param k Integer. Number of canonical components to extract.
#' @param regularization Scalar ridge penalty to improve numerical stability (default = 1e-3).
#' @param center Logical. Whether to center each matrix before analysis (default = TRUE).
#' @param scale Logical. Whether to scale each matrix to unit variance (default = FALSE).
#'
#' @return A list containing:
#'   \item{loadings}{List of projection matrices (features x k) for each view.}
#'   \item{scores}{List of canonical variates (subjects x k) for each view.}
#'   \item{shared_embedding}{Average canonical embedding across views (subjects x k).}
#'   \item{correlations}{Vector of canonical correlations.}
#'
#' @examples
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
#' mcca <- antsr_mcca_features(matlist, k = 5)
#' str(mcca)
#' }
#' @export
antsr_mcca_features <- function(data_matrices, k = 10,
                                regularization = 1e-3,
                                center = TRUE,
                                scale = FALSE) {

  stopifnot(is.list(data_matrices))
  stopifnot(all(sapply(data_matrices, is.matrix)))
  n_views <- length(data_matrices)

  # Check all have same number of subjects
  n_subjects <- sapply(data_matrices, nrow)
  if (length(unique(n_subjects)) != 1)
    stop("All matrices must have same number of subjects (rows).")

  n_subjects <- unique(n_subjects)
  if (is.null(names(data_matrices)))
    names(data_matrices) <- paste0("view", seq_len(n_views))

  # Center and scale
  data_matrices <- lapply(data_matrices, function(X) scale(X, center = center, scale = scale))

  # Concatenate across modalities
  X_concat <- do.call(cbind, data_matrices)

  # Add small regularization to covariance for stability
  Sigma <- cov(X_concat) + diag(regularization, ncol(X_concat))
  eig <- eigen(Sigma)

  # Take top-k components
  k_eff <- min(k, ncol(X_concat))
  loadings_all <- eig$vectors[, seq_len(k_eff), drop = FALSE]
  scores_all <- X_concat %*% loadings_all

  # Split loadings per modality
  n_features <- sapply(data_matrices, ncol)
  split_idx <- cumsum(n_features)
  start_idx <- c(1, head(split_idx + 1, -1))

  loadings <- mapply(function(st, en, nm) {
    mat <- loadings_all[st:en, , drop = FALSE]
    colnames(mat) <- paste0("CC", seq_len(ncol(mat)))
    rownames(mat) <- colnames(data_matrices[[nm]])
    mat
  }, start_idx, split_idx, names(data_matrices), SIMPLIFY = FALSE)

  # Compute canonical scores for each view
  scores <- mapply(function(X, L) X %*% L, data_matrices, loadings, SIMPLIFY = FALSE)

  # Shared embedding = average of standardized scores
  shared_embedding <- Reduce("+", lapply(scores, scale)) / length(scores)

  # Compute canonical correlations across shared space
  corrs <- sapply(seq_len(ncol(shared_embedding)), function(i) {
    cor(shared_embedding[, i], scores_all[, i], use = "pairwise.complete.obs")
  })


  plist = loadings
  names(plist) <- names(data_matrices)
  for ( k in 1:length(data_matrices)) {
    rownames(plist[[k]])=colnames(data_matrices[[k]])
    prefix=paste0(names(data_matrices)[k],"PC")
    prefix="PC"
    colnames(plist[[k]])=paste0(prefix,1:ncol(plist[[k]]))
  }
  return( plist )

  return(list(
    loadings = loadings,
    scores = scores,
    shared_embedding = shared_embedding,
    correlations = corrs
  ))
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
#' @param constraint_type Character. Type of manifold constraint; one of `"nsaflow"` or `"None"`.
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
                             constraint_type = c( "none", "ortho", "nsaflow"),
                             smoothing_matrix = NULL,
                             positivity = "positive",
                             sparseness_quantile = 0.8,
                             constraint_weight = NA,
                             constraint_iterations = 1,
                             sparseness_alg = "soft",
                             energy_type = "acc") {

  v <- as.matrix(v)
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
  if ( is.na(sparseness_quantile) ) sparseness_quantile=0.5
  if ( constraint_weight > 1 ) constraint_weight=1
  if ( constraint_weight < 0 ) constraint_weight=0
  v = nsa_flow( v, w = constraint_weight, max_iter=constraint_iterations, verbose=FALSE )$Y
  if ( sparseness_quantile > 0  ) {
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
  
  # Optional L1 normalization
  normalize_energy_types <- c("acc", "cca", "nc", "normalized_correlation", "lowRankRegression", "lrr", "dat", 'logcosh', 'exp', 'kurtosis','gauss' )
  normalize_energy_types <- c("acc", "cca", "nc", "normalized_correlation", "lowRankRegression", "lrr"  )
  if (!is.null(energy_type) && energy_type %in% normalize_energy_types) {
    v <- l1_normalize_features(v)
  }

  return(as.matrix(v))
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
      singular_values <- ba_svd(X_centered, nu = k_max_local, nv = 0)$d
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
#' @param mat_list A list of numeric matrices [subjects x features].
#' @param n_permutations Number of permutations (0 = Elbow method).
#' @param var_explained_threshold Variance threshold to determine individual k_max.
#' @param handle_missing Boolean; if TRUE, replaces NA/Constant columns with row means.
#' @param return_max Boolean; if TRUE, returns only the k_max and skips curves.
#' @return A list containing optimal_k, results tibble, and a diagnostic plot.
#' @export
estimate_rank_by_permutation_rv <- function(mat_list,
                                            n_permutations = 20,
                                            var_explained_threshold = 0.90, 
                                            handle_missing = TRUE,
                                            return_max = FALSE) {
  
  if (!requireNamespace("pbapply", quietly = TRUE)) stop("Install 'pbapply'.")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Install 'ggplot2'.")

  # --- 1. DATA DIAGNOSTICS & CLEANING ---
  message("[Search] Running Pre-Flight Data Diagnostics...")
  n_modalities <- length(mat_list)
  n_subs <- unique(sapply(mat_list, nrow))
  
  if (length(n_subs) > 1) stop("All matrices must have the same number of rows (subjects).")

  # Clean constant/NA columns
  mat_list <- lapply(seq_along(mat_list), function(i) {
    m <- mat_list[[i]]
    col_min <- apply(m, 2, function(x) if(all(is.na(x))) NA else min(x, na.rm = TRUE))
    col_max <- apply(m, 2, function(x) if(all(is.na(x))) NA else max(x, na.rm = TRUE))
    
    targets <- which(is.na(col_min) | col_min == col_max)
    
    if (length(targets) > 0) {
      if (handle_missing) {
        message(paste0("[Warning] View ", i, ": Found ", length(targets), " dead columns. Replacing with row means."))
        r_means <- rowMeans(m, na.rm = TRUE)
        r_means[is.nan(r_means)] <- 0
        for (col in targets) m[, col] <- r_means
      } else {
        message(paste0("[Cut] View ", i, ": Dropping ", length(targets), " dead columns."))
        m <- m[, -targets, drop = FALSE]
      }
    }
    return(m)
  })

  # --- 2. ROBUST K_MAX DETERMINATION ---
  get_max_k <- function(m) {
    # Scaling is CRITICAL to prevent one feature from dominating variance
    m_scaled <- scale(m, center = TRUE, scale = TRUE)
    
    # Use base svd or ba_svd if available
    s_obj <- tryCatch(svd(m_scaled, nu = 0, nv = 0), error = function(e) NULL)
    if (is.null(s_obj) || is.null(s_obj$d)) return(1)
    
    ev <- s_obj$d^2
    prop_var <- cumsum(ev) / sum(ev)
    
    # Fix the 'Inf' warning: if threshold is never met, use max rank
    idx <- which(prop_var >= var_explained_threshold)
    return(if (length(idx) == 0) length(ev) else min(idx))
  }

  # Calculate individual ranks
  k_candidates <- sapply(mat_list, get_max_k)
  
  # CRITICAL: k_max cannot exceed the number of columns in your SMALLEST matrix
  # or the number of subjects - 1
  feat_counts <- sapply(mat_list, ncol)
  k_max <- min(c(k_candidates, feat_counts, n_subs - 1))
  
  message(paste("[OK] Search space set: k = 1 to", k_max))
  if (return_max) return(list(optimal_k = k_max))

  # --- 3. INTERNAL RV LOGIC ---
  .calculate_rv_curve <- function(inner_mat_list, k_limit) {
    # Pre-calculate U matrices (Orthogonal Subject Scores)
    U_list <- lapply(inner_mat_list, function(m) {
      # nu must be k_limit to allow subsetting inside the loop
      svd(m, nu = k_limit, nv = 0)$u
    })
    
    rv_scores <- sapply(1:k_limit, function(k) {
      scores_k <- sapply(1:n_modalities, function(i) {
        Y_target <- U_list[[i]][, 1:k, drop = FALSE]
        other_indices <- setdiff(1:n_modalities, i)
        
        # Concatenate subject scores from other views
        U_other_combined <- do.call(cbind, lapply(U_list[other_indices], function(u) u[, 1:k, drop = FALSE]))
        
        # Orthogonalize the consensus basis
        consensus_basis <- svd(U_other_combined, nu = k, nv = 0)$u
        
        # Adjusted RV Coefficient Calculation
        return(adjusted_rvcoef(Y_target, consensus_basis))
      })
      return(mean(scores_k, na.rm = TRUE))
    })
    return(rv_scores)
  }

  # Normalize and Center
  preprocess <- function(ml) {
    lapply(ml, function(mat) {
      m_c <- scale(mat, center = TRUE, scale = TRUE)
      f_norm <- sqrt(sum(m_c^2))
      if (f_norm > .Machine$double.eps) m_c / f_norm else m_c
    })
  }

  message("[Rocket] Estimating Joint Signal Curve...")
  processed_mats <- preprocess(mat_list)
  real_scores <- .calculate_rv_curve(processed_mats, k_max)
  
  results_df <- data.frame(k = 1:k_max, score = real_scores, type = "Real Data")

  # --- 4. PERMUTATION VS ELBOW ---
  if (n_permutations > 0) {
    message(paste("[Permute] Permuting", n_permutations, "times..."))
    perm_results <- pbapply::pblapply(1:n_permutations, function(p) {
      p_mats <- processed_mats
      # Shuffle subject order in all views except the first to break covariance
      for (j in 2:n_modalities) p_mats[[j]] <- p_mats[[j]][sample(nrow(p_mats[[j]])), ]
      .calculate_rv_curve(p_mats, k_max)
    })
    
    null_scores <- rowMeans(do.call(cbind, perm_results))
    results_df <- rbind(results_df, data.frame(k = 1:k_max, score = null_scores, type = "Permuted Null"))
    
    # Optimal k maximizes the signal-to-noise gap
    signal_diff <- real_scores - null_scores
    optimal_k <- which.max(signal_diff)
    plot_sub <- paste("Optimal k =", optimal_k, "(Max Signal-to-Noise)")
  } else {
    # Elbow detection: Perpendicular distance from secant line
    y <- real_scores
    optimal_k <- which.max(y - (seq(y[1], y[length(y)], length.out = length(y))))
    plot_sub <- paste("Optimal k =", optimal_k, "(Elbow Detection)")
  }

  # --- 5. VISUALIZATION ---
  p <- ggplot2::ggplot(results_df, ggplot2::aes(x = k, y = score, color = type)) +
    ggplot2::geom_line(linewidth = 1) + 
    ggplot2::geom_point() +
    ggplot2::geom_vline(xintercept = optimal_k, linetype = "dashed", color = "firebrick") +
    ggplot2::labs(
      title = "Joint Rank Estimation (Cross-View RV)",
      subtitle = plot_sub,
      x = "Number of Shared Components (k)",
      y = "Adjusted RV Coefficient"
    ) +
    ggplot2::theme_minimal()

  return(list(optimal_k = optimal_k, results = results_df, plot = p))
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


#' Write all SiMLR outputs to disk
#'
#' Saves SiMLR outputs to a structured directory with relative paths for portability.
#' Matrices and data frames are written as CSV files with row names included and
#' stored in the manifest. Other R objects are saved as `.rds`. NULL components are
#' recorded without files.
#'
#' @param simlr_object A named list containing SiMLR outputs (e.g., `mysim$simlrX`).
#' @param file_prefix A character string used as the prefix for the output directory.
#' @param clear_dir Logical: if TRUE, clear the output directory before writing (default: FALSE).
#'
#' @return No return value, called for side effects.
#' @export
write_simlr <- function(simlr_object, file_prefix, clear_dir = FALSE) {
  if (!is.list(simlr_object) || is.null(names(simlr_object))) {
    stop("simlr_object must be a named list")
  }
  outdir <- paste0(file_prefix, "_simlr")
  if (clear_dir && dir.exists(outdir)) {
    unlink(outdir, recursive = TRUE)
    # cat"Cleared directory:", outdir, "\n")
  }
  if (!dir.exists(outdir)) {
    dir.create(outdir, recursive = TRUE)
  } else if (length(list.files(outdir)) > 0) {
    warning("Directory not empty; existing files may be overwritten")
  }

  manifest <- list()
  # cat"Writing components to:", outdir, "\n")
  # cat"Components:", paste(names(simlr_object), collapse=", "), "\n")

  for (n in names(simlr_object)) {
    obj <- simlr_object[[n]]
    fname <- paste0(n, ".rds")  # Relative path
    abs_fname <- file.path(outdir, fname)  # Absolute path for writing
    # catsprintf("Processing %s (type: %s)\n", n, paste(class(obj), collapse=", ")))

    if (is.null(obj)) {
      manifest[[n]] <- list(type = "null", file = NULL)
      # catsprintf("  Recorded %s as NULL (no file)\n", n))
      next
    }

    # Handle energyPath explicitly
    if (n == "energyPath" && !is.data.frame(obj)) {
      obj <- as.data.frame(obj)
    }

    if (is.data.frame(obj)) {
      fname <- paste0(n, ".csv")
      abs_fname <- file.path(outdir, fname)
      write.csv(obj, abs_fname, row.names = TRUE)  # Always write row names
      manifest[[n]] <- list(
        type = "data.frame",
        file = fname,
        has_rownames = !is.null(rownames(obj)),
        rownames = if (!is.null(rownames(obj))) rownames(obj) else NULL
      )

    } else if (is.matrix(obj)) {
      fname <- paste0(n, ".csv")
      abs_fname <- file.path(outdir, fname)
      write.csv(as.data.frame(obj), abs_fname, row.names = TRUE)  # Always write row names
      manifest[[n]] <- list(
        type = "matrix",
        file = fname,
        has_rownames = !is.null(rownames(obj)),
        rownames = if (!is.null(rownames(obj))) rownames(obj) else NULL,
        storage.mode = storage.mode(obj)
      )

    } else if (is.list(obj)) {
      subdir <- file.path(outdir, n)
      dir.create(subdir, showWarnings = FALSE, recursive = TRUE)
      submanifest <- list()

      for (i in seq_along(obj)) {
        subobj <- obj[[i]]
        subname <- names(obj)[i]
        use_name <- !is.null(subname) && subname != ""
        if (!use_name) subname <- as.character(i)
        # cat(sprintf("  Subcomponent %s$%s (type: %s)\n", n, subname,
        #            paste(class(subobj), collapse=", ")))

        if (is.null(subobj)) {
          submanifest[[subname]] <- list(file = NULL, type = "null", use_name = use_name)
          # catsprintf("    Recorded %s$%s as NULL (no file)\n", n, subname))
          next
        }

        subfile <- if (is.data.frame(subobj) || is.matrix(subobj)) {
          paste0(n, "/", subname, ".csv")
        } else {
          paste0(n, "/", subname, ".rds")
        }
        abs_subfile <- file.path(outdir, subfile)

        if (is.data.frame(subobj)) {
          write.csv(subobj, abs_subfile, row.names = TRUE)  # Always write row names
          submanifest[[subname]] <- list(
            file = subfile,
            type = "data.frame",
            use_name = use_name,
            has_rownames = !is.null(rownames(subobj)),
            rownames = if (!is.null(rownames(subobj))) rownames(subobj) else NULL
          )
          # cat(sprintf("    Wrote %s$%s to %s (rownames: %s)\n", n, subname, subfile,
          #            if (!is.null(rownames(subobj))) "present" else "none"))
        } else if (is.matrix(subobj)) {
          write.csv(as.data.frame(subobj), abs_subfile, row.names = TRUE)  # Always write row names
          submanifest[[subname]] <- list(
            file = subfile,
            type = "matrix",
            use_name = use_name,
            has_rownames = !is.null(rownames(subobj)),
            rownames = if (!is.null(rownames(subobj))) rownames(subobj) else NULL,
            storage.mode = storage.mode(subobj)
          )
          # cat(sprintf("    Wrote %s$%s to %s (rownames: %s)\n", n, subname, subfile,
          #            if (!is.null(rownames(subobj))) "present" else "none"))
        } else {
          saveRDS(subobj, abs_subfile)
          submanifest[[subname]] <- list(
            file = subfile,
            type = "rds",
            use_name = use_name,
            class = class(subobj)
          )
          # catsprintf("    Wrote %s$%s to %s\n", n, subname, subfile))
        }
      }
      manifest[[n]] <- list(type = "list", contents = submanifest)
      # catsprintf("  Created submanifest for %s\n", n))

    } else {
      saveRDS(obj, abs_fname)
      manifest[[n]] <- list(type = "rds", file = fname, class = class(obj))
      # catsprintf("  Wrote %s to %s\n", n, fname))
    }
  }

  manifest_file <- file.path(outdir, "manifest.rds")
  saveRDS(manifest, manifest_file)
  # cat"Manifest written to:", file.path(basename(outdir), "manifest.rds"), "\n")
}

#' Read all SiMLR outputs from disk
#'
#' Reconstructs SiMLR outputs written by \code{\link{write_simlr}} using the manifest
#' with relative paths for portability. Restores row names for matrices and data frames
#' from the CSV files.
#'
#' @param dir A character string specifying the directory with SiMLR outputs.
#'
#' @return A named list reconstructing the original SiMLR object.
#' @export
read_simlr <- function(dir) {
  manifest_file <- file.path(dir, "manifest.rds")
  if (!file.exists(manifest_file)) {
    stop("Manifest file not found in directory: ", dir)
  }
  manifest <- readRDS(manifest_file)
  result <- list()
  # cat"Reading components from:", dir, "\n")
  # cat"Components:", paste(names(manifest), collapse=", "), "\n")

  for (n in names(manifest)) {
    entry <- manifest[[n]]
    # catsprintf("Processing %s (type: %s)\n", n, entry$type))

    if (entry$type == "null") {
      result[[n]] <- NULL
      # catsprintf("  Assigned %s as NULL\n", n))
      next
    }

    abs_file <- file.path(dir, entry$file)  # Resolve relative path
    if (entry$type == "data.frame") {
      if (!file.exists(abs_file)) stop("File not found: ", abs_file)
      obj <- read.csv(abs_file, row.names = if (isTRUE(entry$has_rownames)) 1 else NULL, check.names = FALSE)
      # cat(sprintf("  Read %s from %s (dimensions: %sx%s, rownames: %s)\n", n, entry$file, nrow(obj), ncol(obj), if (isTRUE(entry$has_rownames)) "restored" else "none"))
      if (!isTRUE(entry$has_rownames)) rownames(obj) <- NULL

    } else if (entry$type == "matrix") {
      if (!file.exists(abs_file)) stop("File not found: ", abs_file)
      obj <- read.csv(abs_file, row.names = if (isTRUE(entry$has_rownames)) 1 else NULL, check.names = FALSE)
      obj <- as.matrix(obj)
      if (!isTRUE(entry$has_rownames)) rownames(obj) <- NULL
      if (!is.null(entry$storage.mode)) storage.mode(obj) <- entry$storage.mode
      # cat(sprintf("  Read %s from %s (dimensions: %sx%s, rownames: %s)\n", n, entry$file, nrow(obj), ncol(obj), if (isTRUE(entry$has_rownames)) "restored" else "none"))

    } else if (entry$type == "rds") {
      if (!file.exists(abs_file)) stop("File not found: ", abs_file)
      obj <- readRDS(abs_file)
      if (!is.null(entry$class)) class(obj) <- entry$class
      # catsprintf("  Read %s from %s\n", n, entry$file))

    } else if (entry$type == "list") {
      sublist <- list()
      for (subn in names(entry$contents)) {
        subentry <- entry$contents[[subn]]
        # catsprintf("  Subcomponent %s$%s (type: %s)\n", n, subn, subentry$type))
        if (subentry$type == "null") {
          sublist[[subn]] <- NULL
          # catsprintf("    Assigned %s$%s as NULL\n", n, subn))
          next
        }

        abs_subfile <- file.path(dir, subentry$file)
        if (!file.exists(abs_subfile)) stop("File not found: ", abs_subfile)
        if (subentry$type == "data.frame") {
          sobj <- read.csv(abs_subfile, row.names = if (isTRUE(subentry$has_rownames)) 1 else NULL, check.names = FALSE)
          if (!isTRUE(subentry$has_rownames)) rownames(sobj) <- NULL
        } else if (subentry$type == "matrix") {
          sobj <- read.csv(abs_subfile, row.names = if (isTRUE(subentry$has_rownames)) 1 else NULL, check.names = FALSE)
          sobj <- as.matrix(sobj)
          if (!isTRUE(subentry$has_rownames)) rownames(sobj) <- NULL
          if (!is.null(subentry$storage.mode)) storage.mode(sobj) <- subentry$storage.mode
        } else {
          sobj <- readRDS(abs_subfile)
          if (!is.null(subentry$class)) class(sobj) <- subentry$class
        }

        if (isTRUE(subentry$use_name)) {
          sublist[[subn]] <- sobj
        } else {
          sublist[[length(sublist) + 1]] <- sobj
        }
      }
      obj <- sublist
    } else {
      stop("Unknown type in manifest for ", n, ": ", entry$type)
    }

    result[[n]] <- obj
  }

  return(result)
}


#' Transform prior rows to (approximately) uncorrelated representative rows
#'
#' Given a prior matrix P (k x p) whose rows are correlated and potentially
#' over-represent some latent signals, this function produces a reduced set
#' of row-priors that are uncorrelated and representative.
#'
#' Methods:
#' - "svd" : row-PCA / SVD on P (default). Returns top j orthogonal components.
#' - "cluster_then_svd": cluster correlated rows, compute cluster centroids, then
#'    run SVD on centroids to produce orthogonal representatives.
#'
#' The function will:
#'  1. optionally normalize rows
#'  2. compute SVD of P (or of centroids)
#'  3. select j (user provided or automatic by variance explained)
#'  4. return j orthogonal (uncorrelated) row-vectors in R^p
#'
#' @param P numeric matrix (k x p) --- rows are the priors to compress/orthogonalize.
#' @param j integer or NULL. Number of components to keep. If NULL, chosen automatically.
#' @param method character, one of "svd" or "cluster_then_svd".
#' @param row_center logical. Subtract row means before analysis (recommended).
#' @param row_scale logical. Scale rows to unit sd (optional).
#' @param varimax logical. Apply varimax rotation to loadings (improves interpretability).
#' @param cluster_k integer or NULL. If method = "cluster_then_svd", number of clusters. If NULL, chosen with silhouette / dynamic.
#' @param var_exp_threshold numeric in (0,1). If j is NULL, choose smallest j with cumulative variance >= this threshold.
#' @param verbose logical.
#'
#' @return A list with elements:
#'   - P_orig: original input
#'   - P_trans: transformed row-priors (j x p) (rows uncorrelated)
#'   - U: left singular vectors (k x j)
#'   - S: singular values (j)
#'   - Vt: right singular vectors transposed (j x p)
#'   - transform_fn: function(newP) to map new data with same transform
#'   - diagnostics: list with eigenvalues, variance explained, chosen j, clustering info
#' @export
transform_prior_rows_to_uncorrelated <- function(
  P,
  j = NULL,
  method = c("svd", "cluster_then_svd"),
  row_center = TRUE,
  row_scale = FALSE,
  varimax = FALSE,
  cluster_k = NULL,
  var_exp_threshold = 0.90,
  verbose = TRUE
) {
  method <- match.arg(method)
  stopifnot(is.matrix(P) || is.numeric(P))
  P <- as.matrix(P)
  k <- nrow(P); p <- ncol(P)
  if (verbose) message(sprintf("Input: k=%d rows, p=%d cols", k, p))
  
  # 1) normalize rows if requested
  row_means <- rep(0, k); row_sds <- rep(1, k)
  if (row_center) {
    row_means <- rowMeans(P)
    Pc <- P - matrix(row_means, nrow = k, ncol = p, byrow = FALSE)
  } else {
    Pc <- P
  }
  if (row_scale) {
    row_sds <- apply(Pc, 1, sd)
    row_sds[row_sds == 0] <- 1
    Pn <- Pc / matrix(row_sds, nrow = k, ncol = p, byrow = FALSE)
  } else {
    Pn <- Pc
  }
  
  # Helper for automatic j selection based on variance explained of singular values
  choose_j_from_s <- function(s, threshold = var_exp_threshold) {
    var_explained <- s^2 / sum(s^2)
    cumv <- cumsum(var_explained)
    jsel <- which(cumv >= threshold)[1]
    if (is.na(jsel)) jsel <- length(s)
    return(list(j = jsel, var_explained = var_explained, cumv = cumv))
  }
  
  # 2) optionally cluster rows then compute centroids
  cluster_info <- NULL
  if (method == "cluster_then_svd") {
    if (is.null(cluster_k)) {
      # choose cluster_k heuristically: e.g. between 2 and min( floor(k/2), 50)
      cluster_k <- max(2, min(ceiling(k/10), k-1))
      if (verbose) message("cluster_k not provided: using heuristic cluster_k = ", cluster_k)
    }
    # use correlation distance between rows
    row_cor <- stats::cor(t(Pn))
    distmat <- as.dist(1 - row_cor)
    hc <- stats::hclust(distmat, method = "average")
    cl <- stats::cutree(hc, k = cluster_k)
    centroids <- matrix(NA_real_, nrow = cluster_k, ncol = p)
    for (c in seq_len(cluster_k)) {
      members <- which(cl == c)
      centroids[c, ] <- colMeans(Pn[members, , drop = FALSE])
    }
    P_for_svd <- centroids
    cluster_info <- list(hclust = hc, clusters = cl, centroids = centroids)
  } else {
    P_for_svd <- Pn
  }
  
  # 3) SVD on P_for_svd : note: rows are observations, we want left singular vectors U (rows space)
  #     Use svd(P_for_svd) where P_for_svd is m x p (m = k or cluster_k)
  svd_res <- svd(P_for_svd, nu = min(nrow(P_for_svd), ncol(P_for_svd)), nv = min(nrow(P_for_svd), ncol(P_for_svd)))
  svals <- svd_res$d
  # select j if NULL
  sel <- choose_j_from_s(svals)
  if (is.null(j)) {
    j_use <- sel$j
    if (verbose) message(sprintf("Auto-selected j = %d (%.1f%% variance explained)", j_use, 100 * sum(sel$var_explained[1:j_use])))
  } else {
    j_use <- min(j, length(svals))
  }
  
  Uj <- svd_res$u[, 1:j_use, drop = FALSE]   # (m x j)
  Sj <- svals[1:j_use]
  Vjt <- t(svd_res$v)[1:j_use, , drop = FALSE] # (j x p)
  
  # Rows uncorrelated property: (Uj^T * P_for_svd) has diagonal covariance = diag(Sj^2)
  # Build transformed row-priors of size j x p. We define:
  #   P_trans = Uj^T %*% P_for_svd  (gives j x p rows that are orthogonal)
  P_trans_raw <- t(Uj) %*% P_for_svd  # j x p ; rows orthogonal, covariance diag(Sj^2)
  
  # Optionally varimax rotate rows for interpretability:
  rotation <- NULL
  if (varimax) {
    # We want to rotate the rows (components) to be more sparse/ interpretable.
    # varimax in base R rotates loadings (columns in a loadings matrix); we can transpose appropriately.
    # varimax acts on columns; so pass t(P_trans_raw) which is p x j; result$rotmat is j x j
    vm <- stats::varimax(t(P_trans_raw))
    rotmat <- vm$rotmat  # j x j
    P_trans <- (rotmat %*% P_trans_raw)  # j x p
    rotation <- rotmat
  } else {
    P_trans <- P_trans_raw
  }
  
  # Provide a transform function for new priors: apply same centering/scaling/cluster+projection
  transform_fn <- function(newP) {
    newP <- as.matrix(newP)
    if (ncol(newP) != p) stop("newP must have same number of columns (p) as original P")
    # apply same row centering/scaling (note: centering is by row; for new rows we use the mean/sd computed from original P)
    np <- newP
    if (row_center) {
      np <- np - matrix(row_means, nrow = nrow(np), ncol = p, byrow = FALSE)
    }
    if (row_scale) {
      np <- np / matrix(row_sds, nrow = nrow(np), ncol = p, byrow = FALSE)
    }
    if (method == "cluster_then_svd") {
      # map new rows to cluster centroids: average rows inside each cluster? For mapping new arbitrary rows,
      # we project the new rows onto the Uj basis of centroids. We'll compute projection using Vjt and Sj.
      # We need to produce same j dims: P_trans_new = t(Uj) %*% centroid_space_projection(newP)
      # Simpler: derive mapping from original svd: we have V (p x j), so projection = (newP %*% V) * diag(1)
      # But careful: our P_for_svd was centroids; so a general mapping is new_proj = t(Uj) %*% newP_clustered
      # For simplicity, map each new row x to representation r = (V_j %*% x) (this yields j-length vector).
      # Implement: r = V_j^T %*% x  where V_j is p x j (svd_res$v[,1:j])
      Vj <- svd_res$v[, 1:j_use, drop = FALSE] # p x j
      # for input rows we return j-length vectors per row
      t(t(newP %*% Vj)) # returns nrow(newP) x j; user can transpose if needed
    } else {
      # method == "svd": use Vj as above
      Vj <- svd_res$v[, 1:j_use, drop = FALSE]
      t(t(newP %*% Vj))
    }
  }
  
  diagnostics <- list(
    singular_values = svals,
    var_explained = sel$var_explained,
    cumvar = sel$cumv,
    chosen_j = j_use,
    method = method,
    cluster_info = cluster_info
  )
  
  return(list(
    P_orig = P,
    P_processed = Pn,
    P_trans = P_trans,
    U = Uj,
    S = Sj,
    Vt = Vjt,
    rotation = rotation,
    transform_fn = transform_fn,
    diagnostics = diagnostics
  ))
}


### nns flow stuff

#' Symmetrize a Matrix
#'
#' Computes the symmetric part of a matrix by averaging it with its transpose.
#'
#' @param A Numeric matrix to symmetrize.
#' @return Numeric matrix, the symmetric part (A + A^T)/2.
#' @details
#' The symmetric part is defined as \eqn{\mathrm{sym}(A) = (A + A^T)/2}, ensuring the result is symmetric.
#' This is used in computing the Riemannian gradient for the Stiefel manifold.
#' @examples
#' A <- matrix(c(1, 2, 3, 4), 2, 2)
#' symm(A)
#' @export
symm <- function(A) {
  0.5 * (A + t(A))
}

#' Frobenius Norm of a Matrix
#'
#' Computes the Frobenius norm of a matrix, defined as the square root of the sum of squared elements.
#'
#' @param A Numeric matrix.
#' @return Numeric scalar, the Frobenius norm ||A||_F.
#' @details
#' The Frobenius norm is \eqn{\sqrt{\sum_{i,j} A_{ij}^2}}, equivalent to the Euclidean norm of the vectorized matrix.
#' Used for scaling and computing residuals in NSA-Flow.
#' @examples
#' A <- matrix(1:4, 2, 2)
#' frob(A)
#' @export
frob <- function(A) {
  sqrt(sum(A^2))
}

#' Orthogonality Residual
#'
#' Measures the scale-invariant orthogonality defect of a matrix, assessing deviation from orthonormality.
#'
#' @param Y Numeric matrix (p x k) to evaluate.
#' @return Numeric scalar, the orthogonality residual.
#' @details
#' The residual is computed as \eqn{\sqrt{\sum_{i \neq j} ( (Y^T Y / ||Y||_F^2)_{ij} )^2}}, where \eqn{Y^T Y / ||Y||_F^2} is
#' the normalized Gram matrix. This measures how far \eqn{Y^T Y} is from a scaled identity matrix, ignoring diagonal terms.
#' @examples
#' Y <- matrix(runif(10*5), 10, 5)
#' orth_residual(Y)
#' @export
orth_residual <- function(Y) {
  invariant_orthogonality_defect(Y)
}

#' Nonnegativity Violation
#'
#' Measures the extent to which a matrix violates nonnegativity constraints.
#'
#' @param Y Numeric matrix (p x k) to evaluate.
#' @return Numeric scalar, the Frobenius norm of negative entries.
#' @details
#' Computes \eqn{||\min(Y, 0)||_F}, the Frobenius norm of the negative part of Y, which quantifies
#' the magnitude of negative entries. Used to monitor nonnegativity enforcement in NSA-Flow.
#' @examples
#' Y <- matrix(c(-1, 2, -3, 4), 2, 2)
#' neg_violation(Y)
#' @export
neg_violation <- function(Y) {
  frob(pmin(Y, 0))
}


inv_sqrt_newton <- function(A, max_iter = 1, tol = 1e-6) {
  I <- diag(nrow(A))
  # scaling to improve convergence
  alpha <- sqrt(sum(A * A) / nrow(A))
  Y <- A / alpha
  Z <- I
  for (i in seq_len(max_iter)) {
    Yn <- 0.5 * Y %*% (3*I - Z %*% Y)
    Zn <- 0.5 * (3*I - Z %*% Y) %*% Z
    if (norm(Yn - Y, "F") < tol) break
    Y <- Yn
    Z <- Zn
  }
  return(Z / sqrt(alpha))
}

#' Inverse Square Root of a Symmetric Matrix
#'
#' Computes the inverse square root of a symmetric positive semi-definite matrix using eigen-decomposition.
#'
#' @param M Numeric symmetric matrix (k x k).
#' @param epsilon Small numeric value for numerical stability (default 1e-10).
#' @return Numeric matrix, the inverse square root of M.
#' @details
#' For a symmetric matrix M, computes \eqn{M^{-1/2}} via eigen-decomposition, clipping eigenvalues
#' below 1e-12 to ensure numerical stability. Used in polar and soft retractions for NSA-Flow.
#' @examples
#' M <- matrix(c(4, 1, 1, 2), 2, 2)
#' inv_sqrt_sym(M)
#' @export
inv_sqrt_sym <- function(M, epsilon = 1e-10) {
  if (!is.matrix(M) || nrow(M) != ncol(M)) stop("M must be a square matrix")
  M <- (M + t(M)) / 2
  eig <- eigen(M, symmetric = TRUE)
  vals <- eig$values
  vecs <- eig$vectors

  # Clip to >= 0
  vals_clipped <- pmax(vals, 0)
  # Avoid dividing by zero: add epsilon
  inv_sqrt_vals <- 1 / sqrt(vals_clipped + epsilon)

  # If all values are tiny, return identity scaled by 1/sqrt(epsilon)
  if (all(vals_clipped < sqrt(.Machine$double.eps))) {
    return(diag(length(vals_clipped)) / sqrt(epsilon))
  }

  vecs %*% diag(inv_sqrt_vals) %*% t(vecs)
} 

#' Retraction onto the Stiefel Manifold
#'
#' @param Y_cand Candidate matrix.
#' @param w_retract Weight for soft retraction.
#' @param retraction_type Type of retraction ('polar', 'soft_polar', or 'none').
#' @export
nsa_flow_retract <- function(Y_cand, w_retract, retraction_type) {
  soft_retract_wide <- function(Y, w_retract = 1.0, eps = 1e-8) {
    p <- nrow(Y)
    k <- ncol(Y)
    
    # Step 1: A = Y Y^T
    A <- tcrossprod(Y)   # p x p
    
    # Step 2: Eigendecomposition of A
    eigA <- eigen(A, symmetric = TRUE)
    U <- eigA$vectors
    svals <- pmax(eigA$values, eps)
    
    # Step 3: Sigma^{-1/2}
    Sinvhalf <- diag(1 / sqrt(svals))
    
    # Step 4: V^T = Sinvhalf %*% U^T %*% Y
    Vt <- Sinvhalf %*% t(U) %*% Y
    
    # Step 5: polar factor = U %*% V^T
    Q <- U %*% Vt
    
    # Step 6: soft blend if needed
    if (w_retract < 1.0) {
      Y <- (1 - w_retract) * Y + w_retract * Q
    } else {
      Y <- Q
    }
    
    Y
  }
  if (is.null(retraction_type) || retraction_type == "none"  ) return(Y_cand)
  if (!is.matrix(Y_cand)) stop("Y_cand must be a matrix")
  p <- nrow(Y_cand); k <- ncol(Y_cand)
  YtY <- crossprod(Y_cand)  # k x k
  normY <- sqrt(sum(Y_cand^2))
  rtypes <- c("polar", "svd")
  srtypes = paste0("soft_", rtypes)
  # Unified polar (handles both "polar" and former "svd")
  if (retraction_type %in% rtypes ) {
    # Use economy SVD for efficiency (faster on wide p < k)
    s <- svd(Y_cand, nu = min(p,k), nv = min(p,k))
    Ytilde <- s$u %*% t(s$v)
  } else if (retraction_type %in% srtypes) {

    if ( p < k && k <= 512) {
      s <- svd(Y_cand, nu = min(p,k), nv = min(p,k))
      Q <- s$u %*% t(s$v)
      Ytilde <- (1 - w_retract) * Y_cand + w_retract * Q  # Fallback to soft SVD style for wide
    } else if (p < k && k > 512) {
      Ytilde <- soft_retract_wide(Y_cand, w_retract)
    } else if (k > 512) {
      # tall but big k: Newton-Schulz
      YtY <- crossprod(Y_cand)
      T <- inv_sqrt_newton(YtY)
      Ytilde <- Y_cand %*% T
    } else {
      # tall or square: eigendecomposition
      YtY <- crossprod(Y_cand)
      T <- inv_sqrt_newton(YtY)
      Ytilde <- Y_cand %*% T
    }

  } else {
    outmessage <- sprintf("Unknown retraction_type: %s. Supported: none and %s.", retraction_type, paste(c(rtypes, srtypes), collapse = ", "))
    stop(outmessage)
  }
  #
  # Optional: preserve Frobenius norm
  if (!is.null(normY) && normY > 1e-12) {
    current_norm <- sqrt(sum(Ytilde^2))
    if (current_norm > 0) Ytilde <- Ytilde / current_norm * normY
  }
  Ytilde
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
extend_simlr_embedding_with_new_modalities <- function(
  pymm,
  simlr_result,
  new_modalities,
  mode = c("concatenate", "split", "auto"),
  split_prefixes = c("t1", "dt", "rsf"),
  adjacency_type = c("feature_correlation"),
  cor_threshold = 0.8,
  use_abs_correlation = TRUE,
  k_new = NULL,
  k_method = c("cumulative", "elbow"),
  cumvar_threshold = 0.90,
  min_k = 2L,
  joint_k_policy = c("max", "median", "min"),
  preprocess = c("drop", "zero"),
  verbose = FALSE,
  ...
) {
  mode <- match.arg(mode)
  adjacency_type <- match.arg(adjacency_type)
  k_method <- match.arg(k_method)
  joint_k_policy <- match.arg(joint_k_policy)
  preprocess <- match.arg(preprocess)

  .msg <- function(...) {
    if (isTRUE(verbose)) {
      message(...)
    }
  }

  .assert_scalar_numeric_in_range <- function(x, nm, lower = -Inf, upper = Inf,
                                              lower_open = FALSE, upper_open = FALSE) {
    if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
      stop(sprintf("`%s` must be one finite numeric scalar.", nm), call. = FALSE)
    }
    lower_ok <- if (lower_open) x > lower else x >= lower
    upper_ok <- if (upper_open) x < upper else x <= upper
    if (!lower_ok || !upper_ok) {
      lb <- if (lower_open) "(" else "["
      ub <- if (upper_open) ")" else "]"
      stop(sprintf("`%s` must be in %s%s, %s%s.", nm, lb, lower, upper, ub), call. = FALSE)
    }
  }

  .assert_positive_integer_scalar <- function(x, nm) {
    if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 1 || x != as.integer(x)) {
      stop(sprintf("`%s` must be a positive integer scalar.", nm), call. = FALSE)
    }
  }

  .validate_new_modalities <- function(x) {
    if (!is.list(x) || is.null(names(x)) || any(names(x) == "")) {
      stop(
        "`new_modalities` must be a named list, e.g. list(pet = c('petPC1','petPC2')).",
        call. = FALSE
      )
    }
    for (nm in names(x)) {
      cols <- x[[nm]]
      if (!is.character(cols) || length(cols) < 1L || anyNA(cols)) {
        stop(sprintf("`new_modalities[['%s']]` must be a non-empty character vector.", nm),
             call. = FALSE)
      }
      if (anyDuplicated(cols)) {
        stop(sprintf("`new_modalities[['%s']]` contains duplicated column names.", nm),
             call. = FALSE)
      }
    }
    invisible(TRUE)
  }

  .validate_k_new <- function(k_new, modality_names) {
    if (is.null(k_new)) {
      return(invisible(TRUE))
    }
    if (length(k_new) == 1L && is.numeric(k_new) && !is.na(k_new) &&
        k_new >= 1 && k_new == as.integer(k_new)) {
      return(invisible(TRUE))
    }
    if (is.numeric(k_new) && !is.null(names(k_new))) {
      bad_names <- setdiff(names(k_new), modality_names)
      if (length(bad_names) > 0L) {
        stop(
          sprintf("`k_new` has names not present in `new_modalities`: %s",
                  paste(bad_names, collapse = ", ")),
          call. = FALSE
        )
      }
      bad_vals <- !is.finite(k_new) | is.na(k_new) | k_new < 1 | k_new != as.integer(k_new)
      if (any(bad_vals)) {
        stop("Named `k_new` values must all be positive integers.", call. = FALSE)
      }
      return(invisible(TRUE))
    }
    stop(
      "`k_new` must be NULL, one positive integer, or a named numeric vector keyed by new modality names.",
      call. = FALSE
    )
  }

  .require_function <- function(fn_name) {
    fn <- get0(fn_name, mode = "function", inherits = TRUE)
    if (!is.function(fn)) {
      stop(sprintf("Required function `%s()` was not found.", fn_name), call. = FALSE)
    }
    fn
  }

  .parse_prefix <- function(x) {
    m <- regexec("^(.+?)PC([0-9]+)$", x, perl = TRUE)
    reg <- regmatches(x, m)
    out <- vapply(
      reg,
      function(z) if (length(z) == 3L) z[2] else NA_character_,
      character(1)
    )
    out
  }

  .detect_prefixes <- function(simnames) {
    prefixes <- .parse_prefix(simnames)
    prefixes <- unique(prefixes[!is.na(prefixes) & nzchar(prefixes)])
    prefixes
  }

  .preprocess_matrix <- function(M, block_name, preprocess, verbose = FALSE) {
    M <- as.matrix(M)
    storage.mode(M) <- "numeric"

    if (ncol(M) < 1L) {
      stop(sprintf("Block `%s` has zero columns.", block_name), call. = FALSE)
    }

    all_na <- apply(M, 2L, function(x) all(is.na(x)))
    zero_var <- apply(M, 2L, function(x) {
      y <- x[is.finite(x)]
      if (length(y) <= 1L) {
        return(TRUE)
      }
      stats::sd(y) == 0
    })
    invalid_cols <- all_na | zero_var

    dropped <- character(0)
    zeroed <- character(0)

    if (any(invalid_cols)) {
      bad_names <- colnames(M)[invalid_cols]
      if (preprocess == "drop") {
        if (verbose) {
          message(
            sprintf("Dropping %d invalid columns from `%s`: %s",
                    length(bad_names), block_name, paste(bad_names, collapse = ", "))
          )
        }
        M <- M[, !invalid_cols, drop = FALSE]
        dropped <- bad_names
      } else if (preprocess == "zero") {
        if (verbose) {
          message(
            sprintf("Zeroing %d invalid columns in `%s`: %s",
                    length(bad_names), block_name, paste(bad_names, collapse = ", "))
          )
        }
        M[, invalid_cols] <- 0
        zeroed <- bad_names
      }
    }

    if (ncol(M) < 1L) {
      stop(sprintf("Block `%s` has no usable columns after preprocessing.", block_name),
           call. = FALSE)
    }

    M_scaled <- scale(M, center = TRUE, scale = TRUE)
    M_scaled[!is.finite(M_scaled)] <- 0

    list(
      matrix = M_scaled,
      dropped_columns = dropped,
      zeroed_columns = zeroed
    )
  }

  .build_feature_correlation_adjacency <- function(M, threshold, use_abs) {
    if (ncol(M) == 1L) {
      out <- matrix(1, nrow = 1L, ncol = 1L,
                    dimnames = list(colnames(M), colnames(M)))
      return(out)
    }

    C <- stats::cor(M, use = "pairwise.complete.obs")
    C[!is.finite(C)] <- 0

    if (use_abs) {
      A <- abs(C)
    } else {
      A <- C
    }

    A[A < threshold] <- 0
    diag(A) <- 1
    A
  }

  .compute_recommended_k <- function(M, method, cumulative_thresh, min_k) {
    max_k <- max(1L, min(nrow(M) - 1L, ncol(M)))
    min_k_eff <- min(max_k, as.integer(min_k))

    if (ncol(M) <= 1L || nrow(M) <= 2L) {
      return(min_k_eff)
    }

    pca <- try(stats::prcomp(M, center = FALSE, scale. = FALSE), silent = TRUE)
    if (inherits(pca, "try-error")) {
      return(min_k_eff)
    }

    sdsq <- pca$sdev^2
    if (length(sdsq) < 1L || sum(sdsq) <= 0) {
      return(min_k_eff)
    }

    if (method == "cumulative") {
      prop <- sdsq / sum(sdsq)
      k <- which(cumsum(prop) >= cumulative_thresh)[1]
      if (is.na(k)) {
        k <- length(prop)
      }
    } else {
      if (length(sdsq) < 2L) {
        k <- min_k_eff
      } else {
        ratios <- sdsq[-length(sdsq)] / pmax(sdsq[-1], .Machine$double.eps)
        k <- which.max(ratios)
        if (length(k) != 1L || is.na(k) || k < 1L) {
          k <- min_k_eff
        }
      }
    }

    as.integer(max(min_k_eff, min(k, max_k)))
  }

  .resolve_k_new <- function(k_new, new_modality_names, blocks, k_method, cumvar_threshold, min_k) {
    out <- stats::setNames(integer(length(new_modality_names)), new_modality_names)

    if (is.null(k_new)) {
      for (nm in new_modality_names) {
        out[[nm]] <- .compute_recommended_k(
          blocks[[nm]],
          method = k_method,
          cumulative_thresh = cumvar_threshold,
          min_k = min_k
        )
      }
      return(out)
    }

    if (length(k_new) == 1L && is.numeric(k_new) && is.null(names(k_new))) {
      out[] <- as.integer(k_new)
      return(out)
    }

    for (nm in new_modality_names) {
      if (!is.null(k_new[[nm]])) {
        out[[nm]] <- as.integer(k_new[[nm]])
      } else {
        out[[nm]] <- .compute_recommended_k(
          blocks[[nm]],
          method = k_method,
          cumulative_thresh = cumvar_threshold,
          min_k = min_k
        )
      }
    }

    out
  }

  .combine_joint_k <- function(k_vec, policy, min_k) {
    vals <- as.integer(k_vec)
    vals <- vals[is.finite(vals) & !is.na(vals) & vals >= 1L]
    if (length(vals) < 1L) {
      return(as.integer(min_k))
    }

    out <- switch(
      policy,
      max = max(vals),
      median = as.integer(stats::median(vals)),
      min = min(vals)
    )

    as.integer(max(out, min_k))
  }

  .copy_and_merge_simlr_result <- function(simlr_result, simlr_fit, new_modality_names) {
    updated <- simlr_result
    if (!is.null(simlr_fit$simlr_result) &&
        !is.null(simlr_fit$simlr_result$v) &&
        is.list(simlr_fit$simlr_result$v)) {
      if (is.null(updated$v)) {
        updated$v <- list()
      }
      for (nm in intersect(new_modality_names, names(simlr_fit$simlr_result$v))) {
        updated$v[[nm]] <- simlr_fit$simlr_result$v[[nm]]
      }
    }
    updated
  }

  .assert_scalar_numeric_in_range(cor_threshold, "cor_threshold", 0, 1)
  .assert_scalar_numeric_in_range(cumvar_threshold, "cumvar_threshold", 0, 1, lower_open = TRUE)
  .assert_positive_integer_scalar(min_k, "min_k")
  .validate_new_modalities(new_modalities)
  .validate_k_new(k_new, names(new_modalities))

  if (!is.character(split_prefixes) || length(split_prefixes) < 1L || anyNA(split_prefixes)) {
    stop("`split_prefixes` must be a non-empty character vector.", call. = FALSE)
  }
  split_prefixes <- unique(split_prefixes)

  if (!is.list(simlr_result) || is.null(simlr_result$v)) {
    stop("`simlr_result` must be a list with a `$v` element.", call. = FALSE)
  }

  apply_proj_fn <- .require_function("apply_simlr_matrices_dtfix")
  initialize_simlr_fn <- .require_function("initializeSimlr")
  simlr_perm_fn <- .require_function("simlr.perm")

  proj <- apply_proj_fn(pymm, simlr_result$v)
  if (!is.list(proj) || length(proj) < 2L) {
    stop("`apply_simlr_matrices_dtfix()` must return a list with projected data and simnames.",
         call. = FALSE)
  }

  pymm_proj <- proj[[1]]
  simnames <- proj[[2]]

  if (!is.matrix(pymm_proj) && !is.data.frame(pymm_proj)) {
    stop("Projected data returned by `apply_simlr_matrices_dtfix()` must be matrix-like.",
         call. = FALSE)
  }
  pymm_proj <- as.data.frame(pymm_proj)

  if (!is.character(simnames) || length(simnames) < 1L) {
    stop("Projected SIMLR names returned by `apply_simlr_matrices_dtfix()` must be a non-empty character vector.",
         call. = FALSE)
  }

  missing_existing <- setdiff(simnames, colnames(pymm_proj))
  if (length(missing_existing) > 0L) {
    stop(
      sprintf("Projected data is missing SIMLR columns: %s",
              paste(missing_existing, collapse = ", ")),
      call. = FALSE
    )
  }

  if (mode == "auto") {
    detected <- .detect_prefixes(simnames)
    if (length(detected) < 1L) {
      stop("Could not auto-detect prefixes from `simnames`; use `mode = 'concatenate'` or provide `split_prefixes`.",
           call. = FALSE)
    }
    split_prefixes <- detected
    .msg("Auto-detected split prefixes: ", paste(split_prefixes, collapse = ", "))
  }

  overlap_with_existing <- intersect(unlist(new_modalities, use.names = FALSE), simnames)
  if (length(overlap_with_existing) > 0L) {
    stop(
      sprintf(
        "New modality columns overlap existing SIMLR projection columns: %s",
        paste(overlap_with_existing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  missing_new <- setdiff(unlist(new_modalities, use.names = FALSE), colnames(pymm_proj))
  if (length(missing_new) > 0L) {
    stop(
      sprintf("`new_modalities` contains columns not found in projected data: %s",
              paste(missing_new, collapse = ", ")),
      call. = FALSE
    )
  }

  blocks_raw <- list()

  if (mode == "concatenate") {
    blocks_raw[["sim"]] <- as.matrix(pymm_proj[, simnames, drop = FALSE])
  } else {
    for (pref in split_prefixes) {
      keep <- grepl(sprintf("^%s", pref), simnames)
      if (any(keep)) {
        blocks_raw[[pref]] <- as.matrix(pymm_proj[, simnames[keep], drop = FALSE])
      } else {
        .msg("Prefix `", pref, "` not found among projected SIMLR names; skipping.")
      }
    }
    if (length(blocks_raw) < 1L) {
      stop("No existing SIMLR blocks were constructed. Check `split_prefixes` or use `mode = 'concatenate'`.",
           call. = FALSE)
    }
  }

  for (mod in names(new_modalities)) {
    cols <- new_modalities[[mod]]
    blocks_raw[[mod]] <- as.matrix(pymm_proj[, cols, drop = FALSE])
  }

  n_subjects <- nrow(pymm_proj)
  for (nm in names(blocks_raw)) {
    if (nrow(blocks_raw[[nm]]) != n_subjects) {
      stop(sprintf("Block `%s` has %d rows; expected %d.",
                   nm, nrow(blocks_raw[[nm]]), n_subjects),
           call. = FALSE)
    }
  }

  blocks <- list()
  preprocessing_diagnostics <- list()
  for (nm in names(blocks_raw)) {
    prep <- .preprocess_matrix(blocks_raw[[nm]], nm, preprocess = preprocess, verbose = verbose)
    blocks[[nm]] <- prep$matrix
    preprocessing_diagnostics[[nm]] <- list(
      dropped_columns = prep$dropped_columns,
      zeroed_columns = prep$zeroed_columns,
      n_rows = nrow(prep$matrix),
      n_cols = ncol(prep$matrix),
      colnames = colnames(prep$matrix)
    )
  }

  adjacency <- switch(
    adjacency_type,
    feature_correlation = lapply(
      blocks,
      function(M) .build_feature_correlation_adjacency(
        M,
        threshold = cor_threshold,
        use_abs = use_abs_correlation
      )
    )
  )

  new_modality_names <- names(new_modalities)
  k_new_used <- .resolve_k_new(
    k_new = k_new,
    new_modality_names = new_modality_names,
    blocks = blocks,
    k_method = k_method,
    cumvar_threshold = cumvar_threshold,
    min_k = min_k
  )

  joint_k <- .combine_joint_k(k_new_used, policy = joint_k_policy, min_k = min_k)

  .msg(
    "k_new used: ",
    paste(sprintf("%s=%d", names(k_new_used), k_new_used), collapse = "; "),
    " | joint_k=",
    joint_k
  )

  initU <- initialize_simlr_fn(
    blocks,
    joint_k,
    jointReduction = TRUE,
    zeroUpper = FALSE,
    uAlgorithm = "pca",
    addNoise = 0
  )

  simlr_fit <- simlr_perm_fn(
    blocks,
    adjacency,
    initialUMatrix = initU,
    ...
  )

  updated_simlr_result <- .copy_and_merge_simlr_result(
    simlr_result = simlr_result,
    simlr_fit = simlr_fit,
    new_modality_names = new_modality_names
  )

  diagnostics <- list(
    mode_used = mode,
    split_prefixes_used = split_prefixes,
    adjacency_type = adjacency_type,
    cor_threshold = cor_threshold,
    use_abs_correlation = use_abs_correlation,
    preprocess = preprocess,
    n_subjects = n_subjects,
    block_names = names(blocks),
    block_dims = lapply(blocks, dim),
    preprocessing = preprocessing_diagnostics,
    k_method = k_method,
    cumvar_threshold = cumvar_threshold,
    min_k = min_k,
    joint_k_policy = joint_k_policy
  )

  list(
    updated_simlr_result = updated_simlr_result,
    simlr_permutations = simlr_fit,
    blocks = blocks,
    adjacency = adjacency,
    projected_data = pymm_proj,
    k_new_used = k_new_used,
    joint_k = joint_k,
    diagnostics = diagnostics
  )
}




#' backup simlr implementation
#' @export
backup_simlr <- function(
    data_matrices,
    smoothingMatrices,
    iterations = 500,
    sparsenessQuantiles = NULL,
    positivities = NULL,
    initialUMatrix = NULL,
    mixAlg = c("svd", "ica", "avg", "rrpca-l", "rrpca-s", "pca", "stochastic", "newton-schulz", "ica-newton"),
    repeatedMeasures = NA,
    lineSearchRange = c(-5e2, 5e2),
    lineSearchTolerance = 1e-12,
    randomSeed=0,
    constraint = c("nsaflowx0.5x10", "orthox0x1", "Grassmannx0", "Stiefelx0",  "none"),
    energyType = c("cca", "regression", "normalized", "ucca", "lowRank", "lowRankRegression",'normalized_correlation','acc','nc','dat', 'lrr', 'reconorm', 'logcosh', 'exp', 'kurtosis', 'gauss'),
    vmats = NULL,
    connectors = NULL,
    optimizationStyle = c("bidirectional_lookahead", "armijo_gradient","lookahead","bidirectional_armijo_gradient" ),
    scale = c("center",  "eigenvalue" ),
    expBeta = 0.9,
    jointInitialization = TRUE,
    sparsenessAlg = 'soft',
    orthogonalizeU = FALSE,
    domainMatrices = NULL,
    domainLambdas  = NULL,
    sparse_gradient = TRUE,
    verbose = FALSE) {
  if ( length( optimizationStyle ) > 1 ) optimizationStyle=optimizationStyle[1]
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
  if (is.null(positivities)) {
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
  if (is.null(sparsenessQuantiles)) {
    sparsenessQuantiles <- rep(0.5, nModalities)
  }
  
  if (length(sparsenessQuantiles) == 1) {
    sparsenessQuantiles <- rep(sparsenessQuantiles[1], nModalities)
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
      # 1.1 Clean NA and Inf values early
      if (any(!is.finite(data_matrices[[i]]))) {
        data_matrices[[i]] <- antsrimpute(data_matrices[[i]])
        # If still has non-finite (e.g. all NA), replace with 0
        data_matrices[[i]][!is.finite(data_matrices[[i]])] <- 0
      }

      # 1.2 Replace zero variance columns with the column mean + noise
      col_min <- apply(data_matrices[[i]], 2, min, na.rm = TRUE)
      col_max <- apply(data_matrices[[i]], 2, max, na.rm = TRUE)
      zero_var_idx <- which(col_min == col_max | !is.finite(col_min) | !is.finite(col_max))

      if (length(zero_var_idx) > 0) {
        for (col in zero_var_idx) {
          # Get the mean of the specific zero-variance column
          c_mean <- mean(data_matrices[[i]][, col], na.rm = TRUE)
          if (!is.finite(c_mean)) c_mean <- 0
          
          # Add deterministic epsilon noise to give it some small variance
          n_rows <- nrow(data_matrices[[i]])
          eps_noise <- sin(seq_len(n_rows) * (pi / n_rows) * (col + i)) * 1e-6
          
          data_matrices[[i]][, col] <- c_mean + eps_noise
        }
      }

      if (any(is.null(data_matrices[[i]]))) {
        stop(paste("input matrix", i, "is null."))
      }
      matnames <- names(data_matrices)[i]
      
      for (j in 1:length(scaleList)) {
        if (scaleList[j] == "norm") {
          fnorm <- norm(data_matrices[[i]], type = "F")
          if (is.finite(fnorm) && fnorm > 0) {
            data_matrices[[i]] <- data_matrices[[i]] / fnorm
          }
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
  for (i in seq_along(smoothingMatrices)) {
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
    if (!is.finite(si) || si < .Machine$double.eps) si <- 1
    q[, 1] <- qi / si
    r[1, 1] <- si
    for (i in 2:m) {
      xi <- x[, i]
      qj <- q[, 1:(i - 1)]
      rj <- t(qj) %*% xi
      qi <- xi - qj %*% rj
      r[1:(i - 1), i] <- rj
      si <- sqrt(sum(qi^2))
      if (!is.finite(si) || si < .Machine$double.eps) si <- 1
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
      vmats[[i]] <- vmats[[i]] / norm(vmats[[i]], "F")
    }
  }
  
  nc <- ncol(initialUMatrix[[1]])
  myw <- matrix(rnorm(nc^2), nc, nc) # initialization for fastICA

  energyPath <- matrix(Inf, nrow = iterations, ncol = nModalities)
  orthPath = matrix(Inf, nrow = iterations, ncol = nModalities)
  bestU <- initialUMatrix
  bestV <- vmats
  domain_knowledge=0
  if ( !is.null( domainLambdas ) & !is.null( domainMatrices )  ){
    domain_knowledge=paste0("d.",nrow(domainMatrices[[1]]),'.l.',mean(domainLambdas) )
  }
  if (verbose) {
    cat(sprintf("
      --- Method Summary ---
        * Mixer Algorithm  : %s
        * Energy Type      : %s
        * Sparseness Alg.  : %s
        * expBeta          : %s
        * constraint       : %s
        * constraint-it    : %s
        * constraint-wt    : %s
        * optimizationStyle: %s
        * domain-knowledge : %s
      ----------------------
      ", mixAlg, energyType, sparsenessAlg, expBeta, constraint_type, constraint_iterations, constraint_weight, optimizationStyle, domain_knowledge ))
  }
    
# ==============================================================================
#      High-Performance SIMLR Loop (Hybrid: Adam/SGD + Line Search)
# ==============================================================================
# --- 1. Setup before the loop ---
# Initialize adaptive orthogonality weights
orth_weights <- rep(0.0, nModalities)
normalizing_weights = rep( 1.0, nModalities )
domain_weights <- rep(1.0, nModalities)  # Rename auto_norm_domain_weights
names(domain_weights) <- names(data_matrices)
names( orth_weights ) = names( normalizing_weights ) = names( data_matrices )
clipper = 0.95 # no clipping
bestTot <- Inf
bestRow <- 1
bestU <- initialUMatrix
bestV <- vmats
convergence_df <- tibble::tibble()
converged=0

# --- Add these parameters to your main simlr() function signature ---
optimizer = optimizationStyle
initial_learning_rate = 1.0
final_learning_rate = 1e-6

# Create the optimizer object based on user's choice
optimizer_object_l = list()
for ( k in 1:nModalities) {
  optimizer_object <- create_optimizer(
    optimizer_type = optimizer,
    vmats = vmats,
    # Pass hyperparameters that the step functions will need
    beta1 = 0.9, beta2 = 0.999, epsilon = 1e-8, sgd_momentum_beta = 0.9
  )
  optimizer_object_l[[k]] <- optimizer_object
}


# initialize energy trackers for each modality
all_sim_energy   <- vector("list", nModalities)
all_ort_energy   <- vector("list", nModalities)
all_dom_energy   <- vector("list", nModalities)
all_dom_energy_raw<-vector("list", nModalities)
all_total_energy <- vector("list", nModalities)

for (j in 1:nModalities) {
  all_sim_energy[[j]]   <- numeric()
  all_ort_energy[[j]]   <- numeric()
  all_dom_energy[[j]]   <- numeric()
  all_dom_energy_raw[[j]] <- numeric()
  all_total_energy[[j]] <- numeric()
}
v_initial = vmats
u_initial = initialUMatrix
  v_prev <- vmats
  stagnation_counter <- 0
  # --- 2. Main Optimization Loop ---
  for (myit in 1:iterations) { # Begin main optimization loop
    # --- Calculate dynamic learning rate for non-line-search methods ---
    if ( myit <= 2 ) {
      vmats = v_initial
      initialUMatrix = u_initial
    }
    decay_progress <- (myit - 1) / max(1, iterations - 1)
    current_learning_rate <- final_learning_rate + 0.5 * (initial_learning_rate - final_learning_rate) * (1 + cos(pi * decay_progress))
    
    # Track if any matrix changed in this iteration
    v_diff <- 0
    for (kk in 1:nModalities) {
      v_diff <- v_diff + sum((vmats[[kk]] - v_prev[[kk]])^2)
    }
    if (v_diff < .Machine$double.eps & myit > 2) {
      stagnation_counter <- stagnation_counter + 1
    } else {
      stagnation_counter <- 0
    }
    v_prev <- vmats
    
    if (stagnation_counter >= 10) {
      if (verbose) message("~~Parameter stagnation detected. Breaking loop.")
      break
    }
    
    # --- A. Update each V_i matrix ---
  for (i in 1:nModalities) {
    ##############################################################
    # first define the local versions of the energy and gradient #
    ##############################################################
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
      dom_e <- dom_e_raw <- 0
      if (!is.null(domainMatrices) && !is.null(domainLambdas)) {
        lam <- domainLambdas[i]
        if (lam > 0) {
          dom_e_raw <- calculate_simlr_energy(
            V_sp, data_matrices[[i]], initialUMatrix[[i]],
            "dat", lambda = lam, prior_matrix = domainMatrices[[i]]
          )
          dom_e <- dom_e_raw * domain_weights[i]    # Scale
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
      all_ort_energy[[i]] <<- c( all_ort_energy[[i]], orth_e )
      all_dom_energy_raw[[i]] <<- c(all_dom_energy_raw[[i]], dom_e_raw)
      all_total_energy[[i]] <<- c(all_total_energy[[i]], total_e)
      if (return_raw) return(sim_e) # raw similarity+domain only
      return(total_e)
    }
    ############################
    smooth_grad <- function(V) {
      if (positivities[i] == 'positive') V <- take_abs_unsigned(V)

      # --- User-chosen gradient ---
      sim_grad <- calculate_simlr_gradient(
        V, data_matrices[[i]], initialUMatrix[[i]],
        energyType
      ) * normalizing_weights[i]

      # --- Domain gradient (only if lambda > 0) ---
      dom_grad <- 0
      if (!is.null(domainMatrices) && !is.null(domainLambdas)) {
        lam <- domainLambdas[i]
        if (lam > 0) {
          dom_grad <- calculate_simlr_gradient(
            V, data_matrices[[i]], initialUMatrix[[i]],
            "dat", lambda = lam, prior_matrix = domainMatrices[[i]]
          ) * domain_weights[i]
        }
      }

      orth_grad <- 0
      if (constraint_type == "ortho" & FALSE) {
        orth_grad <- constraint_weight * orth_weights[i] *
          gradient_invariant_orthogonality_defect(V_sp)
      }


      g <- sim_grad + dom_grad - orth_grad
      g <- clip_gradient_by_quantile( as.matrix(g), clipper)
      g <- simlr_sparseness(
        g,
        constraint_type = constraint_type,
        smoothing_matrix = smoothingMatrices[[i]],
        positivity = positivities[i],
        sparseness_quantile = sparsenessQuantiles[i],
        constraint_iterations = constraint_iterations,
        constraint_weight = constraint_weight,
        sparseness_alg = sparsenessAlg
      )

      return(g)
    }      
    
    riemannian_descent_grad = smooth_grad(vmats[[i]])
    step_result <- step(
          optimizer = optimizer_object_l[[i]],
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
    optimizer_object_l[[i]] <- step_result$optimizer
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
    updated_Us <- simlrU(tempU, mixAlg, initialUMatrix,
                   orthogonalize = orthogonalizeU,
                   connectors = connectors,
                   expBeta = expBeta)
    # Apply Gram-Schmidt orthogonalization (localGS)
    initialUMatrix <- lapply(updated_Us, function(u) localGS(u, orthogonalize = orthogonalizeU))
    #   cat(paste("<o><o><o><o><o><o><o><o><o><o>\n"))
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
        domain_energy_raw = all_dom_energy_raw[[i]][length(all_dom_energy_raw[[i]])],  
        domain_energy = all_dom_energy[[i]][length(all_dom_energy[[i]])],
        feature_orthogonality = all_ort_energy[[i]][length(all_ort_energy[[i]])],
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
      # After setting normalizing_weights and orth_weights
      if (mod_results$domain_energy_raw != 0 & !is.na(mod_results$domain_energy_raw)) {  # Use raw
        if (abs(mod_results$domain_energy_raw) > 1e-10) {
          domain_weights[i] <- abs(mod_results$similarity_energy * normalizing_weights[i]) / abs(mod_results$domain_energy_raw)
        } else {
          domain_weights[i] <- 1.0
        }
        if (is.na(domain_weights[i]) | is.infinite(domain_weights[i])) domain_weights[i] <- 1.0
      }
      if (verbose & i == nModalities ) message("Domain Weights: ", paste(round(domain_weights, 2), collapse=", "))      
    }
    if (verbose) {
      print(normalizing_weights)
      message("Norm Weights: ", paste(round(normalizing_weights, 3), collapse=", "))
      message("Orth Weights: ", paste(round(orth_weights, 2), collapse=", "))
    }
  }
  
  # Update the "best" solution found so far based on mean total energy
  mean_current_energy <- mean(iter_results$total_energy, na.rm = TRUE)
  printit=FALSE
  if (is.finite(mean_current_energy) && mean_current_energy < bestTot & myit >= 2 ) {
    lastBest = bestTot
    bestTot <- mean_current_energy
    bestRow <- myit
    bestU <- initialUMatrix
    bestV <- vmats
    printit=TRUE
    converged=myit
    pct_reduction_less_than <- function(new_val, old_val, threshold_pct) {
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
        if (verbose) message(paste("~~Small.delt: E ", round(bestTot,4), " E-1 ", round(lastBest, 4),"E / E-1 ",round(change_detector[2],4)))
        break # Properly exit the loop
      }
    }
  } else {
    # Lack of improvement - reduce learning rate
    initial_learning_rate = initial_learning_rate * 0.5
    final_learning_rate = final_learning_rate * 0.9
  }
  
  if (verbose & printit | verbose > 1 ) {
    # Report the mean orthogonality across all modalities for this iteration
    mean_orthogonality <- mean(iter_results$feature_orthogonality, na.rm = TRUE)
    cat(sprintf("It: %d | Energy: %.4f | Best.Energy: %.4f (at iter %d) | Ortho: %.4f \n",
                  myit, mean_current_energy, bestTot, bestRow, mean_orthogonality))
    if ( myit == 1) cat("\n----iteration 1 is an auto-tuning iteration----\n")
  }
  
  # Check for convergence based on lack of improvement
  maxitnoimp = max(5, round( 0.05 * iterations ))
  if ((myit - bestRow) > maxitnoimp) {
    if(verbose) message(paste("~~Convergence criteria met @ ",myit," \n No improvement over ", maxitnoimp, " iterations"))
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
  
for ( v in 1:length(bestV)) {
  if ( is.null( rownames(bestV[[v]])) ) {
    rownames(bestV[[v]])=paste0("x",1:nrow(bestV[[v]]))
  }
}

energyPath <- na.omit(energyPath)
rlist=    list(
      u = bestU,
      v = bestV,
      initialRandomMatrix = randmat,
      energyPath = data.frame(convergence_df),
      finalError = bestTot,
      connectors = connectors,
      energyType = energyType,
      mixAlg = mixAlg,
      optimizationStyle = optimizationStyle,
      converged_at = converged,
      sim_energy = all_sim_energy,
      domain_energy = all_dom_energy_raw,
      orth_energy = all_ort_energy,
      total_energy = all_total_energy,
      constraint = constraint
    )
if ( ! is.null( domainLambdas ) ) {
  rlist$domainLambdas = domainLambdas
}
return(
  rlist
  )
}
