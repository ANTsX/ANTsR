# fusedRidge.R
# Manifold-Regularized Fused Ridge Regression
# Fits a joint Fused Ridge regression model across multiple binarized thresholds,
# smoothing the coefficient trajectories using a 1D Graph Laplacian.
# Compatible with ANTsR package specifications.

#' Manifold-Regularized Fused Ridge Regression
#'
#' Fits a joint Fused Ridge regression model across multiple binarized thresholds,
#' smoothing the coefficient trajectories using a 1D Graph Laplacian.
#'
#' @param X_pcs Matrix of predictor variables (e.g., brain signatures).
#' @param y_raw Vector of continuous exposure or response variable.
#' @param thresholds Vector of thresholds at which to binarize y_raw.
#' @param covariates Optional matrix or data frame of covariates to adjust for. Covariates are unpenalized.
#' @param lambda1 Ridge penalty weight (defaults to 0.5).
#' @param lambda2 Fusion penalty weight (defaults to 0.5).
#' @param family Model family, passed to glmnet. Defaults to "binomial".
#' @param standardize Boolean indicating whether to scale the predictor matrix. Defaults to TRUE.
#' @param foldid Optional fold IDs for group-structured cross-validation.
#' @param topK Optional integer. If provided, fits a first pass model, selects the union of the \code{topK} largest absolute feature weights across all thresholds, and refits the model using only those features (enables sparsity control via two-pass refitting).
#' @param thresh Convergence threshold for \code{glmnet}.
#' @details
#' Fits a joint Fused Ridge regression model across multiple binarized thresholds of a continuous response variable,
#' smoothing the coefficient trajectories using a 1D Graph Laplacian.
#' 
#' The model is formulated by transforming the predictor matrix using a transformation matrix 
#' \eqn{H = (\lambda_1 I + \lambda_2 L)^{-1/2}}, where \eqn{L} is a 1D Graph Laplacian matrix. 
#' The design matrix is stacked block-diagonally for unpenalized covariates and using Kronecker products for penalized features:
#' \eqn{Z = H \otimes X}.
#' This transformation maps the fused ridge problem back into standard ridge regression, allowing the use of highly optimized solvers like \code{glmnet}.
#' 
#' @references
#' Lettink, A., Chinapaw, M. J. M., & van Wieringen, W. N. (2023). Two-dimensional fused targeted ridge regression for health indicator prediction from accelerometer data. Journal of the Royal Statistical Society Series C: Applied Statistics, 72(4), 1064-1078. \doi{10.1093/jrsssc/qlad038}
#' 
#' Li, H., & Li, Y. (2008). Network-constrained regularization and variable selection for analysis of genomic data. Bioinformatics, 24(9), 1175-1182. \doi{10.1093/bioinformatics/btn081}
#' 
#' @return A list containing the fitted glmnet model, cross-validation results, optimal lambda,
#'         reconstructed full coefficient matrix, and other helper parameters.
#' @export
fusedRidge <- function(X_pcs, y_raw, thresholds, covariates = NULL,
                       lambda1 = 0.5, lambda2 = 0.5, family = "binomial",
                       standardize = TRUE, foldid = NULL, topK = NULL,
                       thresh = 1e-04) {
  # Input validation
  X_pcs <- as.matrix(X_pcs)
  N <- nrow(X_pcs)
  M <- ncol(X_pcs)
  J <- length(thresholds)
  
  if (length(y_raw) != N) {
    stop("Length of y_raw must match the number of rows in X_pcs")
  }
  
  # Binarize response
  y_matrix <- matrix(0, nrow = N, ncol = J)
  for (j in seq_len(J)) {
    y_matrix[, j] <- ifelse(y_raw >= thresholds[j], 1, 0)
  }
  y_stacked <- as.vector(y_matrix)
  
  # Preprocess covariates
  if (!is.null(covariates)) {
    X_covs <- as.matrix(covariates)
    if (nrow(X_covs) != N) {
      stop("Number of rows in covariates must match X_pcs")
    }
    covs_names <- colnames(X_covs)
    if (is.null(covs_names)) {
      covs_names <- paste0("Cov", seq_len(ncol(X_covs)))
      colnames(X_covs) <- covs_names
    }
  } else {
    X_covs <- matrix(1, nrow = N, ncol = 1) # Dummy intercept spacer
    covs_names <- NULL
  }
  K <- ncol(X_covs)
  
  # Standardize predictors if requested
  if (standardize) {
    X_pcs_std <- scale(X_pcs)
    mean_X <- attr(X_pcs_std, "scaled:center")
    sd_X <- attr(X_pcs_std, "scaled:scale")
  } else {
    X_pcs_std <- X_pcs
    mean_X <- NULL
    sd_X <- NULL
  }
  
  # Construct 1D Graph Laplacian L (J x J)
  L <- matrix(0, nrow = J, ncol = J)
  diag(L) <- 2
  if (J > 1) {
    L[1, 1] <- 1
    L[J, J] <- 1
    for (j in 1:(J-1)) {
      L[j, j+1] <- -1
      L[j+1, j] <- -1
    }
  } else {
    L[1, 1] <- 0
  }
  
  # Compute transformation matrix H = (lambda1 * I + lambda2 * L)^(-1/2)
  A <- lambda1 * diag(J) + lambda2 * L
  A_eig <- eigen(A, symmetric = TRUE)
  # Ensure eigenvalues are positive to avoid square root of negatives/zeros
  eigen_vals <- pmax(A_eig$values, 1e-10)
  H <- A_eig$vectors %*% diag(1 / sqrt(eigen_vals)) %*% t(A_eig$vectors)
  
  # Stack covariates block-diagonally (unpenalized per threshold)
  X_covs_stacked <- kronecker(diag(J), X_covs)
  n_covs_stacked <- ncol(X_covs_stacked)
  
  # Stack brain features using H^T kronecker X_pcs
  Z_tilde <- kronecker(H, X_pcs_std)
  
  # Combine design matrix
  X_stacked <- cbind(X_covs_stacked, Z_tilde)
  
  # Set penalty factors (0 for covariates, 1 for brain features)
  p_factor <- c(rep(0, n_covs_stacked), rep(1, ncol(Z_tilde)))
  
  # Setup foldid for group-structured CV if not provided
  if (is.null(foldid)) {
    # Default to 10 folds grouped by subject
    subject_folds <- sample(rep(seq_len(10), length.out = N))
    foldid <- rep(subject_folds, J)
  } else {
    if (length(foldid) == N) {
      foldid <- rep(foldid, J)
    } else if (length(foldid) != N * J) {
      stop("Length of foldid must be either N or N * J")
    }
  }
  
  # Run cross-validation to select optimal Ridge lambda
  cv_ridge <- glmnet::cv.glmnet(X_stacked, y_stacked, family = family,
                               penalty.factor = p_factor, alpha = 0,
                               foldid = foldid, keep = TRUE, thresh = thresh)
  
  # Fit final model using optimal lambda
  fit_ridge <- glmnet::glmnet(X_stacked, y_stacked, family = family,
                             penalty.factor = p_factor, alpha = 0,
                             lambda = cv_ridge$lambda.min, thresh = thresh)
  
  # Extract stacked coefficients and reconstruct original spaces
  coef_stacked <- as.matrix(coef(fit_ridge))
  a0 <- coef_stacked[1, 1]
  
  # Grab covariates coefficients
  coef_covs_stacked <- coef_stacked[2:(n_covs_stacked + 1), 1]
  coef_covs_matrix <- matrix(coef_covs_stacked, nrow = K, ncol = J)
  rownames(coef_covs_matrix) <- colnames(X_covs)
  colnames(coef_covs_matrix) <- paste0("T", seq_len(J))
  
  # Grab brain signature coefficients (in transformed space)
  coef_Z <- coef_stacked[(n_covs_stacked + 2):nrow(coef_stacked), 1]
  theta_matrix <- matrix(coef_Z, nrow = M, ncol = J)
  
  # Project back to original feature space: B = Theta * H
  coefs_full <- theta_matrix %*% H
  rownames(coefs_full) <- colnames(X_pcs)
  colnames(coefs_full) <- paste0("T", seq_len(J))
  
  if (!is.null(topK)) {
    if (!is.numeric(topK) || length(topK) != 1 || topK <= 0) {
      stop("topK must be a positive integer.")
    }
    topK <- as.integer(topK)
    # Identify the topK features per threshold
    selected_features <- c()
    for (j in seq_len(J)) {
      vals <- abs(coefs_full[, j])
      keep_idx <- order(vals, decreasing = TRUE)[1:min(topK, M)]
      selected_features <- union(selected_features, keep_idx)
    }
    # Sort selected features for consistency
    selected_features <- sort(selected_features)
    
    # Re-fit using only selected features
    X_pcs_selected <- X_pcs[, selected_features, drop = FALSE]
    results_selected <- fusedRidge(
      X_pcs = X_pcs_selected,
      y_raw = y_raw,
      thresholds = thresholds,
      covariates = covariates,
      lambda1 = lambda1,
      lambda2 = lambda2,
      family = family,
      standardize = standardize,
      foldid = foldid,
      topK = NULL, # Prevent infinite recursion
      thresh = thresh
    )
    
    # Map coefficients back to full feature space
    coefs_full_new <- matrix(0, nrow = M, ncol = J)
    if (!is.null(colnames(X_pcs))) {
      rownames(coefs_full_new) <- colnames(X_pcs)
    }
    colnames(coefs_full_new) <- colnames(results_selected$coefs_full)
    coefs_full_new[selected_features, ] <- results_selected$coefs_full
    
    results_selected$coefs_full <- coefs_full_new
    results_selected$selected_features <- selected_features
    results_selected$mean_X <- mean_X
    results_selected$sd_X <- sd_X
    return(results_selected)
  }
  
  # Return structured results
  results <- list(
    fit = fit_ridge,
    cv = cv_ridge,
    optimal_lambda = cv_ridge$lambda.min,
    a0 = a0,
    coefs_covs = coef_covs_matrix,
    coefs_full = coefs_full,
    theta_matrix = theta_matrix,
    H = H,
    thresholds = thresholds,
    family = family,
    y_matrix = y_matrix,
    mean_X = mean_X,
    sd_X = sd_X,
    covs_names = covs_names
  )
  class(results) <- "fusedRidge"
  return(results)
}

#' Predict Method for Fused Ridge Regression
#'
#' Obtains predictions from a fitted \code{fusedRidge} object on new data.
#'
#' @param object A fitted \code{fusedRidge} object.
#' @param newx Matrix of new predictor variables.
#' @param newcovs Optional matrix or data frame of new covariates. Must be provided if the model was trained with covariates.
#' @param type Type of prediction. \code{"link"} returns the linear predictor, and \code{"response"} returns the fitted probabilities (only relevant for \code{family = "binomial"}).
#' @param topK Optional integer. If provided, keeps only the \code{topK} absolute largest feature weights for prediction, zeroing out all others (enables sparsity control).
#' @param ... Additional arguments (not used).
#' @return A matrix of predictions with dimensions \code{nrow(newx) x length(thresholds)}.
#' @method predict fusedRidge
#' @export
predict.fusedRidge <- function(object, newx, newcovs = NULL, type = c("link", "response"), topK = NULL, ...) {
  type <- match.arg(type)
  
  newx <- as.matrix(newx)
  N_new <- nrow(newx)
  
  # Standardize newx using training scale parameters
  if (!is.null(object$mean_X) && !is.null(object$sd_X)) {
    newx_std <- scale(newx, center = object$mean_X, scale = object$sd_X)
  } else {
    newx_std <- newx
  }
  
  # Check/process covariates
  if (is.null(object$covs_names)) {
    if (!is.null(newcovs)) {
      warning("newcovs was provided but the model was trained without covariates. Ignoring newcovs.")
    }
    X_covs_new <- matrix(1, nrow = N_new, ncol = 1)
  } else {
    if (is.null(newcovs)) {
      stop("newcovs must be provided as the model was trained with covariates.")
    }
    X_covs_new <- as.matrix(newcovs)
    if (nrow(X_covs_new) != N_new) {
      stop("Number of rows in newcovs must match newx")
    }
    if (!all(object$covs_names %in% colnames(X_covs_new))) {
      if (ncol(X_covs_new) != length(object$covs_names)) {
        stop("newcovs columns do not match the covariates used in training.")
      }
    } else {
      # Reorder columns to match training order
      X_covs_new <- X_covs_new[, object$covs_names, drop = FALSE]
    }
  }
  
  # Sparse feature weights selection (topK largest absolute coefficients per threshold)
  B_full <- object$coefs_full
  if (!is.null(topK)) {
    if (!is.numeric(topK) || length(topK) != 1 || topK <= 0) {
      stop("topK must be a positive integer.")
    }
    topK <- as.integer(topK)
    M <- nrow(B_full)
    if (topK < M) {
      for (j in seq_len(ncol(B_full))) {
        vals <- abs(B_full[, j])
        keep_idx <- order(vals, decreasing = TRUE)[1:topK]
        set_zero_idx <- setdiff(seq_len(M), keep_idx)
        B_full[set_zero_idx, j] <- 0
      }
    }
  }
  
  # Compute linear predictor: eta = a0 + X_covs_new %*% object$coefs_covs + newx_std %*% B_full
  eta <- object$a0 + X_covs_new %*% object$coefs_covs + newx_std %*% B_full
  
  if (type == "response") {
    if (object$family == "binomial") {
      return(1 / (1 + exp(-eta)))
    }
  }
  
  return(eta)
}
