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
#' @param X_pcs Matrix of predictor variables. For high-dimensional data (e.g., voxels), it is recommended to use dimensionality reduction (e.g., PCA) first, as the internal Kronecker product can be memory-intensive.
#' @param y_raw Vector of continuous exposure or response variable.
#' @param thresholds Vector of thresholds at which to binarize y_raw.
#' @param covariates Optional matrix or data frame of covariates to adjust for. Covariates are unpenalized. An intercept is automatically added if not present.
#' @param lambda1 Ridge penalty weight (defaults to 0.5).
#' @param lambda2 Fusion penalty weight (defaults to 0.5).
#' @param family Model family, passed to glmnet. Defaults to "binomial".
#' @param standardize Boolean indicating whether to scale the predictor matrix. Defaults to TRUE.
#' @param foldid Optional fold IDs for group-structured cross-validation.
#' @param topK Optional integer. If provided, fits a first pass model, selects the union of the \code{topK} largest absolute feature weights across all thresholds, and refits the model using only those features (enables sparsity control via two-pass refitting).
#' @param thresh Convergence threshold for \code{glmnet}.
#' @param nlambda Number of lambda values to use in \code{glmnet} (defaults to 100).
#' @param nfolds Number of folds for cross-validation (defaults to 10).
#' @param ... Additional arguments passed to \code{glmnet::cv.glmnet} and \code{glmnet::glmnet}.
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
                       thresh = 1e-04, nlambda = 100, nfolds = 10, ...) {
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
    X_covs_raw <- as.matrix(covariates)
    if (nrow(X_covs_raw) != N) {
      stop("Number of rows in covariates must match X_pcs")
    }
    
    # Ensure raw covariates have column names
    if (is.null(colnames(X_covs_raw))) {
      colnames_raw <- paste0("Cov", seq_len(ncol(X_covs_raw)))
    } else {
      colnames_raw <- colnames(X_covs_raw)
    }
    
    # Ensure an intercept column is present to allow per-threshold intercepts
    is_intercept <- apply(X_covs_raw, 2, function(x) all(x == 1))
    if (any(is_intercept)) {
      X_covs <- X_covs_raw
      colnames(X_covs) <- colnames_raw
    } else {
      X_covs <- cbind(1, X_covs_raw)
      colnames(X_covs) <- c("(Intercept)", colnames_raw)
    }
    
    covs_names <- colnames(X_covs)
  } else {
    X_covs <- matrix(1, nrow = N, ncol = 1) # Per-threshold intercept
    colnames(X_covs) <- "(Intercept)"
    covs_names <- "(Intercept)"
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
    # Default to nfolds folds grouped by subject
    subject_folds <- sample(rep(seq_len(nfolds), length.out = N))
    foldid <- rep(subject_folds, J)
  } else {
    if (length(foldid) == N) {
      foldid <- rep(foldid, J)
    } else if (length(foldid) != N * J) {
      stop("Length of foldid must be either N or N * J")
    }
  }
  
  # Run cross-validation to select optimal Ridge lambda
  # Set intercept = FALSE because we provide per-threshold intercepts in X_covs_stacked
  cv_ridge <- glmnet::cv.glmnet(X_stacked, y_stacked, family = family,
                               penalty.factor = p_factor, alpha = 0,
                               foldid = foldid, keep = TRUE, thresh = thresh,
                               standardize = FALSE, intercept = FALSE,
                               nlambda = nlambda, ...)
  
  # Fit final model using optimal lambda
  fit_ridge <- glmnet::glmnet(X_stacked, y_stacked, family = family,
                             penalty.factor = p_factor, alpha = 0,
                             lambda = cv_ridge$lambda.min, thresh = thresh,
                             standardize = FALSE, intercept = FALSE,
                             nlambda = nlambda, ...)
  
  # Extract stacked coefficients and reconstruct original spaces
  coef_stacked <- as.matrix(coef(fit_ridge))
  a0_global <- coef_stacked[1, 1] # Should be 0 since intercept = FALSE
  
  # Grab covariates coefficients
  coef_covs_stacked <- coef_stacked[2:(n_covs_stacked + 1), 1]
  coef_covs_matrix <- matrix(coef_covs_stacked, nrow = K, ncol = J)
  rownames(coef_covs_matrix) <- covs_names
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
      thresh = thresh,
      nlambda = nlambda,
      nfolds = nfolds,
      ...
    )
    
    # Map coefficients back to full feature space
    coefs_full_new <- matrix(0, nrow = M, ncol = J)
    theta_matrix_new <- matrix(0, nrow = M, ncol = J)
    if (!is.null(colnames(X_pcs))) {
      rownames(coefs_full_new) <- colnames(X_pcs)
      rownames(theta_matrix_new) <- colnames(X_pcs)
    }
    colnames(coefs_full_new) <- colnames(results_selected$coefs_full)
    colnames(theta_matrix_new) <- colnames(results_selected$coefs_full)
    
    coefs_full_new[selected_features, ] <- results_selected$coefs_full
    theta_matrix_new[selected_features, ] <- results_selected$theta_matrix
    
    results_selected$coefs_full <- coefs_full_new
    results_selected$theta_matrix <- theta_matrix_new
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
    a0 = a0_global,
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
  if (!is.null(object$covs_names)) {
    if (is.null(newcovs)) {
      # Handle case where only intercept was used
      if (length(object$covs_names) == 1 && object$covs_names == "(Intercept)") {
        X_covs_new <- matrix(1, nrow = N_new, ncol = 1)
        colnames(X_covs_new) <- "(Intercept)"
      } else {
        stop("newcovs must be provided as the model was trained with covariates.")
      }
    } else {
      X_covs_new_raw <- as.matrix(newcovs)
      if (nrow(X_covs_new_raw) != N_new) {
        stop("Number of rows in newcovs must match newx")
      }
      
      # Ensure (Intercept) is present if it was in training
      if ("(Intercept)" %in% object$covs_names && !("(Intercept)" %in% colnames(X_covs_new_raw))) {
        X_covs_new <- cbind(1, X_covs_new_raw)
        colnames(X_covs_new)[1] <- "(Intercept)"
      } else {
        X_covs_new <- X_covs_new_raw
      }
      
      if (!all(object$covs_names %in% colnames(X_covs_new))) {
        stop("newcovs columns do not match the covariates used in training.")
      }
      # Reorder columns to match training order
      X_covs_new <- X_covs_new[, object$covs_names, drop = FALSE]
    }
  } else {
    # No covariates (including intercept) - fallback
    X_covs_new <- matrix(0, nrow = N_new, ncol = 0)
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
  # a0 will be 0 if the model was trained with my update (intercept=FALSE)
  eta <- object$a0 + X_covs_new %*% object$coefs_covs + newx_std %*% B_full
  
  if (type == "response") {
    if (object$family == "binomial") {
      return(1 / (1 + exp(-eta)))
    } else if (object$family == "gaussian") {
      return(eta)
    } else if (object$family == "poisson") {
      return(exp(eta))
    }
  }
  
  return(eta)
}

#' Direct Manifold-Regularized Fused Ridge Regression (General)
#'
#' Fits a joint Fused Ridge regression model across multiple binarized thresholds of a
#' continuous response variable using direct optimization (L-BFGS). This avoids the memory-intensive
#' Kronecker product design matrix expansion used in \code{fusedRidge}, making it highly scalable
#' for high-dimensional predictors.
#'
#' @param X_pcs Matrix of predictor variables.
#' @param y_raw Vector of continuous exposure or response variable.
#' @param thresholds Vector of thresholds at which to binarize y_raw.
#' @param covariates Optional matrix or data frame of covariates to adjust for. Covariates are unpenalized. An intercept is automatically added if not present.
#' @param lambda1 Ridge penalty weight (defaults to 0.5).
#' @param lambda2 Fusion penalty weight (defaults to 0.5).
#' @param family Model family. Supported values are \code{"binomial"}, \code{"gaussian"}, and \code{"poisson"}. Defaults to \code{"binomial"}.
#' @param standardize Boolean indicating whether to scale the predictor matrix. Defaults to TRUE.
#' @param foldid Optional fold IDs. If \code{cv = TRUE}, used for group-structured cross-validation.
#' @param topK Optional integer. If provided, fits a first pass model, selects the union of the \code{topK} largest absolute feature weights across all thresholds, and refits the model using only those features.
#' @param thresh Convergence threshold. Ignored (provided for compatibility with \code{fusedRidge}).
#' @param nlambda Number of lambda values to use in the cross-validation grid. Defaults to 20.
#' @param nfolds Number of folds for cross-validation (defaults to 10).
#' @param cv Boolean indicating whether to perform pathwise group-structured cross-validation to select the optimal overall regularization scale. Defaults to TRUE.
#' @param optim_control Optional list of control parameters passed to \code{optim}. Defaults to \code{list(maxit = 200)}.
#' @param ... Additional arguments passed to \code{optim}.
#'
#' @details
#' Fits a joint Fused Ridge regression model across multiple binarized thresholds of a continuous response variable,
#' smoothing the coefficient trajectories using a 1D Graph Laplacian. Unlike \code{fusedRidge}, which transforms the
#' design matrix using a Kronecker product expansion, \code{fusedRidgeDirect} optimizes the loss function directly
#' using R's built-in \code{optim} function with the \code{"L-BFGS-B"} method and analytical gradients.
#'
#' If \code{cv = TRUE}, it performs pathwise group-structured cross-validation (with warm starts) to select the optimal
#' overall scale multiplier for the penalties, matching the cross-validation behavior of \code{fusedRidge}.
#'
#' @return A list of class \code{c("fusedRidgeDirect", "fusedRidge")} containing:
#'         \item{fit}{The final fitted optim object (or list of options).}
#'         \item{cv}{Cross-validation details including grid values, mean validation errors, and standard deviations.}
#'         \item{optimal_lambda}{Optimal lambda multiplier selected by cross-validation (or NA if cv = FALSE).}
#'         \item{a0}{0 (intercepts are included in \code{coefs_covs}).}
#'         \item{coefs_covs}{Reconstructed covariate coefficients matrix.}
#'         \item{coefs_full}{Reconstructed full coefficient matrix.}
#'         \item{theta_matrix}{Coefficients in the original space.}
#'         \item{H}{Identity matrix placeholder.}
#'         \item{thresholds}{Input thresholds vector.}
#'         \item{family}{Model family.}
#'         \item{y_matrix}{Binarized target matrix.}
#'         \item{mean_X}{Scaling center parameters.}
#'         \item{sd_X}{Scaling scale parameters.}
#'         \item{covs_names}{Names of covariates.}
#' @export
fusedRidgeDirect <- function(X_pcs, y_raw, thresholds, covariates = NULL,
                             lambda1 = 0.5, lambda2 = 0.5, family = "binomial",
                             standardize = TRUE, foldid = NULL, topK = NULL,
                             thresh = 1e-04, nlambda = 20, nfolds = 10,
                             cv = TRUE, optim_control = list(maxit = 200), ...) {
  # Input validation
  X_pcs <- as.matrix(X_pcs)
  N <- nrow(X_pcs)
  M <- ncol(X_pcs)
  J <- length(thresholds)
  
  if (length(y_raw) != N) {
    stop("Length of y_raw must match the number of rows in X_pcs")
  }
  
  family <- match.arg(family, c("binomial", "gaussian", "poisson"))
  
  # Binarize response
  y_matrix <- matrix(0, nrow = N, ncol = J)
  for (j in seq_len(J)) {
    y_matrix[, j] <- ifelse(y_raw >= thresholds[j], 1, 0)
  }
  
  # Preprocess covariates
  if (!is.null(covariates)) {
    X_covs_raw <- as.matrix(covariates)
    if (nrow(X_covs_raw) != N) {
      stop("Number of rows in covariates must match X_pcs")
    }
    
    # Ensure raw covariates have column names
    if (is.null(colnames(X_covs_raw))) {
      colnames_raw <- paste0("Cov", seq_len(ncol(X_covs_raw)))
    } else {
      colnames_raw <- colnames(X_covs_raw)
    }
    
    # Ensure an intercept column is present to allow per-threshold intercepts
    is_intercept <- apply(X_covs_raw, 2, function(x) all(x == 1))
    if (any(is_intercept)) {
      X_covs <- X_covs_raw
      colnames(X_covs) <- colnames_raw
    } else {
      X_covs <- cbind(1, X_covs_raw)
      colnames(X_covs) <- c("(Intercept)", colnames_raw)
    }
    
    covs_names <- colnames(X_covs)
  } else {
    X_covs <- matrix(1, nrow = N, ncol = 1) # Per-threshold intercept
    colnames(X_covs) <- "(Intercept)"
    covs_names <- "(Intercept)"
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
  
  # Core fit function helper
  fit_direct_model <- function(X_tr, Y_tr, C_tr, l1, l2, par_start) {
    N_tr <- nrow(X_tr)
    
    loss_fn <- function(par) {
      W <- matrix(par[1:(J * M)], nrow = J, ncol = M)
      V <- matrix(par[(J * M + 1):(J * M + J * K)], nrow = J, ncol = K)
      
      Z <- X_tr %*% t(W) + C_tr %*% t(V)
      
      if (family == "binomial") {
        log_P <- plogis(Z, log.p = TRUE)
        log_1_P <- plogis(Z, lower.tail = FALSE, log.p = TRUE)
        
        term1 <- Y_tr * log_P
        term1[Y_tr == 0] <- 0
        term2 <- (1 - Y_tr) * log_1_P
        term2[Y_tr == 1] <- 0
        
        error_loss <- -sum(term1 + term2) / (N_tr * J)
      } else if (family == "gaussian") {
        error_loss <- 0.5 * sum((Z - Y_tr)^2) / (N_tr * J)
      } else if (family == "poisson") {
        P <- exp(pmin(Z, 50))
        error_loss <- sum(P - Y_tr * Z) / (N_tr * J)
      }
      
      l1_pen <- l1 * sum(W^2)
      l2_pen <- 0
      if (J > 1) {
        l2_pen <- l2 * sum((W[2:J, , drop = FALSE] - W[1:(J-1), , drop = FALSE])^2)
      }
      
      return(error_loss + l1_pen + l2_pen)
    }
    
    grad_fn <- function(par) {
      W <- matrix(par[1:(J * M)], nrow = J, ncol = M)
      V <- matrix(par[(J * M + 1):(J * M + J * K)], nrow = J, ncol = K)
      
      Z <- X_tr %*% t(W) + C_tr %*% t(V)
      
      if (family == "binomial") {
        P <- 1 / (1 + exp(-Z))
      } else if (family == "gaussian") {
        P <- Z
      } else if (family == "poisson") {
        P <- exp(pmin(Z, 50))
      }
      
      G <- (P - Y_tr) / (N_tr * J)
      
      grad_W <- t(G) %*% X_tr + 2 * l1 * W
      if (J > 1) {
        LW <- matrix(0, nrow = J, ncol = M)
        LW[1, ] <- W[1, ] - W[2, ]
        if (J > 2) {
          for (j in 2:(J-1)) {
            LW[j, ] <- 2 * W[j, ] - W[j-1, ] - W[j+1, ]
          }
        }
        LW[J, ] <- W[J, ] - W[J-1, ]
        grad_W <- grad_W + 2 * l2 * LW
      }
      
      grad_V <- t(G) %*% C_tr
      
      return(c(as.vector(grad_W), as.vector(grad_V)))
    }
    
    opt <- optim(
      par = par_start,
      fn = loss_fn,
      gr = grad_fn,
      method = "L-BFGS-B",
      control = optim_control,
      ...
    )
    return(opt)
  }
  
  eval_error_loss <- function(par, X_val, Y_val, C_val) {
    N_val <- nrow(X_val)
    W <- matrix(par[1:(J * M)], nrow = J, ncol = M)
    V <- matrix(par[(J * M + 1):(J * M + J * K)], nrow = J, ncol = K)
    
    Z <- X_val %*% t(W) + C_val %*% t(V)
    
    if (family == "binomial") {
      log_P <- plogis(Z, log.p = TRUE)
      log_1_P <- plogis(Z, lower.tail = FALSE, log.p = TRUE)
      
      term1 <- Y_val * log_P
      term1[Y_val == 0] <- 0
      term2 <- (1 - Y_val) * log_1_P
      term2[Y_val == 1] <- 0
      
      return(-sum(term1 + term2) / (N_val * J))
    } else if (family == "gaussian") {
      return(0.5 * sum((Z - Y_val)^2) / (N_val * J))
    } else if (family == "poisson") {
      P <- exp(pmin(Z, 50))
      return(sum(P - Y_val * Z) / (N_val * J))
    }
  }
  
  # Cross-validation logic
  if (cv) {
    if (is.null(foldid)) {
      subject_folds <- sample(rep(seq_len(nfolds), length.out = N))
    } else {
      if (length(foldid) == N) {
        subject_folds <- foldid
      } else if (length(foldid) == N * J) {
        subject_folds <- foldid[1:N]
      } else {
        stop("Length of foldid must be either N or N * J")
      }
      nfolds <- length(unique(subject_folds))
    }
    
    # Grid of overall scale multiplier
    lambda_grid <- 10^seq(1, -4, length.out = nlambda)
    cv_matrix <- matrix(0, nrow = nfolds, ncol = nlambda)
    
    for (f in 1:nfolds) {
      train_idx <- which(subject_folds != f)
      val_idx <- which(subject_folds == f)
      
      # Handle empty fold case if foldid is non-consecutive
      if (length(val_idx) == 0 || length(train_idx) == 0) next
      
      X_tr <- X_pcs_std[train_idx, , drop = FALSE]
      Y_tr <- y_matrix[train_idx, , drop = FALSE]
      C_tr <- X_covs[train_idx, , drop = FALSE]
      
      X_val <- X_pcs_std[val_idx, , drop = FALSE]
      Y_val <- y_matrix[val_idx, , drop = FALSE]
      C_val <- X_covs[val_idx, , drop = FALSE]
      
      par_current <- rep(0, J * (M + K))
      
      # Pathwise warm start optimization
      for (l_idx in seq_along(lambda_grid)) {
        l <- lambda_grid[l_idx]
        opt_fold <- fit_direct_model(X_tr, Y_tr, C_tr, l * lambda1, l * lambda2, par_current)
        cv_matrix[f, l_idx] <- eval_error_loss(opt_fold$par, X_val, Y_val, C_val)
        par_current <- opt_fold$par
      }
    }
    
    mean_cv_err <- colMeans(cv_matrix)
    opt_idx <- which.min(mean_cv_err)
    optimal_lambda_val <- lambda_grid[opt_idx]
    
    # Fit final model with optimal weights
    l1_final <- optimal_lambda_val * lambda1
    l2_final <- optimal_lambda_val * lambda2
    
    # Pathwise warm start to optimal lambda on full dataset
    par_current <- rep(0, J * (M + K))
    for (l in lambda_grid[1:opt_idx]) {
      opt_final <- fit_direct_model(X_pcs_std, y_matrix, X_covs, l * lambda1, l * lambda2, par_current)
      par_current <- opt_final$par
    }
    
    cv_obj <- list(
      lambda = lambda_grid,
      cvm = mean_cv_err,
      cvsd = apply(cv_matrix, 2, sd) / sqrt(nfolds),
      cvraw = cv_matrix,
      lambda.min = optimal_lambda_val
    )
  } else {
    # No CV: fit directly using original lambda1 and lambda2
    opt_final <- fit_direct_model(X_pcs_std, y_matrix, X_covs, lambda1, lambda2, rep(0, J * (M + K)))
    cv_obj <- NULL
    optimal_lambda_val <- NA
    l1_final <- lambda1
    l2_final <- lambda2
  }
  
  # Extract optimized parameters
  coef_W <- matrix(opt_final$par[1:(J * M)], nrow = J, ncol = M)
  coef_V <- matrix(opt_final$par[(J * M + 1):(J * M + J * K)], nrow = J, ncol = K)
  
  coef_covs_matrix <- t(coef_V)
  rownames(coef_covs_matrix) <- covs_names
  colnames(coef_covs_matrix) <- paste0("T", seq_len(J))
  
  coefs_full <- t(coef_W)
  if (!is.null(colnames(X_pcs))) {
    rownames(coefs_full) <- colnames(X_pcs)
  }
  colnames(coefs_full) <- paste0("T", seq_len(J))
  
  # Two-pass refitting with sparsity control (topK largest feature weights)
  if (!is.null(topK)) {
    if (!is.numeric(topK) || length(topK) != 1 || topK <= 0) {
      stop("topK must be a positive integer.")
    }
    topK <- as.integer(topK)
    selected_features <- c()
    for (j in seq_len(J)) {
      vals <- abs(coefs_full[, j])
      keep_idx <- order(vals, decreasing = TRUE)[1:min(topK, M)]
      selected_features <- union(selected_features, keep_idx)
    }
    selected_features <- sort(selected_features)
    
    X_pcs_selected <- X_pcs[, selected_features, drop = FALSE]
    results_selected <- fusedRidgeDirect(
      X_pcs = X_pcs_selected,
      y_raw = y_raw,
      thresholds = thresholds,
      covariates = covariates,
      lambda1 = lambda1,
      lambda2 = lambda2,
      family = family,
      standardize = standardize,
      foldid = foldid,
      topK = NULL,
      thresh = thresh,
      nlambda = nlambda,
      nfolds = nfolds,
      cv = cv,
      optim_control = optim_control,
      ...
    )
    
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
    fit = opt_final,
    cv = cv_obj,
    optimal_lambda = optimal_lambda_val,
    a0 = 0,
    coefs_covs = coef_covs_matrix,
    coefs_full = coefs_full,
    theta_matrix = t(coef_W),
    H = diag(J),
    thresholds = thresholds,
    family = family,
    y_matrix = y_matrix,
    mean_X = mean_X,
    sd_X = sd_X,
    covs_names = covs_names
  )
  class(results) <- c("fusedRidgeDirect", "fusedRidge")
  return(results)
}

#' Predict Method for Direct Fused Ridge Regression
#'
#' Obtains predictions from a fitted \code{fusedRidgeDirect} object on new data.
#'
#' @param object A fitted \code{fusedRidgeDirect} object.
#' @param newx Matrix of new predictor variables.
#' @param newcovs Optional matrix or data frame of new covariates. Must be provided if the model was trained with covariates.
#' @param type Type of prediction. \code{"link"} returns the linear predictor, and \code{"response"} returns the fitted probabilities (only relevant for \code{family = "binomial"} or \code{"poisson"}).
#' @param topK Optional integer. If provided, keeps only the \code{topK} absolute largest feature weights for prediction, zeroing out all others (enables sparsity control).
#' @param ... Additional arguments (passed to \code{predict.fusedRidge}).
#' @return A matrix of predictions with dimensions \code{nrow(newx) x length(thresholds)}.
#' @method predict fusedRidgeDirect
#' @export
predict.fusedRidgeDirect <- function(object, newx, newcovs = NULL, type = c("link", "response"), topK = NULL, ...) {
  # Direct mapping to predict.fusedRidge since structures are identical
  predict.fusedRidge(object = object, newx = newx, newcovs = newcovs, type = type, topK = topK, ...)
}
