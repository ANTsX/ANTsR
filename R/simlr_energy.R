

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
  # This is the DEFINITIVE, CORRECTED gradient for J = tr(U'XV) / ||U'XV||_F
  C <- crossprod(U, X %*% V)
  trace_C <- sum(diag(C))
  norm_C_sq <- sum(C^2)
  if (!is.finite(norm_C_sq) || norm_C_sq < .Machine$double.eps) return(V * 0)
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
  if (!is.finite(norm_U) || !is.finite(norm_XV) || 
      norm_U < .Machine$double.eps || norm_XV < .Machine$double.eps) return(2.0)
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
  if (!is.finite(norm_U) || !is.finite(norm_XV) || 
      norm_U < .Machine$double.eps || norm_XV < .Machine$double.eps) return(V * 0)

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
  if (!is.finite(frobenius_norm) || frobenius_norm < .Machine$double.eps) return(0)
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
  if (!is.finite(norm_C) || norm_C < .Machine$double.eps) {
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
  if (!is.finite(norm_U) || !is.finite(norm_XV) || 
      norm_U < .Machine$double.eps || norm_XV < .Machine$double.eps) {
    return(0)
  }

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
  if (!is.finite(norm_U) || !is.finite(norm_XV) || 
      norm_U < .Machine$double.eps || norm_XV < .Machine$double.eps) {
    return(V * 0)
  }

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
  if (!is.finite(norm_X_sq) || norm_X_sq < .Machine$double.eps) {
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
  if (!is.finite(norm_X_sq) || norm_X_sq < .Machine$double.eps) {
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
#' @param U A weighting matrix [n x k].
#' @param V A loading matrix [p x k].
#' @param nonlinearity Non-linearity function ("logcosh", "exp", "kurtosis", or "gauss").
#' @param a Parameter for gaussian nonlinearity (default: 1).
#' @return A single numeric value representing the ICA energy.
#' @keywords internal
.calculate_ica_energy <- function(X, U, V, nonlinearity = "logcosh", a = 1) {
  # Input validation
  stopifnot(
    is.matrix(X) && is.matrix(V),
    "Matrix dimensions are not compatible." = ncol(X) == nrow(V),
    "Nonlinearity must be 'logcosh', 'exp', 'kurtosis' or 'gauss'." = nonlinearity %in% c("logcosh", "exp", "gauss", "kurtosis"),
    "Parameter a must be positive for gaussian nonlinearity." = a > 0
  )
  
  # Calculate sources
  Vmod = l1_normalize_features(V)
  S <- (t(U) %*% X) %*% Vmod
  
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
#' of the specified non-linearity, with bilinear source S = U^T X V.
#'
#' @param X A data matrix [n x p].
#' @param U A weighting matrix [n x k].
#' @param V A loading matrix [p x k].
#' @param nonlinearity Non-linearity function ("logcosh", "exp", "kurtosis", or "gauss").
#' @param a Parameter for gaussian nonlinearity (default: 1).
#' @return A matrix [p x k] representing a descent direction for the ICA energy.
#' @keywords internal
#' @export
.calculate_ica_gradient <- function(X, U, V, nonlinearity = "logcosh", a = 1) {
  # Defensive dimension checks
  stopifnot(
    is.matrix(X) && is.matrix(U) && is.matrix(V),
    "Matrix dimensions are not compatible." = (nrow(X) == nrow(U)) && (ncol(X) == nrow(V)) && (ncol(U) == ncol(V)),
    "Nonlinearity must be 'logcosh', 'exp', 'kurtosis', or 'gauss'." = nonlinearity %in% c("logcosh", "exp", "gauss", "kurtosis"),
    "Parameter a must be positive for gaussian nonlinearity." = a > 0
  )
  
  # Calculate sources: S = U^T X V
  Vmod <- l1_normalize_features(V)
  S <- t(U) %*% X %*% Vmod  # k x k matrix
  
  # Compute gradient based on nonlinearity
  if (nonlinearity == "logcosh") {
    gradient <- (1/nrow(S)) * (t(X) %*% U %*% tanh(S))
  } else if (nonlinearity == "exp") {
    gradient <- (1/nrow(S)) * (t(X) %*% U %*% (S * exp(-S^2 / 2)))
  } else if (nonlinearity == "gauss") {
    gradient <- (1/nrow(S)) * (t(X) %*% U %*% (a * S * exp(-a * S^2)))
  } else if (nonlinearity == "kurtosis") {
    gradient <- (1/nrow(S)) * (t(X) %*% U %*% (S^3))
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
#' @export
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
#' @export
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
  
  # Handle the edge case where the threshold is zero or non-finite
  if (!is.finite(threshold) || threshold < .Machine$double.eps) {
    return(gradient) # No clipping needed if threshold is zero or invalid
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

#' Calculate SiMLR energy for optimization
#'
#' Computes the energy for SiMLR based on the specified energy type, with partial
#' matching for energy_type. Returns negative values for maximization objectives
#' to align with minimization goals.
#'
#' @param V Matrix V in the SiMLR decomposition.
#' @param X Data matrix for the modality.
#' @param U Matrix U in the SiMLR decomposition.
#' @param energy_type Character string specifying the energy type. Supports partial
#'   matching (e.g., "reg" for "regression", "cca" for "cca" or "acc").
#'   Valid options: regression, reconorm, lowRankRegression, lrr, cca, acc,
#'   logcosh, kurtosis, exp, gauss, normalized_correlation, dat.
#' @param lambda Numeric, weight for domain energy (default = 1.0).
#' @param prior_matrix Optional prior matrix for domain energy (used with "dat").
#' @param verbose Integer, verbosity level: 0 (none), 1 (log matched energy_type) (default = 0).
#'
#' @return Numeric energy value (negative for maximization objectives).
#' @export
calculate_simlr_energy <- function(V, X, U, energy_type, lambda = 1.0, prior_matrix = NULL, verbose = 0) {
  if (!is.numeric(verbose) || verbose < 0 || verbose > 1 || verbose != as.integer(verbose)) {
    stop("verbose must be 0 or 1")
  }
  
  # Validate inputs
  if (!is.matrix(V) || !is.matrix(X) || !is.matrix(U)) {
    stop("V, X, and U must be matrices")
  }
  if (!is.numeric(lambda) || lambda < 0) {
    stop("lambda must be a non-negative numeric value")
  }
  if (!is.null(prior_matrix) && !is.matrix(prior_matrix)) {
    stop("prior_matrix must be a matrix or NULL")
  }
  
  # Define valid energy types
  valid_energy_types <- c(
    "regression",
    "reconorm",
    "lowRankRegression",
    "lrr",
    "cca",
    "acc",
    "logcosh",
    "kurtosis",
    "exp",
    "gauss",
    "normalized_correlation",
    "dat"
  )
  
  # Partial matching for energy_type (case-insensitive)
  energy_type <- tolower(energy_type)
  matched_type <- pmatch(energy_type, tolower(valid_energy_types), nomatch = NA_integer_)
  if (is.na(matched_type)) {
    stop(sprintf("Unknown energy_type: '%s'. Valid options are: %s",
                 energy_type, paste(valid_energy_types, collapse = ", ")))
  }
  if (sum(pmatch(energy_type, tolower(valid_energy_types), nomatch = 0) > 0) > 1) {
    stop(sprintf("Ambiguous energy_type: '%s' matches multiple options: %s",
                 energy_type, paste(valid_energy_types[pmatch(energy_type, tolower(valid_energy_types), nomatch = 0) > 0], collapse = ", ")))
  }
  energy_type <- valid_energy_types[matched_type]
  
  if (verbose >= 1) {
    message(sprintf("Matched energy_type: %s", energy_type))
  }
  
  # Assign prior_matrix to Z for "dat" energy type
  Z <- prior_matrix
  
  # Compute energy based on matched energy_type
  energy <- switch(energy_type,
    regression = .calculate_regression_error(X, U, V),
    reconorm = .calculate_normed_regression_error(X, U, V),
    lowRankRegression = .calculate_angular_distance(X, U, V),
    lrr = .calculate_angular_distance(X, U, V),
    cca = -.calculate_abs_canonical_covariance(X, U, V),
    acc = -.calculate_abs_canonical_covariance(X, U, V),
    logcosh = .calculate_ica_energy(X, U, V, nonlinearity = energy_type),
    kurtosis = .calculate_ica_energy(X, U, V, nonlinearity = energy_type),
    exp = .calculate_ica_energy(X, U, V, nonlinearity = energy_type),
    gauss = .calculate_ica_energy(X, U, V, nonlinearity = energy_type),
    normalized_correlation = -.calculate_procrustes_correlation(X, U, V),
    dat = .calculate_domain_energy(V, Z, lambda),
    stop(sprintf("Internal error: matched energy_type '%s' not handled in switch", energy_type))
  )
  
  return(energy)
}

#' Calculate SiMLR Similarity Gradient
#'
#' This dispatcher computes the gradient for the similarity part of the SiMLR
#' objective, ensuring it is a descent direction for the energy function. Supports
#' partial matching for energy_type.
#'
#' @param V The current loading matrix [p x k].
#' @param X The data matrix for the modality [n x p].
#' @param U The shared basis matrix [n x k].
#' @param energy_type Character string specifying the similarity objective.
#'   Supports partial matching (e.g., "reg" for "regression", "cca" for "cca" or "acc").
#'   Valid options: regression, reconorm, lowRankRegression, lrr, cca, acc,
#'   normalized_correlation, nc, logcosh, kurtosis, exp, gauss, dat.
#' @param clipping_threshold Optional numeric value for gradient clipping.
#' @param lambda Numeric, weight for domain energy (default = 1.0).
#' @param prior_matrix Optional matrix of prior weights, same shape as V (used with "dat").
#' @param verbose Integer, verbosity level: 0 (none), 1 (log matched energy_type) (default = 0).
#'
#' @return A matrix [p x k] representing the descent direction.
#' @export
calculate_simlr_gradient <- function(V, X, U, 
  energy_type, clipping_threshold = NULL, 
  lambda = 1.0, prior_matrix = NULL, verbose = 0) {
  if (!is.numeric(verbose) || verbose < 0 || verbose > 1 || verbose != as.integer(verbose)) {
    stop("verbose must be 0 or 1")
  }
  
  # Validate inputs
  if (!is.matrix(V) || !is.matrix(X) || !is.matrix(U)) {
    stop("V, X, and U must be matrices")
  }
  if (!is.numeric(lambda) || lambda < 0) {
    stop("lambda must be a non-negative numeric value")
  }
  if (!is.null(prior_matrix) && !is.matrix(prior_matrix)) {
    stop("prior_matrix must be a matrix or NULL")
  }
  if (!is.null(clipping_threshold) && (!is.numeric(clipping_threshold) || clipping_threshold <= 0)) {
    stop("clipping_threshold must be a positive numeric value or NULL")
  }
  
  # Define valid energy types
  valid_energy_types <- c(
    "regression",
    "reconorm",
    "lowRankRegression",
    "lrr",
    "cca",
    "acc",
    "normalized_correlation",
    "nc",
    "logcosh",
    "kurtosis",
    "exp",
    "gauss",
    "dat"
  )
  
  # Partial matching for energy_type (case-insensitive)
  energy_type <- tolower(energy_type)
  matched_type <- pmatch(energy_type, tolower(valid_energy_types), nomatch = NA_integer_)
  if (is.na(matched_type)) {
    stop(sprintf("Unknown energy_type: '%s'. Valid options are: %s",
                 energy_type, paste(valid_energy_types, collapse = ", ")))
  }
  if (sum(pmatch(energy_type, tolower(valid_energy_types), nomatch = 0) > 0) > 1) {
    stop(sprintf("Ambiguous energy_type: '%s' matches multiple options: %s",
                 energy_type, paste(valid_energy_types[pmatch(energy_type, tolower(valid_energy_types), nomatch = 0) > 0], collapse = ", ")))
  }
  energy_type <- valid_energy_types[matched_type]
  
  if (verbose >= 1) {
    message(sprintf("Matched energy_type: %s", energy_type))
  }
  
  # Assign prior_matrix to Z for "dat" energy type
  Z <- prior_matrix
  
  # Compute gradient based on matched energy_type
  gradient <- switch(energy_type,
    regression = .calculate_regression_gradient(X, U, V),
    reconorm = .calculate_normed_regression_error_gradient(X, U, V),
    lowRankRegression = .calculate_angular_distance_gradient(X, U, V),
    lrr = .calculate_angular_distance_gradient(X, U, V),
    cca = .calculate_abs_canonical_covariance_gradient(X, U, V),
    acc = .calculate_abs_canonical_covariance_gradient(X, U, V),
    normalized_correlation = .calculate_procrustes_gradient(X, U, V),
    nc = .calculate_procrustes_gradient(X, U, V),
    logcosh = .calculate_ica_gradient(X, U, V, nonlinearity = energy_type),
    kurtosis = .calculate_ica_gradient(X, U, V, nonlinearity = energy_type),
    exp = .calculate_ica_gradient(X, U, V, nonlinearity = energy_type),
    gauss = .calculate_ica_gradient(X, U, V, nonlinearity = energy_type),
    dat = .calculate_domain_gradient(V, Z, lambda),
    stop(sprintf("Internal error: matched energy_type '%s' not handled in switch", energy_type))
  )
  
  # Apply gradient clipping if a threshold is provided
  if (!is.null(clipping_threshold) && clipping_threshold > 0) {
    gradient <- clip_gradient_by_quantile(gradient)
  }
  
  return(as.matrix(gradient))
}


#' Optimal initializer for SiMLR
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

