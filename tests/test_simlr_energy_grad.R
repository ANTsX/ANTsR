library(testthat)
library(numDeriv)

# ==============================================================================
#     Expressive and Communicative Test Suite for SIMLR Gradient Functions
# ==============================================================================

# --- 1. Custom Expectation for Gradient Comparison (with Success Reporting) ---

#' Expectation: Are two gradient matrices numerically equal?
#'
#' This custom testthat expectation compares an analytical gradient to a
#' numerical one. It provides a detailed diagnostic report upon failure and
#' an explicit success message upon passing.
#'
#' @param analytical_grad The gradient matrix computed by the analytical function.
#' @param numerical_grad The gradient matrix computed by `numDeriv::grad`.
#' @param tolerance The tolerance for `expect_equal`.
expect_grad_equal <- function(analytical_grad, numerical_grad, tolerance = 1e-6) {
  
  # Get the objects for comparison
  act <- testthat::quasi_label(rlang::enquo(analytical_grad), arg = "analytical_grad")
  exp <- testthat::quasi_label(rlang::enquo(numerical_grad), arg = "numerical_grad")
  
  # Perform the comparison
  comparison <- all.equal(act$val, exp$val, tolerance = tolerance)
  is_equal <- isTRUE(comparison)
  
  # If the test passes, print a success message.
  if (is_equal) {
    testthat::succeed("Gradients match within tolerance.")
    # Return invisibly to not clutter the console when tests pass
    return(invisible(act$val))
  }
  
  # If the test fails, build a detailed failure message
  failure_message <- {
    cos_sim <- sum(act$val * exp$val) / (sqrt(sum(act$val^2)) * sqrt(sum(exp$val^2)))
    rel_err <- mean(abs(act$val - exp$val) / (abs(exp$val) + 1e-9))
    abs_diff <- mean(abs(act$val - exp$val))
    
    mismatch_snippet <- paste(utils::capture.output(utils::head(data.frame(
      Analytical = as.vector(act$val),
      Numerical  = as.vector(exp$val),
      Difference = as.vector(act$val) - as.vector(exp$val)
    ), 5)), collapse = "\n")
    
    sprintf(
      "Gradient mismatch detected.\n\n- Cosine Similarity: %.6f (should be exactly 1.0)\n- Mean Absolute Difference: %.2e\n- Mean Relative Error: %.2e\n\nFirst 5 mismatches:\n%s",
      cos_sim, abs_diff, rel_err, mismatch_snippet
    )
  }
  
  # Use `fail()` with the custom message
  testthat::fail(failure_message)
}


# --- 2. Test Execution ---
context("Verifying SIMLR Objective Gradients")

# --- Setup a standard test environment ---
# (Make sure to source your R/simlr_objectives.R and R/simlr_dispatchers.R files first)
set.seed(42)
n <- 50; p <- 20; k <- 4
X_test <- scale(matrix(rnorm(n * p), n, p))
U_test <- qr.Q(qr(matrix(rnorm(n * k), n, k)))
V_test <- matrix(rnorm(p * k), p, k)

# --- Define a generic test function ---
verify_gradient <- function(energy_type_string) {
  test_that(paste("Gradient for energy type '", energy_type_string, "' is correct"), {
    
    # Define the energy function E(V) to be numerically differentiated.
    energy_func <- function(v_vec) {
      calculate_simlr_energy(V = matrix(v_vec, p, k), X = X_test, U = U_test, energy_type = energy_type_string)
    }
    
    # Calculate the analytical DESCENT direction from our dispatcher.
    analytical_descent_dir <- calculate_simlr_gradient(V = V_test, X = X_test, U = U_test, energy_type = energy_type_string)
    
    # numDeriv::grad returns an ASCENT direction, grad(E).
    numerical_ascent_grad_E <- matrix(grad(energy_func, as.vector(V_test)), p, k)
    
    # The analytical function should return a descent direction, which is -grad(E).
    # Therefore, we compare our analytical result to the negative of the numerical gradient.
    expect_grad_equal(analytical_descent_dir, -numerical_ascent_grad_E)
  })
}

# --- Run the tests for all key energy types ---
# With the final corrected gradient, all of these will now pass and report success.
verify_gradient("regression")
verify_gradient("lowRankRegression") # This is angular_distance
verify_gradient("cca")
verify_gradient("normalized_correlation") # This is procrustes_corr


# --- Setup a standard test environment for all tests ---
set.seed(42)
n <- 50; p <- 20; k <- 4
X_test <- scale(matrix(rnorm(n * p), n, p))
U_test <- qr.Q(qr(matrix(rnorm(n * k), n, k)))
V_test <- matrix(rnorm(p * k), p, k)


# --- Define a generic test function for both static and dynamic checks ---
verify_gradient_and_descent <- function(energy_type_string) {
  
  # ==============================================================
  # Test 1: Static check of analytical vs. numerical gradient
  # ==============================================================
  test_that(paste("Analytical gradient for '", energy_type_string, "' is correct"), {
    
    # Define the energy function E(V) to be numerically differentiated.
    energy_func <- function(v_vec) {
      calculate_simlr_energy(V = matrix(v_vec, p, k), X = X_test, U = U_test, energy_type = energy_type_string)
    }
    
    # Get analytical DESCENT direction from our dispatcher.
    analytical_descent_dir <- calculate_simlr_gradient(V = V_test, X = X_test, U = U_test, energy_type = energy_type_string)
    
    # numDeriv::grad returns an ASCENT direction, grad(E).
    numerical_ascent_grad_E <- matrix(grad(energy_func, as.vector(V_test)), p, k)
    
    # We expect our descent direction to be the negative of the ascent gradient.
    expect_grad_equal(analytical_descent_dir, -numerical_ascent_grad_E, tolerance = 1e-5)
  })
  
  # ==============================================================
  # Test 2: Dynamic check to ensure the gradient improves the objective
  # ==============================================================
  test_that(paste("Gradient descent for '", energy_type_string, "' improves objective"), {
    
    # Initial state
    V_current <- V_test
    learning_rate <- 0.01
    num_steps <- 5
    
    # Calculate the initial energy
    initial_energy <- calculate_simlr_energy(V = V_current, X = X_test, U = U_test, energy_type = energy_type_string)
    
    # Perform a few steps of gradient descent
    for (i in 1:num_steps) {
      # Get the descent direction
      descent_dir <- calculate_simlr_gradient(V = V_current, X = X_test, U = U_test, energy_type = energy_type_string)
      
      # Simple additive update (we don't need manifold constraints for this basic test)
      V_current <- V_current + learning_rate * descent_dir
    }
    
    # Calculate the final energy
    final_energy <- calculate_simlr_energy(V = V_current, X = X_test, U = U_test, energy_type = energy_type_string)
    
    # --- The Assertion ---
    # The energy after several descent steps MUST be lower than the initial energy.
    # We use expect_lt() which checks if final_energy < initial_energy.
    
    info_message <- sprintf(
      "Energy did not decrease. Initial: %.4f, Final: %.4f",
      initial_energy, final_energy
    )
    
    expect_lt(final_energy, initial_energy, label = info_message)
  })
}

# --- Run the full test suite for all key energy types ---
verify_gradient_and_descent("regression")
verify_gradient_and_descent("lowRankRegression") # This is angular_distance
verify_gradient_and_descent("cca")
verify_gradient_and_descent("normalized_correlation") # This is procrustes_corr




library(testthat)
library(dplyr)

# ==============================================================================
#            Integration Test for the Full `simlr` Algorithm
# ==============================================================================
# This suite tests the end-to-end performance of the `simlr` function. It
# generates a ground-truth dataset with a known shared signal and asserts that
# simlr can successfully recover that signal under various configurations.
# ==============================================================================

# --- 1. Ground Truth Data Generation ---

#' Generate a simulated 3-view dataset for testing simlr
#'
#' @param n_subjects Number of subjects.
#' @param n_features A vector of 3 integers for the number of features in each view.
#' @param k_true The true number of latent components (shared signal).
#' @param noise_level The standard deviation of the Gaussian noise added.
#' @return A list containing `modality_matrices` and the ground-truth `U_true`.
generate_3view_ground_truth <- function(n_subjects = 100,
                                        n_features = c(50, 80, 60),
                                        k_true = 4,
                                        noise_level = 0.5) {
  
  # a) Create the true shared latent signal (U_true)
  U_true <- qr.Q(qr(matrix(rnorm(n_subjects * k_true), n_subjects, k_true)))
  
  # b) Create true, sparse loading matrices (V_true) for each modality
  V_true_mats <- lapply(n_features, function(p) {
    V <- matrix(rnorm(p * k_true), p, k_true)
    V[sample(length(V), size = floor(0.8 * length(V)))] <- 0 # 80% sparse
    return(V)
  })
  
  # c) Generate the observed data matrices (X = U_true * V_true' + Noise)
  modality_matrices <- mapply(function(V, p) {
    signal <- U_true %*% t(V)
    noise <- matrix(rnorm(n_subjects * p, sd = noise_level), n_subjects, p)
    return(signal + noise)
  }, V_true_mats, n_features, SIMPLIFY = FALSE)
  
  names(modality_matrices) <- paste0("View", 1:length(n_features))
  
  return(list(modality_matrices = modality_matrices, U_true = U_true))
}


# --- 2. The Core Test Logic ---

context("Testing simlr end-to-end performance")
preprocess_for_simlr <- function(modality_list) {
  stopifnot(is.list(modality_list))
  scaled_list <- lapply(modality_list, function(mat) {
    mat_centered <- scale(mat, center = TRUE, scale = FALSE)
    frobenius_norm <- sqrt(sum(mat_centered^2))
    if (frobenius_norm > .Machine$double.eps) {
      mat_scaled <- mat_centered / frobenius_norm
    } else {
      mat_scaled <- mat_centered
    }
    return(mat_scaled)
  })
  return(scaled_list)
}
# --- Setup: Generate the data once for all tests in this context ---
set.seed(42)
ground_truth_data <- generate_3view_ground_truth(
  n_subjects = 200,
  n_features = c(40, 250, 600),
  k_true = 3,
  noise_level = 0.05 # Low noise for a clear signal
)
# Pre-process the data as we would in a real analysis
scaled_mats <- preprocess_for_simlr(ground_truth_data$modality_matrices)


# --- Generic Test Function to Avoid Repetition ---
# This function runs simlr with a given config and checks the result.
run_and_verify_simlr <- function(energy, constraint_type, k_to_find, sparsenessAlg=NA) {
  
  test_that(paste("simlr recovers signal with energy '", energy, "' and constraint '", constraint_type, "'"), {
    
    if ( energy %in% c( "regression") ) {
      mixAlg = 'ica'
    } else mixAlg='pca'
    initu = initializeSimlr(
      voxmats = scaled_mats,
      k = k_to_find, uAlgorithm='pca'
    )
    # Run the full simlr algorithm
    # Note: Use a low iteration count to keep tests fast.
    # We don't need full convergence, just proof that it's heading in the right direction.
    result <- simlr( # Use the name of your final, refactored function
      voxmats = scaled_mats,
      iterations = 50,
      energyType = energy,
      constraint = constraint_type,
      mixAlg= mixAlg,
      initialUMatrix = initu,
      positivities=rep("positive", k_to_find),
      sparsenessAlg= sparsenessAlg,
      verbose = FALSE # Keep test output clean
    )
    
    # --- Evaluation ---
        
    # 2. The primary success metric: correlation between true U and found U
    # We take the average of the final U matrices to get the consensus basis
    U_found <- Reduce("+", result$u) / length(result$u)
    U_true <- ground_truth_data$U_true
    
    # Calculate the correlation between all columns of the two matrices
    correlation_matrix <- abs(cor(U_true, U_found))
    
    # Find the best match for each true component to handle column permutation
    # `max.col` gives us the column index of the max value in each row
    best_matches <- max.col(correlation_matrix, "first")
    
    # Get the correlation values of these best matches
    mean_recovery_correlation <- mean(correlation_matrix[cbind(1:k_to_find, best_matches)])
    orthE = simlr_feature_orth( result$v )
    print(paste("Corr: ", mean_recovery_correlation, " orth: ", orthE ))
    # 3. The Main Assertion
    # We expect a high correlation, indicating the signal was recovered.
    expect_gt(mean_recovery_correlation, 0.30,
              label = paste("Mean recovery correlation was low (",
                            round(mean_recovery_correlation, 3),
                            "), indicating failure to find the ground truth signal."))
              
    # Print success message
    succeed(sprintf("Successfully recovered signal with correlation %.3f", mean_recovery_correlation))
  })
}

# --- Run the tests for all relevant configurations ---

# Test the best correlation-based methods
energy_types <- c("normalized_correlation", "lrr", "acc", "regression")
constraint_types <- c("Stiefelx0.1", "orthox0.1", "Grassmannx0.1","Stiefelx0.05", "orthox0.05", "Grassmannx0.05", "none")
for ( x in energy_types ) {
  for ( y in constraint_types ) {
    for ( sp in c(NA,'spmp')) {
      print(paste("Running simlr with energy:", x, "and constraint:", y, " sparsenessAlg:", sp))
      run_and_verify_simlr(energy = x, constraint_type = y, k_to_find = 3, sparsenessAlg=sp)
    }
  }
}
