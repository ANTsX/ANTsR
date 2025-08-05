library(testthat)
library(numDeriv)
library(ANTsR)
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
  noise_level = 0.15 # Low noise for a clear signal
)
# Pre-process the data as we would in a real analysis
scaled_mats <- preprocess_for_simlr(ground_truth_data$modality_matrices)

#' Run a single configuration of simlr and return performance metrics
#'
#' This worker function executes one run of the simlr algorithm and computes
#' the key output metrics: the mean correlation with the ground truth signal
#' and the final feature orthogonality error.
#'
#' @param energy The energyType to use.
#' @param constraint_type The constraint string to use.
#' @param k_to_find The number of components to find.
#' @param sparsenessAlg The sparseness algorithm to use.
#' @param scaled_mats The list of preprocessed input matrices.
#' @param U_true The ground truth U matrix.
#' @return A tibble with one row summarizing the results for this configuration.
run_simlr_config <- function(energy, constraint_type, k_to_find, sparsenessAlg, scaled_mats, U_true) {
  
  # Set up parameters for the run
  mixAlg <- if (energy %in% c("regression")) 'ica' else 'pca'
  initu = initializeSimlr(scaled_mats, k_to_find, uAlgorithm='pca' )
  # We still use tryCatch to handle potential errors in any single run gracefully
  result <- tryCatch({
    simlr(
      voxmats = scaled_mats,
      iterations = 50,
      energyType = energy,
      constraint = constraint_type,
      mixAlg = mixAlg,
      initialUMatrix = initu, # Assuming simlr handles integer initialization
      positivities = rep("positive", length(scaled_mats)), # Corrected this
      sparsenessAlg = sparsenessAlg,
      randomSeed=0,
      verbose = FALSE
    )
  }, error = function(e) {
    # If a run fails, return NULL so we can filter it out later
    warning(paste("Run failed for", energy, constraint_type, sparsenessAlg, ":", e$message))
    return(NULL)
  })
  
  # If the run failed, return a row of NAs
  if (is.null(result)) {
    return(tibble(
      energy_type = energy,
      constraint = constraint_type,
      sparsity_alg = ifelse(is.na(sparsenessAlg), "none", sparsenessAlg),
      mean_correlation = NA_real_,
      feature_orthogonality = NA_real_,
      status = "failed"
    ))
  }
  
  # --- Evaluation ---
  U_found <- Reduce("+", result$u) / length(result$u)
  
  correlation_matrix <- abs(cor(U_true, U_found))
  best_matches <- max.col(correlation_matrix, "first")
  
  mean_recovery_correlation <- mean(correlation_matrix[cbind(1:k_to_find, best_matches)])
  
  # Assuming you have a function `simlr_feature_orth`
  orth_error <- simlr_feature_orth(result$v)
  
  # Return a clean, one-row tibble
  tibble(
    energy_type = energy,
    constraint = constraint_type,
    sparsity_alg = ifelse(is.na(sparsenessAlg), "none", sparsenessAlg),
    mean_correlation = mean_recovery_correlation,
    feature_orthogonality = orth_error,
    status = "success"
  )
}


#' Run a parameter sweep for simlr and tabulate performance.
#'
#' This function iterates through a grid of specified energy types, constraint
#' types, and sparsity algorithms. It runs simlr for each configuration and
#' collects key performance metrics into a summary data frame.
#'
#' @return A data frame where each row corresponds to one simlr run, with
#'   columns for the parameters and the resulting performance metrics.
tabulate_simlr_performance <- function() {
  
  # --- Setup: Generate Ground Truth Data ---
  set.seed(42)
  ground_truth_data <- generate_3view_ground_truth(
    n_subjects = 100,
    n_features = c(250, 1098, 606),
    k_true = 3,
    noise_level = 0.25
  )
  
  # Pre-process the data
  scaled_mats <- preprocess_for_simlr(ground_truth_data$modality_matrices)
  U_true <- ground_truth_data$U_true
  k_to_find <- 3
  
  # --- Define Parameter Grid ---
  param_grid <- expand.grid(
    energy = c("normalized_correlation", "lrr", "acc", "regression"),
    constraint = c("Stiefelx0.1", "orthox0.1", "Grassmannx0.1", "Stiefelx0.25", "orthox0.25", "Grassmannx0.25", "Stiefelx0.5", "orthox0.5", "Grassmannx0.5", "Stiefelx0",  "Grassmannx0", "none"),
    sparsity = c(NA, 'spmp'),
    stringsAsFactors = FALSE
  )
  
  # --- Run the Parameter Sweep ---
  message(paste("Starting parameter sweep for", nrow(param_grid), "configurations..."))
  
  # Use purrr::pmap_dfr for a clean way to iterate over rows of the grid
  # and automatically row-bind the results.
  results_table <- purrr::pmap_dfr(param_grid, function(energy, constraint, sparsity) {
    
    cat(".") # Print a dot for progress
    
    run_simlr_config(
      energy = energy,
      constraint_type = constraint,
      k_to_find = k_to_find,
      sparsenessAlg = sparsity,
      scaled_mats = scaled_mats,
      U_true = U_true
    )
  })
  
  message("\nParameter sweep complete.")
  return(results_table)
}

# Run the entire analysis
performance_summary <- tabulate_simlr_performance()

# View the results
print(performance_summary)

# You can now easily analyze the results, for example:
# Find the top 5 best-performing configurations based on correlation
top_performers <- performance_summary %>%
  filter(status == "success") %>%
  arrange(desc(mean_correlation))

print("Top 5 Performing Configurations:")
print(head(top_performers, 5))


# Analyze the trade-off between correlation and orthogonality
library(ggplot2)
print( 
  ggplot(performance_summary, aes(x = feature_orthogonality, y = mean_correlation, color = energy_type)) +
    geom_point(alpha = 0.8) +
    facet_wrap(~constraint, scales = "free_x") +
    theme_bw() +
    labs(title = "SIMLR Performance: Correlation vs. Orthogonality",
          x = "Feature Orthogonality Error (Lower is Better)",
          y = "Mean Correlation with Ground Truth (Higher is Better)")
)



# Analyze the trade-off between correlation and orthogonality
print( 
  ggplot(performance_summary, aes(x = feature_orthogonality, y = mean_correlation, color = constraint)) +
    geom_point(alpha = 0.8) +
    facet_wrap(~energy_type, scales = "free_x") +
    theme_bw() +
    labs(title = "SIMLR Performance: Correlation vs. Orthogonality",
          x = "Feature Orthogonality Error (Lower is Better)",
          y = "Mean Correlation with Ground Truth (Higher is Better)")
)

