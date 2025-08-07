library(testthat)
library(numDeriv)
library(ANTsR)
set.seed(42)
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


compare_v_matrices <- function(V_true_list, V_found_list) {
  
  if (length(V_true_list) != length(V_found_list)) {
    stop("Input lists must have the same number of modalities.")
  }
  
  # Helper function to calculate cosine similarity between two vectors
  cosine_sim <- function(a, b) {
    sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
  }
  
  # Use purrr::map2_dfr for a clean, parallel iteration over both lists
  summary_df <- purrr::map2_dfr(V_true_list, V_found_list, .id = "modality", ~{
    V_true <- .x
    V_found <- .y
    
    k <- ncol(V_true)
    
    if (is.null(V_found) || ncol(V_found) != k) {
      # Handle cases where a run failed or produced wrong dimensions
      return(tibble::tibble(
        mean_cosine_similarity = NA_real_,
        permutation_map = "mismatch"
      ))
    }
    
    # --- 1. Find the Best Permutation ---
    
    # Calculate the k x k matrix of absolute correlations
    correlation_matrix <- abs(cor(V_true, V_found))
    
    # Use a greedy approach to find the best permutation. This is effective
    # and simple. For perfect guarantees, the Hungarian algorithm could be used.
    permutation_map <- numeric(k)
    remaining_found_cols <- 1:k
    
    for (i in 1:k) {
      # Find the best match for true column `i` among the remaining found columns
      best_match_idx <- which.max(correlation_matrix[i, remaining_found_cols])
      best_match_col <- remaining_found_cols[best_match_idx]
      
      permutation_map[i] <- best_match_col
      
      # Remove this column from the pool of available matches
      remaining_found_cols <- setdiff(remaining_found_cols, best_match_col)
    }
    
    # Reorder the V_found matrix according to the permutation map
    V_found_permuted <- V_found[, permutation_map]
    
    # --- 2. Align Signs ---
    
    # Calculate the signs of the correlations of the matched pairs
    original_correlations <- diag(cor(V_true, V_found_permuted))
    signs_to_flip <- sign(original_correlations)
    
    # Apply the sign flip. diag(signs) creates the sign-flipping matrix
    V_found_aligned <- V_found_permuted %*% diag(signs_to_flip)
    
    # --- 3. Calculate Final Similarity Metric ---
    
    # Now that V_found is aligned, calculate the cosine similarity for each pair
    final_similarities <- sapply(1:k, function(i) {
      cosine_sim(V_true[, i], V_found_aligned[, i])
    })

    final_dice <- sapply(1:k, function(i) {
      dice_overlap_soft_vector(V_true[, i], V_found_aligned[, i], quantile=0.66 )
    })

    # The final summary metric is the mean of these similarities
    mean_cosine_similarity <- mean(final_similarities)
    mean_dice <- mean(final_dice)
    
    # Create a readable string for the permutation map
    permutation_string <- paste(sprintf("%d->%d", 1:k, permutation_map), collapse = ", ")
    
    tibble::tibble(
      mean_cosine_similarity = mean_cosine_similarity,
      mean_dice = mean_dice,
      permutation_map = permutation_string
    )
  })
  
  return(summary_df)
}

# --- 1. Ground Truth Data Generation ---

#' Generate a Simulated Multi-View Dataset for Testing SIMLR
#'
#' This function creates simulated multi-view data with a known underlying
#' latent structure. It can generate data based on a "fully_shared" model or a
#' more realistic "partially_shared" model.
#'
#' @param n_subjects Number of subjects (rows).
#' @param n_features A vector of integers specifying the number of features for each view.
#' @param k_shared The number of latent components that are common to ALL views.
#' @param noise_level The standard deviation of the Gaussian noise added.
#' @param simulation_type The model for the ground truth.
#' @param k_unique_per_view An integer specifying how many unique components to
#'   generate for EACH view. Only used with `simulation_type = "partially_shared"`.
#'
#' @return A list containing:
#'   \item{modality_matrices}{The final list of [n x p] data matrices.}
#'   \item{U_true}{The full ground truth basis [n x (k_shared + k_unique*n_views)]
#'     that was used to generate the data.}
#'   \item{U_shared}{The ground truth basis for ONLY the shared components [n x k_shared].
#'     This is what `simlr` should ideally recover.}
#'   \item{V_shared_mats}{A list of the ground truth loading matrices corresponding
#'     to ONLY the shared components. Each matrix is [p x k_shared]. This is
#'     what the recovered `result$v` should be compared against.}
#'
#' @export
generate_multiview_ground_truth <- function(n_subjects = 100,
                                            n_features = c(50, 80, 60),
                                            k_shared = 4,
                                            noise_level = 0.5,
                                            simulation_type = c("fully_shared", "partially_shared"),
                                            k_unique_per_view = 1) {

  simulation_type <- match.arg(simulation_type)
  n_modalities <- length(n_features)
  
  # --- Create the Shared Signal Component (common to both models) ---
  U_shared <- qr.Q(qr(matrix(rnorm(n_subjects * k_shared), n_subjects, k_shared)))
  
  # Create the loading matrices for the shared part
  V_shared_mats <- lapply(n_features, function(p) {
    V <- matrix(rnorm(p * k_shared), p, k_shared)
#    V = simlr_sparseness(V, constraint_type = "none", positivity = 'positive', sparseness_quantile = 0.8, sparseness_alg = NA, energy_type = 'acc' )
    # Make them sparse for realism
#    V[sample(length(V), size = floor(0.8 * length(V)))] <- 0
    return(V)
  })
  
  # --- Generate Data Based on Simulation Type ---
  if (simulation_type == "fully_shared") {
    
    U_true <- U_shared # In this model, the full truth is just the shared part
    
    modality_matrices <- mapply(function(V_s, p) {
      signal <- U_shared %*% t(V_s)
      noise <- matrix(rnorm(n_subjects * p, sd = noise_level), n_subjects, p)
      return(signal + noise)
    }, V_shared_mats, n_features, SIMPLIFY = FALSE)

  } else { # "partially_shared"
    
    # a) Create the unique signals
    U_unique_list <- lapply(1:n_modalities, function(i) {
      qr.Q(qr(matrix(rnorm(n_subjects * k_unique_per_view), n_subjects, k_unique_per_view)))
    })
    
    # b) Combine shared and unique U's to form the full generative basis
    U_true <- do.call(cbind, c(list(U_shared), U_unique_list))
    
    # c) Create the full, block-sparse V matrices needed for data generation
    V_generative_mats <- lapply(1:n_modalities, function(i) {
      p <- n_features[i]
      v_blocks <- list()
      
      # The first block is the shared loadings we already created
      v_blocks$shared <- V_shared_mats[[i]]
      
      # Create the unique and zero blocks
      for (j in 1:n_modalities) {
        if (i == j) {
          v_blocks[[paste0("unique_", j)]] <- matrix(rnorm(p * k_unique_per_view), p, k_unique_per_view)
        } else {
          v_blocks[[paste0("unique_", j)]] <- matrix(0, nrow = p, ncol = k_unique_per_view)
        }
      }
      return(do.call(cbind, v_blocks))
    })
    
    # d) Generate the final data matrices
    modality_matrices <- mapply(function(V_gen, p) {
      signal <- U_true %*% t(V_gen)
      noise <- matrix(rnorm(n_subjects * p, sd = noise_level), n_subjects, p)
      return(signal + noise)
    }, V_generative_mats, n_features, SIMPLIFY = FALSE)
  }
  
  names(modality_matrices) <- paste0("View", 1:length(n_features))
  
  return(list(
    modality_matrices = modality_matrices,
    U_true = U_true,
    U_shared = U_shared,
    V_shared_mats = V_shared_mats # THE NEW, CONTROLLABLE OUTPUT
  ))
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
featp=c(40, 80, 120)*3
kuniq=20
ground_truth_data <- generate_multiview_ground_truth(
  n_subjects = 200,
  n_features = featp,
  k_shared = 3,
  k_unique_per_view = kuniq, # More unique components for realism
  simulation_type="partially_shared",
  noise_level = 0.05 # Low noise for a clear signal
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
run_simlr_config <- function(energy, constraint_type, k_to_find, sparsenessAlg, scaled_mats, U_true, V_true ) {
  
  # Set up parameters for the run
  mixAlg <- 'pca'
  if (energy %in% c("regression","reconorm")) mixAlg <- 'ica'
  mixAlg <- 'svd'
  initu = initializeSimlr(scaled_mats, k_to_find, uAlgorithm='pca', jointReduction=FALSE )
  # We still use tryCatch to handle potential errors in any single run gracefully
  result <- tryCatch({
    simlr(
      voxmats = scaled_mats,
      iterations = 1000,
      energyType = energy,
      constraint = constraint_type,
      mixAlg = mixAlg,
      initialUMatrix = initu, # Assuming simlr handles integer initialization
      positivities = rep("positive", length(scaled_mats)), # Corrected this
      sparsenessAlg = sparsenessAlg,
      scale = c(  "centerAndScale" ),
      randomSeed=808,
      expBeta = 0.9,
      optimizationStyle = "lineSearch", # Use Adam optimizer
      verbose = 0
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
      converged_at = NA_integer_,
      mean_correlation = NA_real_,
      feature_orthogonality = NA_real_,
      sparsity_alg = ifelse(is.na(sparsenessAlg), "none", sparsenessAlg),
      status = "failed"
    ))
  }

  v_comparison=( compare_v_matrices( V_true, result$v ) )
  overall_mean_similarity <- mean(v_comparison$mean_cosine_similarity, na.rm = TRUE)
  overall_mean_dice = mean( v_comparison$mean_dice, na.rm = TRUE)
  
  # --- Evaluation ---
#  projected_mats <- mapply(function(X, V) {
#      # This is the correct operation: X_i %*% V_i
#      X %*% V
#    }, scaled_mats, result$v, SIMPLIFY = FALSE)
#  U_found <- Reduce("+", projected_mats) / length(projected_mats)
#  correlation_matrix <- abs(cor(U_true, U_found))
#  best_matches <- max.col(correlation_matrix, "first")
#  mean_recovery_correlation <- mean(correlation_matrix[cbind(1:k_to_find, best_matches)])

  # Assuming you have a function `simlr_feature_orth`
  orth_error <- simlr_feature_orth(result$v)

  # Return a clean, one-row tibble
  tabres=tibble(
    energy_type = energy,
    constraint = constraint_type,
    converged_at = result$converged_at,
    mean_correlation = overall_mean_similarity,
    mean_dice = overall_mean_dice,
    feature_orthogonality = orth_error,
    sparsity_alg = ifelse(is.na(sparsenessAlg), "none", sparsenessAlg),
    status = "success"
  )
  print(tabres)
  return(tabres)
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
  # Pre-process the data
  scaled_mats <- preprocess_for_simlr(ground_truth_data$modality_matrices)
  U_true <- ground_truth_data$U_true
  U_shared = ground_truth_data$U_shared
  k_to_find <- 3
  

  mycs = c(  "Stiefelx0", "Grassmannx0",  "orthox0.2x50", "orthox0.15x50", "orthox0.1x50","orthox0.08x50", "orthox0.06x50","orthox0.04x50","orthox0.02x50", "orthox0.01x50",  "orthox0.005x50", "orthox0.001x50", "none" )
  mycs = c( "Grassmannx0", "Stiefelx0", "orthox0.1x50",  "none" )
  mycs = c( "Grassmannx0", "Stiefelx0", "orthox0.01x100", "orthox0.1x100", "orthox0.1x5",   "none" )
  eggs = c(  "regression", "reconorm", "normalized_correlation", "lrr", "acc")
  # --- Define Parameter Grid ---
  param_grid <- expand.grid(
    energy = eggs,
    constraint = mycs,
    sparsity = c(NA),
#    sparsity = c('spmp'),
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
      U_true = U_shared,
      V_true = ground_truth_data$V_shared_mats
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

print("Top K Performing Configurations:")
print(head(data.frame(top_performers), 5))


library(ggplot2)
library(ggrepel)
library(viridis)  # for better color palettes
performance_summary <- performance_summary %>%
  mutate(
    orth.wt = case_when(
      constraint == "none" ~ 0,
      grepl("x0$", constraint) ~ 1,
      TRUE ~ as.numeric(sub(".*x", "", constraint))
    )
  )

library(ggplot2)
library(ggrepel)

print(
ggplot(performance_summary, aes(
    x = feature_orthogonality,
    y = mean_correlation,
    color = orth.wt,
    label = constraint
  )) +
  geom_point(size = 4, alpha = 0.85) +
  geom_text_repel(size = 3, max.overlaps = 30, seed = 42) +
  facet_wrap(~energy_type, scales = "free_x") +
  scale_color_viridis_c(option = "turbo", name = "Orthogonality\nWeight") +
  theme_bw(base_size = 14) +
  labs(
    title = "SIMLR Performance: Dice vs. Orthogonality",
    x = "Feature Orthogonality Error (Lower is Better)",
    y = "Quantized Dice with Ground Truth (Higher is Better)"
  ) +
  theme(
    legend.position = "right",
    strip.text = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold")
  ) )

  # ggsave("simlr_performance_scatter.png", width = 12, height = 8, dpi = 300)