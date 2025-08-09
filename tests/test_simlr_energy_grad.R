# ==============================================================================
#      Comprehensive Evaluation Framework for SIMLR Performance
# ==============================================================================
# This script provides a framework to test simlr's ability to disentangle
# shared vs. specific signals from a complex, realistic simulated dataset.
#
# To Run:
# 1. Source all necessary functions:
#    - Your final `simlr` function and its dependencies.
#    - The `generate_structured_multiview_data` function.
# 2. Run this script.
# ==============================================================================

# --- Load Required Libraries ---
library(dplyr)
library(tibble)
# install.packages("pracma") # For the subspace angle calculation
library(pracma)
library(ANTsR)
library(ggplot2)
# ==============================================================================
#           1. Evaluation Helper Functions
# ==============================================================================

#' Compare two loading matrices (V_true vs V_found) via aligned cosine similarity
#' @return A single numeric value: the mean cosine similarity of the best-matched columns.
compare_feature_loadings <- function(V_true, V_found) {
  if (is.null(V_found) || !is.matrix(V_found) || any(dim(V_true) != dim(V_found))) {
    warning("V_found is invalid or has mismatched dimensions. Returning NA.")
    return(NA_real_)
  }
  
  cosine_sim <- function(a, b) { sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2))) }
  k <- ncol(V_true)
  
  correlation_matrix <- abs(cor(V_true, V_found))
  permutation_map <- numeric(k)
  remaining_found_cols <- 1:k
  
  for (i in 1:k) {
    if (length(remaining_found_cols) == 1) {
      best_match_col <- remaining_found_cols
    } else {
      best_match_idx <- which.max(correlation_matrix[i, remaining_found_cols])
      best_match_col <- remaining_found_cols[best_match_idx]
    }
    permutation_map[i] <- best_match_col
    remaining_found_cols <- setdiff(remaining_found_cols, best_match_col)
  }
  
  V_found_permuted <- V_found[, permutation_map, drop = FALSE]
  signs_to_flip <- sign(diag(cor(V_true, V_found_permuted)))
  V_found_aligned <- V_found_permuted %*% diag(signs_to_flip, nrow = k, ncol = k)
  
  final_similarities <- sapply(1:k, function(i) cosine_sim(V_true[, i], V_found_aligned[, i]))
  return(mean(final_similarities, na.rm = TRUE))
}

#' Compare two latent spaces (U_true vs U_found)
#' @return A named list of metrics: mean_correlation and subspace_angle_degrees.
compare_latent_spaces <- function(U_true, U_found) {
  if (is.null(U_found) || !is.matrix(U_found) || any(dim(U_true) != dim(U_found))) {
    warning("U_found is invalid or has mismatched dimensions. Returning NAs.")
    return(list(mean_correlation = NA_real_, subspace_angle_degrees = NA_real_))
  }
  
  # Metric 1: Mean correlation of best-matched components
  correlation_matrix <- abs(cor(U_true, U_found))
  best_matches <- max.col(correlation_matrix, "first")
  mean_corr <- mean(correlation_matrix[cbind(1:ncol(U_true), best_matches)])
  
  # Metric 2: Angle between subspaces
  # This measures how much the entire space spanned by U_found differs from
  # the space spanned by U_true. A value near 0 is perfect.
  # We use the principal angles function from the `pracma` package.
  principal_angles <- pracma::subspace(U_true, U_found) * 180 / pi # Convert to degrees
  mean_subspace_angle <- mean(principal_angles)
  
  return(list(
    mean_correlation = mean_corr,
    subspace_angle_degrees = mean_subspace_angle
  ))
}


# ==============================================================================
#           2. The Main Evaluation Harness
# ==============================================================================

#' Run and Evaluate a Single SIMLR Configuration
#'
#' This function runs a single instance of `simlr` on simulated data and
#' performs a comprehensive evaluation against the ground truth.
#'
#' @param params A list containing the parameters for this run (e.g., energy, constraint).
#' @param data_list The list of simulated modality data matrices.
#' @param ground_truth The ground truth object from the simulation function.
#' @return A one-row tibble summarizing the performance metrics for this run.
run_and_evaluate_simlr <- function(params, data_list, ground_truth) {
  
  # --- Setup ---
  k_to_find <- ncol(ground_truth$U_shared) # We ask simlr to find the SHARED signal
  
  # Use a consistent initialization for all runs
  init_u <- initializeSimlr(data_list, k = k_to_find, uAlgorithm = 'pca', jointReduction=FALSE)
  if ( params$energy %in% c("regression","reg") ) mixer='ica' else mixer='pca'
  # --- Run SIMLR ---
  result <- tryCatch({
    simlr(
      voxmats = data_list,
      initialUMatrix = init_u,
      iterations = 500, # A reasonable number for a test
      energyType = params$energy,
      constraint = params$constraint,
      sparsenessQuantiles = rep(0.8,length(data_list)),
      positivities=rep('positive',length(data_list)),
      optimizationStyle=params$optimizer,
      mixAlg=mixer,
      scale=c("centerAndScale"),
      randomSeed=42,
      verbose = 1
    )
  }, error = function(e) {
    warning(paste("\nRun failed for", params$energy, params$constraint, ":", e$message))
    return(NULL)
  })
  
  # --- Prepare result row ---
  result_row <- as_tibble(params)
  
  if (is.null(result) || length(result$u) == 0) {
    return(result_row %>% mutate(status = "failed", across(everything(), ~replace_na(., "NA"))))
  }
  
  # --- Evaluate Performance ---
  
  # 1. Latent Space (U) Evaluation
  U_found_consensus <- Reduce("+", result$u) / length(result$u)
  u_eval <- compare_latent_spaces(ground_truth$U_shared, U_found_consensus)
  
  # 2. Feature Space (V) Evaluation
  v_eval_list <- purrr::map2(ground_truth$V_shared, result$v, compare_feature_loadings)
  mean_v_similarity <- mean(unlist(v_eval_list), na.rm = TRUE)
  
  # 3. Add metrics to the result row
  result_row <- result_row %>%
    mutate(
      status = "success",
      u_corr = u_eval$mean_correlation,
      u_subspace_angle = u_eval$subspace_angle_degrees,
      v_cos_sim = mean_v_similarity,
      conv = result$converged_at
    )
  
  return(result_row)
}


# ==============================================================================
#           3. Main Script Execution
# ==============================================================================

# --- Generate a challenging dataset ---
set.seed(42)
ground_truth <- generate_structured_multiview_data(
  n_subjects = 1000,
  n_features = c(100, 150, 120),
  k_shared = 4,
  k_specific = 10, # Each modality has 3 strong unique signals
  noise_sd = 0.2
)

# Pre-process the data (centering and scaling)
preprocessed_data <- ground_truth$data_list

mypts = list_simlr_optimizers()
# --- Define the parameter grid for the experiment ---
param_grid <- expand.grid(
  energy = c("normalized_correlation", "regression", "acc","lrr"),
  constraint = c("Stiefelx0","Grassmannx0",  "none"),
  optimizer=c("rmsprop","ls_rmsprop","nadam","ls_nadam","adam","ls_adam"),
  stringsAsFactors = FALSE
)

# --- Run the experiment ---
message("Starting SIMLR performance evaluation on structured data...")
performance_results <- purrr::map_dfr(
  split(param_grid, seq(nrow(param_grid))),
  ~run_and_evaluate_simlr(params = .x, data_list = preprocessed_data, ground_truth = ground_truth$ground_truth)
)

# --- Analyze and Report Results ---
cat("\n\n--- SIMLR Performance Evaluation Summary ---\n")

# Rank by a composite score prioritizing U recovery and V recovery
ranked_performance <- performance_results %>%
  filter(status == "success") %>%
  mutate(
    # Score rewards high correlation and low subspace angle
    performance_score = (u_corr + v_cos_sim) - (u_subspace_angle / 90)
  ) %>%
  arrange(desc(performance_score))



# --- Plot 3: Performance Heatmap ---
library(tidyr)

# Prepare data for the heatmap
heatmap_data <- ranked_performance %>%
  select(energy, constraint, u_corr, u_subspace_angle, v_cos_sim) %>%
  # Normalize metrics to a 0-1 scale for comparable coloring
  mutate(
    U_Correlation = u_corr,
    Subspace_Error = 1 - (u_subspace_angle / 90), # Invert so higher is better
    V_Similarity = v_cos_sim
  ) %>%
  select(energy, constraint, U_Correlation, Subspace_Error, V_Similarity) %>%
  pivot_longer(
    cols = c(U_Correlation, Subspace_Error, V_Similarity),
    names_to = "metric",
    values_to = "score"
  )

plot3 <- ggplot(heatmap_data, aes(x = constraint, y = energy, fill = score)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = round(score, 2)), color = "white", size = 3.5, fontface = "bold") +
  facet_wrap(~metric, ncol = 1) +
  scale_fill_viridis_c(option = "inferno", name = "Performance\n(Higher is Better)", limits = c(0, 1)) +
  theme_minimal(base_size = 12) +
  labs(
    title = "SIMLR Performance Matrix",
    subtitle = "Comparing energy functions and constraints across key metrics",
    x = "Constraint Type",
    y = "Energy Function"
  ) +
  theme(
    legend.position = "right",
    strip.text = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )




# Assuming 'ranked_performance' is your data frame from the run
performance_data <- ranked_performance %>%
  # Make sure the data is ordered by performance for some plots
  arrange(desc(performance_score)) %>%
  # Create cleaner, more readable factor levels for plotting
  mutate(
    # Combine parameters into a single, unique ID for each run
    configuration = paste(energy, constraint, optimizer, sep = " | "),
    # Create a simpler constraint family for coloring/shaping
    constraint_family = case_when(
      grepl("Stiefel", constraint)   ~ "Stiefel (Hard)",
      grepl("Grassmann", constraint) ~ "Grassmann (Hard)",
      grepl("ortho", constraint)     ~ "Ortho (Soft)",
      TRUE                           ~ "None"
    ) %>% factor(levels = c("Stiefel (Hard)", "Grassmann (Hard)", "Ortho (Soft)", "None"))
  )


  library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel)
library(forcats)
library(viridis)
plot1 <- ggplot(
    performance_data %>% top_n(20, performance_score), # Show only the top 20 for clarity
    aes(
      x = performance_score,
      y = fct_reorder(configuration, performance_score), # Order configurations by score
      color = energy
    )
  ) +
  geom_segment(aes(x = 0, xend = performance_score, yend = fct_reorder(configuration, performance_score)),
               linewidth = 0.8, alpha = 0.5) +
  geom_point(aes(shape = optimizer), size = 4) +
  scale_color_viridis_d(option = "plasma", name = "Energy Function") +
  scale_shape_manual(name = "Optimizer", values = c("adam" = 16, "nadam" = 17, "rmsprop" = 15)) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Top SIMLR Configurations by Performance Score",
    subtitle = "Performance score balances recovery of shared signal (U and V) and convergence speed",
    x = "Overall Performance Score (Higher is Better)",
    y = "Configuration (Energy | Constraint | Optimizer)"
  ) +
  theme(
    panel.grid.major.y = element_line(linetype = "dashed", color = "gray85"),
    axis.text.y = element_text(size = 9)
  )

print(plot3)
print(plot1)




library(tidyr)
library(ggplot2)
library(forcats)
library(viridis)

# Assuming 'performance_data' is your data frame after the initial setup

# Prepare data for the heatmap.
# THE FIX: We remove the reference to the non-existent 'u_subspace_angle' column
# and focus on the two key recovery metrics you do have.
profile_data <- performance_data %>%
  # Select and rename the key metrics for plotting
  select(
    configuration,
    `U Recovery (Correlation)` = u_corr,
    `V Recovery (Similarity)` = v_cos_sim
  ) %>%
  # Pivot the data into a "long" format for ggplot
  pivot_longer(
    cols = c(`U Recovery (Correlation)`, `V Recovery (Similarity)`),
    names_to = "metric",
    values_to = "score"
  ) %>%
  # Show only the top 15 overall configurations for clarity
  filter(configuration %in% (top_n(performance_data, 15, performance_score) %>% pull(configuration)))


# The ggplot call remains largely the same, but now it will work.
plot2 <- ggplot(
    profile_data,
    aes(
      x = metric,
      # Order configurations by their mean score across the two metrics
      y = fct_reorder(configuration, score, .fun = mean, .desc = TRUE),
      fill = score
    )
  ) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = round(score, 2)), color = "black", size = 4) +
  scale_fill_viridis_c(
    option = "cividis",
    name = "Performance\nScore",
    limits = c(0, 1) # Set limits for a consistent color scale
  ) +
  facet_wrap(~ metric, scales = "free_x", labeller = label_wrap_gen(width = 20)) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Performance Profile of Top SIMLR Configurations",
    subtitle = "Comparing performance across U and V recovery metrics",
    x = "Performance Metric",
    y = "Configuration"
  ) +
  theme(
    axis.text.x = element_blank(), # The facet titles serve as the x-axis labels
    axis.ticks.x = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "right"
  )

print(plot2)

# --- Plot 3: Interactive Exploration ---
# install.packages("plotly")
library(plotly)

p_interactive <- ggplot(performance_data, aes(
    x = u_subspace_angle, 
    y = u_corr, 
    # Create a custom text label for the hover tooltip
    text = paste(
      "<b>Config:</b>", configuration,
      "<br><b>V-Sim:</b>", round(v_cos_sim, 3),
      "<br><b>U-Corr:</b>", round(u_corr, 3),
      "<br><b>Angle Err:</b>", round(u_subspace_angle, 2)
    ),
    color = energy, 
    shape = constraint_family,
    size = v_cos_sim # Use V similarity for point size
  )) +
  geom_point(alpha = 0.8) +
  scale_color_viridis_d(option = "plasma", name = "Energy") +
  scale_shape_manual(name = "Constraint", values = c("Stiefel (Hard)"=17, "Grassmann (Hard)"=18, "Ortho (Soft)"=15, "None"=16)) +
  scale_size_continuous(range = c(2, 10), name = "V Recovery") +
  theme_bw(base_size = 12) +
  labs(
    title = "Interactive SIMLR Performance Explorer",
    x = "Subspace Angle Error (Lower is Better)",
    y = "Shared Signal Correlation (Higher is Better)"
  )

# Convert the ggplot object to an interactive plotly object
interactive_plot <- ggplotly(p_interactive, tooltip = "text")

# View the interactive plot
print(interactive_plot)

print(plot1)
print(ranked_performance)

