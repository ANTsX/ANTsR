
library(ANTsR)
# --- 1. Load Required Libraries ---
.install_and_load <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message(paste("Installing required package:", pkg))
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
}
.install_and_load(c("dplyr", "tibble", "purrr", "ggplot2", "ggrepel", "viridis", "progress", "tidyr", "forcats", "pracma", "clue", "DT", "htmltools"))



preprocess_for_simlr <- function(modality_list) {
  lapply(modality_list, function(mat) {
    mat_centered <- scale(mat, center = TRUE, scale = FALSE)
    frobenius_norm <- sqrt(sum(mat_centered^2))
    if (frobenius_norm > .Machine$double.eps) mat_centered / frobenius_norm else mat_centered
  })
}

compare_feature_loadings <- function(V_true, V_found) {
  if (is.null(V_found) || !is.matrix(V_found) || ncol(V_found) == 0) return(NA_real_)
  k_true <- ncol(V_true); k_found <- ncol(V_found); k_comp <- min(k_true, k_found)
  if (k_comp == 0) return(NA_real_)
  V_true_comp <- V_true[, 1:k_comp, drop = FALSE]; V_found_comp <- V_found[, 1:k_comp, drop = FALSE]
  cosine_sim <- function(a, b) { sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2))) }
  correlation_matrix <- (abs(cor(V_true_comp, V_found_comp)))
  if ( any(is.na(correlation_matrix))) {
    print(V_true_comp)
    message("imputing")
    print(correlation_matrix)
    correlation_matrix <- antsrimpute(correlation_matrix)
  }
  permutation_map <- as.vector(clue::solve_LSAP(correlation_matrix, maximum = TRUE))
  V_found_permuted <- V_found_comp[, permutation_map, drop = FALSE]
  signs_to_flip <- sign(diag(cor(V_true_comp, V_found_permuted))); signs_to_flip[signs_to_flip == 0] <- 1
  V_found_aligned <- V_found_permuted %*% diag(signs_to_flip, nrow = k_comp, ncol = k_comp)
  final_similarities <- sapply(1:k_comp, function(i) cosine_sim(V_true_comp[, i], V_found_aligned[, i]))
  return(mean(final_similarities, na.rm = TRUE))
}

compare_latent_spaces <- function(U_true, U_found) {
  if (is.null(U_found) || !is.matrix(U_found) || ncol(U_found) == 0) {
    return(list(mean_correlation = NA_real_, subspace_angle_degrees = NA_real_))
  }
  k_true <- ncol(U_true); k_found <- ncol(U_found); k_comp <- min(k_true, k_found)
  if (k_comp == 0) return(list(mean_correlation = NA_real_, subspace_angle_degrees = NA_real_))
  U_true_comp <- U_true[, 1:k_comp, drop = FALSE]; U_found_comp <- U_found[, 1:k_comp, drop = FALSE]
  correlation_matrix <- abs(cor(U_true_comp, U_found_comp))
  best_matches <- max.col(correlation_matrix, "first")
  mean_corr <- mean(correlation_matrix[cbind(1:k_comp, best_matches)])
  mean_subspace_angle <- mean(pracma::subspace(U_true_comp, U_found_comp) * 180 / pi)
  return(list(mean_correlation = mean_corr, subspace_angle_degrees = mean_subspace_angle))
}


# ==============================================================================
#           3. The Main Evaluation Harness
# ==============================================================================

run_and_evaluate_simlr <- function(params, data_list, ground_truth) {
  result_row <- as_tibble(params)
  init_u <- initializeSimlr(data_list, k = params$k_to_find, uAlgorithm = 'pca', jointReduction = TRUE )
  mixer <- if (params$energy %in% c("regression", "reg")) 'ica' else 'pca'

  result <- tryCatch({
    simlr(
      voxmats = data_list, initialUMatrix = init_u, 
      energyType = params$energy, constraint = params$constraint,
      scale=c("centerAndScale"),
      optimizationStyle = params$optimizer, 
      mixAlg = mixer, verbose = 1
    )
  }, error = function(e) { 
      warning(paste("\nRun failed for:", paste(params, collapse="|"), "\nMessage:", e$message))
      return(NULL)
  })

  # --- Corrected Error Handling Block ---
  if (is.null(result) || length(result$u) == 0) {
    return(result_row %>% mutate(
      status = "failed", u_corr = NA_real_, u_subspace_angle = NA_real_, v_cos_sim = NA_real_
    ))
  }
  
  U_found_consensus <- Reduce("+", result$u) / length(result$u)
  u_eval <- compare_latent_spaces(ground_truth$U_shared, U_found_consensus)
  v_eval <- mean(sapply(seq_along(data_list), function(i) {
    compare_feature_loadings(ground_truth$V_shared[[i]], result$v[[i]])
  }), na.rm = TRUE)
  
  return(
    result_row %>%
    mutate(
      status = "success", u_corr = u_eval$mean_correlation,
      u_subspace_angle = u_eval$subspace_angle_degrees, v_cos_sim = v_eval
    )
  )
}

tabulate_simlr_performance <- function(k_shared_true, k_unique_per_view) {
  set.seed(42)
  fbig=c(201, 499, 666)
  fsmall=round(fbig*0.1)
  ground_truth <- generate_structured_multiview_data(
    n_subjects = 400, n_features = fsmall,
    k_shared = k_shared_true, k_specific = k_unique_per_view, noise_sd = 0.1
  )
  preprocessed_data <- preprocess_for_simlr(ground_truth$data_list)
  
  k_to_find1 = 2 #estimate_rank_by_permutation_rv( preprocessed_data, n_permutations=0, return_max=FALSE )$optimal_k
  k_to_find2 = 4 # estimate_rank_by_permutation_rv( preprocessed_data, n_permutations=0, return_max=TRUE )$optimal_k
  print(paste('k_to_find1 ', k_to_find1,k_to_find2))
  param_grid <- expand.grid(
    energy = c("normalized_correlation", "regression", "acc", "lrr"),
    constraint = c("Stiefelx0", "Grassmannx0", "none"),
    optimizer = c("adam", "ls_adam" ),
    k_to_find = c(k_to_find1, k_to_find2),
    stringsAsFactors = FALSE
  )
  
  message(paste("Starting parameter sweep for", nrow(param_grid), "configurations..."))
  pb <- progress::progress_bar$new(format = "[:bar] :percent ETA: :eta", total = nrow(param_grid))
  
  results_list <- purrr::map(split(param_grid, seq(nrow(param_grid))), ~{
    pb$tick()
    run_and_evaluate_simlr(params = .x, data_list = preprocessed_data, ground_truth = ground_truth$ground_truth)
  })
  
  message("\nParameter sweep complete.")
  return(dplyr::bind_rows(results_list) %>% mutate(k_true = k_shared_true))
}

analyze_and_visualize_performance <- function(performance_summary) {
  if (!("k_true" %in% names(performance_summary))) stop("Input must contain a 'k_true' column.")
  k_shared_true <- performance_summary$k_true[1]

  performance_data <- performance_summary %>%
    filter(status == "success") %>%
    mutate(
      u_recovery_score = u_corr,
      v_recovery_score = v_cos_sim,
      u_alignment_score = 1 - (u_subspace_angle / 90),
      performance_score = (0.5 * u_recovery_score) + (0.3 * v_recovery_score) + (0.2 * u_alignment_score),
      configuration = paste(energy, gsub("x.*", "", constraint), optimizer, k_to_find, sep = " | "),
      k_label = case_when(
        k_to_find == k_shared_true ~ paste0("k=True (", k_shared_true, ")"),
        k_to_find > k_shared_true  ~ paste0("k=Over (", k_to_find, ")"),
        k_to_find < k_shared_true  ~ paste0("k=Under (", k_to_find, ")")
      ),
      constraint_family = case_when(
        grepl("Stiefel", constraint)   ~ "Stiefel", grepl("Grassmann", constraint) ~ "Grassmann",
        TRUE ~ "None"
      ) %>% factor(levels = c("Stiefel", "Grassmann", "None"))
    ) %>%
    arrange(desc(performance_score))

  cat("\n\n--- Top 10 Performing Configurations ---\n")
  print(head(performance_data %>% select(energy, constraint, optimizer, k_to_find, performance_score, u_corr, v_cos_sim, u_subspace_angle), 10))

  cat("\n--- Generating Visualizations ---\n")
  
  plot1 <- ggplot(performance_data %>% slice_head(n = 20),
                  aes(x = performance_score, y = fct_reorder(configuration, performance_score), color = energy)) +
    geom_segment(aes(xend = 0, yend = fct_reorder(configuration, performance_score))) +
    geom_point(aes(shape = optimizer), size = 3.5) +
    scale_color_viridis_d(option = "plasma", name = "Energy") +
    scale_x_continuous(limits = c(0, 1)) +
    theme_minimal(base_size = 11) +
    labs(title = "Top SIMLR Configurations by Performance Score", x = "Performance Score (0 to 1, Higher is Better)", y = "Configuration")

  k_labels_unique <- unique(performance_data$k_label); shape_values <- c(16, 17, 15)[1:length(k_labels_unique)]; names(shape_values) <- k_labels_unique
  
  plot2 <- ggplot(performance_data, aes(x = u_subspace_angle, y = u_corr, color = optimizer)) +
    geom_point(aes(size = v_cos_sim, shape = k_label), alpha = 0.8) +
    geom_text_repel(aes(label = energy), size = 3, max.overlaps = 10, seed = 42) +
    facet_wrap(~constraint_family, scales = "free_x") +
    scale_color_viridis_d(option = "magma", name = "Optimizer") +
    scale_size_continuous(range = c(2, 8), name = "V Recovery (Cos Sim)") +
    scale_shape_manual(values = shape_values, name = "k Specification") +
    theme_bw(base_size = 12) +
    labs(title = "Performance Trade-off: U Recovery vs. Subspace Error", x = "Subspace Angle Error (Degrees, Lower is Better)", y = "Shared Signal Correlation (U, Higher is Better)")

  return(list(ranked_performance_data = performance_data, plot_ranking = plot1, plot_tradeoffs = plot2))
}


create_interactive_performance_table <- function(performance_data) {
  display_data <- performance_data %>%
    select(Energy = energy, Constraint = constraint, Optimizer = optimizer,
           `k True` = k_true, `k Found` = k_to_find,
           `U Corr` = u_corr, `V Sim` = v_cos_sim, `Angle Err` = u_subspace_angle,
           Score = performance_score) %>%
    arrange(desc(Score))

  interactive_table <- DT::datatable(display_data, extensions = 'Buttons',
    options = list(pageLength = 15, lengthMenu = c(15, 25, 50, -1),
                   searchHighlight = TRUE, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')),
    caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                                      htmltools::strong('SIMLR Performance Benchmark Results')),
    filter = 'top', rownames = FALSE, class = 'cell-border stripe hover') %>%
  formatStyle(columns = c('U Corr', 'V Sim', 'Score'),
              background = styleColorBar(range(display_data[, c('U Corr', 'V Sim', 'Score')], na.rm=TRUE), '#63c584'),
              backgroundSize = '98% 88%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center') %>%
  formatStyle(columns = c('Angle Err'),
              background = styleColorBar(range(display_data$`Angle Err`, na.rm=TRUE), '#d65f5f', angle = -90),
              backgroundSize = '98% 88%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center') %>%
  formatRound(columns = c('U Corr', 'V Sim', 'Angle Err', 'Score'), digits = 3)
  return(interactive_table)
}

# ==============================================================================
#           4. Main Execution Block
# ==============================================================================

if (interactive()) {
  # --- 1. Define the experiment's ground truth ---
  ks <- 5 # True number of shared components
  ku <- 9 # Number of unique components per modality

  # --- 2. Run the full parameter sweep ---
  performance_summary <- tabulate_simlr_performance(k_shared_true = ks, k_unique_per_view = ku)

  # --- 3. Analyze and create static plots ---
  analysis_results <- analyze_and_visualize_performance(performance_summary)

  # --- 4. Create the beautiful interactive table ---
  interactive_results_table <- create_interactive_performance_table(analysis_results$ranked_performance_data)

  # --- 5. Print all outputs ---
  cat("\n\n--- Static Visualizations ---\n")
  print(analysis_results$plot_ranking)
  print(analysis_results$plot_tradeoffs)
  
  cat("\n\n--- Interactive Results Table ---\n")
  print(interactive_results_table)
}
