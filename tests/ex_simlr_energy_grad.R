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



# --- Original Functions (Unchanged) ---
extract_ortho_strength <- function(constraint_str) {
  if (constraint_str == 'none') {
    return(0.0)
  } else if (grepl("^orthox", constraint_str)) {
    # Updated regex to be more robust
    val_str <- sub("orthox([0-9.]+).*", "\\1", constraint_str)
    return(as.numeric(val_str))
  }
  return(NA_real_) 
}

preprocess_for_simlr <- function(modality_list) {
  lapply(modality_list, function(mat) {
    mat_centered <- scale(mat, center = TRUE, scale = FALSE)
    frobenius_norm <- sqrt(sum(mat_centered^2))
    if (frobenius_norm > .Machine$double.eps) mat_centered / frobenius_norm else mat_centered
  })
}

compare_feature_loadings <- function(V_true, V_found) {
  if (is.null(V_found) || !is.matrix(V_found) || ncol(V_found) == 0) return(NA_real_)
  V_found=na.omit(V_found)
  k_true <- ncol(V_true); k_found <- ncol(V_found); k_comp <- min(k_true, k_found)
  if (k_comp == 0) return(NA_real_)
  V_true_comp <- V_true[, 1:k_comp, drop = FALSE]; V_found_comp <- V_found[, 1:k_comp, drop = FALSE]
  cosine_sim <- function(a, b) { sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2))) }
  correlation_matrix <- (abs(cor(V_true_comp, V_found_comp)))
  if ( any(is.na(correlation_matrix))) {
    correlation_matrix[is.na(correlation_matrix)]=mean(correlation_matrix,na.rm=TRUE)
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


run_and_evaluate_simlr <- function(params, data_list, ground_truth) {
  result_row <- as_tibble(params)
  # NOTE: Assuming initializeSimlr and simlr are from ANTsR and loaded.
  # This part of the code relies on the ANTsR package having these functions.
  init_u <- initializeSimlr(data_list, k = params$k_to_find, uAlgorithm = 'pca', jointReduction = TRUE )
  # mixer <- if (params$energy %in% c("regression", "reg")) 'ica' else 'pca'

  result <- tryCatch({
    simlr(
      data_matrices = data_list, initialUMatrix = init_u, 
      mixAlg = params$mix,
      energyType = params$energy, constraint = params$constraint,
      scale=c("centerAndScale",'norm'),
      optimizationStyle = params$optimizer, 
      verbose = 0 # Set verbose to 0 to avoid clutter
    )
  }, error = function(e) { 
      warning(paste("\nRun failed for:", paste(params, collapse="|"), "\nMessage:", e$message))
      return(NULL)
  })

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
  
  # Call the placeholder function for rank estimation
  k_to_find1 <- estimate_rank_by_permutation_rv(preprocessed_data, n_permutations = 0, return_max = FALSE)$optimal_k
  k_to_find2 <- estimate_rank_by_permutation_rv(preprocessed_data, n_permutations = 0, return_max = TRUE)$optimal_k
  
  message(paste('Using estimated k values:', k_to_find1, 'and', k_to_find2))
  
  # **CORRECTED ORTHOS VECTOR**
  orthos <- c("orthox1x1",  "orthox0.01x1", "orthox0.005x1", "orthox0.002x1", "orthox0.001x1", "orthox0x0" )
  
  param_grid <- expand.grid(
    energy = c("regression", "acc", "nc", "lrr"),
    constraint = orthos,
    mix = c("ica","pca","svd"),
    optimizer = c("nadam","adam"),
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
      ortho_strength = purrr::map_dbl(constraint, extract_ortho_strength)
    ) %>%
    mutate(
      u_recovery_score = u_corr,
      v_recovery_score = v_cos_sim,
      u_alignment_score = 1 - (u_subspace_angle / 90),
      performance_score = (0.5 * u_recovery_score) + (0.3 * v_recovery_score) + (0.2 * u_alignment_score),
      configuration = paste(energy, constraint, optimizer, k_to_find, sep = " | "),
      k_label = case_when(
        k_to_find == k_shared_true ~ paste0("k=True (", k_shared_true, ")"),
        k_to_find > k_shared_true  ~ paste0("k=Over (", k_to_find, ")"),
        k_to_find < k_shared_true  ~ paste0("k=Under (", k_to_find, ")")
      )
    ) %>%
    arrange(desc(performance_score))

  cat("\n\n--- Top 10 Performing Configurations ---\n")
  print(head(performance_data %>% select(energy, constraint, optimizer, k_to_find, performance_score, u_corr, v_cos_sim), 10))

  cat("\n--- Generating Visualizations ---\n")
  
  plot1 <- ggplot(performance_data %>% slice_head(n = 20),
                  aes(x = performance_score, y = fct_reorder(configuration, performance_score), color = energy)) +
    geom_segment(aes(xend = 0, yend = fct_reorder(configuration, performance_score)), linewidth = 1) +
    geom_point(aes(shape = optimizer), size = 3.5) +
    scale_color_viridis_d(option = "plasma", name = "Energy") +
    scale_x_continuous(limits = c(0, 1)) +
    theme_minimal(base_size = 11) +
    labs(title = "Top 20 SIMLR Configurations by Performance Score", x = "Performance Score (Higher is Better)", y = "Configuration")

  # **CORRECTED PLOT 2**
  plot2 <- ggplot(performance_data, aes(x = u_subspace_angle, y = u_corr, color = ortho_strength)) +
    geom_point(aes(size = v_cos_sim, shape = k_label), alpha = 0.8) +
    scale_color_viridis_c(option = "magma", name = "Ortho Strength", trans = "log10") +
    scale_size_continuous(range = c(2, 8), name = "V Recovery (Cos Sim)") +
    scale_shape_manual(values = c(16, 17, 15)[1:length(unique(performance_data$k_label))], name = "k Specification") +
    facet_wrap(~energy, ncol = 2) +
    theme_bw(base_size = 12) +
    labs(title = "Performance Trade-off by Energy Type",
         subtitle = "Color indicates orthogonality strength.",
         x = "Subspace Angle Error (Degrees, Lower is Better)",
         y = "Shared Signal Correlation (U, Higher is Better)")

  plot3 <- ggplot(performance_data, 
                  aes(x = factor(ortho_strength), y = performance_score, fill = energy)) +
    geom_boxplot(alpha=0.7) +
    scale_fill_viridis_d(option = "plasma", name = "Energy") +
    theme_minimal(base_size = 12) +
    labs(title = "Overall Performance vs. Orthogonality Strength",
         x = "Orthogonality Strength (Weight)", y = "Overall Performance Score") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  return(list(ranked_performance_data = performance_data, 
              plot_ranking = plot1, 
              plot_tradeoffs = plot2,
              plot_orthogonality_effect = plot3))
}

create_interactive_performance_table <- function(performance_data) {
  display_data <- performance_data %>%
    select(Energy = energy, Constraint = constraint, Optimizer = optimizer,
           `k Found` = k_to_find,
           `U Corr` = u_corr, `V Sim` = v_cos_sim, `Angle Err` = u_subspace_angle,
           Score = performance_score) %>%
    arrange(desc(Score))

  DT::datatable(display_data, extensions = 'Buttons',
    options = list(pageLength = 15, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')),
    caption = 'SIMLR Performance Benchmark Results',
    filter = 'top', rownames = FALSE) %>%
  formatStyle(columns = c('U Corr', 'V Sim', 'Score'),
              background = styleColorBar(range(display_data[, c('U Corr', 'V Sim', 'Score')], na.rm=TRUE), '#63c584'),
              backgroundSize = '98% 88%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center') %>%
  formatStyle(columns = 'Angle Err',
              background = styleColorBar(range(display_data$`Angle Err`, na.rm=TRUE), '#d65f5f', angle = -90),
              backgroundSize = '98% 88%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center') %>%
  formatRound(columns = c('U Corr', 'V Sim', 'Angle Err', 'Score'), digits = 3)
}

# ==============================================================================
#           4. Main Execution Block
# ==============================================================================

if (interactive()) {
  # --- 1. Define the experiment's ground truth ---
  ks <- 5 # True number of shared components
  ku <- 9 # Number of unique components per modality

  # --- 2. Run the full parameter sweep ---
  # This part is computationally intensive.
  performance_summary <- tabulate_simlr_performance(k_shared_true = ks, k_unique_per_view = ku)

  if ( FALSE ) {
    # For demonstration, we will create mock data that looks like the real output.
    # In a real run, you would use the line above.
    message("Creating mock data for demonstration purposes...")
    orthos_mock <- c("orthox1x1", "orthox0.12x1", "orthox0.08x1", "orthox0.04x1", 
                    "orthox0.01x1", "orthox0.005x1", "orthox0.002x1", "orthox0.001x1", "none")
    param_grid_mock <- expand.grid(
      energy = c("regression", "acc", "nc", "lrr"),
      constraint = orthos_mock,
      optimizer = c("adam", "nadam"),
      k_to_find = c(5, 8),
      stringsAsFactors = FALSE
    )
    performance_summary <- as_tibble(param_grid_mock) %>%
      mutate(
        k_true = ks,
        status = "success",
        u_corr = runif(n(), 0.7, 0.95),
        v_cos_sim = runif(n(), 0.6, 0.9),
        u_subspace_angle = runif(n(), 5, 40)
      )
  }

  # --- 3. Analyze and create static plots ---
  analysis_results <- analyze_and_visualize_performance(performance_summary)

  # --- 4. Create the beautiful interactive table ---
  interactive_results_table <- create_interactive_performance_table(analysis_results$ranked_performance_data)

  # --- 5. Print all outputs ---
  cat("\n\n--- Static Visualizations ---\n")
  print(analysis_results$plot_ranking)
  print(analysis_results$plot_tradeoffs)
  print(analysis_results$plot_orthogonality_effect)
  
  cat("\n\n--- Interactive Results Table ---\n")
  print(interactive_results_table)
}