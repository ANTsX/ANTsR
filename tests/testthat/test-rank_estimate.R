
set.seed(42)
library(ANTsR)
library(ggplot2)
myksh=8
myku=3
hard_case_data <- generate_structured_multiview_data(
  n_subjects = 200, n_features = c(100, 150, 120),
  k_shared = myksh, k_specific = myku, noise_sd = 0.033
)

results_final <- estimate_rank_by_permutation_rv(hard_case_data$data_list, n_permutations=0 )

cat("\n--- Final Test on 'Hard Case' using Permutation-Bounded RV ---\n")
print(paste("True Shared Rank:", myksh))
print(paste("True Total Rank:", myksh+myku))
print(paste("Estimated Shared Rank:", results_final$optimal_k))
print(results_final$plot)