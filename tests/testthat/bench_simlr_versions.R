
library(ANTsR)
library(dplyr)
library(ggplot2)

# Synthetic Multiview Data Generation
generate_multiview_data <- function(n=100, p1=50, p2=60, k=3, noise=0.1) {
  set.seed(42)
  U_true <- matrix(rnorm(n * k), n, k)
  V1 <- matrix(rnorm(k * p1), k, p1)
  V2 <- matrix(rnorm(k * p2), k, p2)
  X1 <- U_true %*% V1 + matrix(rnorm(n * p1, sd = noise), n, p1)
  X2 <- U_true %*% V2 + matrix(rnorm(n * p2, sd = noise), n, p2)
  return(list(X1 = X1, X2 = X2, U_true = U_true))
}

data <- generate_multiview_data()
matlist <- list(Mod1 = data$X1, Mod2 = data$X2)

# Sourcing R/simlr.R defines both simlr and simlrZZ
source("R/simlr_optimizers.R")
source("R/simlr.R")

# Run Recent Version (simlrZZ)
cat("Running SIMLR Recent Version (simlrZZ)...\n")
start_time <- Sys.time()
mix='newton-schulz'
mix='ica'
# mix='ica-newton'
# mix='svd'
ebber=0.9
oppter='adam'
mye='regression'
res_recent <- simlr( matlist, initialUMatrix = 3, iterations = 100,
                   sparse_gradient = TRUE, energyType = mye,  mixAlg=mix,
                   expBeta=ebber,
                   optimizationStyle = oppter, verbose=2)
end_time <- Sys.time()
time_recent <- as.numeric(difftime(end_time, start_time, units = "secs"))

# Run Reference Version (simlr)
cat("\nRunning SIMLR Reference Version (simlr)...\n")
start_time <- Sys.time()
res_ref <- backup_simlr(matlist, initialUMatrix = 3, iterations = 100, 
                energyType = mye,  mixAlg=mix,
                expBeta=ebber,
                optimizationStyle = oppter, verbose=2 )
end_time <- Sys.time()
time_ref <- as.numeric(difftime(end_time, start_time, units = "secs"))

cat("\n--- Comparison Results ---\n")
cat(sprintf("Recent Time (simlrZZ): %.3f s\n", time_recent))
cat(sprintf("Ref Time    (simlr)  : %.3f s\n", time_ref))

# Check final energy
cat(sprintf("Recent Final Error: %.6f\n", res_recent$finalError))
cat(sprintf("Ref Final Error:    %.6f\n", res_ref$finalError))

# Plot energy paths
path_recent <- res_recent$energyPath %>% mutate(version = "simlrZZ")
path_ref <- res_ref$energyPath %>% mutate(version = "simlr")

combined_path <- bind_rows(path_recent, path_ref)

p <- ggplot(combined_path, aes(x = iteration, y = total_energy, color = version)) +
  geom_line() +
  facet_wrap(~modality, scales="free_y") +
  theme_minimal() +
  labs(title = "SIMLR Convergence Comparison", subtitle = "Recent (simlrZZ) vs Reference (simlr)")

ggsave("simlr_convergence_comparison.png", p, width = 10, height = 6)

cat("\nPlot saved to simlr_convergence_comparison.png\n")
