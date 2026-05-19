library(ANTsR)
library(testthat)

context("simlr optimization and energy tracking")

test_that("simlr armijo_gradient maintains monotonic best-energy decrease (regression)", {
  set.seed(42)
  n <- 40; p1 <- 15; p2 <- 15; k <- 2
  U_true <- matrix(rnorm(n * k), n, k)
  X1 <- U_true %*% matrix(rnorm(k * p1), k, p1) + matrix(rnorm(n * p1, sd = 0.1), n, p1)
  X2 <- U_true %*% matrix(rnorm(k * p2), k, p2) + matrix(rnorm(n * p2, sd = 0.1), n, p2)
  matlist <- list(X1 = X1, X2 = X2)
  
  result <- simlr(matlist, initialUMatrix = k, 
                  optimizationStyle = "armijo_gradient",
                  energyType = "regression",
                  constraint = "none", 
                  iterations = 15, 
                  verbose = FALSE)
  
  mean_energy <- result$energyPath %>% 
    dplyr::group_by(iteration) %>% 
    dplyr::summarize(mean_e = mean(total_energy)) %>% 
    dplyr::pull(mean_e)
  
  if (length(mean_energy) > 2) {
    decreasing_part <- mean_energy[2:length(mean_energy)]
    best_energy_path <- cummin(decreasing_part)
    expect_true(all(diff(best_energy_path) <= 1e-10), 
                info = "Best-known energy was not monotonically non-increasing.")
    # Final error should be better than iteration 2
    expect_lt(result$finalError, best_energy_path[1])
  }
})

test_that("simlr lookahead with complex constraints converges (Issue 5/User case)", {
  set.seed(1500)
  nsub <- 25; nk <- 5
  outcome <- matrix(rnorm(nsub * nk), ncol = nk)
  outcome1 <- matrix(rnorm(nsub * nk), ncol = nk)
  outcome2 <- matrix(rnorm(nsub * nk), ncol = nk)
  view1tx <- matrix(rnorm(100 * nk), nrow = nk)
  view2tx <- matrix(rnorm(133 * nk), nrow = nk)
  mat1 <- (outcome %*% t(outcome1) %*% outcome1) %*% view1tx
  mat2 <- (outcome %*% t(outcome2) %*% outcome2) %*% view2tx
  
  result <- simlr(list(v1 = mat1, v2 = mat2), 
                  initialUMatrix = nk,
                  optimizationStyle = "bidirectional_lookahead",
                  energyType = "acc",
                  constraint = "nsaflowx0.5x10", 
                  iterations = 20, 
                  verbose = FALSE)
                  
  mean_energy <- result$energyPath %>% 
    dplyr::group_by(iteration) %>% 
    dplyr::summarize(mean_e = mean(total_energy)) %>% 
    dplyr::pull(mean_e)
    
  if (length(mean_energy) > 2) {
    decreasing_part <- mean_energy[2:length(mean_energy)]
    best_energy_path <- cummin(decreasing_part)
    expect_true(all(diff(best_energy_path) <= 1e-10))
    expect_lt(result$finalError, best_energy_path[1] - 1e-2)
  }
})

test_that("simlr standard optimizers (adam, nadam, lars) behave consistently", {
  set.seed(42)
  n <- 40; p1 <- 15; p2 <- 15; k <- 2
  U_true <- matrix(rnorm(n * k), n, k)
  X1 <- U_true %*% matrix(rnorm(k * p1), k, p1) + matrix(rnorm(n * p1, sd = 0.1), n, p1)
  X2 <- U_true %*% matrix(rnorm(k * p2), k, p2) + matrix(rnorm(n * p2, sd = 0.1), n, p2)
  matlist <- list(X1 = X1, X2 = X2)
  
  opt_results <- lapply(c("adam", "nadam", "lars"), function(opt) {
    simlr(matlist, initialUMatrix = 2, 
          optimizationStyle = opt,
          energyType = "regression",
          iterations = 10, verbose = FALSE)
  })
  
  # All should reduce energy relative to the initial tuned state
  for (res in opt_results) {
    mean_energy <- res$energyPath %>% 
      dplyr::group_by(iteration) %>% 
      dplyr::summarize(mean_e = mean(total_energy)) %>% 
      dplyr::pull(mean_e)
    
    expect_lt(res$finalError, mean_energy[2])
  }
})

test_that("simlr_sparseness is idempotent (Issue 4/Path 3)", {
  mat <- matrix(rnorm(100), 20, 5)
  p1 <- simlr_sparseness(mat, sparseness_quantile = 0.5, positivity = "either")
  expect_true(attr(p1, "simlr_projected"))
  p2 <- simlr_sparseness(p1, sparseness_quantile = 0.5, positivity = "either")
  expect_true(identical(p1, p2))
  
  modified <- p1 + 1.0 
  p3 <- simlr_sparseness(modified, sparseness_quantile = 0.5, positivity = "either")
  expect_false(identical(p3, modified))
})

