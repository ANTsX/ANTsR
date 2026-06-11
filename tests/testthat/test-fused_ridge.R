# Unit tests for the fusedRidge function
# Verifies dimensional consistency, edge cases, and statistical behaviors.

test_that("fusedRidge dimensional consistency", {
  set.seed(42)
  N <- 50
  M <- 10
  J <- 5
  
  # Generate random data
  X_pcs <- matrix(rnorm(N * M), nrow = N, ncol = M)
  colnames(X_pcs) <- paste0("PC", 1:M)
  y_raw <- rnorm(N)
  thresholds <- seq(-1, 1, length.out = J)
  covariates <- matrix(rnorm(N * 2), nrow = N, ncol = 2)
  colnames(covariates) <- c("Cov1", "Cov2")
  
  # Run model
  res <- fusedRidge(X_pcs, y_raw, thresholds, covariates = covariates, lambda1 = 0.5, lambda2 = 0.5)
  
  # Check output structures
  expect_equal(dim(res$coefs_full), c(M, J))
  expect_equal(dim(res$coefs_covs), c(3, J)) # Intercept + 2 covariates
  expect_equal(res$family, "binomial")
  expect_s3_class(res, "fusedRidge")
})

test_that("fusedRidge handles null covariates", {
  set.seed(42)
  N <- 40
  M <- 8
  J <- 3
  
  X_pcs <- matrix(rnorm(N * M), nrow = N, ncol = M)
  y_raw <- rnorm(N)
  thresholds <- c(-0.5, 0, 0.5)
  
  # Run model with no covariates
  res <- fusedRidge(X_pcs, y_raw, thresholds, covariates = NULL, lambda1 = 0.5, lambda2 = 0.5)
  
  expect_equal(dim(res$coefs_full), c(M, J))
  expect_equal(dim(res$coefs_covs), c(1, J)) # Only intercept/dummy spacer
})

test_that("fusedRidge error handling on dimension mismatch", {
  N <- 30
  M <- 5
  J <- 2
  
  X_pcs <- matrix(rnorm(N * M), nrow = N, ncol = M)
  y_raw_bad <- rnorm(N + 5) # Mismatch
  thresholds <- c(0, 0.5)
  
  expect_error(fusedRidge(X_pcs, y_raw_bad, thresholds), "Length of y_raw must match")
  
  y_raw_good <- rnorm(N)
  covariates_bad <- matrix(rnorm((N - 5) * 2), nrow = N - 5, ncol = 2) # Mismatch
  expect_error(fusedRidge(X_pcs, y_raw_good, thresholds, covariates = covariates_bad), "Number of rows in covariates must match")
})

test_that("fusedRidge behaves correctly when lambda2 = 0 (independent ridge equivalence)", {
  set.seed(42)
  N <- 50
  M <- 5
  J <- 3
  
  X_pcs <- matrix(rnorm(N * M), nrow = N, ncol = M)
  y_raw <- rnorm(N)
  thresholds <- c(-0.5, 0, 0.5)
  
  # Fit fusedRidge with lambda2 = 0
  res_fused <- fusedRidge(X_pcs, y_raw, thresholds, lambda1 = 1.0, lambda2 = 0)
  
  # Check H is identity matrix (since lambda1=1.0 and lambda2=0)
  expect_equal(res_fused$H, diag(J))
  expect_equal(dim(res_fused$coefs_full), c(M, J))
})

test_that("predict.fusedRidge works correctly", {
  set.seed(42)
  N <- 30
  M <- 4
  J <- 3
  
  X_pcs <- matrix(rnorm(N * M), nrow = N, ncol = M)
  colnames(X_pcs) <- paste0("PC", 1:M)
  y_raw <- rnorm(N)
  thresholds <- c(-0.5, 0, 0.5)
  covariates <- matrix(rnorm(N * 2), nrow = N, ncol = 2)
  colnames(covariates) <- c("Cov1", "Cov2")
  
  # 1. Model with covariates
  res <- fusedRidge(X_pcs, y_raw, thresholds, covariates = covariates, lambda1 = 0.5, lambda2 = 0.5)
  
  # New test data
  newx <- matrix(rnorm(10 * M), nrow = 10, ncol = M)
  colnames(newx) <- colnames(X_pcs)
  newcovs <- matrix(rnorm(10 * 2), nrow = 10, ncol = 2)
  colnames(newcovs) <- colnames(covariates)
  
  # Predict link
  pred_link <- predict(res, newx, newcovs = newcovs, type = "link")
  expect_equal(dim(pred_link), c(10, J))
  
  # Predict response
  pred_resp <- predict(res, newx, newcovs = newcovs, type = "response")
  expect_equal(dim(pred_resp), c(10, J))
  expect_true(all(pred_resp >= 0 & pred_resp <= 1))
  
  # Error handling: missing covariates
  expect_error(predict(res, newx, newcovs = NULL), "newcovs must be provided")
  
  # 2. Model with NULL covariates
  res_nocov <- fusedRidge(X_pcs, y_raw, thresholds, covariates = NULL, lambda1 = 0.5, lambda2 = 0.5)
  
  # Predict without covariates
  pred_nocov_link <- predict(res_nocov, newx, type = "link")
  expect_equal(dim(pred_nocov_link), c(10, J))
  
  # Predict with newcovs (should ignore them but use its own intercept)
  pred_nocov_extra <- predict(res_nocov, newx, newcovs = newcovs)
  expect_equal(dim(pred_nocov_extra), c(10, J))
  
  # 3. topK sparsity control tests
  # Predict with topK = 2 (model has M = 4 features)
  pred_top2 <- predict(res, newx, newcovs = newcovs, type = "link", topK = 2)
  expect_equal(dim(pred_top2), c(10, J))
  
  # Check invalid topK inputs throw error
  expect_error(predict(res, newx, newcovs = newcovs, topK = -1), "topK must be a positive integer")
  expect_error(predict(res, newx, newcovs = newcovs, topK = "two"), "topK must be a positive integer")
})

test_that("fusedRidge fits correctly with topK option (two-pass refitting)", {
  set.seed(42)
  N <- 50
  M <- 10
  J <- 3
  
  X_pcs <- matrix(rnorm(N * M), nrow = N, ncol = M)
  colnames(X_pcs) <- paste0("PC", 1:M)
  y_raw <- rnorm(N)
  thresholds <- c(-0.5, 0, 0.5)
  
  # Fit with topK = 3
  res_top3 <- fusedRidge(X_pcs, y_raw, thresholds, covariates = NULL, lambda1 = 0.5, lambda2 = 0.5, topK = 3)
  
  # Check dimensions of coefficients
  expect_equal(dim(res_top3$coefs_full), c(M, J))
  
  # Ensure only the selected features have non-zero coefficients across all thresholds
  selected <- res_top3$selected_features
  expect_true(length(selected) >= 3 && length(selected) <= 3 * J)
  
  # Check theta_matrix dimensions and mapping
  expect_equal(dim(res_top3$theta_matrix), c(M, J))
  
  non_selected <- setdiff(seq_len(M), selected)
  if (length(non_selected) > 0) {
    expect_true(all(res_top3$coefs_full[non_selected, ] == 0))
    expect_true(all(res_top3$theta_matrix[non_selected, ] == 0))
  }
  
  # Error handling: invalid topK parameter in fit
  expect_error(fusedRidge(X_pcs, y_raw, thresholds, topK = -1), "topK must be a positive integer")
  expect_error(fusedRidge(X_pcs, y_raw, thresholds, topK = "two"), "topK must be a positive integer")
})

test_that("fusedRidge handles 'thresh' parameter", {
  set.seed(42)
  N <- 50
  M <- 10
  J <- 3
  
  X_pcs <- matrix(rnorm(N * M), nrow = N, ncol = M)
  y_raw <- rnorm(N)
  thresholds <- c(-0.5, 0, 0.5)
  
  # Run model with a custom thresh
  res <- fusedRidge(X_pcs, y_raw, thresholds, thresh = 1e-4)
  expect_s3_class(res, "fusedRidge")
  
  # Ensure it is passed to the recursive call if topK is used
  res_topK <- fusedRidge(X_pcs, y_raw, thresholds, topK = 5, thresh = 1e-4)
  expect_s3_class(res_topK, "fusedRidge")
})

test_that("fusedRidgeDirect dimensional consistency", {
  set.seed(42)
  N <- 50
  M <- 10
  J <- 5
  
  X_pcs <- matrix(rnorm(N * M), nrow = N, ncol = M)
  colnames(X_pcs) <- paste0("PC", 1:M)
  y_raw <- rnorm(N)
  thresholds <- seq(-1, 1, length.out = J)
  covariates <- matrix(rnorm(N * 2), nrow = N, ncol = 2)
  colnames(covariates) <- c("Cov1", "Cov2")
  
  res <- fusedRidgeDirect(X_pcs, y_raw, thresholds, covariates = covariates, lambda1 = 0.5, lambda2 = 0.5)
  
  expect_equal(dim(res$coefs_full), c(M, J))
  expect_equal(dim(res$coefs_covs), c(3, J)) # Intercept + 2 covariates
  expect_equal(res$family, "binomial")
  expect_s3_class(res, "fusedRidgeDirect")
})

test_that("fusedRidgeDirect handles null covariates", {
  set.seed(42)
  N <- 40
  M <- 8
  J <- 3
  
  X_pcs <- matrix(rnorm(N * M), nrow = N, ncol = M)
  y_raw <- rnorm(N)
  thresholds <- c(-0.5, 0, 0.5)
  
  res <- fusedRidgeDirect(X_pcs, y_raw, thresholds, covariates = NULL, lambda1 = 0.5, lambda2 = 0.5)
  
  expect_equal(dim(res$coefs_full), c(M, J))
  expect_equal(dim(res$coefs_covs), c(1, J)) # Only intercept
})

test_that("fusedRidgeDirect error handling on dimension mismatch", {
  N <- 30
  M <- 5
  J <- 2
  
  X_pcs <- matrix(rnorm(N * M), nrow = N, ncol = M)
  y_raw_bad <- rnorm(N + 5)
  thresholds <- c(0, 0.5)
  
  expect_error(fusedRidgeDirect(X_pcs, y_raw_bad, thresholds), "Length of y_raw must match")
  
  y_raw_good <- rnorm(N)
  covariates_bad <- matrix(rnorm((N - 5) * 2), nrow = N - 5, ncol = 2)
  expect_error(fusedRidgeDirect(X_pcs, y_raw_good, thresholds, covariates = covariates_bad), "Number of rows in covariates must match")
})

test_that("predict.fusedRidgeDirect works correctly", {
  set.seed(42)
  N <- 30
  M <- 4
  J <- 3
  
  X_pcs <- matrix(rnorm(N * M), nrow = N, ncol = M)
  colnames(X_pcs) <- paste0("PC", 1:M)
  y_raw <- rnorm(N)
  thresholds <- c(-0.5, 0, 0.5)
  covariates <- matrix(rnorm(N * 2), nrow = N, ncol = 2)
  colnames(covariates) <- c("Cov1", "Cov2")
  
  res <- fusedRidgeDirect(X_pcs, y_raw, thresholds, covariates = covariates, lambda1 = 0.5, lambda2 = 0.5)
  
  newx <- matrix(rnorm(10 * M), nrow = 10, ncol = M)
  colnames(newx) <- colnames(X_pcs)
  newcovs <- matrix(rnorm(10 * 2), nrow = 10, ncol = 2)
  colnames(newcovs) <- colnames(covariates)
  
  pred_link <- predict(res, newx, newcovs = newcovs, type = "link")
  expect_equal(dim(pred_link), c(10, J))
  
  pred_resp <- predict(res, newx, newcovs = newcovs, type = "response")
  expect_equal(dim(pred_resp), c(10, J))
  expect_true(all(pred_resp >= 0 & pred_resp <= 1))
  
  expect_error(predict(res, newx, newcovs = NULL), "newcovs must be provided")
  
  res_nocov <- fusedRidgeDirect(X_pcs, y_raw, thresholds, covariates = NULL, lambda1 = 0.5, lambda2 = 0.5)
  
  pred_nocov_link <- predict(res_nocov, newx, type = "link")
  expect_equal(dim(pred_nocov_link), c(10, J))
  
  pred_nocov_extra <- predict(res_nocov, newx, newcovs = newcovs)
  expect_equal(dim(pred_nocov_extra), c(10, J))
  
  pred_top2 <- predict(res, newx, newcovs = newcovs, type = "link", topK = 2)
  expect_equal(dim(pred_top2), c(10, J))
  
  expect_error(predict(res, newx, newcovs = newcovs, topK = -1), "topK must be a positive integer")
  expect_error(predict(res, newx, newcovs = newcovs, topK = "two"), "topK must be a positive integer")
})

test_that("fusedRidgeDirect fits correctly with topK option", {
  set.seed(42)
  N <- 50
  M <- 10
  J <- 3
  
  X_pcs <- matrix(rnorm(N * M), nrow = N, ncol = M)
  colnames(X_pcs) <- paste0("PC", 1:M)
  y_raw <- rnorm(N)
  thresholds <- c(-0.5, 0, 0.5)
  
  res_top3 <- fusedRidgeDirect(X_pcs, y_raw, thresholds, covariates = NULL, lambda1 = 0.5, lambda2 = 0.5, topK = 3)
  
  expect_equal(dim(res_top3$coefs_full), c(M, J))
  
  selected <- res_top3$selected_features
  expect_true(length(selected) >= 3 && length(selected) <= 3 * J)
  
  non_selected <- setdiff(seq_len(M), selected)
  if (length(non_selected) > 0) {
    expect_true(all(res_top3$coefs_full[non_selected, ] == 0))
  }
  
  expect_error(fusedRidgeDirect(X_pcs, y_raw, thresholds, topK = -1), "topK must be a positive integer")
  expect_error(fusedRidgeDirect(X_pcs, y_raw, thresholds, topK = "two"), "topK must be a positive integer")
})

test_that("fusedRidgeDirect supports alternative families (gaussian, poisson)", {
  set.seed(42)
  N <- 30
  M <- 5
  J <- 3
  
  X_pcs <- matrix(rnorm(N * M), nrow = N, ncol = M)
  colnames(X_pcs) <- paste0("PC", 1:M)
  y_raw <- rnorm(N)
  thresholds <- c(-0.5, 0, 0.5)
  covariates <- matrix(rnorm(N * 2), nrow = N, ncol = 2)
  colnames(covariates) <- c("Cov1", "Cov2")
  
  # Gaussian fit and predict
  res_gauss <- fusedRidgeDirect(X_pcs, y_raw, thresholds, covariates = covariates,
                                lambda1 = 0.5, lambda2 = 0.5, family = "gaussian")
  expect_equal(res_gauss$family, "gaussian")
  pred_gauss <- predict(res_gauss, X_pcs, newcovs = covariates, type = "response")
  expect_equal(dim(pred_gauss), c(N, J))
  
  # Poisson fit and predict
  res_pois <- fusedRidgeDirect(X_pcs, y_raw, thresholds, covariates = covariates,
                               lambda1 = 0.5, lambda2 = 0.5, family = "poisson")
  expect_equal(res_pois$family, "poisson")
  pred_pois <- predict(res_pois, X_pcs, newcovs = covariates, type = "response")
  expect_equal(dim(pred_pois), c(N, J))
  expect_true(all(pred_pois >= 0))
})

test_that("fusedRidgeDirect cross-validation tunes lambda correctly", {
  set.seed(42)
  N <- 40
  M <- 8
  J <- 3
  
  X_pcs <- matrix(rnorm(N * M), nrow = N, ncol = M)
  colnames(X_pcs) <- paste0("PC", 1:M)
  y_raw <- rnorm(N)
  thresholds <- c(-0.5, 0, 0.5)
  covariates <- matrix(rnorm(N * 2), nrow = N, ncol = 2)
  
  # Fit with CV = TRUE
  res_cv <- fusedRidgeDirect(X_pcs, y_raw, thresholds, covariates = covariates,
                             lambda1 = 0.5, lambda2 = 0.5, cv = TRUE,
                             nlambda = 5, nfolds = 3)
  
  expect_false(is.null(res_cv$cv))
  expect_true(is.numeric(res_cv$optimal_lambda))
  expect_equal(length(res_cv$cv$cvm), 5)
  expect_equal(dim(res_cv$coefs_full), c(M, J))
  
  # Fit with CV = FALSE
  res_nocv <- fusedRidgeDirect(X_pcs, y_raw, thresholds, covariates = covariates,
                               lambda1 = 0.5, lambda2 = 0.5, cv = FALSE)
  
  expect_true(is.null(res_nocv$cv))
  expect_true(is.na(res_nocv$optimal_lambda))
})



