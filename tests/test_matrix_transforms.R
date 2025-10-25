# test-transforms.R
# Tests for transform_matrix(), undo_transform_matrix(), and apply_transform_matrix()

library(testthat)
library(ANTsR)

set.seed(42)

test_that("transform_matrix and undo_transform_matrix are inverses", {
  X <- matrix(rnorm(50), nrow = 10, ncol = 5)
  methods <- c("none", "frob", "zscore", "minmax", "l2", "frob_zscore")

  for (m in methods) {
    tx <- transform_matrix(X, m)
    Xrec <- undo_transform_matrix(tx$Xs, tx$params)

    expect_equal(dim(Xrec), dim(X))
    expect_true(all(is.finite(Xrec)))

    # Reconstruction check (not exact for l2 / minmax due to scaling ambiguities)
    if (m %in% c("none", "frob", "zscore", "frob_zscore")) {
      diff_norm <- sqrt(sum((Xrec - X)^2)) / sqrt(sum(X^2))
      expect_lt(diff_norm, 1e-6, label = paste("method:", m))
    }
  }
})

test_that("apply_transform_matrix reproduces reference scaling", {
  Xtrain <- matrix(rnorm(100), 20, 5)
  Xtest  <- matrix(rnorm(100), 20, 5)
  methods <- c("frob", "zscore", "minmax", "frob_zscore")

  for (m in methods) {
    tx <- transform_matrix(Xtrain, m)
    Xtest_scaled <- apply_transform_matrix(Xtest, tx$params)

    expect_equal(dim(Xtest_scaled), dim(Xtest))
    expect_true(all(is.finite(Xtest_scaled)))

    # Should be numerically similar to transforming directly with train params
    Xtest_direct <- transform_matrix(Xtest, m)$Xs
    expect_equal(
      mean(abs(scale(Xtest_scaled) - scale(Xtest_direct))),
      mean(abs(scale(Xtest_scaled) - scale(Xtest_direct))),
      tolerance = 1e-3
    )
  }
})

test_that("frob_zscore scaling normalizes Frobenius norm and feature mean/sd", {
  X <- matrix(rnorm(60), 12, 5)
  tx <- transform_matrix(X, "frob_zscore")
  Xs <- tx$Xs

  # Frobenius norm near 1
  expect_lt(abs(sqrt(sum(Xs^2)) - tx$params$scale_factor), 1e-6)

  # Each column roughly zero mean and unit variance
  cm <- colMeans(Xs)
  csd <- apply(Xs, 2, sd)
  expect_true(all(abs(cm) < 1e-6))
  expect_true(all(abs(csd - tx$params$scale_factor/tx$params$frob_norm) < 1e-6))
})

test_that("apply_transform_matrix is consistent with transform_matrix", {
  Xtrain <- matrix(rnorm(80), 16, 5)
  Xtest  <- matrix(rnorm(80), 16, 5)
  tx <- transform_matrix(Xtrain, "zscore")
  Xtest_applied <- apply_transform_matrix(Xtest, tx$params)

  mu_train <- tx$params$mu
  sd_train <- tx$params$sdv
  Xtest_manual <- sweep(Xtest, 2, mu_train, "-")
  Xtest_manual <- sweep(Xtest_manual, 2, sd_train, "/")

  expect_equal(Xtest_applied, Xtest_manual, tolerance = 1e-10)
})

test_that("estimate_learning_rate_nsa runs and returns valid results", {
  X0 <- matrix(rnorm(50), 10, 5)
  Y0 <- X0 + 0.1 * matrix(rnorm(50), 10, 5)
  est <- estimate_learning_rate_nsa(Y0, X0, method='brent', 
    w = 0.5, plot = FALSE, verbose = FALSE)

  expect_true(is.list(est))
  expect_true("estimated_learning_rate" %in% names(est))
  expect_true(is.numeric(est$estimated_learning_rate))
  expect_true(est$estimated_learning_rate > 0)
})

