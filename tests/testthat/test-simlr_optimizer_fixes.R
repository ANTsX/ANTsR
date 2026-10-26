# Regression tests for optimizer direction / state / RNG fixes.

quad_target <- function(p, k) matrix(seq_len(p * k) / (p * k), p, k)

test_that("nsa_flow does not change the caller's .Random.seed or RNGkind", {
  old_kind <- RNGkind()
  on.exit(do.call(RNGkind, as.list(old_kind)), add = TRUE)

  set.seed(10)
  Y0 <- matrix(runif(20), 5, 4)
  X0 <- matrix(runif(20), 5, 4)

  # default seed, X0 given
  seed_before <- .Random.seed
  invisible(nsa_flow(Y0, X0, max_iter = 3))
  expect_identical(.Random.seed, seed_before)

  # default seed, X0 = NULL (random perturbation of Y0)
  invisible(nsa_flow(Y0, max_iter = 3))
  expect_identical(.Random.seed, seed_before)

  # non-default RNG kind is preserved
  suppressWarnings(RNGkind("L'Ecuyer-CMRG", "Box-Muller"))
  set.seed(11)
  seed_before <- .Random.seed
  invisible(nsa_flow(Y0, max_iter = 3))
  expect_identical(RNGkind()[1:2], c("L'Ecuyer-CMRG", "Box-Muller"))
  expect_identical(.Random.seed, seed_before)
})

test_that("nsa_flow default seed is deterministic; seed = NULL uses the ambient stream", {
  set.seed(1)
  Y0 <- matrix(runif(20), 5, 4)
  r1 <- nsa_flow(Y0, max_iter = 3)$Y
  set.seed(999)
  r2 <- nsa_flow(Y0, max_iter = 3)$Y
  expect_identical(r1, r2)

  set.seed(5)
  a <- nsa_flow(Y0, max_iter = 3, seed = NULL)$Y
  set.seed(5)
  b <- nsa_flow(Y0, max_iter = 3, seed = NULL)$Y
  set.seed(6)
  d <- nsa_flow(Y0, max_iter = 3, seed = NULL)$Y
  expect_identical(a, b)
  expect_false(identical(a, d))
})
