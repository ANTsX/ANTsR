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

test_that("riemannian_adam steps downhill on a simple quadratic", {
  set.seed(3)
  p <- 10; k <- 3
  Tm <- quad_target(p, k)
  energy <- function(V) sum((V - Tm)^2)
  sym <- function(A) (A + t(A)) / 2
  n_desc <- 0
  for (trial in 1:50) {
    # alternate between orthonormal V with a Riemannian (tangent) descent
    # gradient, as simlr passes, and a general V with a Euclidean gradient
    if (trial %% 2 == 0) {
      V <- qr.Q(qr(matrix(rnorm(p * k), p, k)))
      d <- -2 * (V - Tm)
      d <- d - V %*% sym(crossprod(V, d))
    } else {
      V <- matrix(rnorm(p * k), p, k)
      d <- -2 * (V - Tm)
    }
    opt <- create_optimizer("riemannian_adam", list(V))
    res <- optimizer_step(opt, i = 1, V_current = V, descent_gradient = d,
                          learning_rate = 1e-3, myit = 1)
    if (sum((res$updated_V - V) * d) > 0 && energy(res$updated_V) < energy(V)) {
      n_desc <- n_desc + 1
    }
  }
  expect_equal(n_desc, 50)

  # repeated steps keep decreasing the energy
  V <- matrix(rnorm(p * k), p, k)
  opt <- create_optimizer("riemannian_adam", list(V))
  e0 <- energy(V)
  for (it in 1:20) {
    res <- optimizer_step(opt, 1, V, -2 * (V - Tm), learning_rate = 0.01, myit = it)
    V <- res$updated_V
    opt <- res$optimizer
  }
  expect_lt(energy(V), e0)
})

test_that(".project_to_tangent is the orthogonal tangent projection", {
  set.seed(4)
  V <- matrix(rnorm(24), 8, 3)
  D <- matrix(rnorm(24), 8, 3)
  P <- .project_to_tangent(V, D)
  sym <- function(A) (A + t(A)) / 2
  expect_lt(max(abs(sym(crossprod(V, P)))), 1e-10) # tangent
  expect_equal(.project_to_tangent(V, P), P)        # idempotent
  # orthonormal V: reduces to D - V sym(V'D)
  Q <- qr.Q(qr(V))
  expect_equal(.project_to_tangent(Q, D), D - Q %*% sym(crossprod(Q, D)))
})

test_that(".bidirectional_linesearch tries -d when +d is an ascent direction", {
  Tm <- quad_target(4, 2)
  energy <- function(V) sum((V - Tm)^2)
  V <- Tm + 1
  g <- 2 * (V - Tm) # ascent gradient
  res <- .bidirectional_linesearch(V, descent_direction = g / sqrt(sum(g^2)),
                                   ascent_gradient = g, energy_function = energy)
  expect_gt(res$step_size, 0)
  expect_equal(res$direction, -g / sqrt(sum(g^2)))
  expect_lt(energy(V + res$step_size * res$direction), energy(V))

  # and through simlr's default optimizer: after the minimum is overshot, the
  # Adam momentum still points the old way (an ascent direction for the new
  # gradient); the line search must then step along -d instead of stalling.
  opt <- create_optimizer("bidirectional_lookahead", list(V))
  r1 <- optimizer_step(opt, 1, V, descent_gradient = -2 * (V - Tm),
                       full_energy_function = energy, myit = 1)
  V2 <- Tm - 0.1
  d2 <- -2 * (V2 - Tm)
  r2 <- optimizer_step(r1$optimizer, 1, V2, descent_gradient = d2,
                       full_energy_function = energy, myit = 2)
  m <- r2$optimizer$state[[1]]$m
  expect_lt(sum(m * d2), 0) # momentum direction is an ascent direction here
  expect_lt(energy(r2$updated_V), energy(V2))
})

test_that("lookahead keeps Adam's moment estimates between steps", {
  V <- matrix(1, 3, 2)
  d1 <- matrix(-1, 3, 2)
  d2 <- matrix(-2, 3, 2)
  opt <- create_optimizer("lookahead", list(V))
  r1 <- optimizer_step(opt, 1, V, d1, learning_rate = 0.01, myit = 1)
  expect_equal(r1$optimizer$state[[1]]$m, 0.1 * d1)
  r2 <- optimizer_step(r1$optimizer, 1, r1$updated_V, d2, learning_rate = 0.01, myit = 2)
  expect_equal(r2$optimizer$state[[1]]$m, 0.9 * 0.1 * d1 + 0.1 * d2)
  expect_equal(r2$optimizer$state[[1]]$step, 2)
})

test_that("optimizers honour learning_rate / myit passed through ...", {
  set.seed(7)
  V <- matrix(rnorm(12), 4, 3)
  d <- matrix(rnorm(12), 4, 3)
  for (o in c("lars", "nadam", "rmsprop", "adagrad", "sgd_momentum", "psgd",
              "riemannian_adam")) {
    opt <- create_optimizer(o, list(V))
    s1 <- optimizer_step(opt, 1, V, d, learning_rate = 1e-3, myit = 1)$updated_V - V
    s2 <- optimizer_step(opt, 1, V, d, learning_rate = 1e-1, myit = 1)$updated_V - V
    expect_equal(sqrt(sum(s2^2)) / sqrt(sum(s1^2)), 100, tolerance = 1e-6,
                 label = paste(o, "step ratio"))
  }
  for (o in c("nadam", "riemannian_adam")) {
    opt <- create_optimizer(o, list(V))
    s1 <- optimizer_step(opt, 1, V, d, learning_rate = 1e-2, myit = 1)$updated_V
    s50 <- optimizer_step(opt, 1, V, d, learning_rate = 1e-2, myit = 50)$updated_V
    expect_false(isTRUE(all.equal(s1, s50)), label = paste(o, "uses myit"))
  }
})

test_that("estimate_learning_rate_nsa searches along the descent direction", {
  set.seed(2)
  Y0 <- matrix(runif(50), 10, 5)
  X0 <- matrix(runif(50), 10, 5)
  energy <- function(V) 0.5 * sum((V - X0)^2)
  for (m in c("brent", "grid", "armijo", "golden")) {
    lr <- estimate_learning_rate_nsa(Y0, X0, nsa_energy = energy, method = m,
                                     verbose = FALSE, plot = FALSE)$estimated_learning_rate
    expect_gt(lr, 1e-3, label = paste(m, "learning rate"))
    expect_lt(energy(Y0 + lr * (X0 - Y0)), energy(Y0))
  }
  # the exact line minimiser of this quadratic is alpha = 1
  lr <- estimate_learning_rate_nsa(Y0, X0, nsa_energy = energy, method = "brent",
                                   verbose = FALSE, plot = FALSE)$estimated_learning_rate
  expect_equal(lr, 1, tolerance = 1e-3)
  # default energy (retraction + nonnegativity + defect) also moves off the bound
  lr <- estimate_learning_rate_nsa(Y0, X0, method = "brent", verbose = FALSE,
                                   plot = FALSE)$estimated_learning_rate
  expect_gt(lr, 1e-6)
})
