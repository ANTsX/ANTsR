
# install.packages("testthat")
library(testthat)
library(ANTsR)
# ==============================================================================
#      Test Script for Orthogonality Gradient Descent
# ==============================================================================



# --- 2. The Test Itself ---

test_that("Gradient descent successfully minimizes orthogonality defect", {
  
  # --- a. Setup: Create a non-orthogonal starting matrix ---
  set.seed(123)
  p <- 10 # features
  k <- 4  # components
  A_initial <- matrix(rnorm(p * k), nrow = p, ncol = k)
  
  # Let's make it explicitly non-orthogonal
  A_initial[, 2] <- A_initial[, 1] * 0.5 + A_initial[, 2] * 0.5
  
  # --- b. Gradient Descent Loop ---
  
  A_current <- A_initial
  # The defect is scale-invariant, so its gradient scales as 1 / ||A||_F
  # A step size of 0.001 was tuned for the
  # old, non-normalized defect sum(offdiag(t(A) %*% A)^2), whose gradient is
  # ~||A||^3 larger; with the normalized defect it barely moves.
  learning_rate <- 1
  num_iterations <- 100
  
  # Calculate the starting error
  initial_defect <- invariant_orthogonality_defect(A_current)
  
  cat("\n--- Starting Gradient Descent ---\n")
  cat(sprintf("Initial Defect: %.6f\n", initial_defect))
  
  for (i in 1:num_iterations) {
    # Calculate the descent direction
    grad <- gradient_invariant_orthogonality_defect(A_current)
    
    # Take a small step against the gradient
    A_current <- A_current - learning_rate * grad
  }
  
  # Calculate the final error
  final_defect <- invariant_orthogonality_defect(A_current)
  cat(sprintf("Final Defect after %d iterations: %.6f\n", num_iterations, final_defect))
  
  # --- c. The Assertion ---
  
  # We expect the final defect to be significantly smaller than the initial one.
  # A reduction of at least 100x is a strong sign of success.
  info_message <- sprintf(
    "Gradient descent failed to reduce orthogonality error. Initial: %.4f, Final: %.4f",
    initial_defect, final_defect
  )
  
  expect_lt(final_defect, initial_defect / 100, label = info_message)
  
  succeed("Orthogonality defect was successfully minimized.")
})


test_that("invariant_orthogonality_defect is scale invariant", {
  set.seed(1)
  X <- matrix(rnorm(40), 10, 4)
  d <- invariant_orthogonality_defect(X)
  expect_gt(d, 0)
  expect_equal(invariant_orthogonality_defect(10 * X), d)
  expect_equal(invariant_orthogonality_defect(0.01 * X), d)
  expect_equal(invariant_orthogonality_defect(qr.Q(qr(X))), 0)
  expect_equal(invariant_orthogonality_defect(X * 0), 0)
})

test_that("gradient_invariant_orthogonality_defect matches finite differences", {
  set.seed(2)
  X <- matrix(rnorm(24), 8, 3)
  g <- gradient_invariant_orthogonality_defect(X)
  h <- 1e-6
  num <- X * 0
  for (j in seq_along(X)) {
    E <- X * 0
    E[j] <- h
    num[j] <- (invariant_orthogonality_defect(X + E) -
      invariant_orthogonality_defect(X - E)) / (2 * h)
  }
  expect_equal(g, num, tolerance = 1e-6)
  # scale invariance of f implies <A, grad f(A)> = 0 and grad f(cA) = grad f(A) / c
  expect_equal(sum(X * g), 0, tolerance = 1e-10)
  expect_equal(gradient_invariant_orthogonality_defect(10 * X), g / 10)
})
