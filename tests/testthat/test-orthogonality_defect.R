
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
  learning_rate <- 0.1
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
