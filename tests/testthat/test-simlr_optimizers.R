library(ANTsR)

# ==============================================================================
#      Definitive, Robust Test Suite for SIMLR Optimizer Objects (Corrected)
# ==============================================================================
# This script directly tests each optimizer object to verify its core logic.
# It asserts that a single step from each optimizer results in a measurable
# improvement on a simple, well-defined test problem.
# ==============================================================================

# --- Load Required Libraries ---
library(testthat)

# --- 1. The Optimizer System to be Tested ---
# (This section should contain the complete, correct optimizer code from our
# previous response, including create_optimizer, step, step.hybrid_adam, etc.)

# --- 2. The Test Suite Itself ---
context("Testing SIMLR Optimizer Implementations")

# --- Setup: A simple quadratic objective function to minimize ---
# The minimum is at the matrix of all 2s.
# E(V) = sum((V - 2)^2)
# ascent_grad(E) = 2 * (V - 2)
V_TRUE_MINIMUM <- matrix(2, nrow = 10, ncol = 5)
energy_func_test <- function(V) { sum((V - V_TRUE_MINIMUM)^2) }
gradient_func_test <- function(V) { 2 * (V - V_TRUE_MINIMUM) }


# --- A generic harness to test any optimizer ---
test_optimizer <- function(optimizer_type) {
  
  test_that(paste("A single step of optimizer '", optimizer_type, "' improves the solution"), {
    
    # --- Initialization ---
    set.seed(42)
    p <- 10; k <- 5
    V_start <- matrix(rnorm(p * k, mean = 10, sd = 1), nrow = p, ncol = k)
    
    # --- THE FIX IS HERE: Explicitly define parameters ---
    # We create a list of parameters to pass to the optimizer
    params_for_optimizer <- list(
      learning_rate = 0.1,
      myit = 1,
      beta1 = 0.9,
      beta2 = 0.999
    )
    
    # Create the optimizer object, passing the explicit named arguments
    optimizer <- create_optimizer(optimizer_type, list(V_start), 
                                  params = params_for_optimizer)
    
    initial_distance <- sqrt(energy_func_test(V_start))
    cat(sprintf(
      "\n--- Testing Optimizer: %s ---\nInitial Distance to Minimum: %.4f\n",
      toupper(optimizer_type), initial_distance
    ))
    
    # --- Take a Single Optimization Step ---
    ascent_grad <- gradient_func_test(V_start)
    descent_grad <- -ascent_grad
    
    # Take one step using the generic `step()` function.
    # The `params` list is automatically carried inside the optimizer object.
    step_result <- step(
      optimizer,
      i = 1,
      V_current = V_start,
      descent_gradient = descent_grad,
      full_energy_function = energy_func_test
    )
    
    V_after_one_step <- step_result$updated_V
    
    # --- Evaluation and Assertions ---
    final_distance <- sqrt(energy_func_test(V_after_one_step))
    cat(sprintf("Distance after one step: %.4f\n", final_distance))
    
    info_message <- sprintf(
      "Optimizer '%s' failed to improve the solution. Initial Distance: %.4f, Final Distance: %.4f",
      optimizer_type, initial_distance, final_distance
    )
    expect_lt(final_distance, initial_distance, label = info_message)
    
    succeed(paste("Optimizer '", optimizer_type, "' correctly moved closer to the minimum."))
  })
}


# --- Run the tests for all implemented optimizers ---
cat("\n--- Running Optimizer Unit Tests ---\n")

# To run this test, first source the R/simlr_optimizers.R file
# source("R/simlr_optimizers.R") 

test_optimizer("hybrid_adam")
test_optimizer("adam")
test_optimizer("nadam")
test_optimizer("sgd_momentum")
test_optimizer("rmsprop")
test_optimizer("adagrad")

# test_optimizer("hybrid_nadam")

cat("\n--- All Optimizer Tests Complete ---\n")

