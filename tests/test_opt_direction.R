# Test optimizer update direction
library(testthat)

# Mock energy function (simple quadratic: x^2 + y^2)
nsa_energy <- function(V, it) {
  sum(V^2)
}

# Mock gradient function (gradient of x^2 + y^2 is [2x, 2y])
rgrad <- function(V, it) {
  2 * V
}
for ( optim in list_simlr_optimizers() ) {
  # Test case

  test_that(paste(optim, "Optimizer updates reduce energy"), {
    # Initialize parameters
    Y <- matrix(c(1.0, 1.0), nrow = 2, ncol = 1) # Starting point
  initial_learning_rate <- 1e-3
  it <- 1

  # Create optimizer
  opt <- create_optimizer(
    optimizer = optim, # Assuming a simple SGD optimizer
    vmats = list(Y),
    learning_rate = initial_learning_rate
  )

  # Compute initial energy
  initial_energy <- nsa_energy(Y, it)

  # Perform optimization step
  step_result <- step(
    opt,
    i = 1,
    V_current = Y,
    descent_gradient = rgrad(Y, it) * (-1.0),
    learning_rate = initial_learning_rate,
    full_energy_function = nsa_energy,
    myit = it
  )

  # Get updated values
  Y_new <- step_result$updated_V

  # Compute new energy
  new_energy <- nsa_energy(Y_new, it)

  # Verify energy decreases
  expect_true(
    new_energy < initial_energy,
    info = paste("Initial energy:", initial_energy, "New energy:", new_energy)
  )

  # Verify update direction aligns with negative gradient
  grad <- rgrad(Y, it)
  update <- Y_new - Y
  dot_product <- sum(update * grad)
  expect_true(
    dot_product < 0,
    info = "Update direction should align with negative gradient"
  )
})


}
