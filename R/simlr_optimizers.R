# ==============================================================================
#           SIMLR Optimizer Object System (Final Corrected Version)
# ==============================================================================
# This file provides a modular S3-based system for handling different
# optimization strategies within the simlr algorithm. It is designed to be
# robust, correct, and easily extensible.
#
# Main functions:
#   - create_optimizer(): Constructor to initialize a chosen optimizer.
#   - step(): Generic function to perform one optimization step.
# ==============================================================================

#' Create and Initialize a SIMLR Optimizer
#'
#' This factory function creates an optimizer object that holds the state and
#' update logic for a chosen optimization strategy.
#'
#' @param optimizer_type A string identifying the optimizer. One of:
#'   "hybrid_adam" (Adam direction with Armijo line search - robust),
#'   "adam" (Adam with a learning rate schedule - fast),
#'   "nadam" (Nesterov-accelerated Adam - fast and robust),
#'   "sgd_momentum" (SGD with momentum and learning rate schedule - classic).
#' @param vmats The list of initial V matrices, used to set up the dimensions
#'   of the optimizer's internal state variables.
#' @param ... Additional hyperparameters for the optimizer (e.g., `beta1`, `beta2`).
#'
#' @return An optimizer object (a list with a class attribute).
#' @export
create_optimizer <- function(optimizer_type = "hybrid_adam", vmats, ...) {
  
  # Initialize the state list, which will hold momentum, etc. for each modality
  state <- lapply(vmats, function(v) {
    list(
      # For Adam, Nadam, RMSprop
      m = v * 0,          # 1st moment vector (momentum)
      v = v * 0,          # 2nd moment vector (raw)
      v_max = v * 0,      # For AMSGrad variant of Adam
      
      # For Adagrad
      g_sum_sq = v * 0,   # Sum of squared gradients
      
      # For standard SGD with Momentum
      momentum = v * 0,
      
      # For Line Search methods
      last_step_size = 1.0    )
  })
  
  # The optimizer object is a list containing its type, state, and params
  optimizer <- list(
    type = optimizer_type,
    state = state,
    params = list(...) # Store any extra params like beta1, beta2, learning rates
  )
  
  # Assign a class based on the type, which enables S3 dispatch
  class(optimizer) <- c(optimizer_type, "simlr_optimizer")
  
  return(optimizer)
}


#' Take One Optimization Step
#'
#' This is a generic S3 function that dispatches to the correct update rule
#' based on the optimizer's class.
#'
#' @param optimizer The optimizer object from `create_optimizer()`.
#' @param i The index of the modality currently being updated.
#' @param V_current The current V matrix for modality `i`.
#' @param descent_gradient The Riemannian DESCENT gradient (-grad(E)) for V_current.
#' @param ... Additional arguments required by the specific method (e.g.,
#'   `full_energy_function` for line searches, `learning_rate` for others).
#'
#' @return A list containing `updated_V` and the updated `optimizer` object.
#' @export
step <- function(optimizer, i, V_current, descent_gradient, ...) {
  UseMethod("step")
}


#' @export
step.hybrid_adam <- function(optimizer, i, V_current, descent_gradient, full_energy_function, ...) {
  # This method uses Adam to find a direction and a backtracking line search
  # to find the optimal step size. It is robust and parameter-free.
  
  state <- optimizer$state[[i]]
  params <- optimizer$params
  beta1 <- params$beta1 %||% 0.9
  beta2 <- params$beta2 %||% 0.999
  epsilon <- params$epsilon %||% 1e-8
  
  # Adam state is updated with the descent gradient.
  # The sign doesn't matter for the v_max term as it's squared.
  state$m <- beta1 * state$m + (1 - beta1) * descent_gradient
  state$v <- beta2 * state$v + (1 - beta2) * (descent_gradient^2)
  state$v_max <- pmax(state$v_max, state$v)
  
  # The search direction is the Adam-proposed update vector (a descent direction)
  # We use the raw momentum `m` here, as bias correction is tricky with line search
  search_direction <- state$m / (sqrt(state$v_max) + epsilon)
  
  # Perform line search along this descent direction
  optimal_step_size <- .backtracking_linesearch(
    V_current = V_current,
    descent_direction = search_direction,
    ascent_gradient = -descent_gradient, # The ascent grad is needed for the slope term
    energy_function = full_energy_function,
    initial_step_size = state$last_step_size # Use history for a smart guess
  )
  
  # Store state for next iteration
  # If we took a good step, guess a slightly larger one next time (expansion).
  # If the step was zero, reset to 1.0 to escape flat regions.
  state$last_step_size <- if (optimal_step_size > 1e-9) optimal_step_size * 1.5 else 1.0
  optimizer$state[[i]] <- state
  
  # The final updated parameter is a step ALONG the descent direction
  updated_V <- V_current + optimal_step_size * search_direction
  
  return(list(updated_V = updated_V, optimizer = optimizer))
}


#' @export
step.adam <- function(optimizer, i, V_current, descent_gradient, ...) {
  # This method uses pure Adam with a learning rate (no line search). It is very fast.
  
  state <- optimizer$state[[i]]
  params <- optimizer$params
  beta1 <- params$beta1 %||% 0.9; beta2 <- params$beta2 %||% 0.999
  epsilon <- params$epsilon %||% 1e-8; learning_rate <- params$learning_rate %||% 0.001
  myit <- params$myit %||% 1 # Need current iteration for bias correction
  # Update state with the descent gradient
  state$m <- beta1 * state$m + (1 - beta1) * descent_gradient
  state$v <- beta2 * state$v + (1 - beta2) * (descent_gradient^2)
  state$v_max <- pmax(state$v_max, state$v)

  # Bias correction for momentum and adaptive learning rate
  m_hat <- state$m / (1 - beta1^myit)
  v_hat <- state$v_max / (1 - beta2^myit)
  
  update_direction <- m_hat / (sqrt(v_hat) + epsilon)
  
  # The update step is ALONG the descent direction
  updated_V <- V_current + learning_rate * update_direction
  
  optimizer$state[[i]] <- state
  
  return(list(updated_V = updated_V, optimizer = optimizer))
}


#' @export
step.gd <- function(optimizer, i, V_current, descent_gradient, ...) {
  # Naive gradient descent with exponential learning rate decay
  # update: V_{t+1} = V_t - η_t * ∇f(V_t)
  
  state <- optimizer$state[[i]]
  params <- optimizer$params
  
  learning_rate <- params$learning_rate %||% 0.01
  decay_rate    <- params$decay_rate %||% 1e-3
  
  # Track iteration count in state
  state$iter <- (state$iter %||% 0) + 1
  
  # Exponential decay schedule
  effective_lr <- learning_rate * exp(-decay_rate * state$iter)
  
  # Compute update
  updated_V <- V_current - effective_lr * descent_gradient
  
  # Update state
  optimizer$state[[i]] <- state
  
  return(list(updated_V = updated_V, optimizer = optimizer))
}

#' @export
step.nadam <- function(optimizer, i, V_current, descent_gradient, ...) {
  # This method uses Nesterov-accelerated Adam. It is very fast and powerful.
  
  state <- optimizer$state[[i]]
  params <- optimizer$params
  beta1 <- params$beta1 %||% 0.9
  beta2 <- params$beta2 %||% 0.999
  epsilon <- params$epsilon %||% 1e-8
  learning_rate <- params$learning_rate %||% 0.001
  myit <- params$myit %||% 1

  # Update state with the descent gradient
  state$m <- beta1 * state$m + (1 - beta1) * descent_gradient
  state$v <- beta2 * state$v + (1 - beta2) * (descent_gradient^2)
  
  # Nadam's bias correction is applied to the look-ahead momentum term
  m_hat <- state$m / (1 - beta1^myit)
  
  # This is the Nesterov part: combine the look-ahead momentum with the current gradient
  nesterov_m_hat <- (beta1 * m_hat) + ((1 - beta1) * descent_gradient) / (1 - beta1^myit)
  
  # Standard second moment update with AMSGrad
  state$v_max <- pmax(state$v_max, state$v)
  v_hat <- state$v_max / (1 - beta2^myit)
  
  update_direction <- nesterov_m_hat / (sqrt(v_hat) + epsilon)
  
  # The update step is ALONG the descent direction
  updated_V <- V_current + learning_rate * update_direction
  
  optimizer$state[[i]] <- state
  
  return(list(updated_V = updated_V, optimizer = optimizer))
}

#' @export
step.rmsprop <- function(optimizer, i, V_current, descent_gradient, ...) {
  # RMSprop: Root Mean Square Propagation
  state <- optimizer$state[[i]]
  params <- optimizer$params
  beta <- params$beta %||% 0.9 # Decay rate for squared gradients
  epsilon <- params$epsilon %||% 1e-8
  learning_rate <- params$learning_rate %||% 0.001

  # Update the exponentially decaying average of squared gradients
  state$v <- beta * state$v + (1 - beta) * (descent_gradient^2)
  
  # Update step is scaled by the root of this average
  update_direction <- descent_gradient / (sqrt(state$v) + epsilon)
  updated_V <- V_current + learning_rate * update_direction
  
  optimizer$state[[i]] <- state
  return(list(updated_V = updated_V, optimizer = optimizer))
}

#' @export
step.adagrad <- function(optimizer, i, V_current, descent_gradient, ...) {
  # Adagrad: Adaptive Gradient Algorithm
  # This optimizer is particularly effective for sparse data as it adapts the
  # learning rate for each parameter based on the history of its gradients.
  
  state <- optimizer$state[[i]]
  params <- optimizer$params
  epsilon <- params$epsilon %||% 1e-8
  # Adagrad is sensitive to the learning rate; a higher default is common.
  learning_rate <- params$learning_rate %||% 0.1 

  # --- 1. Accumulate the sum of squared gradients ---
  # The state variable g_sum_sq keeps a running total of the squared
  # gradient for every parameter.
  state$g_sum_sq <- state$g_sum_sq + descent_gradient^2
  
  # --- 2. Calculate the adaptive update ---
  # The update step is scaled by the root of the historical sum.
  # Parameters that have received large gradients in the past will have
  # smaller updates, while parameters with small past gradients will have
  # larger updates.
  
  
  adaptive_denominator <- sqrt(state$g_sum_sq) + epsilon
  updated_V <- V_current + learning_rate * (descent_gradient / adaptive_denominator)
    
  # --- 3. Update and return state ---
  optimizer$state[[i]] <- state
  return(list(updated_V = updated_V, optimizer = optimizer))
}

#' @export
step.sgd_momentum <- function(optimizer, i, V_current, descent_gradient, ...) {
  # Classic SGD with momentum and a learning rate.
  
  state <- optimizer$state[[i]]
  params <- optimizer$params
  beta <- params$beta %||% 0.9
  learning_rate <- params$learning_rate %||% 0.01

  # Update momentum with the descent gradient
  state$momentum <- beta * state$momentum + (1 - beta) * descent_gradient
  
  # Update parameter by taking a step ALONG the momentum descent direction
  updated_V <- V_current + learning_rate * state$momentum
  
  optimizer$state[[i]] <- state
  
  return(list(updated_V = updated_V, optimizer = optimizer))
}


# --- Internal Helper for Line Search ---

#' @keywords internal
.backtracking_linesearch <- function(V_current, descent_direction, ascent_gradient,
                                     energy_function, initial_step_size = 1.0,
                                     alpha = 0.3, beta = 0.8, max_iter = 10) {
  step_size <- initial_step_size
  initial_energy <- energy_function(V_current)
  
  # The slope for the Armijo condition is <grad(E), d>.
  # We have grad(E) (ascent_gradient) and d (descent_direction).
  slope_term <- sum(ascent_gradient * descent_direction)
  
  # This should be negative for a valid descent direction.
  if (slope_term >= 0) {
      warning("Line search given a non-descent direction. Stopping search.")
      return(0)
  }
  
  for (i in 1:max_iter) {
    V_candidate <- V_current + step_size * descent_direction
    new_energy <- tryCatch(energy_function(V_candidate), error = function(e) Inf)
    
    # Armijo sufficient decrease condition:
    # E(V + step*d) <= E(V) + alpha * step * <grad(E), d>
    if (new_energy <= initial_energy + alpha * step_size * slope_term) {
      return(step_size)
    }
    step_size <- beta * step_size
  }
  
  warning("Line search backtracking failed to find a suitable step size.")
  return(0)
}

#' @export
step.ls_adam <- function(optimizer, i, V_current, descent_gradient, full_energy_function, ...) {
  # This method uses standard Adam to determine the search direction and R's
  # `optimize` function to perform a line search for the step size.
  
  state <- optimizer$state[[i]]
  params <- optimizer$params
  beta1 <- params$beta1 %||% 0.9; beta2 <- params$beta2 %||% 0.999
  epsilon <- params$epsilon %||% 1e-8
  myit <- params$myit %||% 1
  
  # --- 1. Get Standard Adam Search Direction ---
  
  # Update state with the descent gradient
  state$m <- beta1 * state$m + (1 - beta1) * descent_gradient
  state$v <- beta2 * state$v + (1 - beta2) * (descent_gradient^2)
  state$v_max <- pmax(state$v_max, state$v) # AMSGrad correction for stability
  
  # Bias correction for momentum and adaptive learning rate
  m_hat <- state$m / (1 - beta1^myit)
  v_hat <- state$v_max / (1 - beta2^myit)
  
  # The standard Adam search direction (without Nesterov)
  search_direction <- m_hat / (sqrt(v_hat) + epsilon)
  
  # --- 2. Perform Line Search with `optimize` ---
  
  lineSearchRange <- params$lineSearchRange %||% c(-1e2, 1e2)
  lineSearchTolerance <- params$lineSearchTolerance %||% 1e-3
  lineSearchTolerance <- params$lineSearchTolerance %||% 1e-3
  lineSearchRange = c( -1, 1. ) * 1e2 # * state$last_step_size

  line_search_result <- tryCatch({
      optimize(
        f = function(step_size) {
          V_proposed <- V_current + step_size * search_direction
          return(full_energy_function(V_proposed))
        },
        interval = lineSearchRange,
        tol = lineSearchTolerance
      )
  }, error = function(e) {
      warning(paste("`optimize` line search failed for ls_adam:", e$message, ". Using zero step."))
      return(list(minimum = 0))
  })
  
  optimal_step_size <- line_search_result$minimum
  
  # --- 3. Update Parameter and State ---
  
  updated_V <- V_current + optimal_step_size * search_direction
  optimizer$state[[i]] <- state
  
  return(list(updated_V = updated_V, optimizer = optimizer))
}

#' @export
step.ls_rmsprop <- function(optimizer, i, V_current, descent_gradient, full_energy_function, ...) {
  # This method uses RMSprop to determine the search direction and R's
  # `optimize` function to perform a line search for the step size.
  
  state <- optimizer$state[[i]]
  params <- optimizer$params
  beta <- params$beta %||% 0.9 # Decay rate, often called rho in RMSprop
  epsilon <- params$epsilon %||% 1e-8
  
  # --- 1. Get RMSprop Search Direction ---
  
  # Update the exponentially decaying average of squared gradients
  state$v <- beta * state$v + (1 - beta) * (descent_gradient^2)
  
  # The search direction is the raw gradient, scaled by the root of the average
  search_direction <- descent_gradient / (sqrt(state$v) + epsilon)

  # --- 2. Perform Line Search with `optimize` ---
  
  lineSearchRange = params$lineSearchRange %||% c(-1e2, 1e2)
  lineSearchTolerance = params$lineSearchTolerance %||% 1e-3
  lineSearchTolerance <- params$lineSearchTolerance %||% 1e-3
  lineSearchRange = c( -1, 1. ) * 1e2 # * state$last_step_size

  line_search_result <- tryCatch({
      optimize(
        f = function(step_size) {
          V_proposed <- V_current + step_size * search_direction
          return(full_energy_function(V_proposed))
        },
        interval = lineSearchRange,
        tol = lineSearchTolerance
      )
  }, error = function(e) {
      warning(paste("`optimize` line search failed for ls_rmsprop:", e$message, ". Using zero step."))
      return(list(minimum = 0))
  })
  
  optimal_step_size <- line_search_result$minimum
  
  # --- 3. Update Parameter and State ---
  
  updated_V <- V_current + optimal_step_size * search_direction
  optimizer$state[[i]] <- state
  
  return(list(updated_V = updated_V, optimizer = optimizer))
}

# ==============================================================================
#           NEW OPTIMIZER: Nadam Direction with `optimize` Line Search
# ==============================================================================
#' @export
step.ls_nadam <- function(optimizer, i, V_current, descent_gradient, full_energy_function, ...) {
  # This method uses Nadam to determine the search direction and R's built-in
  # `optimize` function to perform a high-precision line search for the step size.
  
  state <- optimizer$state[[i]]
  params <- optimizer$params
  beta1 <- params$beta1 %||% 0.9; beta2 <- params$beta2 %||% 0.999
  epsilon <- params$epsilon %||% 1e-8
  myit <- params$myit %||% 1
  # --- 1. Get Nadam Search Direction ---
  
  # Update state with the descent gradient
  state$m <- beta1 * state$m + (1 - beta1) * descent_gradient
  state$v <- beta2 * state$v + (1 - beta2) * (descent_gradient^2)
  
  # Bias correction
  m_hat <- state$m / (1 - beta1^myit)
  v_hat <- state$v / (1 - beta2^myit)
  
  # Nesterov look-ahead component
  nesterov_m_hat <- (beta1 * m_hat) + ((1 - beta1) * descent_gradient) / (1 - beta1^myit)
  
  # This is the "smart" descent direction
  search_direction <- nesterov_m_hat / (sqrt(v_hat) + epsilon)
  
  # --- 2. Perform Line Search with `optimize` ---
  
  # Get search range and tolerance from params, with sensible defaults
  lineSearchRange <- params$lineSearchRange %||% c(0, 1e4)
  lineSearchTolerance <- params$lineSearchTolerance %||% 1e-3
  lineSearchRange = c( -1, 1. ) * 1e2 # * state$last_step_size
  line_search_result <- tryCatch({
      optimize(
        f = function(step_size) {
          # Propose a step along the descent direction and evaluate its energy
          V_proposed <- V_current + step_size * search_direction
          return(full_energy_function(V_proposed))
        },
        interval = lineSearchRange,
        tol = lineSearchTolerance
      )
  }, error = function(e) {
      warning(paste("`optimize` line search failed:", e$message, ". Using zero step."))
      return(list(minimum = 0))
  })
  
  optimal_step_size <- line_search_result$minimum
  
  # --- 3. Update Parameter and State ---
  
  updated_V <- V_current + optimal_step_size * search_direction
  
  # The Adam state is updated regardless of the step taken
  optimizer$state[[i]] <- state
  
  return(list(updated_V = updated_V, optimizer = optimizer))
}



#' @export
step.amsgrad <- function(optimizer, i, V_current, descent_gradient, ...) {
  # AMSGrad optimizer
  
  state <- optimizer$state[[i]]
  params <- optimizer$params
  beta1 <- params$beta1 %||% 0.9
  beta2 <- params$beta2 %||% 0.999
  epsilon <- params$epsilon %||% 1e-8
  lr <- params$learning_rate %||% 0.001
  t <- params$myit %||% 1
  
  # Init state
  state$m <- state$m %||% 0
  state$v <- state$v %||% 0
  state$vhat <- state$vhat %||% 0
  
  # Update biased moments
  state$m <- beta1 * state$m + (1 - beta1) * descent_gradient
  state$v <- beta2 * state$v + (1 - beta2) * (descent_gradient^2)
  
  # Maintain max of past v's
  state$vhat <- pmax(state$vhat, state$v)
  
  # Bias correction
  m_hat <- state$m / (1 - beta1^t)
  
  # Update step
  updated_V <- V_current - lr * m_hat / (sqrt(state$vhat) + epsilon)
  
  state$iter <- t + 1
  optimizer$state[[i]] <- state
  return(list(updated_V = updated_V, optimizer = optimizer))
}

#' @export
step.adadelta <- function(optimizer, i, V_current, descent_gradient, ...) {
  # AdaDelta update rule
  
  state <- optimizer$state[[i]]
  params <- optimizer$params
  rho     <- params$rho %||% 0.95
  epsilon <- params$epsilon %||% 1e-6
  
  # Init state if missing
  state$Eg2 <- state$Eg2 %||% 0
  state$Edx2 <- state$Edx2 %||% 0
  
  # Accumulate gradient squared
  state$Eg2 <- rho * state$Eg2 + (1 - rho) * (descent_gradient^2)
  
  # Compute update
  RMS_dx <- sqrt(state$Edx2 + epsilon)
  RMS_g  <- sqrt(state$Eg2 + epsilon)
  delta_x <- -(RMS_dx / RMS_g) * descent_gradient
  
  # Update parameters
  updated_V <- V_current + delta_x
  
  # Accumulate updates squared
  state$Edx2 <- rho * state$Edx2 + (1 - rho) * (delta_x^2)
  
  optimizer$state[[i]] <- state
  return(list(updated_V = updated_V, optimizer = optimizer))
}


#' @export
step.lbfgs <- function(optimizer, i, V_current, descent_gradient, ...) {
  # Naive memory-1 L-BFGS optimizer, works for vector or matrix parameters
  state <- optimizer$state[[i]]
  params <- optimizer$params
  lr <- params$learning_rate %||% 0.1
  
  # Save original shape
  orig_dim <- dim(V_current)
  
  # Flatten to vector
  V_vec <- as.vector(V_current)
  g_vec <- as.vector(descent_gradient)
  
  # Initialize state
  state$prev_V <- state$prev_V %||% V_vec
  state$prev_g <- state$prev_g %||% g_vec
  state$H <- state$H %||% diag(length(V_vec))
  
  # Compute differences
  s <- V_vec - state$prev_V
  y <- g_vec - state$prev_g
  
  if (sum(y * s) > 1e-12) {
    rho <- 1 / sum(y * s)
    I <- diag(length(s))
    H_new <- (I - rho * tcrossprod(s, y)) %*% state$H %*% (I - rho * tcrossprod(y, s)) + rho * tcrossprod(s)
    state$H <- H_new
    step_dir <- - H_new %*% g_vec
  } else {
    step_dir <- - lr * g_vec  # fallback
  }
  
  # Apply step
  V_vec_new <- V_vec + lr * step_dir
  
  # Reshape back to original shape
  if (!is.null(orig_dim)) {
    updated_V <- matrix(V_vec_new, nrow = orig_dim[1], ncol = orig_dim[2])
  } else {
    updated_V <- V_vec_new
  }
  
  # Save state
  state$prev_V <- V_vec
  state$prev_g <- g_vec
  optimizer$state[[i]] <- state
  
  return(list(updated_V = updated_V, optimizer = optimizer))
}

#' @export
step.lookahead <- function(optimizer, i, V_current, descent_gradient, ...) {
  state <- optimizer$state[[i]]
  params <- optimizer$params
  
  # Lookahead params
  k <- params$k %||% 5       # sync period
  alpha <- params$alpha %||% 0.5
  
  # Initialize state
  state$V_slow <- state$V_slow %||% V_current
  state$step <- state$step %||% 0
  
  # Use Adam as default base optimizer
  base_result <- step.adam(optimizer, i, V_current, descent_gradient, ...)
  V_fast <- base_result$updated_V
  optimizer <- base_result$optimizer
  
  # Update lookahead step
  state$step <- state$step + 1
  if (state$step %% k == 0) {
    state$V_slow <- state$V_slow + alpha * (V_fast - state$V_slow)
    V_fast <- state$V_slow
  }
  
  optimizer$state[[i]] <- state
  return(list(updated_V = V_fast, optimizer = optimizer))
}

# Helper for default values
`%||%` <- function(a, b) if (is.null(a)) b else a

#' List Available SIMLR Optimizers
#'
#' Dynamically returns the names of all optimizer types with registered `step.*` methods.
#'
#' @return A character vector of optimizer names (e.g., "adam", "nadam", etc.)
#' @export
list_simlr_optimizers <- function() {
  methods_vec <- as.character(methods("step"))
  optimizers <- gsub("^step\\.|\\*$", "", methods_vec)
  return(optimizers)
}
