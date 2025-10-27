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
#'   "sgd_momentum" (SGD with momentum and learning rate schedule - classic),
#'   "armijo_gradient" (steepest descent with robust Armijo backtracking line search - maximally robust).
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
step.random_search <- function(optimizer, i, V_current, descent_gradient,
                               full_energy_function, ...) {
  # This optimizer performs random search around V_current.
  # It proposes n_trials random perturbations and picks the candidate
  # that yields the lowest energy (greedy). If no energy function is given,
  # it returns a single perturbation.
  state <- optimizer$state[[i]]
  params <- optimizer$params
  n_trials <- params$n_trials %||% 16L
  noise_scale <- params$noise_scale %||% 0.1
  lr <- params$learning_rate %||% 0.01
  
  best_V <- V_current
  best_energy <- Inf
  
  if (!is.null(full_energy_function)) {
    # Evaluate current energy
    current_energy <- full_energy_function(V_current, return_raw = FALSE)
    best_energy <- current_energy
  }
  
  for (t in seq_len(n_trials)) {
    noise <- matrix(rnorm(length(V_current)), nrow = nrow(V_current))
    perturb <- lr * noise_scale * noise
    candidate <- V_current + perturb
    
    if (!is.null(full_energy_function)) {
      cand_energy <- tryCatch(
        full_energy_function(candidate, return_raw = FALSE),
        error = function(e) Inf
      )
      if (cand_energy < best_energy) {
        best_energy <- cand_energy
        best_V <- candidate
      }
    } else {
      # No energy function → just take the first perturbation
      best_V <- candidate
      break
    }
  }
  
  optimizer$state[[i]] <- state
  return(list(updated_V = best_V, optimizer = optimizer))
}

#' @export
step.random_gradient <- function(optimizer, i, V_current, descent_gradient,
                                 full_energy_function, ...) {
  # This optimizer ignores the actual gradient and generates a random one.
  # It then uses backtracking line search to find an acceptable step size.
  
  descent_gradient = descent_gradient * (-1.0)  
  state <- optimizer$state[[i]]
  params <- optimizer$params
  epsilon <- params$epsilon %||% 1e-8
  
  # Generate a random gradient of the same shape as V_current
  rand_grad <- matrix(rnorm(length(V_current)), nrow = nrow(V_current))
  
  # Normalize gradient to unit RMS, then scale
  rms <- sqrt(mean(rand_grad^2))
  if (rms > 0) rand_grad <- rand_grad / (rms + epsilon)
  
  # Proposed descent direction is the random gradient
  search_direction <- rand_grad
  
  # Perform line search along this direction
  optimal_step_size <- .backtracking_linesearch(
    V_current = V_current,
    descent_direction = search_direction,
    ascent_gradient = -rand_grad, # slope term consistency
    energy_function = full_energy_function,
    initial_step_size = state$last_step_size %||% 1.0
  )
  
  # Store updated step size for next call
  state$last_step_size <- if (optimal_step_size > 1e-9) optimal_step_size * 1.5 else 1.0
  optimizer$state[[i]] <- state
  
  updated_V <- V_current + optimal_step_size * search_direction
  
  return(list(updated_V = updated_V, optimizer = optimizer))
}

#' @export
step.adam <- function(optimizer, i, V_current, descent_gradient, ...) {
  # Standard Adam: momentum + adaptive RMSprop, no cumulative max
  state <- optimizer$state[[i]]
  params <- optimizer$params
  beta1 <- params$beta1 %||% 0.9
  beta2 <- params$beta2 %||% 0.999
  epsilon <- params$epsilon %||% 1e-8
  learning_rate <- params$learning_rate %||% 0.001
  myit <- params$myit %||% 1  # Current iter for bias correction
  # Update moments
  state$m <- beta1 * state$m + (1 - beta1) * descent_gradient
  state$v <- beta2 * state$v + (1 - beta2) * (descent_gradient^2)  # Standard v, no v_max
  # Bias correction
  m_hat <- state$m / (1 - beta1^myit)
  v_hat <- state$v / (1 - beta2^myit)  # Standard: forgetful, not cumulative
  update_direction <- m_hat / (sqrt(v_hat) + epsilon)
  # Update
  updated_V <- V_current + learning_rate * update_direction
  optimizer$state[[i]] <- state
  params$myit <- myit + 1  # Increment for next
  return(list(updated_V = updated_V, optimizer = optimizer))
}

#' @export
step.gd <- function(optimizer, i, V_current, descent_gradient, ...) {
  # Naive gradient descent with exponential learning rate decay
  # update: V_{t+1} = V_t - η_t * ∇f(V_t)
  descent_gradient = descent_gradient * (-1.0)
  state <- optimizer$state[[i]]
  params <- optimizer$params
  
  learning_rate <- params$learning_rate %||% 1e-6
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
  descent_gradient = descent_gradient * (-1.0)
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
  descent_gradient = descent_gradient * (-1.0)
  
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



# ==============================================================================
#           NEW OPTIMIZER: Bidirectional Armijo Gradient
# ==============================================================================
#' @export
step.bidirectional_armijo_gradient <- function(optimizer, i, V_current, descent_gradient, full_energy_function, ...) {
  # This method tries both the provided descent_gradient and its negation, using a robust
  # Armijo backtracking line search to find an energy-reducing step. It is maximally robust
  # to incorrect gradient directions and guarantees an energy-reducing step if possible.
  
  state <- optimizer$state[[i]]
  params <- optimizer$params
  epsilon <- params$epsilon %||% 1e-12
  
  # Normalize the descent_gradient to unit Frobenius norm
  dir_norm <- sqrt(sum(descent_gradient^2))
  if (dir_norm < epsilon) {
    # Stationary point: no update
    return(list(updated_V = V_current, optimizer = optimizer))
  }
  norm_direction <- descent_gradient / dir_norm
  
  # Perform bidirectional line search
  result <- .bidirectional_linesearch(
    V_current = V_current,
    descent_direction = norm_direction,
    ascent_gradient = -descent_gradient,  # grad(E)
    energy_function = full_energy_function,
    initial_step_size = state$last_step_size %||% 1.0
  )
  
  optimal_step_size <- result$step_size
  selected_direction <- result$direction
  
  # Update last_step_size: expand if successful, reset if failed/tiny
  state$last_step_size <- if (optimal_step_size > 1e-9) optimal_step_size * 1.5 else 1.0
  optimizer$state[[i]] <- state
  
  # Apply the step
  updated_V <- V_current + optimal_step_size * selected_direction
  
  return(list(updated_V = updated_V, optimizer = optimizer))
}

# --- Internal Helper for Bidirectional Line Search ---

#' @keywords internal
.bidirectional_linesearch <- function(V_current, descent_direction, ascent_gradient,
                                     energy_function, initial_step_size = 1.0,
                                     alpha = 1e-4, beta = 0.5, min_step = 1e-12) {
  # Tries both descent_direction and its negation, returning the direction and step size
  # that satisfies the Armijo condition with the largest step size. Returns zero step
  # and original direction if neither works.
  
  # Try positive direction
  pos_result <- .robust_backtracking_linesearch(
    V_current = V_current,
    descent_direction = descent_direction,
    ascent_gradient = ascent_gradient,
    energy_function = energy_function,
    initial_step_size = initial_step_size,
    alpha = alpha,
    beta = beta,
    min_step = min_step
  )
  
  # Try negative direction
  neg_result <- .robust_backtracking_linesearch(
    V_current = V_current,
    descent_direction = -descent_direction,
    ascent_gradient = -ascent_gradient,  # Flip for consistency
    energy_function = energy_function,
    initial_step_size = initial_step_size,
    alpha = alpha,
    beta = beta,
    min_step = min_step
  )
  
  # Select the direction with the larger step size
  if (pos_result >= neg_result && pos_result > 0) {
    return(list(step_size = pos_result, direction = descent_direction))
  } else if (neg_result > 0) {
    return(list(step_size = neg_result, direction = -descent_direction))
  } else {
    warning("Bidirectional line search failed to find a suitable step size in either direction.")
    return(list(step_size = 0, direction = descent_direction))
  }
}

#' @export
step.ranger <- function(optimizer, i, V_current, descent_gradient, ...) {
  # Rectified Adam step (warmup for first 2n iters)
  descent_gradient = descent_gradient * (-1.0)
  state <- optimizer$state[[i]]
  params <- optimizer$params
  myit <- params$myit %||% 1
  n <- length(descent_gradient)  # Flatten for simplicity
  beta1 <- params$beta1 %||% 0.9
  beta2 <- params$beta2 %||% 0.999
  # RAdam rectification
  rho_inf <- 2 / (1 - beta2) - 1
  rho_t <- rho_inf - 2 * myit * beta2^myit / (1 - beta2^myit)
  state$m <- beta1 * state$m + (1 - beta1) * descent_gradient
  state$v <- beta2 * state$v + (1 - beta2) * (descent_gradient^2)
  m_hat <- state$m / (1 - beta1^myit)
  v_hat <- state$v / (1 - beta2^myit)
  # Rectify if rho_t < 5*n (warmup)
  r_t <- if (rho_t > 5 * n) sqrt((rho_t - 4) / (rho_inf - 4) * (1 - beta2^myit) / (1 - beta2)) else sqrt(rho_t / (1 - beta2) * (1 - beta2^myit))
  update_direction <- m_hat / (r_t * (sqrt(v_hat) + 1e-8))
  V_radam <- V_current - params$learning_rate * update_direction
  # Lookahead (from your step.lookahead)
  state$V_slow <- state$V_slow %||% V_current
  alpha <- params$alpha %||% 0.5
  k <- params$k %||% 5
  state$step <- state$step %||% 0
  state$step <- state$step + 1
  if (state$step %% k == 0) {
    state$V_slow <- state$V_slow + alpha * (V_radam - state$V_slow)
    V_radam <- state$V_slow
  }
  optimizer$state[[i]] <- state
  params$myit <- myit + 1
  return(list(updated_V = V_radam, optimizer = optimizer))
}


#' @export
step.vsgd <- function(optimizer, i, V_current, descent_gradient, ...) {
  descent_gradient = descent_gradient * (-1.0)  
  state <- optimizer$state[[i]]
  params <- optimizer$params
  learning_rate <- params$learning_rate %||% 0.001
  sigma <- params$sigma %||% 0.1  # Variational noise scale
  # Variational sample: grad + Gaussian noise ~ N(0, sigma^2 * I)
  var_grad <- descent_gradient + matrix(rnorm(length(descent_gradient), sd = sigma), nrow(V_current), ncol(V_current))
  # SGD with momentum on variational grad
  state$m <- 0.9 * state$m + 0.1 * var_grad
  updated_V <- V_current - learning_rate * state$m
  optimizer$state[[i]] <- state
  return(list(updated_V = updated_V, optimizer = optimizer))
}


#' @export
step.riemannian_adam <- function(optimizer, i, V_current, descent_gradient, ...) {
  # Wraps your Adam, but transports update via retraction/projection
  descent_gradient = descent_gradient * (-1.0)
  state <- optimizer$state[[i]]
  params <- optimizer$params
  myit <- params$myit %||% 1
  beta1 <- params$beta1 %||% 0.9
  beta2 <- params$beta2 %||% 0.999
  epsilon <- params$epsilon %||% 1e-8
  # Adam update in tangent space
  state$m <- beta1 * state$m + (1 - beta1) * descent_gradient
  state$v <- beta2 * state$v + (1 - beta2) * (descent_gradient^2)
  m_hat <- state$m / (1 - beta1^myit)
  v_hat <- state$v / (1 - beta2^myit)
  update_tangent <- m_hat / (sqrt(v_hat) + epsilon)
  # "Transport" via retraction: Y + proj(update) (approx parallel transport)
  delta <- update_tangent
  sym_term <- symm(crossprod(V_current, delta))
  transported_update <- delta - V_current %*% sym_term
  updated_V <- V_current + params$learning_rate * transported_update
  optimizer$state[[i]] <- state
  params$myit <- myit + 1
  return(list(updated_V = updated_V, optimizer = optimizer))
}

#' @export
step.lars <- function(optimizer, i, V_current, descent_gradient, ...) {
  # Base SGD + layer-wise scaling (adapt to columns)
  descent_gradient = descent_gradient * (-1.0)
  params <- optimizer$params
  learning_rate <- params$learning_rate %||% 0.001
  # Compute column norms for scaling
  v_norms <- sqrt(colSums(V_current^2))
  g_norms <- sqrt(colSums(descent_gradient^2))
  scales <- v_norms / (g_norms + 1e-8)  # Adaptive per-column lr
  scaled_grad <- sweep(descent_gradient, 2, scales, "*")
  updated_V <- V_current - learning_rate * scaled_grad
  return(list(updated_V = updated_V, optimizer = optimizer))
}

#' @export
step.psgd <- function(optimizer, i, V_current, descent_gradient, ...) {
  descent_gradient = descent_gradient * (-1.0)
  state <- optimizer$state[[i]]
  params <- optimizer$params
  learning_rate <- params$learning_rate %||% 0.001
  beta <- params$beta %||% 0.9  # Momentum
  # Precondition: Simple diagonal Hessian approx (from state$diag_h)
  state$diag_h <- state$diag_h %||% matrix(1, nrow(V_current), ncol(V_current))
  state$diag_h <- (1 - 0.01) * state$diag_h + 0.01 * descent_gradient^2  # EMA update
  preconditioned_grad <- descent_gradient / (state$diag_h + 1e-8)
  # Momentum
  state$m <- beta * state$m + (1 - beta) * preconditioned_grad
  updated_V <- V_current - learning_rate * state$m
  optimizer$state[[i]] <- state
  return(list(updated_V = updated_V, optimizer = optimizer))
}

#' @export
step.armijo_gradient <- function(optimizer, i, V_current, descent_gradient, full_energy_function, ...) {
  # Updated: Added momentum (0.9 decay) for faster convergence; capped backtracks at 20, min alpha=1e-10.
  state <- optimizer$state[[i]]
  params <- optimizer$params
  epsilon <- params$epsilon %||% 1e-12
  # Momentum update (new: blend with previous momentum)
  state$momentum <- if (is.null(state$momentum)) descent_gradient else 0.9 * state$momentum + 0.1 * descent_gradient
  # Use momentum-blended direction
  search_direction <- state$momentum
  # Compute Frobenius norm for normalization (unit direction for consistent scaling)
  dir_norm <- sqrt(sum(search_direction^2))
  if (dir_norm < epsilon) {
    # Stationary point: no update
    return(list(updated_V = V_current, optimizer = optimizer))
  }
  search_direction <- search_direction / dir_norm
  # Perform robust line search along this direction
  optimal_step_size <- .robust_backtracking_linesearch(
    V_current = V_current,
    descent_direction = search_direction,
    ascent_gradient = -descent_gradient, # grad(E)
    energy_function = full_energy_function,
    initial_step_size = state$last_step_size # Warm start from history
  )
  # Update last_step_size: expand if successful, reset if failed/tiny
  state$last_step_size <- if (optimal_step_size > 1e-10) optimal_step_size * 1.5 else 1.0
  optimizer$state[[i]] <- state
  # Apply the step
  updated_V <- V_current + optimal_step_size * search_direction
  return(list(updated_V = updated_V, optimizer = optimizer))
}

# ==============================================================================
# UPDATED OPTIMIZER: Lookahead with Projection
# ==============================================================================
#' @export
step.lookahead <- function(optimizer, i, V_current, descent_gradient, ...) {
  # Updated: Added manifold projection after blending (using symm/crossprod); adaptive k = max(5, myit/10).
  state <- optimizer$state[[i]]
  params <- optimizer$params
  myit <- params$myit %||% 1  # Assume passed from nsa_flow
  k <- max(5, myit / 10)  # Adaptive: rarer sync later
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
    # NEW: Project blend back to tangent space (assumes symm/crossprod available)
    delta <- V_fast - V_current
    sym_term <- symm(crossprod(V_current, delta))
    V_fast <- V_current + (delta - V_current %*% sym_term)
  }
  optimizer$state[[i]] <- state
  return(list(updated_V = V_fast, optimizer = optimizer))
}

# ==============================================================================
# UPDATED OPTIMIZER: Nadam (Standard Forgetful + Full Bias Correction)
# ==============================================================================
#' @export
step.nadam <- function(optimizer, i, V_current, descent_gradient, ...) {
  # Updated: Dropped v_max (use forgetful v_hat); full Nesterov bias correction; increment myit.
  state <- optimizer$state[[i]]
  params <- optimizer$params
  beta1 <- params$beta1 %||% 0.9
  beta2 <- params$beta2 %||% 0.999
  epsilon <- params$epsilon %||% 1e-8
  learning_rate <- params$learning_rate %||% 0.001
  myit <- params$myit %||% 1
  # Update moments (standard Adam)
  state$m <- beta1 * state$m + (1 - beta1) * descent_gradient
  state$v <- beta2 * state$v + (1 - beta2) * (descent_gradient^2)
  # Bias-corrected m_hat
  m_hat <- state$m / (1 - beta1^myit)
  # NEW: Full Nesterov lookahead with bias: beta1 * m_hat + (1-beta1) * grad / (1-beta1^t)
  nesterov_m_hat <- beta1 * m_hat + ((1 - beta1) * descent_gradient) / (1 - beta1^myit)
  # NEW: Forgetful v_hat (no v_max)
  v_hat <- state$v / (1 - beta2^myit)
  update_direction <- nesterov_m_hat / (sqrt(v_hat) + epsilon)
  # The update step is ALONG the descent direction
  updated_V <- V_current + learning_rate * update_direction
  optimizer$state[[i]] <- state
  params$myit <- myit + 1  # NEW: Increment for next call
  return(list(updated_V = updated_V, optimizer = optimizer))
}

# --- Internal Helper for Robust Line Search ---

#' @keywords internal
.robust_backtracking_linesearch <- function(V_current, descent_direction, ascent_gradient,
                                            energy_function, initial_step_size = 1.0,
                                            alpha = 1e-4, beta = 0.5, min_step = 1e-12) {
  step_size <- initial_step_size
  initial_energy <- energy_function(V_current)
  
  # Slope term: <grad(E), d> (should be negative)
  slope_term <- sum(ascent_gradient * descent_direction)
  if (slope_term >= 0) {
    warning("Line search given a non-descent direction. Stopping search.")
    return(0)
  }
  
  # Backtrack until Armijo satisfied or step too small (guaranteed for small steps)
  while (step_size > min_step) {
    V_candidate <- V_current + step_size * descent_direction
    new_energy <- tryCatch(energy_function(V_candidate), error = function(e) Inf)
    
    # Armijo condition
    if (new_energy <= initial_energy + alpha * step_size * slope_term) {
      return(step_size)
    }
    step_size <- beta * step_size
  }
  
  warning("Robust line search failed to find a suitable step size; possibly at local minimum or numerical issue.")
  return(0)
}


# ==============================================================================
#           NEW OPTIMIZER: Bidirectional Lookahead
# ==============================================================================
#' @export
step.bidirectional_lookahead <- function(optimizer, i, V_current, descent_gradient, full_energy_function, ...) {
  # This method combines lookahead with a bidirectional line search on the Adam-proposed direction.
  # It computes the Adam descent direction, then performs a bidirectional Armijo backtracking line
  # search (trying both the direction and its negation) to find a robust energy-reducing step.
  # Finally, it applies lookahead synchronization for improved generalization and stability.
  # It is robust to incorrect gradient directions and aims to guarantee energy reduction when possible.
  
  state <- optimizer$state[[i]]
  params <- optimizer$params
  
  # Lookahead parameters
  k <- params$k %||% 5       # Sync period
  alpha <- params$alpha %||% 0.5
  
  # Adam parameters (base optimizer)
  beta1 <- params$beta1 %||% 0.9
  beta2 <- params$beta2 %||% 0.999
  epsilon <- params$epsilon %||% 1e-8
  myit <- params$myit %||% 1
  
  # Initialize lookahead state
  state$V_slow <- state$V_slow %||% V_current
  state$step <- state$step %||% 0
  
  # --- 1. Compute Adam Descent Direction ---
  
  # Update Adam state with the descent gradient
  state$m <- beta1 * state$m + (1 - beta1) * descent_gradient
  state$v <- beta2 * state$v + (1 - beta2) * (descent_gradient^2)
  state$v_max <- pmax(state$v_max, state$v)
  
  # Bias correction for momentum and adaptive learning rate
  m_hat <- state$m / (1 - beta1^myit)
  v_hat <- state$v_max / (1 - beta2^myit)
  
  # The Adam-proposed descent direction (adaptive momentum)
  search_direction <- m_hat / (sqrt(v_hat) + epsilon)
  
  # --- 2. Perform Bidirectional Line Search ---
  
  bidirectional_result <- .bidirectional_linesearch(
    V_current = V_current,
    descent_direction = search_direction,
    ascent_gradient = -descent_gradient,  # grad(E)
    energy_function = full_energy_function,
    initial_step_size = state$last_step_size %||% 1.0
  )
  
  optimal_step_size <- bidirectional_result$step_size
  selected_direction <- bidirectional_result$direction
  
  # Compute the fast updated V using the selected step and direction
  V_fast <- V_current + optimal_step_size * selected_direction
  
  # --- 3. Apply Lookahead Synchronization ---
  
  state$step <- state$step + 1
  if (state$step %% k == 0) {
    state$V_slow <- state$V_slow + alpha * (V_fast - state$V_slow)
    V_fast <- state$V_slow
  }
  
  # Update last_step_size for line search warm-starting
  state$last_step_size <- if (optimal_step_size > 1e-9) optimal_step_size * 1.5 else 1.0
  
  # Save updated state
  optimizer$state[[i]] <- state
  
  return(list(updated_V = V_fast, optimizer = optimizer))
}


# Helper for default values
`%||%` <- function(a, b) if (is.null(a)) b else a

#' List Available SIMLR Optimizers
#'
#' Dynamically returns the names of all optimizer types with registered `step.*` methods.
#' @param torch Logical; if TRUE, returns a predefined list of optimizers compatible with torch.
#' @return A character vector of optimizer names (e.g., "adam", "nadam", etc.)
#' @export
list_simlr_optimizers <- function( torch = FALSE ) {
  methods_vec <- as.character(methods("step"))
  optimizers <- gsub("^step\\.|\\*$", "", methods_vec)
  optimizers=optimizers[ !(optimizers %in% c("lbfgs", "random_search", "random_gradient"))]
  if ( torch  ) {
    return( c('adabound', 'adagrad', 'adam', 'adamax', 'adamp', 'adamw', 'asgd', 'lars', 'lbfgs', 'lookahead', 'nadam', 'padam', 'pid', 'qhadam', 'qhm', 'radam', 'rmsprop', 'rprop', 'sgd', 'sgd_nesterov', 'sgdp', 'yogi' ) )
  }
  return(optimizers)
}
