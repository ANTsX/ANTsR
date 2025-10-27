#' @title NSA-Flow Optimization via PyTorch
#'
#' @description
#' Performs optimization to balance fidelity to a target matrix and orthogonality
#' of the solution matrix using a weighted objective function. The function supports multiple retraction methods and includes robust convergence checks.  These constraints provide global control over column-wise sparseness by projecting the matrix onto the approximate Stiefel manifold.
#'
#' @param Y0 Numeric matrix of size \code{p x k}, the initial guess for the solution.
#' @param X0 Numeric matrix of size \code{p x k}, the target matrix for fidelity.
#'   If \code{NULL}, initialized as \code{pmax(Y0, 0)} with a small perturbation added to \code{Y0}.
#' @param w Numeric scalar in \code{[0,1]}, weighting the trade-off between fidelity
#'   (1 - w) and orthogonality (w). Default is 0.5.
#' @param retraction Character string specifying the retraction method to enforce
#'   orthogonality constraints.
#' @param max_iter Integer, maximum number of iterations. Default is 100.
#' @param tol Numeric, tolerance for convergence based on relative gradient norm
#'   and energy stability. Default is 1e-6.
#' @param verbose Logical, if \code{TRUE}, prints iteration details. Default is \code{FALSE}.
#' @param seed Integer, random seed for reproducibility. If \code{NULL}, no seed is set.
#'   Default is 42.
#' @param apply_nonneg Logical, if \code{TRUE}, enforces non-negativity on the solution
#'   after retraction. Default is \code{TRUE}.
#' @param optimizer Character string, optimization algorithm to use. The "fast" option 
#'   will select the best option based on whether simplified = TRUE or FALSE. 
#'   otherwise, pass the names of optimizers supported by \code{create_optimizer()} 
#'   as seen in \code{list_simlr_optimizers()}. Default is "fast".
#' @param initial_learning_rate Numeric, initial learning rate for the optimizer.
#'   Default is 1e-3 for non-neg and 1 for unconstrained.  Otherwise, you can use \code{estimate_learning_rate_nsa()} to find a robust value.
#'.  pass a string one of c("brent", "grid", "armijo", "golden", "adaptive") to engage this method.  
#' @param record_every Integer, frequency of recording iteration metrics.
#'   Default is 1 (record every iteration).
#' @param window_size Integer, size of the window for energy stability convergence check.
#'   Default is 5.
#' @param c1_armijo Numeric, Armijo condition constant for line search.
#' @param simplified Logical, if \code{TRUE}, uses the simplified objective
#'   \deqn{\min_U (1 - w) \frac{1}{2} ||U - Z||_F^2 + w \frac{1}{2} ||U^\top U - I_k||_F^2}.
#'   If \code{FALSE}, uses the invariant defect objective. Default is \code{FALSE}.
#' @param project_full_gradient Logical, if \code{TRUE}, projects the full gradient instead 
#' of just the orthogonal component. Default is \code{FALSE}.
#' @param plot Logical, if \code{TRUE}, generates a ggplot of fidelity and orthogonality
#'   traces with dual axes. Default is \code{FALSE}.
#' @param precision Character string, either 'float32' or 'float64' to set the precision
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{Y}: Numeric matrix, the best solution found (lowest total energy).
#'     \item \code{traces}: Data frame with columns \code{iter}, \code{time},
#'           \code{fidelity}, \code{orthogonality}, and \code{total_energy}
#'           for recorded iterations.
#'     \item \code{final_iter}: Integer, number of iterations performed.
#'     \item \code{plot}: ggplot object of the optimization trace
#'           (if \code{plot = TRUE}), otherwise \code{NULL}.
#'     \item \code{best_total_energy}: Numeric, the lowest total energy achieved.
#'   }
#'
#' @details
#' The function minimizes a weighted objective combining fidelity to \code{X0} and
#' orthogonality of \code{Y}, defined as:
#' \deqn{E(Y) = (1 - w) * ||Y - X0||_F^2 / (2 * p * k) + w * defect(Y)}
#' where \code{defect(Y)} measures orthogonality deviation.
#'
#' The optimization uses a Riemannian gradient descent approach with optional
#' retraction to enforce orthogonality constraints. Convergence is checked via
#' relative gradient norm and energy stability over a window of iterations.
#'
#' @examples
#' set.seed(123)
#' Y0 <- matrix(runif(20), 5, 4)
#' X0 <- matrix(runif(20), 5, 4)
#' # The original function relies on helper functions not shown here, such as:
#' # create_optimizer, step, inv_sqrt_sym, symm, and invariant_orthogonality_defect.
#' # The following example is conceptual:
#' # result <- nsa_flow_torch(Y0, X0, w = 0.0, max_iter = 10, verbose = TRUE, plot = TRUE)
#' # print(result$plot)
#' # print(result$traces)
#'
#' @import ggplot2
#' @import reshape2
#' @export
nsa_flow_torch <- function(
  Y0, X0 = NULL, w = 0.5,
  retraction = c(  "soft_polar", "polar",   "none" ),
  max_iter = 500, tol = 1e-5, verbose = FALSE, seed = 42,
  apply_nonneg = TRUE, optimizer = "fast",
  initial_learning_rate = 'default',
  record_every = 1, window_size = 5, c1_armijo=1e-6,
  simplified = FALSE,
  project_full_gradient = FALSE,
  plot = FALSE,
  precision = 'float64'
) {
  if (!is.matrix(Y0)) {
    stop("Y0 must be a numeric matrix.")
  }
  p <- nrow(Y0)
  k <- ncol(Y0)
  if ( optimizer == "fast" ) {
    optimizer <- "lars"
  }


  if (is.null(X0)) {
    if ( apply_nonneg ) X0 <- pmax(Y0, 0) else X0 = Y0
    perturb_scale <- sqrt(sum(Y0^2)) / sqrt(length(Y0)) * 0.05
    Y0 <- Y0 + matrix(rnorm(nrow(Y0) * ncol(Y0), sd = perturb_scale), nrow(Y0), ncol(Y0))
    if (verbose) cat("Added perturbation to Y0\n")
  } else {
    if ( apply_nonneg ) X0 <- pmax(X0, 0)
    if (nrow(X0) != nrow(Y0) || ncol(X0) != ncol(Y0)) stop("X0 must have same dimensions as Y0")
  }

  retraction_type <- match.arg(retraction)

  # Fast ortho terms (used in gradients; optional c_orth scaling)
  compute_ortho_terms <- function(Y, c_orth = 1, simplified = FALSE ) {
    norm2 <- sum(Y^2)
    if (norm2 <= 1e-12 || c_orth <= 0) {
      return(list(grad_orth = matrix(0, nrow(Y), ncol(Y)), defect = 0, norm2 = norm2))
    }
    S <- crossprod(Y)  # Once!
    diagS <- diag(S)
    off_f2 <- sum(S * S) - sum(diagS^2)
    defect <- off_f2 / norm2^2
    Y_S <- Y %*% S
    Y_diag_scale <- sweep(Y, 2, diagS, "*")  # Columns of Y scaled by diagS
    term1 <- (Y_S - Y_diag_scale) / norm2^2
    term2 <- (defect / norm2) * Y
    if ( simplified ) {
      grad_orth <- - c_orth * 2 * Y %*% (S - diag(ncol(Y)))
      } else {
      grad_orth <- c_orth * (term1 - term2)
      }
    list(grad_orth = grad_orth, defect = defect, norm2 = norm2)
  }

  Y <- Y0
  # --- Compute initial scales ---
  g0 <- 0.5 * sum((Y0 - X0)^2) / (p * k)
  if (g0 < 1e-8) g0 <- 1e-8
  d0 <- invariant_orthogonality_defect(Y0)  # Fast!
  if (d0 < 1e-8) d0 <- 1e-8
  # --- Weighting terms ---
  fid_eta <- (1 - w) / (g0 * p * k)
  c_orth <- 4 * w / d0
  fid_eta_pt5 <- (1 - 0.5) / (g0 * p * k)
  c_orth_pt5 <- 4 * 0.5 / d0
  trace <- list()
  recent_energies <- numeric(0)
  t0 <- Sys.time()
  # --- Track best solution ---
  best_Y <- Y
  best_total_energy <- Inf
  # --- Compute initial gradient for relative norm tolerance ---
  grad_fid_init <- fid_eta * (Y - X0) * (-1.0)
  ortho_init <- compute_ortho_terms(Y, c_orth, simplified=simplified)
  grad_orth_init <- ortho_init$grad_orth
  if (c_orth > 0) {
    sym_term_orth_init <- symm(crossprod(Y, grad_orth_init))  # t(Y) %*% = crossprod(Y, .)
    rgrad_orth_init <- grad_orth_init - Y %*% sym_term_orth_init
  } else {
    rgrad_orth_init <- grad_orth_init
  }
  rgrad_init <- grad_fid_init + rgrad_orth_init
  init_grad_norm <- sqrt(sum(rgrad_init^2)) + 1e-8
  nsa_energy_pt5 <- function(Vp) {
    # --- Retraction ---
    Vp <- nsa_flow_retract_auto(Vp, 0.5, retraction)
    # --- Optional non-negativity ---
    Vp <- if (apply_nonneg) pmax(Vp, 0) else Vp
    e <- 0.5 * fid_eta_pt5 * sum((Vp - X0)^2)
    if (c_orth_pt5 > 0) {
      norm2_V <- sum(Vp^2)
      if (norm2_V > 0) {
        defect <- invariant_orthogonality_defect(Vp)  # Fast!
        e <- e + 0.25 * c_orth_pt5 * defect
        }
      }
    e
  }

  # --- Optimizer initialization ---
    if (is.null(initial_learning_rate) || is.na(initial_learning_rate)) {
        initial_learning_rate <- "brent"
    }
    if (is.character(initial_learning_rate)) {
        if (verbose) 
            cat("Estimating robust initial learning rate using optim()...\n")
        lr_res <- estimate_learning_rate_nsa(Y0, X0, w = w, 
            retraction = retraction, nsa_energy = nsa_energy_pt5, 
            apply_nonneg = apply_nonneg, method = initial_learning_rate, 
            verbose = verbose, plot = FALSE)$estimated_learning_rate
          
    } else {
        lr_res <- initial_learning_rate
    }

  print(lr_res)


  torch <- reticulate::import("torch", convert = FALSE)
  pynsa <- reticulate::import("nsa_flow", convert=FALSE )


  if (is.null(pynsa) ) {
    stop("Could not find Python package `nsa_flow` -- please install it first.")
  }

  Y_torch <- torch$tensor(Y0, dtype = torch$float64)
  Xc_torch <- torch$tensor(X0, dtype = torch$float64)

  res <- pynsa$nsa_flow(
    Y_torch, 
    Xc_torch,
    w = w,
    retraction = retraction,
    max_iter = as.integer(max_iter),
    tol = tol,
    verbose = verbose,
    seed = as.integer(seed),
    apply_nonneg = apply_nonneg,
    optimizer = optimizer,
    initial_learning_rate = lr_res,
    record_every = as.integer(record_every),
    window_size = as.integer(window_size),
    simplified = simplified,
    project_full_gradient = project_full_gradient,
    precision=precision
    #
#    armijo_beta = armijo_beta,
#    armijo_c = armijo_c,
  )


  trace_df = as.data.frame(reticulate::py_to_r(res$traces))

  if (plot && !is.null(trace_df) && nrow(trace_df) > 0) {
    max_fid <- max(trace_df$fidelity, na.rm = TRUE)
    max_orth <- max(trace_df$orthogonality, na.rm = TRUE)
    ratio <- if (max_orth > 0) max_fid / max_orth else 1
    energy_plot <- ggplot2::ggplot(trace_df, ggplot2::aes(x = iter)) +
      ggplot2::geom_line(ggplot2::aes(y = fidelity, color = "Fidelity"), size = 1.2) +
      ggplot2::geom_point(ggplot2::aes(y = fidelity, color = "Fidelity"), size = 1.5, alpha = 0.7) +
      ggplot2::geom_line(ggplot2::aes(y = orthogonality * ratio, color = "Orthogonality"), size = 1.2) +
      ggplot2::geom_point(ggplot2::aes(y = orthogonality * ratio, color = "Orthogonality"), size = 1.5, alpha = 0.7) +
      ggplot2::scale_y_continuous(name = "Fidelity Energy",
                                  sec.axis = ggplot2::sec_axis(~ . / ratio, name = "Orthogonality Defect")) +
      ggplot2::scale_color_manual(values = c("Fidelity" = "#1f78b4", "Orthogonality" = "#33a02c")) +
      ggplot2::labs(title = paste("NSA-Flow Optimization Trace: ", retraction),
                    subtitle = "Fidelity and Orthogonality Terms (Dual Scales)",
                    x = "Iteration", color = "Term") +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
                     plot.subtitle = ggplot2::element_text(hjust = 0.5),
                     legend.position = "top",
                     panel.grid.major = ggplot2::element_line(color = "gray80"),
                     panel.grid.minor = ggplot2::element_line(color = "gray90"),
                     axis.title.y.left = ggplot2::element_text(color = "#1f78b4"),
                     axis.text.y.left = ggplot2::element_text(color = "#1f78b4"),
                     axis.title.y.right = ggplot2::element_text(color = "#33a02c"),
                     axis.text.y.right = ggplot2::element_text(color = "#33a02c"))
  }
  Y = as.matrix(res$Y$detach()$numpy())
  rownames(Y) <- rownames(Y0)
  colnames(Y) <- colnames(Y0)
  list(
    Y=Y,
    energy = reticulate::py_to_r(res$best_total_energy),
    traces = trace_df,
    iter = reticulate::py_to_r(res$final_iter),
    plot = if (plot) energy_plot else NULL
  )
}




#' @title NSA-Flow Optimization via PyTorch AutoGrad
#'
#' @description
#' Performs optimization to balance fidelity to a target matrix and orthogonality
#' of the solution matrix using a weighted objective function. The function supports multiple retraction methods and includes robust convergence checks.  These constraints provide global control over column-wise sparseness by projecting the matrix onto the approximate Stiefel manifold.
#'
#' @param Y0 Numeric matrix of size \code{p x k}, the initial guess for the solution.
#' @param X0 Numeric matrix of size \code{p x k}, the target matrix for fidelity.
#'   If \code{NULL}, initialized as \code{pmax(Y0, 0)} with a small perturbation added to \code{Y0}.
#' @param w Numeric scalar in \code{[0,1]}, weighting the trade-off between fidelity
#'   (1 - w) and orthogonality (w). Default is 0.5.
#' @param retraction Character string specifying the retraction method to enforce
#'   orthogonality constraints.
#' @param max_iter Integer, maximum number of iterations. Default is 100.
#' @param tol Numeric, tolerance for convergence based on relative gradient norm
#'   and energy stability. Default is 1e-6.
#' @param verbose Logical, if \code{TRUE}, prints iteration details. Default is \code{FALSE}.
#' @param seed Integer, random seed for reproducibility. If \code{NULL}, no seed is set.
#'   Default is 42.
#' @param apply_nonneg Logical, if \code{TRUE}, enforces non-negativity on the solution
#'   after retraction. Default is \code{TRUE}.
#' @param optimizer Character string, optimization algorithm to use. The "fast" option 
#'   will select the best option based on whether simplified = TRUE or FALSE. 
#'   otherwise, pass the names of optimizers supported by \code{create_optimizer()} 
#'   as seen in \code{list_simlr_optimizers()}. Default is "fast".
#' @param initial_learning_rate Numeric, initial learning rate for the optimizer.
#'   Default is 1e-3 for non-neg and 1 for unconstrained.  Otherwise, you can use \code{estimate_learning_rate_nsa()} to find a robust value.
#'.  pass a string one of c("brent", "grid", "armijo", "golden", "adaptive") to engage this method.  
#' @param record_every Integer, frequency of recording iteration metrics.
#'   Default is 1 (record every iteration).
#' @param window_size Integer, size of the window for energy stability convergence check.
#'   Default is 5.
#' @param c1_armijo Numeric, Armijo condition constant for line search.
#' @param simplified Logical, if \code{TRUE}, uses the simplified objective
#'   \deqn{\min_U (1 - w) \frac{1}{2} ||U - Z||_F^2 + w \frac{1}{2} ||U^\top U - I_k||_F^2}.
#'   If \code{FALSE}, uses the invariant defect objective. Default is \code{FALSE}.
#' @param project_full_gradient Logical, if \code{TRUE}, projects the full gradient instead 
#' of just the orthogonal component. Default is \code{FALSE}.
#' @param plot Logical, if \code{TRUE}, generates a ggplot of fidelity and orthogonality
#'   traces with dual axes. Default is \code{FALSE}.
#' @param precision Character string, either 'float32' or 'float64' to set the precision
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{Y}: Numeric matrix, the best solution found (lowest total energy).
#'     \item \code{traces}: Data frame with columns \code{iter}, \code{time},
#'           \code{fidelity}, \code{orthogonality}, and \code{total_energy}
#'           for recorded iterations.
#'     \item \code{final_iter}: Integer, number of iterations performed.
#'     \item \code{plot}: ggplot object of the optimization trace
#'           (if \code{plot = TRUE}), otherwise \code{NULL}.
#'     \item \code{best_total_energy}: Numeric, the lowest total energy achieved.
#'   }
#'
#' @details
#' The function minimizes a weighted objective combining fidelity to \code{X0} and
#' orthogonality of \code{Y}, defined as:
#' \deqn{E(Y) = (1 - w) * ||Y - X0||_F^2 / (2 * p * k) + w * defect(Y)}
#' where \code{defect(Y)} measures orthogonality deviation.
#'
#' The optimization uses a Riemannian gradient descent approach with optional
#' retraction to enforce orthogonality constraints. Convergence is checked via
#' relative gradient norm and energy stability over a window of iterations.
#'
#' @examples
#' set.seed(123)
#' Y0 <- matrix(runif(20), 5, 4)
#' X0 <- matrix(runif(20), 5, 4)
#' # The original function relies on helper functions not shown here, such as:
#' # create_optimizer, step, inv_sqrt_sym, symm, and invariant_orthogonality_defect.
#' # The following example is conceptual:
#' # result <- nsa_flow_torch(Y0, X0, w = 0.0, max_iter = 10, verbose = TRUE, plot = TRUE)
#' # print(result$plot)
#' # print(result$traces)
#'
#' @import ggplot2
#' @import reshape2
#' @export
nsa_flow_torch_ag <- function(
  Y0, X0 = NULL, w = 0.5,
  retraction = c(  "soft_polar", "polar",   "none" ),
  max_iter = 500, tol = 1e-5, verbose = FALSE, seed = 42,
  apply_nonneg = TRUE, optimizer = "fast",
  initial_learning_rate = 'default',
  record_every = 1, window_size = 5, c1_armijo=1e-6,
  simplified = FALSE,
  project_full_gradient = FALSE,
  plot = FALSE,
  precision = 'float64'
) {
  if (!is.matrix(Y0)) {
    stop("Y0 must be a numeric matrix.")
  }
  p <- nrow(Y0)
  k <- ncol(Y0)
  if ( optimizer == "fast" ) {
    optimizer <- "lars"
  }

  if (is.null(X0)) {
    if ( apply_nonneg ) X0 <- pmax(Y0, 0) else X0 = Y0
    perturb_scale <- sqrt(sum(Y0^2)) / sqrt(length(Y0)) * 0.05
    Y0 <- Y0 + matrix(rnorm(nrow(Y0) * ncol(Y0), sd = perturb_scale), nrow(Y0), ncol(Y0))
    if (verbose) cat("Added perturbation to Y0\n")
  } else {
    if ( apply_nonneg ) X0 <- pmax(X0, 0)
    if (nrow(X0) != nrow(Y0) || ncol(X0) != ncol(Y0)) stop("X0 must have same dimensions as Y0")
  }

  retraction_type <- match.arg(retraction)

  # Fast ortho terms (used in gradients; optional c_orth scaling)
  compute_ortho_terms <- function(Y, c_orth = 1, simplified = FALSE ) {
    norm2 <- sum(Y^2)
    if (norm2 <= 1e-12 || c_orth <= 0) {
      return(list(grad_orth = matrix(0, nrow(Y), ncol(Y)), defect = 0, norm2 = norm2))
    }
    S <- crossprod(Y)  # Once!
    diagS <- diag(S)
    off_f2 <- sum(S * S) - sum(diagS^2)
    defect <- off_f2 / norm2^2
    Y_S <- Y %*% S
    Y_diag_scale <- sweep(Y, 2, diagS, "*")  # Columns of Y scaled by diagS
    term1 <- (Y_S - Y_diag_scale) / norm2^2
    term2 <- (defect / norm2) * Y
    if ( simplified ) {
      grad_orth <- - c_orth * 2 * Y %*% (S - diag(ncol(Y)))
      } else {
      grad_orth <- c_orth * (term1 - term2)
      }
    list(grad_orth = grad_orth, defect = defect, norm2 = norm2)
  }

  Y <- Y0
  # --- Compute initial scales ---
  g0 <- 0.5 * sum((Y0 - X0)^2) / (p * k)
  if (g0 < 1e-8) g0 <- 1e-8
  d0 <- invariant_orthogonality_defect(Y0)  # Fast!
  if (d0 < 1e-8) d0 <- 1e-8
  # --- Weighting terms ---
  fid_eta <- (1 - w) / (g0 * p * k)
  c_orth <- 4 * w / d0
  fid_eta_pt5 <- (1 - 0.5) / (g0 * p * k)
  c_orth_pt5 <- 4 * 0.5 / d0
  trace <- list()
  recent_energies <- numeric(0)
  t0 <- Sys.time()
  # --- Track best solution ---
  best_Y <- Y
  best_total_energy <- Inf
  # --- Compute initial gradient for relative norm tolerance ---
  grad_fid_init <- fid_eta * (Y - X0) * (-1.0)
  ortho_init <- compute_ortho_terms(Y, c_orth, simplified=simplified)
  grad_orth_init <- ortho_init$grad_orth
  if (c_orth > 0) {
    sym_term_orth_init <- symm(crossprod(Y, grad_orth_init))  # t(Y) %*% = crossprod(Y, .)
    rgrad_orth_init <- grad_orth_init - Y %*% sym_term_orth_init
  } else {
    rgrad_orth_init <- grad_orth_init
  }
  rgrad_init <- grad_fid_init + rgrad_orth_init
  init_grad_norm <- sqrt(sum(rgrad_init^2)) + 1e-8
  nsa_energy_pt5 <- function(Vp) {
    # --- Retraction ---
    Vp <- nsa_flow_retract_auto(Vp, 0.5, retraction)
    # --- Optional non-negativity ---
    Vp <- if (apply_nonneg) pmax(Vp, 0) else Vp
    e <- 0.5 * fid_eta_pt5 * sum((Vp - X0)^2)
    if (c_orth_pt5 > 0) {
      norm2_V <- sum(Vp^2)
      if (norm2_V > 0) {
        defect <- invariant_orthogonality_defect(Vp)  # Fast!
        e <- e + 0.25 * c_orth_pt5 * defect
        }
      }
    e
  }

  # --- Optimizer initialization ---
    if (is.null(initial_learning_rate) || is.na(initial_learning_rate)) {
        initial_learning_rate <- "brent"
    }
    if (is.character(initial_learning_rate)) {
        if (verbose) 
            cat("Estimating robust initial learning rate using optim()...\n")
        lr_res <- estimate_learning_rate_nsa(Y0, X0, w = w, 
            retraction = retraction, nsa_energy = nsa_energy_pt5, 
            apply_nonneg = apply_nonneg, method = initial_learning_rate, 
            verbose = verbose, plot = FALSE)$estimated_learning_rate
          
    } else {
        lr_res <- initial_learning_rate
    }

  print(lr_res)


  torch <- reticulate::import("torch", convert = FALSE)
  pynsa <- reticulate::import("nsa_flow", convert=FALSE )


  if (is.null(pynsa) ) {
    stop("Could not find Python package `nsa_flow` -- please install it first.")
  }

  Y_torch <- torch$tensor(Y0, dtype = torch$float64)
  Xc_torch <- torch$tensor(X0, dtype = torch$float64)

  res <- pynsa$nsa_flow_autograd(
    Y_torch, 
    Xc_torch,
    w = w,
    retraction = retraction,
    max_iter = as.integer(max_iter),
    tol = tol,
    verbose = verbose,
    seed = as.integer(seed),
    apply_nonneg = apply_nonneg,
    optimizer = optimizer,
    initial_learning_rate = lr_res,
    record_every = as.integer(record_every),
    window_size = as.integer(window_size),
    precision=precision
  )


  trace_df = as.data.frame(reticulate::py_to_r(res$traces))

  if (plot && !is.null(trace_df) && nrow(trace_df) > 0) {
    max_fid <- max(trace_df$fidelity, na.rm = TRUE)
    max_orth <- max(trace_df$orthogonality, na.rm = TRUE)
    ratio <- if (max_orth > 0) max_fid / max_orth else 1
    energy_plot <- ggplot2::ggplot(trace_df, ggplot2::aes(x = iter)) +
      ggplot2::geom_line(ggplot2::aes(y = fidelity, color = "Fidelity"), size = 1.2) +
      ggplot2::geom_point(ggplot2::aes(y = fidelity, color = "Fidelity"), size = 1.5, alpha = 0.7) +
      ggplot2::geom_line(ggplot2::aes(y = orthogonality * ratio, color = "Orthogonality"), size = 1.2) +
      ggplot2::geom_point(ggplot2::aes(y = orthogonality * ratio, color = "Orthogonality"), size = 1.5, alpha = 0.7) +
      ggplot2::scale_y_continuous(name = "Fidelity Energy",
                                  sec.axis = ggplot2::sec_axis(~ . / ratio, name = "Orthogonality Defect")) +
      ggplot2::scale_color_manual(values = c("Fidelity" = "#1f78b4", "Orthogonality" = "#33a02c")) +
      ggplot2::labs(title = paste("NSA-Flow Optimization Trace: ", retraction),
                    subtitle = "Fidelity and Orthogonality Terms (Dual Scales)",
                    x = "Iteration", color = "Term") +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
                     plot.subtitle = ggplot2::element_text(hjust = 0.5),
                     legend.position = "top",
                     panel.grid.major = ggplot2::element_line(color = "gray80"),
                     panel.grid.minor = ggplot2::element_line(color = "gray90"),
                     axis.title.y.left = ggplot2::element_text(color = "#1f78b4"),
                     axis.text.y.left = ggplot2::element_text(color = "#1f78b4"),
                     axis.title.y.right = ggplot2::element_text(color = "#33a02c"),
                     axis.text.y.right = ggplot2::element_text(color = "#33a02c"))
  }
  Y = as.matrix(res$Y$detach()$numpy())
  rownames(Y) <- rownames(Y0)
  colnames(Y) <- colnames(Y0)
  list(
    Y=Y,
    energy = reticulate::py_to_r(res$best_total_energy),
    traces = trace_df,
    iter = reticulate::py_to_r(res$final_iter),
    plot = if (plot) energy_plot else NULL
  )
}
