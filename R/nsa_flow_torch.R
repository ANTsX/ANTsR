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
#' @param plot Logical, if \code{TRUE}, generates a ggplot of fidelity and orthogonality
#'   traces with dual axes. Default is \code{FALSE}.
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
  plot = FALSE
) {
  if (!is.matrix(Y0)) {
    stop("Y0 must be a numeric matrix.")
  }

  retraction_type <- match.arg(retraction)

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
    initial_learning_rate = initial_learning_rate,
    record_every = as.integer(record_every),
    window_size = as.integer(window_size),
    simplified = simplified,
    #
#    armijo_beta = armijo_beta,
#    armijo_c = armijo_c,
  )

  list(
    Y = as.matrix(res$Y$detach()$numpy()),
    energy = reticulate::py_to_r(res$best_total_energy),
    iter = reticulate::py_to_r(res$final_iter)
  )
}
