#' Run NSA-Flow via PyTorch backend
#'
#' @param Y A numeric matrix
#' @param Xc A numeric matrix
#' @param w_pca,lambda,lr,max_iter,retraction_type,armijo_beta,armijo_c,tol,verbose Control parameters
#'
#' @return A list with elements `Y`, `energy`, and `iter`
#' @export
nsa_flow_torch <- function(
  Y, Xc,
  w_pca = 1.0, lambda = 0.01,
  lr = 1e-2, max_iter = 100L,
  retraction_type = c("soft_polar", "polar", "none"),
  armijo_beta = 0.5, armijo_c = 1e-4,
  tol = 1e-6, verbose = FALSE
) {
  retraction_type <- match.arg(retraction_type)

  torch <- reticulate::import("torch", convert = FALSE)
  pynsa <- reticulate::import("nsa_flow", convert=FALSE )


  if (is.null(pynsa) ) {
    stop("Could not find Python function `nsa_flow_py` in the 'nsa_flow_py' module.")
  }

  Y_torch <- torch$tensor(Y, dtype = torch$float64)
  Xc_torch <- torch$tensor(Xc, dtype = torch$float64)

  res <- pynsa$nsa_flow_py(
    Y_torch, Xc_torch,
    w_pca = w_pca,
    lambda_ = lambda,
    lr = lr,
    max_iter = as.integer(max_iter),
    retraction_type = retraction_type,
    armijo_beta = armijo_beta,
    armijo_c = armijo_c,
    tol = tol,
    verbose = verbose
  )

  list(
    Y = as.matrix(res$Y$numpy()),
    energy = reticulate::py_to_r(res$energy),
    iter = reticulate::py_to_r(res$iter)
  )
}
