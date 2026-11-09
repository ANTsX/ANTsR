# fusedRidgeTorch() and nsa_flow_torch() use Python torch via reticulate.
skip_if_no_torch <- function() {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("torch"), "Python torch is not available")
}
