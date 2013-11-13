N4BiasFieldCorrection <- function(...) {
  .Call("N4BiasFieldCorrection", int_antsProcessArguments(c(...)), PACKAGE = "libRN4BiasFieldCorrection")
} 
