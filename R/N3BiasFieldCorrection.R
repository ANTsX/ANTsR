N3BiasFieldCorrection <- function(...) {
  .Call("N3BiasFieldCorrection", int_antsProcessArguments(c(...)), PACKAGE = "itkImageR")
} 
