antsMotionCorr <- function(...) {
  .Call("antsMotionCorr", int_antsProcessArguments(c(...)), PACKAGE = "itkImageR")
  gc()  # trigger garbage collection
} 
