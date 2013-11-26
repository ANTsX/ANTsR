antsMotionCorr <- function(...) {
  .Call("antsMotionCorr", int_antsProcessArguments(c(...)), PACKAGE = "ANTsR")
  gc()  # trigger garbage collection
} 
