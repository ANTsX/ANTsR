ThresholdImage <- function(...) {
  .Call("ThresholdImage", int_antsProcessArguments(c(...)), PACKAGE = "ANTsR")
} 
