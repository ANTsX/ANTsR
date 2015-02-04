antsAffineInitializer <- function(...) {
  .Call("antsAffineInitializer", .int_antsProcessArguments(c(...)), PACKAGE = "ANTsR")
} 
