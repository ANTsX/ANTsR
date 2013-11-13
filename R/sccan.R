sccan <- function(...) {
  .Call("sccan", int_antsProcessArguments(c(...)), PACKAGE = "libRsccan")
} 
