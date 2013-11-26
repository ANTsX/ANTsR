ImageMath <- function(...) {
  .Call("ImageMath", int_antsProcessArguments(c(...)), PACKAGE = "itkImageR")
} 
