SmoothImage <- function(...) {
  numargs <- nargs()
  if (numargs < 4) {
    print("Usage: SmoothImage Dimensionality InImage SmoothingFactor OutImage ")
    return(0)
  }
  args <- list(...)
  newargs <- list(args[[1]], args[[4]], "G", args[[2]], args[[3]])
  .Call("ImageMath", int_antsProcessArguments(c(newargs)), PACKAGE = "libRImageMath")
} 
