SurfaceCurvature <- function(...) {
  .Call("SurfaceCurvature", int_antsProcessArguments(c(...)), PACKAGE = "libRSurfaceCurvature")
} 
