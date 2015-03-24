.antsMotionCorrStats <- function(...) {
  .Call("antsMotionCorrStats",
    .int_antsProcessArguments(c(...)), PACKAGE = "ANTsR")
}
