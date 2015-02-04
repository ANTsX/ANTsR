#' antsAffineInitializer
#'
#' Wrapper for the ANTs funtion antsAffineInitializer
#'
#' @param ... see ants function
#' @return none
#' @author Avants BB
#'
#' @export antsAffineInitializer
antsAffineInitializer <- function(...) {
  .Call("antsAffineInitializer", .int_antsProcessArguments(c(...)), PACKAGE = "ANTsR")
}
