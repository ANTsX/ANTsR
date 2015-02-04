#' LabelClustersUniquely
#'
#' Wrapper for the ANTs funtion LabelClustersUniquely
#'
#' @param ... see ants function
#' @return none
#' @author Avants BB
#'
#' @export LabelClustersUniquely
LabelClustersUniquely <- function(...) {
pp<-.Call("LabelClustersUniquely", .int_antsProcessArguments(c(...)), PACKAGE = "ANTsR")
}
