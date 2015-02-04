LabelClustersUniquely <- function(...) {
  .Call("LabelClustersUniquely", .int_antsProcessArguments(c(...)), PACKAGE = "ANTsR")
} 
