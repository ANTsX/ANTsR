CheckTopology <- function(...) {
  .Call("CheckTopology", as.character(c(...)))
} 
