PrintHeader <- function(...) {
  .Call("PrintHeader", as.character(c(...)))
} 
