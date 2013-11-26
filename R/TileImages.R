TileImages <- function(...) {
  .Call("TileImages", as.character(c(...)), PACKAGE = "ANTsR")
} 
