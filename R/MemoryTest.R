MemoryTest <- function(...) {
  .Call("MemoryTest", as.character(c(...)))
} 
