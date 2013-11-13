ConvertVectorFieldToVTK <- function(...) {
  .Call("ConvertVectorFieldToVTK", as.character(c(...)))
} 
