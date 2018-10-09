.getValueAtPoint <- function(x, point) {
  if (class(x)[1] != "antsImage") {
    stop("Input must be of class 'antsImage'")
  }
  if ((class(point) != "numeric")) {
    stop("point must be of class 'numeric'")
  }
  
  idx <- as.numeric(antsTransformPhysicalPointToIndex(x, point))
  idx <- floor(idx)
  
  dims <- length(idx)
  
  value <- NA
  if (dims == 2) {
    value <- getPixels(x, i = idx[1], j = idx[2])
  } else if (dims == 3) {
    value <- getPixels(x, i = idx[1], j = idx[2], k = idx[3])
  } else if (dims == 4) {
    value <- getPixels(x, i = idx[1], j = idx[2], k = idx[3], l = idx[4])
  }
  
  return(value[[1]])
  
}
