#' @title Create antsImage into a Vector
#' @description Extracts the array of an \code{antsImage} object then
#' converts it into a vector
#' @param x antsImage object
#' @param ... additional objects
#' @param recursive if \code{TRUE}, the function recursively descends 
#' through lists (and pairlists) combining all their elements into a vector.
#' @describeIn as.antsImage
#' @aliases c,antsImage,NULL,ANY,ANY-method
setMethod(
  "c",
  signature = signature( x = "antsImage"),
  definition = function(x, ..., recursive = FALSE)  {
    x = as.array(x)
    c(x, ..., recursive = recursive)
  }
)
