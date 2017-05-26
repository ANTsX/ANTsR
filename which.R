#' @rdname which
#' @title Which values are TRUE
#' @description Give the TRUE indices of a logical object, allowing for array indices.
#' 
#' @param x antsImage object
#' @param arr.ind logical; should array indices be returned when x is an array?
#' @param useNames logical indicating if the value of arrayInd() 
#' should have (non-null) dimnames at all.
setMethod("which", "antsImage", function(x, arr.ind, useNames) {
  x = coerce_mask(x)
  base::which(x = x, arr.ind = arr.ind, useNames = useNames)  
})

# which = function(x, arr.ind = FALSE, useNames = TRUE) {
#   UseMethod("which")
# }

# #' @rdname which
# #' @export
# which.default = function(x, arr.ind = FALSE, useNames = TRUE) {
#   base::which(x, arr.ind = FALSE, useNames = TRUE)
# }

# #' @rdname which
# #' @method which antsImage
# #' @export
# which.antsImage = function(x, arr.ind = FALSE, useNames = TRUE) {
#   x = coerce_mask(x)
#   which(x = x, arr.ind = arr.ind, useNames = useNames)
# }
