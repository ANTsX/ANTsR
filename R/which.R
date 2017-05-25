#' @rdname which
#' @title Which values are TRUE
#' @description Give the TRUE indices of a logical object, allowing for array indices.
#' #' 
#' @param x antsImage object
#' @param y logical; should array indices be returned when x is an array?
#' @param useNames logical indicating if the value of arrayInd() 
#' should have (non-null) dimnames at all.
#' 
#' @export
setGeneric("which", function(x, arr.ind = FALSE, useNames = TRUE) { 
  standardGeneric("which") 
}
)

#' @rdname which
#' @aliases which,antsImage-method
setMethod("which", signature(x = "antsImage"),
          function(x, arr.ind = FALSE, useNames = TRUE) {
            x = coerce_mask(x)
            which(x = x, arr.ind = arr.ind, useNames = useNames)
          })
