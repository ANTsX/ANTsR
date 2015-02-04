# this file defines the class 'antsMatrix' and its associated methods

# C++ type used to represent an element of the matrix pointer to the actual image
# of C++ type 'itk::image< pixeltype , dimension >::Pointer'

#' An S4 class to hold an antsMatrix imported from ITK types
#'  C++ type used to represent an element of the matrix pointer
#'  to the actual image
#'  C++ type 'itk::image< pixeltype , dimension >::Pointer'
#'
#' @slot elementtype
setClass(Class = "antsMatrix", representation(elementtype = "character", pointer = "externalptr"))

#' @describeIn antsMatrix
setMethod(f = "initialize", signature(.Object = "antsMatrix"), definition = function(.Object,
  elementtype) {
  .Call("antsMatrix", elementtype, PACKAGE = "ANTsR")
})

#' @describeIn antsMatrix
setMethod(f = "as.data.frame", signature(x = "antsMatrix"), definition = function(x) {
  lst <- .Call("antsMatrix_asList", x, PACKAGE = "ANTsR")
  names(lst)[1:(length(lst) - 1)] <- lst[length(lst)]
  lst[[length(lst)]] <- NULL
  return(as.data.frame(lst))
})

#' @describeIn antsMatrix
setMethod(f = "as.list", signature(x = "antsMatrix"), definition = function(x) {
  lst <- .Call("antsMatrix_asList", x, PACKAGE = "ANTsR")
  names(lst)[1:(length(lst) - 1)] <- lst[length(lst)]
  lst[[length(lst)]] <- NULL
  return(lst)
})


#' as.antsMatrix
#'
#' convert types to an antsMatrix
#'
#' @param object An object
#' @param data Numeric vector or data.frame
#' @param Fun Function. Default function is \code{sum}
#' @param ... Extra named arguments passed to FUN
#' @rdname as.antsMatrix
#' @export
setGeneric(name = "as.antsMatrix", def = function(object, ...) standardGeneric("as.antsMatrix"))

#' @describeIn as.antsMatrix
setMethod(f = "as.antsMatrix", signature(object = "list"), definition = function(object,
  elementtype) {
  return(.Call("antsMatrix_asantsMatrix", object, elementtype, PACKAGE = "ANTsR"))
})

#' @describeIn as.antsMatrix
setMethod(f = "as.antsMatrix", signature(object = "data.frame"), definition = function(object,
  elementtype) {
  return(.Call("antsMatrix_asantsMatrix", as.list(object), elementtype, PACKAGE = "ANTsR"))
})

#' @describeIn as.antsMatrix
setMethod(f = "as.list", signature(x = "antsMatrix"), definition = function(x) {
  lst <- .Call("antsMatrix_asList", x, PACKAGE = "ANTsR")
  names(lst)[1:(length(lst) - 1)] <- lst[length(lst)]
  lst[[length(lst)]] <- NULL
  return(lst)
})


#' convert antsMatrix to data frame
#'
#' @param x input matrix
#' @param row.names standard row.names
#' @param optional other options
#' @return data frame is output
#' @author Avants BB
#'
#' @export
as.data.frame.antsMatrix <- function(x, row.names = NULL, optional = FALSE, ...) {
  lst <- .Call("antsMatrix_asList", x, PACKAGE = "ANTsR")
  names(lst)[1:(length(lst))] <- lst[length(lst)]
  lst[[length(lst)]] <- NULL
  as.data.frame(lst)
}

setAs("antsMatrix", "data.frame", function(from) {
  as.data.frame.antsMatrix(from)
})
