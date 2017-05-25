# this file defines the class 'antsMatrix' and its associated methods

# C++ type used to represent an element of the matrix pointer to the actual image
# of C++ type 'itk::image< pixeltype , dimension >::Pointer'

#' @rdname antsMatrix_class 
#' @title antsMatrix Class
#' @description An S4 class to hold an antsMatrix imported from ITK types
#'  C++ type used to represent an element of the matrix pointer
#'  to the actual image
#'  C++ type 'itk::image< pixeltype , dimension >::Pointer'
#'
#' @param .Object input object to convert
#' @param elementtype string e.g. "float"
#' 
#' @slot elementtype string of the type of storage of the matrix e.g. "float"
#' @slot pointer the memory location
setClass(Class = "antsMatrix", representation(
  elementtype = "character", pointer = "externalptr"))

#' @rdname antsMatrix_class
#' @aliases initialize,antsMatrix-method 
setMethod(f = "initialize", signature(.Object = "antsMatrix"), definition = function(.Object,
  elementtype) {
  .Call("antsMatrix", elementtype, PACKAGE = "ANTsR")
})

#' @rdname as.array
#' @aliases as.data.frame,antsMatrix-method 
setMethod(f = "as.data.frame", signature(x = "antsMatrix"), definition = function(x) {
  lst <- .Call("antsMatrix_asList", x, PACKAGE = "ANTsR")
  names(lst)[1:(length(lst) - 1)] <- lst[length(lst)]
  lst[[length(lst)]] <- NULL
  return(as.data.frame(lst))
})

#' @rdname as.array
#' @param row.names NULL or a character vector giving the row names for the 
#' data frame. 
#' @param optional passsed to \code{\link{as.data.frame}}
#' @export
#' @method as.data.frame antsMatrix 
as.data.frame.antsMatrix = function(x, row.names = NULL, optional = FALSE, ...) {
  lst <- .Call("antsMatrix_asList", x, PACKAGE = "ANTsR")
  names(lst)[1:(length(lst) - 1)] <- lst[length(lst)]
  lst[[length(lst)]] <- NULL
  return(as.data.frame(lst, row.names = row.names, optional = optional, ...))
}

#' @rdname as.array
#' @aliases as.matrix,antsMatrix-method
setMethod(f = "as.matrix", signature(x = "antsMatrix"), 
          definition = function(x, ...) {
  as.matrix.data.frame(as.data.frame(x), ...)
})


#' @rdname as.array
#' @aliases as.list,antsMatrix-method
setMethod(f = "as.list", signature(x = "antsMatrix"), definition = function(x, ...) {
  lst <- .Call("antsMatrix_asList", x, PACKAGE = "ANTsR")
  names(lst)[1:(length(lst) - 1)] <- lst[length(lst)]
  lst[[length(lst)]] <- NULL
  return(lst)
})


#' @rdname as.antsMatrix
#' @title Coerce Object to as.antsMatrix
#'
#' @description convert types to an antsMatrix
#'
#' @param object An object
#' @param elementtype e.g. "float" or "double"
#' @param ... other parameters
#' @rdname as.antsMatrix
#' @examples
#' as.antsMatrix(matrix(rnorm(10), nrow=2))
#' @export
setGeneric(name = "as.antsMatrix", def = function(object, 
                                                  elementtype="float", ...)
  standardGeneric("as.antsMatrix"))

#' @rdname as.antsMatrix
#' @aliases as.antsMatrix,list-method
setMethod(f = "as.antsMatrix", signature(object = "list"),
  definition = function(object, elementtype="float") {
  return(.Call("antsMatrix_asantsMatrix", object, elementtype, PACKAGE = "ANTsR"))
})

#' @rdname as.antsMatrix
#' @aliases as.antsMatrix,data.frame-method
setMethod(f = "as.antsMatrix", signature(object = "data.frame"),
  definition = function(object, elementtype="float") {
  return(.Call("antsMatrix_asantsMatrix", as.list(object),
    elementtype, PACKAGE = "ANTsR"))
})

#' @rdname as.antsMatrix
#' @aliases as.antsMatrix,matrix-method
setMethod(f = "as.antsMatrix", signature(object = "matrix"),
  definition = function(object, elementtype="float") {
  return(.Call("antsMatrix_asantsMatrix", as.list(as.data.frame(object)),
    elementtype, PACKAGE = "ANTsR"))
})
