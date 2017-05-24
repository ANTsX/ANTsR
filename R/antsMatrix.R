# this file defines the class 'antsMatrix' and its associated methods

# C++ type used to represent an element of the matrix pointer to the actual image
# of C++ type 'itk::image< pixeltype , dimension >::Pointer'

#' An S4 class to hold an antsMatrix imported from ITK types
#'  C++ type used to represent an element of the matrix pointer
#'  to the actual image
#'  C++ type 'itk::image< pixeltype , dimension >::Pointer'
#'
#' @param .Object input object to convert
#' @param elementtype string e.g. "float"
#' @param x input object to convert
#' @slot elementtype string of the type of storage of the matrix e.g. "float"
#' @slot pointer the memory location
setClass(Class = "antsMatrix", representation(
  elementtype = "character", pointer = "externalptr"))

#' @describeIn antsMatrix
#' @aliases initialize,antsMatrix-method 
setMethod(f = "initialize", signature(.Object = "antsMatrix"), definition = function(.Object,
  elementtype) {
  .Call("antsMatrix", elementtype, PACKAGE = "ANTsR")
})

#' @describeIn antsMatrix
#' @aliases as.data.frame,antsMatrix-method 
setMethod(f = "as.data.frame", signature(x = "antsMatrix"), definition = function(x) {
  lst <- .Call("antsMatrix_asList", x, PACKAGE = "ANTsR")
  names(lst)[1:(length(lst) - 1)] <- lst[length(lst)]
  lst[[length(lst)]] <- NULL
  return(as.data.frame(lst))
})

#' @describeIn antsMatrix
#' @aliases as.matrix,antsMatrix-method
setMethod(f = "as.matrix", signature(x = "antsMatrix"), definition = function(x) {
  as.matrix.data.frame(as.data.frame(x))
})


#' @describeIn antsMatrix
#' @aliases as.list,antsMatrix-method
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
#' @param elementtype e.g. "float" or "double"
#' @param ... other parameters
#' @rdname as.antsMatrix
#' @examples
#' as.antsMatrix(matrix(rnorm(10), nrow=2))
#' @export
setGeneric(name = "as.antsMatrix", def = function(object, 
                                                  elementtype="float", ...)
  standardGeneric("as.antsMatrix"))

#' @describeIn as.antsMatrix
#' @aliases as.antsMatrix,list-method
setMethod(f = "as.antsMatrix", signature(object = "list"),
  definition = function(object, elementtype="float") {
  return(.Call("antsMatrix_asantsMatrix", object, elementtype, PACKAGE = "ANTsR"))
})

#' @describeIn as.antsMatrix
#' @aliases as.antsMatrix,data.frame-method
setMethod(f = "as.antsMatrix", signature(object = "data.frame"),
  definition = function(object, elementtype="float") {
  return(.Call("antsMatrix_asantsMatrix", as.list(object),
    elementtype, PACKAGE = "ANTsR"))
})

#' @describeIn as.antsMatrix
#' @aliases as.antsMatrix,matrix-method
setMethod(f = "as.antsMatrix", signature(object = "matrix"),
  definition = function(object, elementtype="float") {
  return(.Call("antsMatrix_asantsMatrix", as.list(as.data.frame(object)),
    elementtype, PACKAGE = "ANTsR"))
})
