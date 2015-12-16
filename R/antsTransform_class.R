# this file defines the S4 classes related to 'antsTransform' and the associated
# methods



#' An S4 class for an antsTransform
#'
#' C++ type used to represent an ITK image transform.
#'
#' @param object input object to convert
#' @param .Object input object to convert
#' @param precision string e.g. "float" or "double"
#' @param dimension dimensionality of the transform (2,3,or 4)
#' @param type type of transform'
#' etc noting that short is not supported
#' @slot dimension usually 2 or 3 but can be 4
#' @slot precision math precision is float or double'
#' @slot pointer to the memory location of the itk object
setClass(Class = "antsTransform", representation(precision= "character", dimension = "integer",
  type = "character", pointer = "externalptr"))

#' @describeIn antsTransforms
setMethod(f = "show", "antsTransform", function(object){
    cat("antsTransform\n")
    cat("  Dimensions    :", object@dimension, "\n")
    cat("  Precision     :", object@precision, "\n")
    cat("  Type          :", object@type, "\n")
    cat("\n")
})

#' @describeIn antsTransform
setMethod(f = "initialize", signature(.Object = "antsTransform"), definition = function(.Object,
  precision = "float", dimension = 3, type = "AffineTransform", pointer=NULL) {
  return(.Call("antsTransform", precision, dimension, type, PACKAGE = "ANTsR"))
})


#' @title antsTransformSetParameters
#' @description Set parameters of transform
#' @param tx antsTransform
#' @param parameters array of parameters'
#' @return TRUE
#' @examples
#' tx = new("antsTransform")
#' params = antsTransformGetParameters(tx)
#' antsTransformSetParameters(tx, params*2)
antsTransformSetParameters <- function(tx, parameters) {
  return(.Call("antsTransform_SetParameters", tx, parameters, PACKAGE = "ANTsR"))
}

#' @title antsTransformGetParameters
#' @description Get parameters of transform
#' @param tx antsTransform
#' @return array of parameters'
#' @examples
#' tx = new("antsTransform")
#' params = antsTransformGetParameters(tx)
antsTransformGetParameters <- function(tx) {
  return(.Call("antsTransform_GetParameters", tx, PACKAGE = "ANTsR"))
}

#' @title antsApplyTransformToPoint
#' @description Apply transform to spatial point
#' @param tx antsTransform
#' @return array of coordinates
#' @examples
#' tx = new("antsTransform")
#' params = antsTransformGetParameters(tx)
#' antsTransformSetParameters(tx, params*2)
#' pt2 = antsApplyTransformToPoint(tx, c(1,2,3))
antsApplyTransformToPoint <- function(tx, point) {
  return(.Call("antsTransform_TransformPoint", tx, point, PACKAGE = "ANTsR"))
}
