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
  precision = "float", dimension = 3, type = "AffineTransform", parameters=NA) {
  tx = .Call("antsTransform", precision, dimension, type, PACKAGE = "ANTsR")
  if ( !is.na(parameters) )
  {
    antsTransformSetParameters(tx, parameters)
  }

  return( tx )
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

antsTransformFromDisplacementField <- function( field ) {
  return(.Call("antsTransform_FromDisplacementField", field, PACKAGE="ANTsR"))
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

#' @title antsApplyTransformToImage
#' @description Apply transform to spatial point
#' @param tx antsTransform
#' @param image antsImage to transform
#' @param ref antImage giving the reference output space
#' @return antsImage
#' @examples
#' img <- antsImageRead(getANTsRData("r16"))
#' tx = new("antsTransform", precision="float", type="AffineTransform", dimension=2 )
#' antsTransformSetParameters(tx, c(0,-1,1,0,dim(img)[1],0) )
#' img2 = antsApplyTransformToImage(tx, img, img)
antsApplyTransformToImage <- function(tx, image, ref, interpolation="linear") {
  return(.Call("antsTransform_TransformImage", tx, image, ref, tolower(interpolation), PACKAGE = "ANTsR"))
}

#' @title antsTransformRead
#' @description read a transform from file
#' @param filename filename of transform
#' @return antsTransform
#' @examples
#' img <- antsImageRead(getANTsRData("r16"))
#' tx = new("antsTransform", precision="float", type="AffineTransform", dimension=2 )
#' antsTransformSetParameters(tx, c(0,-1,1,0,dim(img)[1],0) )
#' img2 = antsApplyTransformToImage(tx, img, img)
antsTransformRead <- function( filename, dimension=3, precision="float" )  {
  return(.Call("antsTransform_Read", filename, dimension, precision, PACKAGE="ANTsR"))
}

#' @title antsTransformCompose
#' @description compose multiple transforms in reverse queue order
#' @param transformList a list of antsTransforms
#' @return antsTransform of type "CompositeTransform"
#' @examples
#' tx = new("antsTransform", precision="float", type="AffineTransform", dimension=2 )
#' antsTransformSetParameters(tx, c(0,-1,1,0,dim(img)[1],0) )
#' tx2 = new("antsTransform", precision="float", type="AffineTransform", dimension=2 )
#' antsTransformSetParameters(tx2, c(0,-1,1,0,dim(img)[1],0) )
#' tx3 = antsTransformCompose( list(tx, tx2) )
antsTransformCompose <- function( transformList ) {

  # check for type consistency
  precision = transformList[[1]]@precision
  dimension = transformList[[1]]@dimension
  for ( i in 1:length(transformList))
  {
    if ( precision != transformList[[i]]@precision )
    {
      stop("All transform must have the same precision")
    }
    if ( dimension != transformList[[i]]@dimension)
    {
      stop("All transform must have the same dimension")
    }

  }
  return(.Call("antsTransform_Compose", transformList, dimension, precision, PACKAGE="ANTsR"))

}
