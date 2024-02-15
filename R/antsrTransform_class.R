# this file defines the S4 classes related to 'antsrTransform' and the associated
# methods



#' @rdname antsrTransform
#' @title An S4 class for an antsrTransform
#'
#' @description C++ type used to represent an ITK image transform.
#'
#' @param object input object to convert
#' @param .Object input object to convert
#' @param precision string e.g. "float" or "double"
#' @param dimension dimensionality of the transform (2,3,or 4)
#' @param type type of transform'
#' etc noting that short is not supported
#' @param parameters transformation parameters to send to
#' \code{\link{setAntsrTransformParameters}}
#' @slot dimension usually 2 or 3 but can be 4
#' @slot precision math precision is float or double'
#' @slot type The type of transform: usually one of
#' AffineTransform, CenteredAffineTransform, CenteredEuler3DTransform,
#' CenteredRigid2DTransform, CenteredSimilarity2DTransform, Euler2DTransform,
#' Euler3DTransform, QuaternionRigidTransform, Rigid2DTransform,
#' Similarity2DTransform, Similarity3DTransform
#' @slot pointer to the memory location of the itk object
setClass(Class = "antsrTransform",
         representation(precision= "character", dimension = "integer",
  type = "character", pointer = "externalptr"))

#' @rdname antsrTransform
#' @aliases show,antsrTransform-method
setMethod(f = "show", "antsrTransform", function(object){
    cat("antsrTransform\n")
    cat("  Dimensions    :", object@dimension, "\n")
    cat("  Precision     :", object@precision, "\n")
    cat("  Type          :", object@type, "\n")
    cat("\n")
})

#' @rdname antsrTransform
#' @aliases initialize,antsrTransform-method
setMethod(f = "initialize", signature(.Object = "antsrTransform"), definition = function(.Object,
  precision = "float", dimension = 3, type = "AffineTransform", parameters=NA) {
  tx = .Call("antsrTransform", precision, dimension, type, PACKAGE = "ANTsRCore")
  if ( !is.na(parameters) )
  {
    setAntsrTransformParameters(tx, parameters)
  }

  return( tx )
})


#' @title createAntsrTransform
#' @description Create and initialize an antsrTransform
#' @param type type of transform
#' \itemize{
#'   \item{AffineTransform}{}
#'   \item{CenteredAffineTransform}{}
#'   \item{Euler2DTransform}{}
#'   \item{Euler3DTransform}{}
#'   \item{Rigid2DTransform}{}
#'   \item{QuaternionRigidTransform}{}
#'   \item{Similarity2DTransform}{}
#'   \item{CenteredSimilarity2DTransform}{}
#'   \item{Similarity3DTransform}{}
#'   \item{CenteredRigid2DTransform}{}
#'   \item{CenteredEuler3DTransform}{}
#'   \item{DisplacementFieldTransform}{}
#' }
#' @param precision numerical precision
#' @param dimension spatial dimension of transform
#' @param matrix matrix for linear transforms
#' @param offset offset for linear transforms
#' @param center center for linear transforms
#' @param translation translation for linear transforms
#' @param parameters array of parameters
#' @param fixed.parameters array of fixed parameters
#' @param displacement.field multichannel antsImage for non-linear transform
#' @param supported.types flag that returns array of possible transforms types
#' @return antsrTransform or list of transform types
#' @examples
#' trans= c(3,4,5)
#' tx = createAntsrTransform( type="Euler3DTransform", translation=trans )
#' @export
createAntsrTransform <- function( type="AffineTransform", precision="float", dimension=3, matrix=NA,
  offset=NA, center=NA, translation=NA, parameters=NA, fixed.parameters=NA, displacement.field=NULL,
  supported.types=FALSE )
  {

    matrixOffsetTypes = c("AffineTransform", "CenteredAffineTransform", "Euler2DTransform", "Euler3DTransform", "Rigid2DTransform", "QuaternionRigidTransform", "Similarity2DTransform", "CenteredSimilarity2DTransform","Similarity3DTransform", "CenteredRigid2DTransform", "CenteredEuler3DTransform")
    supportedTypes = c(matrixOffsetTypes, "DisplacementFieldTransform")

    if ( supported.types ) {
      return( supportedTypes )
    }


    # Check for valid dimension
    if ( (dimension < 2) || (dimension > 4) )
    {
      stop(paste("Unsupported dimension:", dimension))
    }

    # Check for valid precision
    precisionTypes = c("float", "double")
    if ( sum(precision==precisionTypes)==0)
    {
      stop(paste("Unsupported precision:", precision))
    }

    # Check for supported transform type
    if ( sum(type==supportedTypes)==0 )
    {
      stop(paste("Unsupported type:", type))
    }

    # Check parameters with type
    if (type=="Euler3DTransform")
    {
      dimension = 3
    }
    else if (type=="Euler2DTransform")
    {
      dimension = 2
    }
    else if (type=="Rigid3DTransform")
    {
      dimension = 3
    }
    else if (type=="QuaternionRigidTransform")
    {
      dimension = 3
    }
    else if (type=="Rigid2DTransform")
    {
      dimension = 2
    }
    else if (type=="CenteredRigid2DTransform")
    {
      dimension = 2
    }
    else if (type=="CenteredEuler3DTransform")
    {
      dimension = 3
    }
    else if (type=="Similarity3DTransform")
    {
      dimension = 3
    }
    else if (type=="Similarity2DTransform")
    {
      dimension = 2
    }
    else if (type=="CenteredSimilarity2DTransform")
    {
      dimension = 2
    }

    # If displacement field
    if ( !is.null(displacement.field))
    {
      return( antsrTransformFromDisplacementField(displacement.field) )
    }

    # Transforms that derive from itk::MatrixOffsetTransformBase
    if ( sum(type==matrixOffsetTypes) > 0 )
    {
      return(.Call("antsrTransform_MatrixOffset",type,precision,dimension,matrix,offset,center,translation,parameters,fixed.parameters))
    }

    stop( "Unknown transform type")

  }


#' @title setAntsrTransformParameters
#' @description Set parameters of transform
#' @param transform antsrTransform
#' @param parameters array of parameters
#' @return TRUE
#' @examples
#' tx = new("antsrTransform")
#' params = getAntsrTransformParameters(tx)
#' setAntsrTransformParameters(tx, params*2)
#' @export
setAntsrTransformParameters <- function(transform, parameters) {
  invisible(.Call("antsrTransform_SetParameters", transform, parameters, PACKAGE = "ANTsRCore"))
}

#' @title getAntsrTransformParameters
#' @description Get parameters of transform
#' @param transform antsrTransform
#' @return array of parameters'
#' @examples
#' tx = new("antsrTransform")
#' params = getAntsrTransformParameters(tx)
#' @export
getAntsrTransformParameters <- function(transform) {
  return(.Call("antsrTransform_GetParameters", transform, PACKAGE = "ANTsRCore"))
}

#' @title setAntsrTransformFixedParameters
#' @description Set parameters of transform
#' @param transform antsrTransform
#' @param parameters array of parameters'
#' @return TRUE
#' @examples
#' tx = new("antsrTransform")
#' params = getAntsrTransformFixedParameters(tx)
#' setAntsrTransformFixedParameters(tx, params*2)
#' @export
setAntsrTransformFixedParameters <- function(transform, parameters) {
  invisible(.Call("antsrTransform_SetFixedParameters", transform, parameters, PACKAGE = "ANTsRCore"))
}

#' @title getAntsrTransformFixedParameters
#' @description Get fixed parameters of transform
#' @param transform antsrTransform
#' @return array of fixed parameters
#' @examples
#' tx = new("antsrTransform")
#' params = getAntsrTransformFixedParameters(tx)
#' @export
getAntsrTransformFixedParameters <- function(transform) {
  return(.Call("antsrTransform_GetFixedParameters", transform, PACKAGE = "ANTsRCore"))
}

#' @title antsrTransformFromDisplacementField
#' @description Convert deformation field (multiChannel image) to antsrTransform
#' @param field deformation field (multiChannel image)
#' @return antsrTransform'
#' @export
antsrTransformFromDisplacementField <- function( field ) {
  return(.Call("antsrTransform_FromDisplacementField", field, PACKAGE="ANTsRCore"))
}

#' @title displacementFieldFromAntsrTransform
#' @description Conver antsrTransform to displacement field
#' @param tx antsrTransform
#' @param reference reference antsImage if converting linear transform
#' @return antsImage
#' @export
displacementFieldFromAntsrTransform <- function( tx, reference=NA ) {
  return(.Call("antsrTransform_ToDisplacementField", tx, reference, PACKAGE="ANTsRCore"))
}

#' @title applyAntsrTransform
#' @description Apply transform to point, vector or antsImage data
#' @param transform antsrTransform
#' @param data data to transform
#' @param dataType data type for non-antsImage data. Either "point" or "vector"
#' @param ... options passed to `applyAntsrTransformToImage`
#' @param reference target space for transforming an antsImage
#' @return transformed data
#' @examples
#' tx = createAntsrTransform(dimension=2, precision="float", type="AffineTransform")
#' params = getAntsrTransformParameters(tx)
#' setAntsrTransformParameters(tx, params*2)
#' pt2 = applyAntsrTransform(tx, c(1,2,3))
#' @export
applyAntsrTransform <- function(transform, data, dataType="point", reference=NULL, ...) {

  if ( is.antsImage(data) ) {
    if (is.null(reference) ) {
      reference = data
    }
    return( applyAntsrTransformToImage( transform, data, reference, ...) )
  }
  else {
    ismatrix=TRUE
    if (class(data)[1]=="numeric") {
      data = t(as.matrix(data))
      ismatrix = FALSE
    }

    ret=NA
    if ( dataType == "point") {
      ret = applyAntsrTransformToPoint(transform, data)
    }
    else if ( dataType == "vector") {
      ret = applyAntsrTransformToVector(transform, data)
    }
    else {
      stop("Invalid datatype")
    }

    if ( !ismatrix ) {
      ret = as.numeric(t(ret))
    }

    return(ret)

  }

  # Never reached
  return(NA)
}

#' @title applyAntsrTransformToPoint
#' @description Apply transform to spatial point
#' @param transform antsrTransform
#' @param points a matrix which each row is a spatial point
#' @return array of coordinates
#' @examples
#' tx = new("antsrTransform")
#' params = getAntsrTransformParameters(tx)
#' setAntsrTransformParameters(tx, params*2)
#' pt2 = applyAntsrTransformToPoint(tx, c(1,2,3))
#' @export
applyAntsrTransformToPoint <- function(transform, points) {

  ismatrix=TRUE
  if (class(points)[1]=="numeric") {
    points = t(as.matrix(points))
    ismatrix = FALSE
  }

  ret = .Call("antsrTransform_TransformPoint", transform, points, PACKAGE = "ANTsRCore")

  if ( !ismatrix ) {
    ret = as.numeric(t(ret))
  }

  return(ret)
}

#' @title applyAntsrTransformToVector
#' @description Apply transform to spatial vector
#' @param transform antsrTransform
#' @param vectors a matrix where each row is a vector to transform
#' @return array of coordinates
#' @examples
#' transform = new("antsrTransform", precision="float",
#' type="AffineTransform", dimension=2 )
#' vec2 = applyAntsrTransformToVector(transform, c(1,2,3))
#' @export
applyAntsrTransformToVector <- function(transform, vectors) {

  ismatrix=TRUE
  if (class(vectors)=="numeric") {
    vectors = t(as.matrix(vectors))
    ismatrix = FALSE
  }
  ret = .Call("antsrTransform_TransformVector", transform, vectors, PACKAGE = "ANTsRCore")

  if ( !ismatrix ) {
    ret = as.numeric(t(ret))
  }
  return(ret)
}

#' @title applyAntsrTransformToImage
#' @description Apply transform to spatial point
#' @param transform antsrTransform
#' @param image antsImage to transform
#' @param reference antImage giving the reference output space
#' @param interpolation type of interpolator to use
#' @return antsImage
#' @examples
#' img <- antsImageRead(getANTsRData("r16"))
#' tx = new("antsrTransform", precision="float", type="AffineTransform", dimension=2 )
#' setAntsrTransformParameters(tx, c(0.9,0,0,1.1,10,11) )
#' img2 = applyAntsrTransformToImage(tx, img, img)
#' # plot(img,img2)
#' @export
applyAntsrTransformToImage <- function(transform, image, reference, interpolation="linear") {
  if ( typeof(transform) == "list")
  {
    transform <- composeAntsrTransforms(transform)
  }

  outImg = NA

  # for interpolators that don't support vector pixels: split, transform, merge
  vectorInterp = ( interpolation=="linear" | interpolation=="nearestneighbor" )
  if ( !vectorInterp & (image@isVector | image@components > 1) ) {
    imgList = splitChannels(image)
    imgListOut = lapply(imgList, function(x) applyAntsrTransformToImage(transform, x, reference, interpolation) )
    outImg = mergeChannels(imgListOut)
  }
  else {
    outImg = .Call("antsrTransform_TransformImage", transform, image, reference, tolower(interpolation), PACKAGE = "ANTsRCore")
  }

  return(outImg)
}

#' @title readAntsrTransform
#' @description read a transform from file
#' @param filename filename of transform
#' @param dimension spatial dimension of transform
#' @param precision numerical precision of transform
#' @return antsrTransform
#' @examples
#' trans= c(3,4,5)
#' tx = createAntsrTransform( type="Euler3DTransform", translation=trans )
#' txfile = tempfile(fileext = ".mat")
#' writeAntsrTransform(tx, txfile)
#' tx2 = readAntsrTransform(txfile)
#' testthat::expect_error(readAntsrTransform(txfile, 2), "space dim")
#' @export
readAntsrTransform <- function( filename, dimension=NA, precision="float" )  {
  return(.Call("antsrTransform_Read", filename, dimension, precision, PACKAGE="ANTsRCore"))
}

#' @title writeAntsrTransform
#' @description write antsrTransform to disk
#' @param transform antsrTransform
#' @param filename filename of transform (file extension is ".mat" for affine transforms)
#' @return TRUE
#' @examples
#' trans= c(3,4,5)
#' tx = createAntsrTransform( type="Euler3DTransform", translation=trans )
#' txfile = tempfile(fileext = ".mat")
#' writeAntsrTransform(tx, txfile)
#' @export
writeAntsrTransform <- function(transform, filename )  {
  return(.Call("antsrTransform_Write", transform ,filename, PACKAGE="ANTsRCore"))
}

#' @title invertAntsrTransform
#' @description invert a linear antsrTransform
#' @param transform trasform to invert
#' @return antsrTransform
#' @examples
#' img <- antsImageRead(getANTsRData("r16"))
#' tx = new("antsrTransform", precision="float", type="AffineTransform", dimension=2 )
#' setAntsrTransformParameters(tx, c(0,-1,1,0,dim(img)[1],0) )
#' txinv =  invertAntsrTransform(tx)
#' @export
invertAntsrTransform <- function( transform ) {
  return(.Call("antsrTransform_Inverse", transform))
}

#' @title composeAntsrTransforms
#' @description compose multiple transforms
#' @param transformList a list of antsrTransforms in the reverse order they should be applied
#' @return antsrTransform of type "CompositeTransform"
#' @examples
#' tx = new("antsrTransform", precision="float", type="AffineTransform", dimension=2 )
#' setAntsrTransformParameters(tx, c(0,-1,1,0,0,0) )
#' tx2 = new("antsrTransform", precision="float", type="AffineTransform", dimension=2 )
#' setAntsrTransformParameters(tx2, c(0,-1,1,0,0,0) )
#' tx3 = composeAntsrTransforms( list(tx, tx2) )
#' @export
composeAntsrTransforms <- function( transformList ) {

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
      stop("All transforms must have the same dimension")
    }

  }
  transformList = rev(transformList)
  return(.Call("antsrTransform_Compose", transformList, dimension, precision, PACKAGE="ANTsRCore"))

}
