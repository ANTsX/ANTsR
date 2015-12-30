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

#' @title antsTransformCreate
#' @description Create and initialize a antsTransform
#' @param type type of transforms
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
#' @return antsTransform or list of transform types
#' @examples
#' trans= c(3,4,5)
#' tx = antsTransformCreate( type="Euler3DTransform", translation=trans )
antsTransformCreate <- function( type="AffineTransform", precision="float", dimension=3, matrix=NA,
  offset=NA, center=NA, translation=NA, parameters=NA, fixed.parameters=NA, displacement.field=NA,
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
    if ( !is.na(displacement.field))
    {
      return( antsTransformFromDisplacementField(displacement.field) )
    }

    # Transforms that derive from itk::MatrixOffsetTransformBase
    if ( sum(type==matrixOffsetTypes) > 0 )
    {
      return(.Call("antsTransform_MatrixOffset",type,precision,dimension,matrix,offset,center,translation,parameters,fixed.parameters))
    }

    stop( "Unknown transform type")

  }


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
  invisible(.Call("antsTransform_SetParameters", tx, parameters, PACKAGE = "ANTsR"))
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

#' @title antsApplyTransformToVector
#' @description Apply transform to spatial vector
#' @param transform antsTransform
#' @param vector array of vector coordinates
#' @return array of coordinates
#' @examples
#' \dontrun{
#' vec2 = antsApplyTransformToVector(transform, c(1,2,3))
#' }
antsApplyTransformToVector <- function(transform, vector) {
  return(.Call("antsTransform_TransformVector", transform, vector, PACKAGE = "ANTsR"))
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
#' antsTransformSetParameters(tx, c(0.9,0,0,1.1,10,11) )
#' img2 = antsApplyTransformToImage(tx, img, img)
#' # plot(img,img2)
antsApplyTransformToImage <- function(transform, image, reference, interpolation="linear") {
  if ( typeof(transform) == "list")
  {
    transform <- antsTransformCompose(transform)
  }
  return(.Call("antsTransform_TransformImage", transform, image, reference, tolower(interpolation), PACKAGE = "ANTsR"))
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

antsTransformInverse <- function( transform ) {
  return(.Call("antsTransform_Inverse", transform))
}

#' @title antsTransformCompose
#' @description compose multiple transforms
#' @param transformList a list of antsTransforms in the reverse order they should be applied
#' @return antsTransform of type "CompositeTransform"
#' @examples
#' tx = new("antsTransform", precision="float", type="AffineTransform", dimension=2 )
#' antsTransformSetParameters(tx, c(0,-1,1,0,0,0) )
#' tx2 = new("antsTransform", precision="float", type="AffineTransform", dimension=2 )
#' antsTransformSetParameters(tx2, c(0,-1,1,0,0,0) )
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
  transformList = rev(transformList)
  return(.Call("antsTransform_Compose", transformList, dimension, precision, PACKAGE="ANTsR"))

}
