# this file defines the S4 classes related to 'antsrTransform' and the associated
# methods



#' @rdname antsrMetric
#' @title An S4 class for image metrics between two antsImages
#'
#' @description C++ type used to represent an ITK image metric
#'
#' @param object input object to convert
#' @param .Object input object to convert
#' @param dimension dimensionality of the transform (2,3,or 4)
#' @param type type of transform'
#' etc noting that short is not supported
#' @slot dimension usually 2 or 3 but can be 4
#' @slot precision math precision is float or double'
#' @slot type The type of transform: usually one of
#' AffineTransform, CenteredAffineTransform, CenteredEuler3DTransform,
#' CenteredRigid2DTransform, CenteredSimilarity2DTransform, Euler2DTransform,
#' Euler3DTransform, QuaternionRigidTransform, Rigid2DTransform,
#' Similarity2DTransform, Similarity3DTransform
#' @slot pointer to the memory location of the itk object
## #' @param parameters transformation parameters to send to
## #' \code{\link{setAntsrTransformParameters}}
## #' @param precision string e.g. "float" or "double"
setClass(Class = "antsrMetric",
         representation(precision= "character", dimension = "integer",
         type = "character", pixeltype = "character", isVector = "logical",
         pointer = "externalptr"))

#' @rdname antsrMetric
#' @aliases show,antsrMetric-method
setMethod(f = "show", "antsrMetric", function(object){
    cat("antsrMetric\n")
    cat("  Dimensions    :", object@dimension, "\n")
    cat("  PixelType     :", object@pixeltype, "\n")
    cat("  Type          :", object@type, "\n")
    cat("  isVector      :", object@isVector, "\n")
    cat("\n")
})

#' @rdname antsrMetric
#' @aliases initialize,antsrMetric-method
setMethod(f = "initialize", signature(.Object = "antsrMetric"), definition = function(.Object,
  dimension = 3, type = "MeanSquares") {
  tx = .Call("antsrMetric", pixeltype, dimension, type, PACKAGE = "ANTsRCore")
  return( tx )
})


#' @title antsrMetricCreate
#' @description create object that measures similarity between two images
#' @param fixed the fixed antsImage
#' @param moving the moving antsImage
#' @param type image metric to calculate
#' \itemize{
#'   \item{MeanSquares}{}
#'   \item{Correlation}{}
#'   \item{ANTSNeighborhoodCorrelation}{}
#'   \item{MattesMutualInformation}{}
#'   \item{JointHistogramMutualInformation}{}
#'   \item{Demons}{}
#' }
#' @param nBins number of bins for histogram metrics (default = 32)
#' @param radius radius for neighborhoo metrics (default = 3)
#' @param fixed.mask mask for the fixed image
#' @param moving.mask mask for the moving image
#' @param sampling.strategy sampling strategy, default if full sampling
#' \itemize{
#'   \item{none}{Full sampling}
#'   \item{random}{}
#'   \item{regular}{}
#' }
#' @param sampling.percentage percentage of data to sample when calculating metric
#' @return value of image to image metric
#' @examples
#' x =  antsImageRead( getANTsRData( 'r16' ))
#' y =  antsImageRead( getANTsRData( 'r30' ))
#' metric = antsrMetricCreate(x,y,type="MeanSquares")
#' @export
antsrMetricCreate <- function(
  fixed, moving,
  type=c("MeanSquares", "MattesMutualInformation",
         "ANTSNeighborhoodCorrelation", "Correlation",
         "Demons", "JointHistogramMutualInformation"),
  fixed.mask=NULL, moving.mask=NULL,
  sampling.strategy="none", sampling.percentage=1, nBins=32, radius=3 )
{

  type = match.arg(type)

  dimension = 3
  pixeltype = "float"

  # Check for valid dimension
  if ( (dimension < 2) || (dimension > 4) )
  {
    stop(paste("Unsupported dimension:", dimension))
  }

  isVector=FALSE # For now, no multichannel images
  if ( fixed@isVector | moving@isVector ) {
    stop("Multichannel images are not supported")
  }

  fixed = check_ants(fixed)
  error_not_antsImage(fixed, "fixed")
  
  if ( is.antsImage(fixed) ) {
    dimension = fixed@dimension
    pixeltype = fixed@pixeltype
  } 
  # else {
  #   stop("Invalid fixed image")
  # }

  moving = check_ants(moving)
  error_not_antsImage(moving, "moving")
  
  if ( is.antsImage(moving) ) {
    if (moving@dimension != dimension ) {
      stop("Fixed and Moving images must have same dimension")
    }
    if ( moving@pixeltype != pixeltype ) {
      stop("Fixed and Moving images must have same pixeltype")
    }
  }
  # else {
  #   stop("Invalid moving image")
  # }

  metric = .Call("antsrMetric", pixeltype, dimension, type, isVector, fixed, moving, PACKAGE = "ANTsRCore")

  if (!is.null(fixed.mask)) {
    fixed.mask = check_ants(fixed.mask)
  }
  if ( is.antsImage(fixed.mask) ) {
    antsrMetricSetFixedImageMask(metric, fixed.mask)
  }
  
  if (!is.null(moving.mask)) {
    moving.mask = check_ants(moving.mask)
  }  
  if ( is.antsImage(moving.mask) ) {
    antsrMetricSetMovingImageMask(metric, moving.mask)
  }

  if ( (sampling.strategy=="regular") | (sampling.strategy=="random"))
  {
    if ( (sampling.percentage <= 0.0) | (sampling.percentage > 1.0) ) {
      stop("Invalid sampling.percentage")
    }
    antsrMetricSetSampling(metric, sampling.strategy, sampling.percentage)
  }
  else if (!(sampling.strategy=="none")) {
    stop("Invalid sampling.strategy")
  }

  if ( (type=="MattesMutualInformation") | (type=="JointHistogramMutualInformation")) {
    if ( nBins < 5 ) {
      stop("Number of histogram bins must be >= 5")
    }
    antsrMetricSetNumberOfHistogramBins(metric, nBins)
  }
  else if ( type=="ANTSNeighborhoodCorrelation" ) {
    antsrMetricSetRadius( metric, radius )
  }

  antsrMetricInitialize(metric)

  return(metric)
  }

#' @title antsrMetricSetFixedImage
#' @description set fixed image for image to image metric
#' @param metric an 'antsrMetric'
#' @param image the fixed 'antsImage'
#' @examples
#' x =  antsImageRead( getANTsRData( 'r16' ))
#' y =  antsImageRead( getANTsRData( 'r30' ))
#' metric = antsrMetricCreate(x,y,type="MeanSquares")
#' z = x*2
#' antsrMetricSetFixedImage(metric, z)
#' @note After calling this, must call antsrMetricInitialize(metric)
#' @export
antsrMetricSetFixedImage = function( metric, image ) {
  image = check_ants(image)
  invisible(.Call("antsrMetric_SetImage", metric, image, TRUE, FALSE))
}

  #' @title antsrMetricSetFixedImageMask
  #' @description set fixed image for image to image metric
  #' @param metric an 'antsrMetric'
  #' @param image the fixed 'antsImage'
  #' @examples
  #' x =  antsImageRead( getANTsRData( 'r16' ))
  #' y =  antsImageRead( getANTsRData( 'r30' ))
  #' metric = antsrMetricCreate(x,y,type="MeanSquares")
  #' z = getMask(x)
  #' antsrMetricSetFixedImageMask(metric, z)
  #' @note After calling this, must call antsrMetricInitialize(metric)
  #' @export
antsrMetricSetFixedImageMask = function( metric, image ) {
  image = check_ants(image)
  invisible(.Call("antsrMetric_SetImage", metric, image, TRUE, TRUE))
}

#' @title antsrMetricSetMovingImage
#' @description set moving image for image to image metric
#' @param metric an 'antsrMetric'
#' @param image the moving 'antsImage'
#' @examples
#' x =  antsImageRead( getANTsRData( 'r16' ))
#' y =  antsImageRead( getANTsRData( 'r30' ))
#' metric = antsrMetricCreate(x,y,type="MeanSquares")
#' z = y*2
#' antsrMetricSetMovingImage(metric, z)
#' @note After calling this, must call antsrMetricInitialize(metric)
#' @export
antsrMetricSetMovingImage = function( metric, image ) {
  image = check_ants(image)
  invisible(.Call("antsrMetric_SetImage", metric, image, FALSE, FALSE))
}

#' @title antsrMetricSetMovingImageMask
#' @description set moving image for image to image metric
#' @param metric an 'antsrMetric'
#' @param image the moving 'antsImage'
#' @examples
#' x =  antsImageRead( getANTsRData( 'r16' ))
#' y =  antsImageRead( getANTsRData( 'r30' ))
#' metric = antsrMetricCreate(x,y,type="MeanSquares")
#' z = getMask(y)
#' antsrMetricSetMovingImageMask(metric, z)
#' @note After calling this, must call antsrMetricInitialize(metric)
#' @export
antsrMetricSetMovingImageMask = function( metric, image) {
  image = check_ants(image)
  invisible(.Call("antsrMetric_SetImage", metric, image, FALSE, TRUE))
}

#' @title antsrMetricSetNumberOfHistogramBins
#' @description set histogram bins image to image metric
#' @param metric an 'antsrMetric' of type 'MattesMutualInformation' or 'JointHistogramMutualInformation'
#' @param nBins number of bins (minimum is 5 even for binary data)
#' @examples
#' x =  antsImageRead( getANTsRData( 'r16' ))
#' y =  antsImageRead( getANTsRData( 'r30' ))
#' metric = antsrMetricCreate(x,y,type="MattesMutualInformation")
#' antsrMetricSetNumberOfHistogramBins(metric,12)
#' @export
antsrMetricSetNumberOfHistogramBins = function(
  metric, nBins ) {
  
  if ( ( metric@type != "MattesMutualInformation" ) &
       ( metric@type != "JointHistogramMutualInformation" ) ) {
    stop( "Metric must be a histogram type")
  }
  
  invisible(.Call("antsrMetric_SetNumberOfHistogramBins", metric, nBins ))
}

#' @title antsrMetricSetRadius
#' @description set neighborhood radius
#' @param metric an 'antsrMetric' of type 'ANTSNeighborhoodCorrelation'
#' @param radius radius of neighborhood
#' @examples
#' x =  antsImageRead( getANTsRData( 'r16' ))
#' y =  antsImageRead( getANTsRData( 'r30' ))
#' metric = antsrMetricCreate(x,y,type="ANTSNeighborhoodCorrelation")
#' antsrMetricSetRadius(metric,5)
#' @export
antsrMetricSetRadius = function( metric, radius ) {
  
  if ( metric@type != "ANTSNeighborhoodCorrelation" )  {
    stop( "Metric must be a ANTSNeighborhoodCorrelation")
  }
  
  invisible(.Call("antsrMetric_SetRadius", metric, radius ))
}

#' @title antsrMetricSetSampling
#' @description set image sampling strategy and rate
#' @param metric an 'antsrMetric'
#' @param sampling.strategy sampling strategy, default if full sampling
#' \itemize{
#'   \item{none}{Full sampling}
#'   \item{random}{}
#'   \item{regular}{}
#' }
#' @param sampling.percentage percentage of data to sample when calculating metric
#' @examples
#' x =  antsImageRead( getANTsRData( 'r16' ))
#' y =  antsImageRead( getANTsRData( 'r30' ))
#' metric = antsrMetricCreate(x,y,type="MeanSquares")
#' antsrMetricSetSampling(metric,"random",0.4)
#' @note After calling this, must call antsrMetricInitialize(metric)
#' @export
antsrMetricSetSampling = function(
  metric, sampling.strategy, sampling.percentage ) {
  invisible(.Call("antsrMetric_SetSampling",
                  metric, sampling.strategy, sampling.percentage ))
}

#' @title antsrMetricSetMovingTransform
#' @description set transform for moving image
#' @param metric an 'antsrMetric'
#' @param transform an 'antsrTransform'
#' @examples
#' x =  antsImageRead( getANTsRData( 'r16' ))
#' y =  antsImageRead( getANTsRData( 'r30' ))
#' metric = antsrMetricCreate(x,y,type="ANTSNeighborhoodCorrelation")
#' tx <- createAntsrTransform( precision="double", type="AffineTransform", dimension=2)
#' setAntsrTransformParameters(tx, c(0,-1,1,0,0,0))
#' setAntsrTransformFixedParameters(tx, c(128,128))
#' antsrMetricSetMovingTransform(metric, tx)
#' antsrMetricGetValue(metric)
#' @export
antsrMetricSetMovingTransform = function( metric, transform ) {
  invisible(.Call("antsrMetric_SetTransform", metric, transform, FALSE ))
}

#' @title antsrMetricSetFixedTransform
#' @description set transform for fixed image
#' @param metric an 'antsrMetric'
#' @param transform an 'antsrTransform'
#' @examples
#' x =  antsImageRead( getANTsRData( 'r16' ))
#' y =  antsImageRead( getANTsRData( 'r30' ))
#' metric = antsrMetricCreate(x,y,type="ANTSNeighborhoodCorrelation")
#' tx <- createAntsrTransform( precision="double", type="AffineTransform", dimension=2)
#' setAntsrTransformParameters(tx, c(0,-1,1,0,0,0))
#' setAntsrTransformFixedParameters(tx, c(128,128))
#' antsrMetricSetFixedTransform(metric, tx)
#' antsrMetricGetValue(metric)
#' @export
antsrMetricSetFixedTransform = function( metric, transform ) {
  invisible(.Call("antsrMetric_SetTransform", metric, transform, TRUE ))
}

#' @title antsrMetricGetValue
#' @description get current value of metric
#' @param metric an 'antsrMetric'
#' @examples
#' x =  antsImageRead( getANTsRData( 'r16' ))
#' y =  antsImageRead( getANTsRData( 'r30' ))
#' metric = antsrMetricCreate(x,y,type="MeanSquares")
#' metricValue = antsrMetricGetValue(metric)
#' @return image similarity value
#' @export
antsrMetricGetValue = function( metric ) {
  return( .Call("antsrMetric_GetValue", metric, PACKAGE = "ANTsRCore" ) )
}

#' @title antsrMetricGetDerivative
#' @description get derivative of image metric
#' @param metric an 'antsrMetric'
#' @examples
#' x =  antsImageRead( getANTsRData( 'r16' ))
#' y =  antsImageRead( getANTsRData( 'r30' ))
#' metric = antsrMetricCreate(x,y,type="MeanSquares")
#' tx <- createAntsrTransform( precision="double", type="AffineTransform", dimension=2)
#' setAntsrTransformParameters(tx, c(0,-1,1,0,0,0))
#' setAntsrTransformFixedParameters(tx, c(128,128))
#' antsrMetricSetMovingTransform(metric, tx)
#' metricValue = antsrMetricGetDerivative(metric)
#' @return image similarity value
#' @export
antsrMetricGetDerivative = function( metric ) {
  return( .Call("antsrMetric_GetDerivative", metric, PACKAGE = "ANTsRCore" ) )
}

#' @title antsrMetricInitialize
#' @description prepare to return values
#' @param metric an 'antsrMetric'
#' @examples
#' x =  antsImageRead( getANTsRData( 'r16' ))
#' y =  antsImageRead( getANTsRData( 'r30' ))
#' z = getMask(y)
#' metric = antsrMetricCreate(x,y,type="MeanSquares")
#' antsrMetricSetMovingImageMask(metric, z)
#' antsrMetricInitialize(metric)
#' @note must call this after setting up object, before getting values back
#' @export
antsrMetricInitialize = function( metric ) {
  return(.Call("antsrMetric_Initialize", metric, PACKAGE = "ANTsRCore" ) )
}
