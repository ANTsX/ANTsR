#' histogramMatchImage
#'
#' Match intensity profile with a reference image.
#'
#' @param sourceImage image to undergo intensity transformation.
#' @param referenceImage image providing reference intensity profile.
#' @param numberOfHistogramBins number of histogram levels.
#' @param numberOfMatchPoints number of histogram match points.
#' @param useThresholdAtMeanIntensity use a simple background exclusion criterion.
#' @return source image intensity matched to reference image.
#'
#' @author NJ Tustison
#'
#' @examples
#' sourceImage <- antsImageRead(getANTsRData("r16"), 2)
#' referenceImage <- antsImageRead(getANTsRData("r64"), 2)
#' matchedImage <- histogramMatchImage(sourceImage, referenceImage)
#'
#' @export histogramMatchImage

histogramMatchImage <- function(
    sourceImage,
    referenceImage,
    numberOfHistogramBins = 255,
    numberOfMatchPoints = 64,
    useThresholdAtMeanIntensity = FALSE) {
  outputImage <- ANTsRCore::histogramMatchImageR(
    antsImageClone(sourceImage),
    antsImageClone(referenceImage),
    as.numeric(numberOfHistogramBins),
    as.numeric(numberOfMatchPoints),
    as.numeric(useThresholdAtMeanIntensity)
  )
  return(outputImage)
}


#' Transform image intensities based on histogram mapping.
#'
#' Apply B-spline 1-D maps to an input image for intensity warping.
#'
#' @param sourceImage source image.
#' @param referenceImage reference image.
#' @param sourceMask source mask.
#' @param referenceMask reference mask.
#' @param matchPoints parametric points at which the intensity transform
#' displacements are specified between [0, 1].  Alternatively, a single
#' number can be given and the sequence is linearly spaced in [0, 1].
#' @param transformDomainSize Defines the sampling resolution of the B-spline warping.
#' @return warped intensity image
#' @author Tustison NJ
#' @examples
#'
#' library( ANTsR )
#' image <- antsImageRead( getANTsRData( "r16" ) )
#' transformedImage <- histogramWarpImageIntensities( image, transformDomainSize = 10 )
#' rm(image); gc()
#' rm(transformedImage); gc()
#' @export histogramMatchImage2
histogramMatchImage2 <- function( sourceImage, referenceImage,
  sourceMask = NULL, referenceMask = NULL,
  matchPoints = 64,
  transformDomainSize = 255 )
  {


  if( length( matchPoints ) > 1 )
    {
    if( any( matchPoints < 0.0 ) || any( matchPoints > 1.0 ) )
      {
      stop( "If specifying matchPoints as a vector, values must be in the range [0, 1]." )
      }
    }

  # Use entire image if mask isn't specified.
  if( is.null( sourceMask ) )
    {
    sourceMask <- sourceImage * 0 + 1 
    }
  if( is.null( referenceMask ) )
    {
    referenceMask <- referenceImage * 0 + 1 
    }

  sourceArray <- as.array( sourceImage ) 
  sourceMaskArray <- as.array( sourceMask ) 
  sourceMaskedMin <- min( sourceArray[sourceMaskArray != 0] )
  sourceMaskedMax <- max( sourceArray[sourceMaskArray != 0] )

  referenceArray <- as.array( referenceImage ) 
  referenceMaskArray <- as.array( referenceMask ) 

  parametricPoints <- NULL
  if( length( matchPoints ) > 1 )
    {
    parametricPoints <- matchPoints
    } else {
    parametricPoints <- seq( 0, 1, length.out = matchPoints )
    }
  
  sourceIntensityQuantiles <- quantile( sourceArray[sourceMaskArray != 0], probs = parametricPoints )
  referenceIntensityQuantiles <- quantile( referenceArray[referenceMaskArray != 0], probs = parametricPoints )
  displacements <- referenceIntensityQuantiles - sourceIntensityQuantiles

  scatteredData <- matrix( displacements )
  parametricData <- matrix( parametricPoints ) * ( sourceMaskedMax - sourceMaskedMin + sourceMaskedMin )

  transformDomainOrigin <- sourceMaskedMin
  transformDomainSpacing <- ( sourceMaskedMax - transformDomainOrigin ) / ( transformDomainSize - 1 )

  bsplineHistogramTransform <- fitBsplineObjectToScatteredData( scatteredData, parametricData,
    c( transformDomainOrigin ), c( transformDomainSpacing ), c( transformDomainSize ),
    dataWeights = NULL, isParametricDimensionClosed = NULL, numberOfFittingLevels = 8,
    meshSize = 1, splineOrder = 3 )

  transformDomain <- seq( sourceMaskedMin, sourceMaskedMax, length.out = transformDomainSize )

  transformedSourceArray <- as.array( sourceImage )
  for( i in seq.int( length( transformDomain ) - 1 ) )
    {
    indices <- which( sourceArray >= transformDomain[i] & sourceArray < transformDomain[i+1] )
    intensities <- sourceArray[indices]

    alpha <- ( intensities - transformDomain[i] ) / ( transformDomain[i+1] - transformDomain[i] )
    xfrm <- alpha * ( bsplineHistogramTransform[i+1, 1] - bsplineHistogramTransform[i, 1] ) + bsplineHistogramTransform[i, 1]
    transformedSourceArray[indices] <- intensities + xfrm
    }
  transformedSourceImage <- as.antsImage( transformedSourceArray, reference = image )
  transformedSourceImage[sourceMask == 0] <- sourceImage[sourceMask == 0]

  return( transformedSourceImage )
  }
