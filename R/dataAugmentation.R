#' Randomly transform image data.
#'
#' Given an input image list (possibly multi-modal) and an optional corresponding
#' segmentation image list, this function will perform data augmentation with
#' the following augmentation possibilities:  spatial transformations, added
#' image noise, simulated bias field, and histogram warping.
#'
#' @param inputImageList list of lists of input images to warp.  The internal
#' list sets contains one or more images (per subject) which are assumed to be
#' mutually aligned.  The outer list contains multiple subject lists which are
#' randomly sampled to produce output image list.
#' @param segmentationImageList list of segmentation images corresponding to the
#' input image list (optional).
#' @param pointsetList list of pointsets (matrices) corresponding to the
#' input image list (optional).  If using this option, the transformType must
#' be invertible.
#' @param numberOfSimulations number of output images.  Default = 10.
#' @param referenceImage defines the spatial domain for all output images.  If
#' the input images do not match the spatial domain of the reference image, we
#' internally resample the target to the reference image.  This could have
#' unexpected consequences.  Resampling to the reference domain is performed by
#' testing using \code{antsImagePhysicalSpaceConsistency} then calling
#' \code{resampleImageToTarget} upon failure.
#' @param transformType one of the following options
#' \code{c( "translation", "rigid", "scaleShear", "affine"," deformation" ,
#'   "affineAndDeformation" )}.
#' @param noiseModel one of the following options
#' \code{c( "additivegaussian", "saltandpepper", "shot", "speckle", or "random" )}.
#' Alternatively, one can specify an array or list of one or more of the options and
#' one is selected at random with reasonable, randomized parameters.
#' Note that the "speckle" model takes much longer than the others.
#' #' @param noiseParameters 'additivegaussian': \code{c( mean, standardDeviation )},
#'   'saltandpepper': \code{c( probability, saltValue, pepperValue) }, 'shot':
#'    scale, 'speckle': standardDeviation.  Note that the standard deviation,
#'    scale, and probability values are *max* values and are randomly selected
#'    in the range [0, noise_parameter].  Also, the "mean", "saltValue" and
#'    pepperValue" are assumed to be in the intensity normalized range of [0, 1].
#' @param sdSimulatedBiasField Characterize the standard deviation of the amplitude.
#' @param sdHistogramWarping Determines the strength of the histogram transformation.
#' @param sdAffine Determines the amount of affine transformation.
#' @param sdDeformation Determines the amount of deformable transformation.
#' @param outputNumpyFilePrefix Filename of output numpy array containing all the
#'    simulated images and segmentations.
#' @return list of lists of transformed images and/or outputs to a numpy array.
#' @author Tustison NJ
#' @importFrom stats rnorm runif
#' @examples
#'
#' library( ANTsR )
#' image1 <- antsImageRead( getANTsRData( "r16" ) )
#' image2 <- antsImageRead( getANTsRData( "r64" ) )
#' segmentation1 <- thresholdImage( image1, "Otsu", 3 )
#' segmentation2 <- thresholdImage( image2, "Otsu", 3 )
#' points1 = getCentroids( segmentation1 )[,1:2]
#' points2 = getCentroids( segmentation2 )[,1:2]
#' data <- dataAugmentation(
#'   list( list( image1 ), list( image2 ) ),
#'   list( segmentation1, segmentation2 ),
#'   list( points1, points2 ), transformType = 'scaleShear' )
#' rm(segmentation1); gc()
#' rm(segmentation2); gc()
#' rm(image1); gc()
#' rm(image2); gc()
#' @export dataAugmentation
dataAugmentation <- function( inputImageList,
  segmentationImageList = NULL,
  pointsetList = NULL,
  numberOfSimulations = 10,
  referenceImage = NULL,
  transformType = 'affineAndDeformation',
  noiseModel = 'additivegaussian',
  noiseParameters = c( 0.0, 0.05 ),
  sdSimulatedBiasField = 1.0,
  sdHistogramWarping = 0.05,
  sdAffine = 0.05,
  sdDeformation = 0.2, 
  outputNumpyFilePrefix = NULL,
  verbose = FALSE )
{

  if( is.null( referenceImage ) )
    {
    referenceImage <- inputImageList[[1]][[1]]
    }

  numberOfModalities <- length( inputImageList[[1]] )

  # Set up numpy arrays if outputing to file.

  batchX <- NULL
  batchY <- NULL
  batchYpoints <- NULL
  numberOfPoints <- 0

  if( ! is.null( pointsetList ) )
    {
    numberOfPoints <- nrow( pointsetList[[1]] )
    batchYpoints <- array( NA, dim = c( numberOfSimulations, numberOfPoints, referenceImage@dimension ) )
    }
  if( ! is.null( outputNumpyFilePrefix ) )
    {
    batchX <- array( data = 0,
      dim = c( numberOfSimulations, dim( referenceImage ), numberOfModalities ) )
    if( ! is.null( segmentationImageList ) )
      {
      batchY <- array( data = 0, dim = c( numberOfSimulations, dim( referenceImage ) ) )
      }
    }

  # Spatially transform input image data

  if( verbose )
    {
    cat( "Randomly spatially transforming the image data.\n" )
    }

  transformAugmentation <- randomlyTransformImageData(referenceImage,
      inputImageList = inputImageList,
      segmentationImageList = segmentationImageList,
      numberOfSimulations = numberOfSimulations,
      transformType = transformType,
      sdAffine = sdAffine,
      deformationTransformType = "bspline",
      numberOfRandomPoints = 1000,
      sdNoise = sdDeformation,
      numberOfFittingLevels = 4,
      meshSize = 1,
      sdSmoothing = 4.0,
      inputImageInterpolator = 'linear',
      segmentationImageInterpolator = 'nearestNeighbor' )

  simulatedImageList <- list()
  simulatedSegmentationImageList <- list()
  simulatedPointsetList <- list()
  subjectIDs = rep( NA, numberOfSimulations )
  for( i in seq.int( numberOfSimulations ) )
    {
    if( verbose )
      {
      cat( "Processing simulation ", i, "\n" )
      }

    segmentation <- NULL
    if( ! is.null( segmentationImageList ) )
      {
      segmentation <- transformAugmentation$simulatedSegmentationImages[[i]]
      simulatedSegmentationImageList[[i]] <- segmentation
      if( ! is.null( batchY ) )
        {
        if( referenceImage@dimension == 2 )
          {
          batchY[i,,] <- as.array( segmentation )
          } else {
          batchY[i,,,] <- as.array( segmentation )
          }
        }
      }

    whichSubject <- transformAugmentation$whichSubject[i]
    subjectIDs[ i ] = whichSubject
    if( ! is.null( pointsetList ) )
      {
      simulatedTransform <- transformAugmentation$simulatedTransforms[[i]]
      simulatedTransformInverse <- invertAntsrTransform( simulatedTransform )
      simulatedPoints <- applyAntsrTransformToPoint( simulatedTransformInverse, pointsetList[[whichSubject]] )
      simulatedPointsetList[[i]] <- simulatedPoints
      if( ! is.null( batchYpoints ) )
        {
        batchYpoints[i,,] <- simulatedPoints
        }
      }

    simulatedLocalImageList <- list()
    for( j in seq.int( numberOfModalities ) )
      {
      if( verbose )
        {
        cat( "    Modality ", j, "\n" )
        }

      image <- transformAugmentation$simulatedImages[[i]][[j]]
      imageRange <- range( image )

      # Normalize to [0, 1] before applying augmentation

      if( verbose )
        {
        cat( "        Normalizing to [0, 1].\n" )
        }

      image <- iMath( image, "Normalize" )

      # Noise

      if( ! is.null( noiseModel ) )
        {
        if( length( noiseModel ) > 1 )
          {
          noiseModel <- sample( noiseModel, 1 )

          if( noiseModel == "additivegaussian" )
            {
            noiseParameters <- c( runif(1, 0.0, 1.0 ), runif( 1, 0.01, 0.25 ) )
            } else if( noiseModel == "saltandpepper" ) {
            noiseParameters <- c( runif(1, 0.0, 0.25 ), runif( 1, 0.0, 0.25 ), runif( 1, 0.75, 1.0 ) )
            } else if( noiseModel == "shot") {
            noiseParameters <- c( runif( 1, 10, 1000 ) )
            } else if( noiseModel == "shot") {
            noiseParameters <- c( runif( 1, 0.1, 1 ) )
            } else {
            stop( "Unrecognized noise model." )
            }
          }

        if( verbose )
          {
          cat( "        Adding noise (",  noiseModel,  ").\n" )
          }

        if( tolower( noiseModel ) == "additivegaussian" )
          {
          parameters <- c( noiseParameters[1], runif( 1, min = 0.0, max = noiseParameters[2] ) )
          image <- addNoiseToImage( image, noiseModel = "additivegaussian", noiseParameters = parameters )
          } else if( tolower( noiseModel ) == "saltandpepper" ) {
          parameters <- c( runif( 1, min = 0.0, max = noiseParameters[1] ), noiseParameters[2], noiseParameters[3] )
          image <- addNoiseToImage( image, noiseModel = "saltandpepper", noiseParameters = parameters )
          } else if( tolower( noiseModel ) == "shot" ) {
          parameters <- c( runif(1, min = 0.0, max = noiseParameters[1] ) )
          image <- addNoiseToImage( image, noiseModel = "shot", noiseParameters = parameters )
          } else if( tolower( noiseModel ) == "speckle" ) {
          parameters <- c( runif( 1, min = 0.0, max = noiseParameters[1] ) )
          image <- addNoiseToImage( image, noiseModel = "speckle", noiseParameters = parameters )
          } else {
          stop( "Unrecognized noise model." )
          }
        }

      # Simulated bias field

      if ( sdSimulatedBiasField > 0 )
        {

        if( verbose )
          {
          cat( "        Adding simulated bias field.\n" )
          }

        logField <- simulateBiasField(image, numberOfPoints = 10,
          sdBiasField = sdSimulatedBiasField, numberOfFittingLevels = 2, meshSize = 10 ) %>%
          iMath( "Normalize" )
        logField <- ( exp( logField ) )^sample( c( 2, 3, 4 ), 1 )
        image <- image * logField
        }

      # Histogram intensity warping

      if ( sdHistogramWarping > 0 )
        {

        if( verbose )
          {
          cat( "        Performing intensity histogram warping.\n" )
          }

        breakPoints <- c( 0.2, 0.4, 0.6, 0.8 )
        displacements = rnorm( length( breakPoints ), mean = 0, sd = sdHistogramWarping )
        image <- histogramWarpImageIntensities( image, breakPoints = breakPoints,
            clampEndPoints = c( FALSE, FALSE ), displacements = displacements )
        }

      # Rescale to original intensity range

      if( verbose )
        {
        cat( "        Rescaling to original intensity range.\n" )
        }

      image <- iMath( image, "Normalize" ) * ( imageRange[2] - imageRange[1] ) + imageRange[1]

      simulatedLocalImageList[[j]] <- image

      if( ! is.null( batchX ) )
        {
        if( referenceImage@dimension == 2 )
          {
          batchX[i,,,j] <- as.array( image )
          } else {
          batchX[i,,,,j] <- as.array( image )
          }
        }
      }

    simulatedImageList[[i]] <- simulatedLocalImageList
    }

  if( ! is.null( batchX ) )
    {
    np <- reticulate::import( "numpy" )
    if( verbose )
      {
      cat( "Writing images to numpy array\n" )
      }
    if( ! is.null( outputNumpyFilePrefix ) )
      {
      np$save( paste0( outputNumpyFilePrefix, "SimulatedImages.npy"), batchX  )
      }
    }

  if( ! is.null( batchY ) )
    {
    np <- reticulate::import( "numpy" )
    if( verbose )
      {
      cat( "Writing segmentation images to numpy array\n" )
      }
    if( ! is.null( outputNumpyFilePrefix ) )
      {
      np$save( paste0( outputNumpyFilePrefix, "SimulatedSegmentationImages.npy"), batchY )
      }
    }

  if( ! is.null( batchYpoints ) )
    {
    np <- reticulate::import( "numpy" )
    if( verbose )
      {
      cat( "Writing points to numpy array\n" )
      }
    if( ! is.null( outputNumpyFilePrefix ) )
      {
      np$save( paste0( outputNumpyFilePrefix, "SimulatedPointsets.npy"), batchYpoints )
      }
    }

  if( is.null( segmentationImageList ) & is.null( pointsetList ) )
    {
    return( list( simulatedImages = simulatedImageList,
      subjectIDs = subjectIDs ) )
    } else if( is.null( segmentationImageList ) ) {
    return( list( simulatedImages = simulatedImageList,
                  simulatedPointsetList = simulatedPointsetList,
                  subjectIDs = subjectIDs ) )
    } else if( is.null( pointsetList ) ) {
    return( list( simulatedImages = simulatedImageList,
                  simulatedSegmentationImages = simulatedSegmentationImageList,
                  subjectIDs = subjectIDs ) )
    } else {
    return( list( simulatedImages = simulatedImageList,
                  simulatedSegmentationImages = simulatedSegmentationImageList,
                  simulatedPointsetList = simulatedPointsetList,
                  subjectIDs = subjectIDs ) )

    }

}
