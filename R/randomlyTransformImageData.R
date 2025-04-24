#' Randomly transform image data (optional: with corresponding segmentations).
#'
#' Apply rigid, affine and/or deformable maps to an input set of training
#' images.  The reference image domain defines the space in which this happens.
#'
#' @param referenceImage defines the spatial domain for all output images.  If
#' the input images do not match the spatial domain of the reference image, we
#' internally resample the target to the reference image.  This could have
#' unexpected consequences.  Resampling to the reference domain is performed by
#' testing using \code{antsImagePhysicalSpaceConsistency} then calling
#' \code{resampleImageToTarget} upon failure.
#' @param inputImageList list of lists of input images to warp.  The internal
#' list sets contains one or more images (per subject) which are assumed to be
#' mutually aligned.  The outer list contains multiple subject lists which are
#' randomly sampled to produce output image list.
#' @param segmentationImageList list of segmentation images corresponding to the
#' input image list (optional).
#' @param numberOfSimulations number of output images.  Default = 10.
#' @param transformType one of the following options
#' \code{c( "translation", "rotation", "rigid", "scaleShear", "affine"," deformation" ,
#'   "affineAndDeformation" )}.  Default = \"affine\".
#' @param sdAffine parameter dictating deviation amount from identity for
#' random linear transformations.  Default = 0.02.
#' @param deformationTransformType one of the following options
#' \code{c( "bspline", "exponential" )} if deformation is specified in the
#' \code{transformType}.  Default = \"bspline\".
#' @param numberOfRandomPoints  number of displacement points for the deformation
#' field.  Default = 1000.
#' @param sdNoise standard deviation of the displacement field
#' noise (in mm).  Default = 10.0.
#' @param numberOfFittingLevels (bspline deformation only) number of fitting levels.
#' Default = 4.
#' @param meshSize (bspline deformation only) scalar or n-D vector determining fitting
#' resolution.  Default = 1.
#' @param sdSmoothing (exponential deformation only) standard deviation of the
#' Gaussian smoothing in mm.  Default = 4.0.
#' @param inputImageInterpolator one of the following options
#' \code{ c( "linear", "gaussian", "bspline" )}.  Default = \"linear\".
#' @param segmentationImageInterpolator one of the following options
#' \code{ c( "nearestNeighbor", "genericLabel" )}.  Default =
#' \"nearestNeighbor\".
#' @return list (if no directory set) or boolean for success, failure
#' @author Avants BB, Tustison NJ
#' @importFrom stats rnorm
#' @examples
#'
#' library( ANTsR )
#' image1 <- antsImageRead( getANTsRData( "r16" ) )
#' image2 <- antsImageRead( getANTsRData( "r64" ) )
#' segmentation1 <- thresholdImage( image1, "Otsu", 3 )
#' segmentation2 <- thresholdImage( image2, "Otsu", 3 )
#' data <- randomlyTransformImageData( image1,
#'   list( list( image1 ), list( image2 ) ),
#'   list( segmentation1, segmentation2 ) )
#' rm(segmentation1); gc()
#' rm(segmentation2); gc()
#' rm(image1); gc()
#' rm(image2); gc()
#' @export randomlyTransformImageData
randomlyTransformImageData <- function( referenceImage,
  inputImageList,
  segmentationImageList = NULL,
  numberOfSimulations = 10,
  transformType = 'affine',
  sdAffine = 0.02,
  deformationTransformType = c( "bspline", "exponential" ),
  numberOfRandomPoints = 1000,
  sdNoise = 10.0,
  numberOfFittingLevels = 4,
  meshSize = 1,
  sdSmoothing = 4.0,
  inputImageInterpolator = 'linear',
  segmentationImageInterpolator = 'nearestNeighbor' )
{
  createRandomLinearTransform <- function(
    image, fixedParameters, transformType = 'affine', sdAffine = 0.02 )
    {

    polarDecomposition <- function( X )
      {
      svdX <- svd( X )
      P <- svdX$u %*% diag( svdX$d ) %*% t( svdX$u )
      Z <- svdX$u %*% t( svdX$v )
      if( det( Z ) < 0 )
        {
        D <- diag( nrow( X ) )
        D[1, 1] <- -1.0
        Z <- Z %*% D
        }
      return( list( P = P, Z = Z, Xtilde = P %*% Z ) )
      }

    transform <- createAntsrTransform( precision = "float",
      type = "AffineTransform", dimension = image@dimension )
    setAntsrTransformFixedParameters( transform, fixedParameters )
    identityParameters <- getAntsrTransformParameters( transform )

    randomEpsilon <- stats::rnorm( length( identityParameters ), mean = 0,
      sd = sdAffine )
    if( transformType == 'translation' )
      {
      randomEpsilon[1:( length( identityParameters ) - image@dimension )] <- 0
      } else if( transformType == 'rotation' ) {
      randomEpsilon[( length( identityParameters ) - image@dimension ):length( randomEpsilon )] <- 0
      }

    randomParameters <- identityParameters + randomEpsilon
    randomMatrix <- matrix( randomParameters[
      1:( length( randomParameters ) - image@dimension )],
        ncol = image@dimension )
    decomposition <- polarDecomposition( randomMatrix )

    if( transformType == "rotation" || transformType == "rigid" )
      {
      randomMatrix <- decomposition$Z
      }
    if( transformType == "affine" )
      {
      randomMatrix <- decomposition$Xtilde
      }
    if( transformType == "scaleShear" )
      {
      randomMatrix <- decomposition$P
      }

    randomParameters[1:( length( identityParameters ) - image@dimension )] <-
      as.numeric( randomMatrix )
    setAntsrTransformParameters( transform, randomParameters )
    return( transform )
    }

  createRandomDisplacementFieldTransform <- function( domainImage,
    fieldType = c( "bspline", "exponential" ), numberOfRandomPoints = 1000,
    sdNoise = 10.0, numberOfFittingLevels = 4, meshSize = 1, sdSmoothing = 4.0 )
    {
    displacementField <- simulateDisplacementField( domainImage,
      fieldType = fieldType, numberOfRandomPoints = numberOfRandomPoints,
      sdNoise = sdNoise, enforceStationaryBoundary = TRUE,
      numberOfFittingLevels = numberOfFittingLevels, meshSize = meshSize,
      sdSmoothing = sdSmoothing )
    displacementFieldTransform <- antsrTransformFromDisplacementField( displacementField )
    return( displacementFieldTransform )
    }

  admissibleTransforms <- c( "translation", "rotation", "rigid", "scaleShear", "affine",
    "affineAndDeformation", "deformation" )
  if( !( transformType %in% admissibleTransforms ) )
    {
    stop( paste0( "The specified transform, ", transformType,
      " is not a possible option.  Please see help menu." ) )
    }

  # Get the fixed parameters from the reference image.

  fixedParameters <- getCenterOfMass( referenceImage  )
  numberOfSubjects <- length( inputImageList )

  randomIndices <- sample( numberOfSubjects, size = numberOfSimulations,
    replace = TRUE )

  simulatedImageList <- list()
  simulatedSegmentationImageList <- list()
  simulatedTransforms <- list()
  for( i in seq_len( numberOfSimulations ) )
    {
    singleSubjectImageList <- inputImageList[[randomIndices[i]]]
    singleSubjectSegmentationImage <- NULL
    if( ! is.null( segmentationImageList[[1]] ) )
      {
      singleSubjectSegmentationImage <- segmentationImageList[[randomIndices[i]]]
      }

    if( !antsImagePhysicalSpaceConsistency( referenceImage, singleSubjectImageList[[1]] ) )
      {
      for( j in seq_len( length( singleSubjectImageList ) ) )
        {
        singleSubjectImageList[[j]] <- resampleImageToTarget(
          singleSubjectImageList[[j]], referenceImage,
            interpType = inputImageInterpolator )
        }
      if( ! is.null( singleSubjectSegmentationImage ) )
        {
        singleSubjectSegmentationImage <- resampleImageToTarget(
          singleSubjectSegmentationImage, referenceImage,
            interpType = segmentationImageInterpolator )
        }
      }

    transforms <- list()
    if( transformType == 'deformation' )
      {
      deformableTransform <- createRandomDisplacementFieldTransform(
        referenceImage, deformationTransformType, numberOfRandomPoints,
        sdNoise, numberOfFittingLevels, meshSize, sdSmoothing )
      transforms <- list( deformableTransform )
      }
    if( transformType == 'affineAndDeformation' )
      {
      deformableTransform <- createRandomDisplacementFieldTransform(
        referenceImage, deformationTransformType, numberOfRandomPoints,
        sdNoise, numberOfFittingLevels, meshSize, sdSmoothing )
      linearTransform <- createRandomLinearTransform( referenceImage,
        fixedParameters, 'affine', sdAffine )
      transforms <- list( deformableTransform, linearTransform )
      }
    if( transformType %in% admissibleTransforms[1:4] )
      {
      linearTransform <- createRandomLinearTransform( referenceImage,
        fixedParameters, transformType, sdAffine )
      transforms <- list( linearTransform )
      }

    simulatedTransforms[[i]] <- composeAntsrTransforms( transforms )

    singleSubjectSimulatedImageList <- list()
    for( j in seq_len( length( singleSubjectImageList ) ) )
      {
      singleSubjectImage <- singleSubjectImageList[[j]]
      singleSubjectSimulatedImageList[[j]] <- applyAntsrTransform(
        simulatedTransforms[[i]], singleSubjectImage, referenceImage,
        interpolation = inputImageInterpolator )
      }
    simulatedImageList[[i]] <- singleSubjectSimulatedImageList

    if( ! is.null( singleSubjectSegmentationImage ) )
      {
      simulatedSegmentationImageList[[i]] <- applyAntsrTransform(
        simulatedTransforms[[i]], singleSubjectSegmentationImage, referenceImage,
          interpolation = segmentationImageInterpolator )
      }
    }

  if( is.null( segmentationImageList[[1]] ) )
    {
    return( list( simulatedImages = simulatedImageList,
                  simulatedTransforms = simulatedTransforms,
                  whichSubject = randomIndices ) )
    }
  else
    {
    return( list( simulatedImages = simulatedImageList,
      simulatedSegmentationImages = simulatedSegmentationImageList,
      simulatedTransforms = simulatedTransforms,
      whichSubject = randomIndices ) )
    }
}
