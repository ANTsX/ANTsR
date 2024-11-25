#' Perform label image registration.
#'
#' Perform pairwise registration using fixed and moving sets of label 
#' images (and, optionally, sets of corresponding intensity images).
#'
#' @param fixedLabelImages A single (or set of) fixed label image(s).
#' @param movingLabelImages A single (or set of) moving label image(s).
#' @param fixedIntensityImages Optional---a single (or set of) fixed 
#' intensity image(s).
#' @param movingIntensityImages Optional---a single (or set of) moving 
#' intensity image(s).
#' @param fixedMask Defines region for similarity metric calculation 
#' in the space of the fixed image.
#' @param movingMask Defines region for similarity metric calculation 
#' in the space of the moving image.
#' @param typeOfLinearTransform Use label images with the centers of 
#' mass to a calculate linear transform of type \code{'rigid'}, 
#' \code{'similarity'}, \code{'affine'}.
#' @param typeOfTransform Only works with deformable-only transforms, 
#' specifically the family of \code{antsRegistrationSyN*[so]} or 
#' \code{antsRegistrationSyN*[bo]} transforms.  See 'type_of_transform' 
#' in \code{antsRegistration}.
#' @param labelImageWeighting Float or vector of floats giving the relative 
#' weighting for the label images.
#' @param outputPrefix String definining the output prefix for the filenames
#' of the output transform files.
#' @param randomSeed Definition for deformable registration.
#' @param verbose Print progress to the screen.
#' @return outputs a list containing:
#' \itemize{
#'   \item{fwdtransforms: }{Transforms to move from moving to fixed image.}
#'   \item{invtransforms: }{Transforms to move from fixed to moving image.}
#' }
#' Output of 1 indicates failure.
#' @author Tustison NJ
#' @examples
#' \dontrun{
#' r16 <- antsImageRead( getANTsRData( "r16" ) )
#' r16Seg1 <- thresholdImage( r16, "Kmeans", 3 ) - 1
#' r16Seg2 <- thresholdImage( r16, "Kmeans", 5 ) - 1
#' r64 <- antsImageRead( getANTsRData( "r64" ) )
#' r64Seg1 <- thresholdImage( r64, "Kmeans", 3 ) - 1
#' r64Seg2 <- thresholdImage( r64, "Kmeans", 5 ) - 1
#' reg <- labelImageRegistration( list( r16Seg1, r16Seg2 ),
#'                                      list( r64Seg1, r64Seg2 ),  
#'                                      fixedIntensityImages = r16,
#'                                      movingIntensityImages = r64,
#'                                      typeOfLinearTransform = 'affine',
#'                                      typeOfTransform = 'antsRegistrationSyNQuick[bo]',
#'                                      labelImageWeighting = c( 1.0, 2.0 ),
#'                                      verbose = TRUE )
#' }
#'
#' @export labelImageRegistration
labelImageRegistration <- function( fixedLabelImages, movingLabelImages,
    fixedIntensityImages = NULL, movingIntensityImages = NULL,
    fixedMask = NULL, movingMask = NULL,
    typeOfLinearTransform = 'affine', 
    typeOfTransform = 'antsRegistrationSyNQuick[so]',
    labelImageWeighting = 1.0,
    outputPrefix = '',
    randomSeed = NULL, 
    verbose = FALSE )
{     
  # Preform validation check on the input

  if( ANTsR::is.antsImage( fixedLabelImages ) )
    {
    fixedLabelImages <- list( antsImageClone( fixedLabelImages ) )
    }
  if( ANTsR::is.antsImage( movingLabelImages ) )
    {
    movingLabelImages <- list( antsImageClone( movingLabelImages ) )
    }

  if( length( fixedLabelImages ) != length( movingLabelImages ) ) 
    {
    stop( "The number of fixed and moving label images do not match." )
    }
  
  if( ! is.null( fixedIntensityImages ) || ! is.null( movingIntensityImages ) )
    {
    if( ANTsR::is.antsImage( fixedIntensityImages ) )
      {
      fixedIntensityImages <- list( antsImageClone( fixedIntensityImages ) )
      }
    if( ANTsR::is.antsImage( movingIntensityImages ) )
      {
      movingIntensityImages <- list( antsImageClone( movingIntensityImages ) )
      }
    if( length( fixedIntensityImages ) != length( movingIntensityImages ) ) 
      {
      stop( "The number of fixed and moving intensity images do not match." )
      }
    }
    
  labelImageWeights <- labelImageWeighting
  if( is.numeric( labelImageWeighting ) && ! is.vector( labelImageWeighting ) )
    {
    labelImageWeights <- rep( labelImageWeighting, length( fixedLabelImages ) ) 
    } else {
    if( length( fixedLabelImages ) != length( labelImageWeights ) )
      {
      stop( paste( "The length of labelImageWeights must",
                   "match the number of label image pairs." ) )
      }
    }

  imageDimension <- fixedLabelImages[[1]]@dimension

  if( outputPrefix == '' || is.null( outputPrefix ) || length( outputPrefix ) == 0 )
    {
    outputPrefix <- tempfile()
    }

  allowableLinearTransforms <- c( 'rigid', 'similarity', 'affine' )
  if( ! typeOfLinearTransform %in% allowableLinearTransforms )
    {
    stop( "Unrecognized linear transform." ) 
    }

  doDeformable <- TRUE
  if( is.null( typeOfTransform ) || length( typeOfTransform ) == 0 )
    {
    doDeformable <- FALSE
    }

  commonLabelIds <- list()
  totalNumberOfLabels <- 0
  for( i in seq.int( length( fixedLabelImages ) ) )
    {
    fixedLabelGeoms <- labelGeometryMeasures( fixedLabelImages[[i]] )
    fixedLabelIds <-fixedLabelGeoms$Label
    movingLabelGeoms <- labelGeometryMeasures( movingLabelImages[[i]] )
    movingLabelIds <- movingLabelGeoms$Label
    commonLabelIds[[i]] <- intersect( fixedLabelIds, movingLabelIds )
    totalNumberOfLabels = totalNumberOfLabels + length( commonLabelIds[[i]] )
    if( verbose )
      {
      message( paste0( "Common label ids for image pair ", i, ": ", commonLabelIds[i] ) ) 
      }
    if( length( commonLabelIds[[i]]) == 0 )
      {
      stop( paste0( "No common labels for image pair ", i ) )
      }
    }
  
  if( verbose )
    {
    message( "Total number of labels: ", totalNumberOfLabels )
    }

  ##############################
  #
  #    Linear transform
  #
  ##############################

  linearXfrm <- NULL
  if( ! is.null( typeOfLinearTransform ) )
    {
    if( verbose )
      {
      message( "\n\nComputing linear transform.\n" )
      }

    if( totalNumberOfLabels < 3 )
      {
      stop( "  Number of labels must be >= 3." ) 
      }  

    fixedCentersOfMass <- array( data = 0, c( totalNumberOfLabels, imageDimension ) )  
    movingCentersOfMass <- array( data = 0, c( totalNumberOfLabels, imageDimension ) )  

    deformableMultivariateExtras <- list()

    count <- 1
    for( i in seq.int( length( commonLabelIds ) ) )
      {
      for( j in seq.int( length( commonLabelIds[[i]] ) ) )
        {
        label <- commonLabelIds[[i]][j]
        if( verbose )
          {
          message( paste0( "  Finding centers of mass for image pair ", i, ", label ", label ) )
          }
        fixedSingleLabelImage <- thresholdImage( fixedLabelImages[[i]], label, label, 1, 0 )  
        fixedCentersOfMass[count,] <- getCenterOfMass( fixedSingleLabelImage )
        movingSingleLabelImage <- thresholdImage( movingLabelImages[[i]], label, label, 1, 0 )  
        movingCentersOfMass[count,] <- getCenterOfMass( movingSingleLabelImage )
        count <- count + 1
        if( doDeformable ) 
          {
          deformableMultivariateExtras[[i]] <- list( "MSQ", fixedSingleLabelImage,
                                                     movingSingleLabelImage, 
                                                     labelImageWeights[i], 0)
          }
        } 
      }

    linearXfrm <- fitTransformToPairedPoints( movingCentersOfMass, 
                                              fixedCentersOfMass, 
                                              transformType = typeOfLinearTransform,
                                              verbose = verbose )

    linearXfrmFile <- paste0( outputPrefix, "0GenericAffine.mat" )
    writeAntsrTransform( linearXfrm, linearXfrmFile )
    }

  ##############################
  #
  #    Deformable transform
  #
  ##############################

  if( doDeformable )
    {
    if( verbose )
      {
      message( "\n\nComputing deformable transform using images.\n" )
      }

    doQuick <- FALSE
    doRepro <- FALSE 

    if( grepl( "Quick", typeOfTransform ) )
      {
      doQuick <- TRUE
      } else {
      doRepro <- TRUE
      randomSeed <- 1
      }

    intensityMetricParameter <- NULL
    splineDistance <- 26
    if( grepl( "\\[", typeOfTransform ) && grepl("\\]", typeOfTransform ) ) 
      {
      subtypeOfTransform <- strsplit( strsplit( typeOfTransform, "\\[")[[1]][2], "\\]" )[[1]][1]
      if( ! ( grepl( "bo", subtypeOfTransform ) || grepl( "so", subtypeOfTransform ) ) )
        {
        stop( "Only 'so' or 'bo' transforms are available." ) 
        }
      if( grepl( ",", subtypeOfTransform ) ) 
        {
        subtypeOfTransformArgs <- strsplit( subtypeOfTransform, "," )[[1]]
        subtypeOfTransform <- subtypeOfTransformArgs[1]
        intensityMetricParameter <- subtypeOfTransformArgs[2]
        if( length( subtypeOfTransformArgs ) > 2 ) 
          {
          splineDistance <- subtypeOfTransformArgs[3]
          }
        }
      }

    synTransform <- "SyN[0.1,3,0]"
    if( subtypeOfTransform == "bo" ) 
      {
      synTransform <- paste0("BSplineSyN[0.1,", splineDistance, ",0,3]")
      }
    synStage <- list( "--transform", synTransform )

    if( doQuick ) 
      {
      synConvergence <- "[100x70x50x0,1e-6,10]"
      } else {
        synConvergence <- "[100x70x50x20,1e-6,10]"
      }
    synShrinkFactors <- "8x4x2x1"
    synSmoothingSigmas <- "3x2x1x0vox"

    synStage <- lappend( synStage, list(
      "--convergence", synConvergence,
      "--shrink-factors", synShrinkFactors,
      "--smoothing-sigmas", synSmoothingSigmas
      ) )

    intensityMetric <- NULL
    if( ! is.null( fixedIntensityImages ) && ! is.null( movingIntensityImages ) )
      {
      if( doQuick )
        {
        intensityMetric <- "MI" 
        if( is.null( intensityMetricParameter ) )
          {
          intensityMetricParameter <- 32 
          }
        }
      if( ! doQuick || doRepro )
        {
        intensityMetric <- "CC" 
        if( is.null( intensityMetricParameter ) )
          {
          intensityMetricParameter <- 2
          }
        }
      for( i in seq.int( length( fixedIntensityImages ) ) )  
        {
        synStage <- lappend( synStage, list(
            "--metric", paste0( intensityMetric, "[",
              antsrGetPointerName( fixedIntensityImages[[i]] ), ",",
              antsrGetPointerName( movingIntensityImages[[i]] ), ",",
              "1.0,", as.character( intensityMetricParameter ), "]" ) ) )  
        }
      }
    for( kk in seq.int( length( deformableMultivariateExtras ) ) )
      {
      synStage <- lappend( synStage, list(
          "--metric", paste0( "MSQ[",
            antsrGetPointerName( deformableMultivariateExtras[[kk]][[2]] ), ",",
            antsrGetPointerName( deformableMultivariateExtras[[kk]][[3]] ), ",",
            as.character( deformableMultivariateExtras[[kk]][[4]] ), ",0.0]" ) ) )  
      } 

    args <- NULL
    if( is.null( linearXfrm ) )
      {
      args <- list(
        "-d", as.character( imageDimension ),
        "-o", outputPrefix )
      } else {
      args <- list(
        "-d", as.character( imageDimension ),
        "-r", linearXfrmFile,
        "-o", outputPrefix )
      }
    args <- lappend( args, synStage ) 

    fixedMaskString <- "NA"
    if( ! is.null( fixedMask ) )
      {
      fixedMaskBinary <- antsImageClone( thresholdImage( fixedMask, 0, 0, 0, 1), "unsigned char" ) 
      fixedMaskString <- antsrGetPointerName( fixedMaskBinary )
      }

    movingMaskString <- "NA"
    if( ! is.null( movingMask ) )
      {
      movingMaskBinary <- antsImageClone( thresholdImage( movingMask, 0, 0, 0, 1), "unsigned char" ) 
      movingMaskString <- antsrGetPointerName( movingMaskBinary )
      }

    maskOption <- paste0( "[", fixedMaskString, ",", movingMaskString, "]" )
    args <- lappend( args, list( "-x", maskOption ) )

    args <- lappend( args, list( "--float", "1" ) )

    if( ! is.null( randomSeed ) )
      {
      args <- lappend( "--random-seed", randomSeed )
      }

    if( verbose )  
      {
      args <- lappend( args, list( "-v", "1" ) )
      cat( "antsRegistration", paste( unlist( args ) ), "\n" )
      }
    args <- .int_antsProcessArguments( c( args ) )
    ANTsRCore::AntsRegistration( args )
    }

  allXfrms <- Sys.glob( paste0( outputPrefix, "*", "[0-9]*" ) )

  findInverseWarps <- grep( "[0-9]InverseWarp.nii.gz", allXfrms )
  findForwardWarps <- grep( "[0-9]Warp.nii.gz", allXfrms )

  if( length( findInverseWarps ) > 0 ) 
    {
    fwdtransforms <- c( allXfrms[findForwardWarps[1]], linearXfrmFile ) 
    invtransforms <- c( linearXfrmFile, allXfrms[findInverseWarps[1]] ) 
    } else {
    fwdtransforms <- c( linearXfrmFile ) 
    invtransforms <- c( linearXfrmFile ) 
    }

  if( verbose )
    {
    message( "\n\nResulting transforms" )   
    if( length( findInverseWarps ) > 0 ) 
      {
      message( paste0( "  fwdtransforms: [", fwdtransforms[1], ", ", fwdtransforms[2], "]" ) )
      message( paste0( "  invtransforms: [", invtransforms[1], ", ", invtransforms[2], "]" ) )
      } else {
      message( paste0( "  fwdtransforms: [", fwdtransforms[1], "]" ) )
      message( paste0( "  invtransforms: [", invtransforms[1], "]" ) )
      }
    }

  return( list( fwdtransforms = fwdtransforms,
                invtransforms = invtransforms ) )
}