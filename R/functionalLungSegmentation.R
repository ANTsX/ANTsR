#' Ventilation-based segmentation of hyperpolarized gas lung MRI.
#'
#' Lung segmentation into four classes based on ventilation as described in
#' this paper:
#'
#'     \url{https://pubmed.ncbi.nlm.nih.gov/21837781/}
#'
#' @param image input proton-weighted MRI.
#' @param mask mask image designating the region to segment.
#' 0/1 = background/foreground.
#' @param numberOfIterations number of Atropos <--> bias correction iterations
#' (outer loop).
#' @param numberOfAtroposIterations number of Atropos iterations (inner loop).
#' If \code{numberOfAtroposIterations = 0}, this is equivalent to K-means with
#' no MRF priors.
#' @param mrfParameters parameters for MRF in Atropos.
#' @param numberOfClusters number of tissue classes (default = 4)
#' @param clusterCenters initialization centers for k-means
#' @param biasCorrection apply n3, n4, or no bias correction (default = "n4").
#' @param verbose print progress to the screen.
#' @return segmentation image, probability images, and processed input
#' image.
#' @author Tustison NJ
#' @examples
#' \dontrun{
#'
#' library( ANTsR )
#'
#' image <- antsImageRead( "lung.nii.gz" )
#' mask <- antsImageRead( "mask.nii.gz" )
#' output <- functionalLungSegmentation( image, mask )
#' antsImageWrite( output$segmentationImage, "outputSegmentation.nii.gz" )
#'
#' }
#' @export
functionalLungSegmentation <- function( image, mask, numberOfIterations = 2,
  numberOfAtroposIterations = 5, mrfParameters = "[0.7,2x2x2]",
  numberOfClusters = 6, clusterCenters = NA, biasCorrection = "n4",
  verbose = TRUE )
  {

  if( image@dimension != 3 )
    {
    stop( "Function only works for 3-D images." )
    }

  if( missing( mask ) )
    {
    stop( "Mask is missing." )
    }

  if( numberOfIterations < 1 )
    {
    stop( "numberOfIterations must be >= 1.")
    }

  generatePureTissueN4WeightMask <- function( probabilityImages )
    {
    numberOfProbabilityImages <- length( probabilityImages )

    pureTissueMask <- antsImageClone( probabilityImages[[1]] ) * 0
    for( i in seq.int( numberOfProbabilityImages ) )
      {
      negationImage <- antsImageClone( probabilityImages[[1]] ) * 0 + 1
      for( j in seq.int( numberOfProbabilityImages ) )
        {
        if( i != j )
          {
          negationImage <- negationImage * ( 1 - probabilityImages[[j]] )
          }
        pureTissueMask <- pureTissueMask + negationImage * probabilityImages[[i]]
        }
      }
    return( pureTissueMask )
    }

  dilatedMask <- mask %>% iMath( "MD", 5 )
  weightMask <- NULL

  numberOfAtroposN4Iterations <- numberOfIterations
  for( i in seq.int( numberOfAtroposN4Iterations ) )
    {
    if( verbose == TRUE )
      {
      message( paste0( "Outer: ", i, " out of ", numberOfAtroposN4Iterations, "\n" ) )
      }

    preprocessedImage <- antsImageClone( image )

    quantiles <- quantile( preprocessedImage, c( 0, 0.995 ) )
    preprocessedImage[preprocessedImage < quantiles[1]] <- quantiles[1]
    preprocessedImage[preprocessedImage > quantiles[2]] <- quantiles[2]

    if( verbose == TRUE )
      {
      message( paste0( "Outer:  bias correction.\n" ) )
      }

    if( tolower( biasCorrection ) == "n4" )
      {
      preprocessedImage <- n4BiasFieldCorrection( preprocessedImage, mask = dilatedMask,
        shrinkFactor = 2, convergence = list( iters = c( 50, 50, 50, 50 ), tol = 0.0000000001 ),
        splineParam = 200, returnBiasField = FALSE, weight_mask = weightMask, verbose = verbose )
      } else if( tolower( biasCorrection ) == "n3" ) {
      preprocessedImage <- n3BiasFieldCorrection( preprocessedImage, downsampleFactor = 2 )
      }
    preprocessedImage <- ( preprocessedImage - min( preprocessedImage ) ) /
      ( max( preprocessedImage ) - min( preprocessedImage) )

    if( verbose == TRUE )
      {
      message( paste0( "Outer:  Atropos segmentation.\n" ) )
      }
    atroposInitialization <- paste0( "Kmeans[", numberOfClusters, "]" )
    if( ! all( is.na( clusterCenters ) ) )
      {
      if( length( clusterCenters ) != numberOfClusters )
        {
        stop( "numberOfClusters should match the vector size of the clusterCenters." )
        } else {
        clusterCentersString <- paste0( clusterCenters, collapse = "x" )
        atroposInitialization <- paste0( "Kmeans[", numberOfClusters, ",", clusterCentersString, "]" )
        }
      }
    posteriorFormulation <- "Socrates[0]"
    if( i > 1 )
      {
      atroposInitialization <- atroposOutput$probabilityimages
      posteriorFormulation = "Socrates[1]"
      }
    iterations <- paste0( "[", numberOfAtroposIterations, ",0]" )
    atroposOutput <- atropos( preprocessedImage, x = dilatedMask, i = atroposInitialization,
      m = mrfParameters, c = iterations, priorweight = 0.0, verbose = verbose, p = posteriorFormulation )

    weightMask <- generatePureTissueN4WeightMask( atroposOutput$probabilityimages[2:numberOfClusters] )
    }

  maskedSegmentationImage <- atroposOutput$segmentation * mask
  maskedProbabilityImages <- list()
  for( i in seq.int( length( atroposOutput$probabilityimages ) ) )
    {
    maskedProbabilityImages[[i]] <- atroposOutput$probabilityimages[[i]] * mask
    }

  return( list( segmentationImage = maskedSegmentationImage,
                probabilityImages = maskedProbabilityImages ,
                processedImage = preprocessedImage ) )
  }
