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
#' @param numberOfIterations number of Atropos <--> N4 iterations (outer loop).
#' @param numberOfAtroposIterations number of Atropos iterations (inner loop).
#' If \code{numberOfAtroposIterations = 0}, this is equivalent to K-means with
#' no MRF priors.
#' @param mrfParameters parameters for MRF in Atropos.
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
functionalLungSegmentation <- function( image, mask, numberOfIterations = 0,
  numberOfAtroposIterations = 0, mrfParameters = "[0.3,2x2x2]", verbose = TRUE )
  {

  if( image@dimension != 3 )
    {
    stop( "Function only works for 3-D images." )
    }

  if( missing( mask ) )
    {
    stop( "Mask is missing." )
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
      message( paste0( "Atropos/N4 iteration: ", i, " out of ", numberOfAtroposN4Iterations, "\n" ) )
      }

    preprocessedImage <- antsImageClone( image )

    quantiles <- quantile( preprocessedImage, c( 0, 0.995 ) )
    preprocessedImage[preprocessedImage < quantiles[1]] <- quantiles[1]
    preprocessedImage[preprocessedImage > quantiles[2]] <- quantiles[2]

    if( verbose == TRUE )
      {
      message( paste0( "Atropos/N4:  N4 bias correction.\n" ) )
      }
    preprocessedImage <- n4BiasFieldCorrection( preprocessedImage, mask = dilatedMask,
      shrinkFactor = 2, convergence = list( iters = c( 50, 50, 50, 50 ), tol = 0.0000000001 ),
      splineParam = 200, returnBiasField = FALSE, weight_mask = weightMask, verbose = verbose )
    preprocessedImage <- 1000 * ( preprocessedImage - min( preprocessedImage ) ) /
      ( max( preprocessedImage ) - min( preprocessedImage) )

    if( verbose == TRUE )
      {
      message( paste0( "Atropos/N4:  Atropos segmentation.\n" ) )
      }
    atroposInitialization <- "Kmeans[4]"
    posteriorFormulation <- "Socrates[0]"
    if( i > 1 )
      {
      atroposInitialization <- atroposOutput$probabilityimages
      posteriorFormulation = "Socrates[1]"
      }
    iterations <- paste0( "[", numberOfAtroposIterations, ",0]" )
    atroposOutput <- atropos( preprocessedImage, x = dilatedMask, i = atroposInitialization,
      m = mrfParameters, c = iterations, priorweight = 0.0, verbose = verbose, p = posteriorFormulation )

    weightMask <- generatePureTissueN4WeightMask( atroposOutput$probabilityimages[2:4] )
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
