#' Compute SUVR from PET time series images
#'
#' The standardized uptake value (SUV) ratio (SUVR) is a semi-quantative
#' transformation that is performed on a PET image before intepretation.  This
#' function will run rigid motion correction on the PET time series before
#' computing SUVR.  The implementation also requires an anatomical image and a
#' segmentation that defines the reference region.  This function will produce
#' a mask, a motion corrected image and a PET suvr image mapped to the reference
#' anatomical space.
#'
#' @param petTime pet time series antsImage
#' @param anatomicalImage antsImage
#' @param anatomicalSegmentation antsImage in the same space as anatomicalImage
#' @param smoothingParameter physical space smoothing parameter see \code{smoothImage}
#' @param labelValue the integer value of the reference region as it
#' appears in anatomicalSegmentation.  If not set, we assume a binary segmentation.
#' @param subtractBackground if TRUE, the method will attempt to estimate the
#' background (non-anatomical) activation levels before computing SUVR. In this
#' case, SUVR will be computed after subtracting this background value from the
#' mean activation image.
#' @return suvr antsImage
#' @author Avants BB
#' @examples
#'
#' \dontrun{
#' suvr <- petSUVR( petTime, seg )
#' }
#'
#' @export petSUVR
petSUVR <- function(
  petTime,
  anatomicalImage,
  anatomicalSegmentation,
  smoothingParameter = 2.5,
  labelValue = 0,
  subtractBackground = FALSE )
{
pet = getAverageOfTimeSeries( petTime )
temp = antsrMotionCalculation( petTime, pet, typeofTransform = "BOLDRigid", verbose=F )$moco_img
pet = getAverageOfTimeSeries( temp )
temp = antsrMotionCalculation( petTime, pet, typeofTransform = "BOLDRigid", verbose=F )$moco_img
pet = getAverageOfTimeSeries( temp )
petmask = getMask( pet )
petreg = antsRegistration( anatomicalImage, pet, typeofTransform = "Rigid", verbose=FALSE )
petmask = antsApplyTransforms( anatomicalImage, petmask,
  transformlist = petreg$fwdtransforms, interpolator='NearestNeighbor' )
# NOTE: here, the pet image is now in the anatomical space
pets = smoothImage( petreg$warpedmovout, smoothingParameter, sigmaInPhysicalCoordinates = TRUE )
if ( subtractBackground )
  {
  bigbrain = thresholdImage( anatomicalSegmentation, 1, Inf ) %>%
    iMath("MD", 3 ) - ( thresholdImage( tarseg, 1, Inf ) %>% iMath("MD", 1 ) )
  petbkgd = mean( pets[ bigbrain == 1 ]  )
  petbkgdscale = 1.0
  pets = pets - petbkgd * petbkgdscale
  }
if ( labelValue > 0 )
  {
  cerebellum = thresholdImage( anatomicalSegmentation, labelValue, labelValue )
  pcerebct = mean( pets[ cerebellum == 1 & petmask == 1 ] )
  } else pcerebct = mean( pets[ anatomicalSegmentation == 1 & petmask == 1 ] )
petSUVR = pets / pcerebct
return(
  list( petSUVR = petSUVR,
        petMask = petmask,
        petMOCO = temp )
      )
}
