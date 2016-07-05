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
#' @param petTime pet time series antsImage or 3D image
#' @param anatomicalImage antsImage
#' @param anatomicalSegmentation antsImage in the same space as anatomicalImage
#' @param smoothingParameter physical space smoothing parameter see \code{smoothImage}
#' @param labelValue the integer value of the reference region as it
#' appears in anatomicalSegmentation.  If not set, we assume a binary segmentation.
#' @param subtractBackground if TRUE, the method will attempt to estimate the
#' background (non-anatomical) activation levels before computing SUVR. In this
#' case, SUVR will be computed after subtracting this background value from the
#' mean activation image.
#' @param debug boolean option activating simple and fast approach
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
  subtractBackground = FALSE,
  debug = FALSE )
{
idim = petTime@dimension
# petTime = petTime - min( petTime )
if ( idim == 4 ) pet = getAverageOfTimeSeries( petTime )
if ( idim == 3 ) pet = antsImageClone( petTime )
if ( idim == 4 )
  {
  if ( pet@dimension == 3 )
    petRef = as.antsImage( as.array( petTime )[,,,1] )
  if ( pet@dimension == 2 )
    petRef = as.antsImage( as.array( petTime )[,,1] )
  petRef = antsCopyImageInfo( pet, petRef )
  temp = antsrMotionCalculation( petTime, petRef, typeofTransform = "Rigid", verbose=F )$moco_img
  pet = getAverageOfTimeSeries( temp )
  }
petmask = getMask( pet )
if ( subtractBackground )
  {
  petbkgd = mean( pet[ petmask == 0 ]  )
  pet = pet - petbkgd
  }
typetx = "Rigid"
# if ( debug ) typetx = "QuickRigid"
petreg = antsRegistration( anatomicalImage, pet, typeofTransform = typetx, verbose = debug )
petmask = antsApplyTransforms( anatomicalImage, petmask,
  transformlist = petreg$fwdtransforms, interpolator='NearestNeighbor' )
temp = antsApplyTransforms( anatomicalImage, temp,
  transformlist = petreg$fwdtransforms, interpolator='Linear', imagetype=3 )
# NOTE: here, the pet image is now in the anatomical space
pets = petreg$warpedmovout
if ( labelValue > 0 )
  {
  cerebellum = thresholdImage( anatomicalSegmentation, labelValue, labelValue )
  pcerebct = mean( pets[ cerebellum == 1 & petmask == 1 ] )
  } else pcerebct = mean( pets[ anatomicalSegmentation == 1 & petmask == 1 ] )
petSUVR = smoothImage(  pets / pcerebct, smoothingParameter,
  sigmaInPhysicalCoordinates = TRUE )
return(
  list( petSUVR = petSUVR,
        petMask = petmask,
        petMOCO = temp )
      )
}
