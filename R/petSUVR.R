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
#' @param anatomicalSegmentation antsImage in the same space as anatomicalImage.
#' This image should contain a whole brain mask in addition to a labeled
#' reference region for computing SUVR.
#' @param smoothingParameter physical space smoothing parameter see \code{smoothImage}
#' @param labelValue the integer value of the reference region as it
#' appears in anatomicalSegmentation.  If not set, we assume a binary segmentation.
#' @param subtractBackground if TRUE, the method will attempt to estimate the
#' background (non-anatomical) activation levels before computing SUVR. In this
#' case, SUVR will be computed after subtracting this background value from the
#' mean activation image.
#' @param mapToPet boolean option causing pet to be used as fixed image in the
#' rigid registration between anatomical and pet
#' @param noMotionCorr boolean option activating simple and fast approach that
#' may be better for dynamic data with insufficient per-frame information
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
  mapToPet = FALSE,
  noMotionCorr = FALSE )
{
idim = petTime@dimension
# petTime = petTime - min( petTime )
if ( idim == 4 ) pet = getAverageOfTimeSeries( petTime )
if ( idim == 3 ) {
  pet = antsImageClone( petTime )
  petMoco = antsImageClone( petTime )
}
if ( idim == 4 )
  {
  if ( pet@dimension == 3 )
    petRef = as.antsImage( as.array( petTime )[,,,1] )
  if ( pet@dimension == 2 )
    petRef = as.antsImage( as.array( petTime )[,,1] )
  petRef = antsCopyImageInfo( pet, petRef )
  typetx = "Rigid"
  if ( noMotionCorr ) petMoco = antsImageClone( petTime )
  if ( !noMotionCorr ) {
    if ( length( smoothingParameter ) == 1 & idim == 4 )
      smoothingParameterT = c( rep( smoothingParameter, 3 ), 0 )
    petTimeSmoothed = smoothImage(  petTime, smoothingParameterT,
      sigmaInPhysicalCoordinates = TRUE )
    petMoco = antsrMotionCalculation( petTimeSmoothed, petRef,
      typeofTransform = typetx, verbose=F )$moco_img
    }
  pet = getAverageOfTimeSeries( petMoco )
  }
typetx = "Rigid"
pets = smoothImage(  getAverageOfTimeSeries( petTime ),
  smoothingParameter, sigmaInPhysicalCoordinates = TRUE )
petmaskOrig = getMask( pets )
if ( subtractBackground )
  {
  petbkgd = mean( pet[ petmaskOrig == 0 ]  )
  pet = pet - petbkgd
  }
if ( mapToPet )
  {
  petreg = antsRegistration( pets, anatomicalImage, typeofTransform = typetx,
    mask = petmaskOrig, verbose = FALSE )
  wti = TRUE
  }
else {
  brainmask = getMask( anatomicalSegmentation )
#  petreg = antsRegistration( anatomicalImage, pets, typeofTransform = typetx,
#    mask = brainmask, verbose = FALSE )
  petreg1 = antsRegistration( anatomicalImage, pets,
    typeofTransform = typetx, verbose = FALSE )
  tempmask = antsApplyTransforms( pets, brainmask,
      whichtoinvert = c( TRUE ), transformlist = petreg1$fwdtransforms )
  petreg = antsRegistration( anatomicalImage * brainmask, pets * tempmask,
    typeofTransform = typetx, verbose = FALSE,
    initialTransform = petreg1$fwdtransforms )
  wti = FALSE
  }
petmask = antsApplyTransforms( anatomicalImage, petmaskOrig,
  whichtoinvert = c( wti ),
  transformlist = petreg$fwdtransforms, interpolator='NearestNeighbor' )
if ( idim == 4 )
  {
  temp = antsApplyTransforms( anatomicalImage, petMoco,
    whichtoinvert = c( wti ),
    transformlist = petreg$fwdtransforms, interpolator='Linear', imagetype=3 )
  pets = getAverageOfTimeSeries( temp )
  }
if ( idim == 3 )
  {
  temp = antsApplyTransforms( anatomicalImage, petMoco,
    whichtoinvert = c( wti ),
    transformlist = petreg$fwdtransforms, interpolator='Linear' )
  pets = antsImageClone( temp )
  }
# NOTE: here, the pet image is now in the anatomical space
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
