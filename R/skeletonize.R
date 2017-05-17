#' Skeletonize a binary segmentation.
#'
#' Employ a topologically constrained label propagation tool in order to thin
#' an existing segmentation into a skeleton.  The function will perform better
#' if the initial segmentation is well-composed.
#'
#' @param x input binary image
#' @param laplacianThreshold between zero and one, controls the speed image
#' @param propagationOption either 0 (none), 1 (well-composed) or 2 (topological) constraint
#' @return output binary image
#' @author Avants BB
#' @examples
#'
#' img = antsImageRead( getANTsRData( 'r16') ) %>% thresholdImage( "Otsu", 3 )
#' skel = skeletonize( thresholdImage( img, 3, 3 ) )
#'
#' @export skeletonize
#'
skeletonize <- function( x,
  laplacianThreshold = 0.25,
  propagationOption = 1 ) {
# ImageMath 3 wm.nii.gz MD wm.nii.gz 1  ImageMath 3 wm.nii.gz ME wm.nii.gz 1
#  wm = iMath( x, "MD", 1 ) %>% iMath( "ME", 1)
  wm = thresholdImage( x, 0, 0 ) #  ImageMath 3 wm.nii.gz Neg wm.nii.gz
  wmd = iMath( wm, "D" ) # ImageMath 3 wmd.nii.gz D wm.nii.gz
  wmdl = iMath( wmd, "Laplacian", 1.0, 1 ) #ImageMath 3 wmdl.nii.gz Laplacian wmd.nii.gz 1.0 1
#  return( wmdl )
  speed = thresholdImage( wmdl, 0.0, laplacianThreshold ) # ThresholdImage 3 wmdl.nii.gz speed.nii.gz 0.0 0.25
  speed = thresholdImage( speed, 0, 0 ) # ImageMath 3 speed.nii.gz Neg speed.nii.gz
  wm = iMath( wm, "GetLargestComponent" ) # ImageMath 3 wm.nii.gz GetLargestComponent wm.nii.gz
  # ImageMath 3 wm_skel.nii.gz PropagateLabelsThroughMask speed.nii.gz wm.nii.gz 20000 1
  wmskel = iMath( speed, "PropagateLabelsThroughMask", wm, 200000, propagationOption  )
  wmskel = thresholdImage( wmskel, 0, 0 ) # ImageMath 3 wm_skel.nii.gz Neg wm_skel.nii.gz
  return( wmskel )
}
