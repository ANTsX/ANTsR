#' Get Mask
#'
#' Get a binary mask image from the given image after thresholding.
#'
#' If \code{cleanup} is \code{>0}, the following steps are applied
#' \enumerate{ \item Erosion with radius 2 voxels \item Retain largest
#' component \item Dilation with radius 1 voxel \item Morphological closing }
#'
#' These functions are available in \link{iMath}, see the operations
#' \dQuote{ME}, \dQuote{GetLargestComponent}, \dQuote{MD}, \dQuote{FillHoles}.
#'
#' @param img Input image. Can be an \code{antsImage} of 2, 3 or 4 dimensions.
#' If \code{img} is NULL, a file chooser dialog will appear.
#' @param lowThresh An inclusive lower threshold for voxels to be included in
#' the mask.
#' @param highThresh An inclusive upper threshold for voxels to be included in
#' the mask.
#' @param cleanup If \code{>0}, morphological operations will be applied to
#' clean up the mask by eroding away small or weakly-connected areas, and
#' closing holes.
#' @return Object of type \code{antsImage} containing the mask image. The
#' voxel intensities will be binarized, 1 for voxels in the mask and 0 outside.
#' @author Shrinidhi KL, Cook PA, Avants BB
#' @keywords mask
#' @examples
#'
#' img<-antsImageRead( getANTsRData("r16") ,2)
#' mask<-getMask( img )
#'
#' @export getMask
getMask <- function(img, lowThresh, highThresh, cleanup = 2) {
  # Binarizes a mask between specified thresholds Input can be a file name or an
  # antsImage, if it is not specified, a file chooser is launched. Works on 3D
  # images only If cleanup == TRUE, small and weakly-connected elements are removed
  # by erosion, and then holes are filled.  Returns: a binary antsImage
  cleanup <- as.numeric(cleanup)
  if (class(img) == "antsImage") {
    if (img@pixeltype != "float") {
      img <- antsImageClone(img, "float")
    }
  }
  if ( missing( lowThresh ) )  lowThresh = mean( img )
  if ( missing( highThresh ) ) highThresh = max( img  )
  if ( ( !is.numeric(lowThresh) )  ||
       ( !is.numeric(highThresh) ) ||
          length(lowThresh) > 1    || length(highThresh) > 1 )
    {
    stop("'lowthresh' and 'highthresh' must be numeric scalars")
    }

  mask_img<-thresholdImage( img, lowThresh, highThresh )

  if (cleanup > 0) {
    imageMath(img@dimension, mask_img, "ME", mask_img, cleanup)
    imageMath(img@dimension, mask_img, "GetLargestComponent", mask_img)
    imageMath(img@dimension, mask_img, "MD", mask_img, cleanup)
    imageMath(img@dimension, mask_img, "FillHoles", mask_img)
    while (  ( min(mask_img) == max(mask_img) ) & cleanup > 0 )
      {
      cleanup <- cleanup - 1
      mask_img <- thresholdImage( img, lowThresh, highThresh )
      if ( cleanup > 0 )
        {
        imageMath(img@dimension, mask_img, "ME", mask_img, cleanup)
        imageMath(img@dimension, mask_img, "MD", mask_img, cleanup)
        imageMath(img@dimension, mask_img, "FillHoles", mask_img)
        }
      if ( cleanup == 0 )
        {
        clustlab<-labelClusters( mask_img, 1 )
        mask_img <- thresholdImage( clustlab, 1, 1 )
        }
      }
  }
  return(mask_img)
}
