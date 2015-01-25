#' Get Mask
#' 
#' Get a binary mask image from the given image after thresholding.
#' 
#' If \code{cleanup} is \code{TRUE}, the following steps are applied
#' \enumerate{ \item Erosion with radius 2 voxels \item Retain largest
#' component \item Dilation with radius 1 voxel \item Morphological closing }
#' 
#' These functions are available in \link{ImageMath}, see the operations
#' \dQuote{ME}, \dQuote{GetLargestComponent}, \dQuote{MD}, \dQuote{FillHoles}.
#' 
#' @param img Input image. Can be a filename of a 3D image or an
#' \code{antsImage} of dimension 3. If \code{img} is NULL, a file chooser
#' dialog will appear.
#' @param lowThresh An inclusive lower threshold for voxels to be included in
#' the mask.
#' @param highThresh An inclusive upper threshold for voxels to be included in
#' the mask.
#' @param cleanup If \code{TRUE}, morphological operations will be applied to
#' clean up the mask by eroding away small or weakly-connected areas, and
#' closing holes.
#' @return Object of type \code{antsImage} containing the masked image. The
#' voxel intensities will be binarized, 1 for voxels in the mask and 0 outside.
#' @author Shrinidhi KL, Cook PA
#' @keywords mask
#' @examples
#' 
#' \dontrun{
#' # input_img.nii is a 3D image
#' mask_img <- get_mask( "input_img.nii" , 500 , 1e9 )
#' }
#' 
#' @export getMask
getMask <- function(img = NULL, lowThresh = 1, highThresh = Inf, 
                    cleanup = 2 ) {
  # Binarizes a mask between specified thresholds Input can be a file name or an
  # antsImage, if it is not specified, a file chooser is launched. Works on 3D
  # images only If cleanup == TRUE, small and weakly-connected elements are removed
  # by erosion, and then holes are filled.  Returns: a binary antsImage
  cleanup<-as.numeric(cleanup)
  if (is.character(img)) {
    if (length(img) != 1) {
      stop("'img' must be a single filename")
    }
    img <- antsImageRead(img, 3, "float")
  } else if (class(img) == "antsImage") {
    if (img@pixeltype != "float") {
      img <- antsImageClone(img, "float")
    }
  } else {
    img <- file.choose()
  }
  
  if ((!is.numeric(lowThresh)) || (!is.numeric(highThresh)) || length(lowThresh) > 
    1 || length(highThresh) > 1) {
    stop("'lowthresh' and 'highthresh' must be numeric scalars")
  }
  
  
  mask_img <- new("antsImage", "float", img@dimension)
  
  ThresholdImage(img@dimension, img, mask_img, lowThresh, highThresh)
  
  if (cleanup > 0 ) {
    ImageMath(img@dimension, mask_img, "ME", mask_img, cleanup)
    ImageMath(img@dimension, mask_img, "GetLargestComponent", mask_img)
    ImageMath(img@dimension, mask_img, "MD", mask_img, cleanup)
    ImageMath(img@dimension, mask_img, "FillHoles", mask_img)
  }
  
  return(mask_img)
  
} 
