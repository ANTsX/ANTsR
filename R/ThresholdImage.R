#' Theshold Image
#' 
#' Perform thresholding on the given image
#' 
#' 
#' @param imageDimension2|3 Number of dimensions of the input image
#' @param inputImage Input image to operate on
#' @param outputImage Result image
#' @param thresh-low Lower edge of threshold window
#' @param thresh-high Higher edge of threshold window
#' @param inside-value Inside value
#' @param outside-value Outside value
#' @param number-of-thresholds Number of thresholds
#' @return 0 -- Success\cr 1 -- Failure
#' @author Shrinidhi KL
#' @examples
#' 
#' \dontrun{
#' ThresholdImage( 3 , 'input_img.nii' , 'output_img.nii' , 2 , 3 )
#' ThresholdImage( 3 , 'input_img.nii' , 'output_img.nii' , 'Otsu' , 3 )
#' }
#' 
#' @export ThresholdImage
ThresholdImage <- function(...) {
  .Call("ThresholdImage", int_antsProcessArguments(c(...)), PACKAGE = "ANTsR")
} 
