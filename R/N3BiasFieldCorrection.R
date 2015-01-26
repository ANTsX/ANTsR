#' Bias Field Correction
#' 
#' Perform Bias Field Correction on the given image
#' 
#' 
#' @param imageDimension2|3 Number of dimensions of the input image
#' @param inputImage Input image to operate on
#' @param outputImage Result image
#' @param shrikFactor Shrink Factor
#' @param maskImage Image to be used as mask
#' @param numberofIterations Number of Iterations
#' @param numberofFittingLevels Number of fitting levels
#' @param outputBiasField Output bias-field
#' @return 0 -- Success\cr 1 -- Failure
#' @author Shrinidhi KL
#' @examples
#' 
#' \dontrun{
#' N3BiasFieldCorrection( 3 , "input_img.nii" , "output_img.nii" , 2 )
#' }
#' 
#' @export N3BiasFieldCorrection
N3BiasFieldCorrection <- function(...) {
  .Call("N3BiasFieldCorrection", int_antsProcessArguments(c(...)), PACKAGE = "ANTsR")
} 
