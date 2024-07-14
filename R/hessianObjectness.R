#' hessianObjectness
#'
#' Interface to ITK filter.  Based on the paper by Westin et al., 
#' "Geometrical Diffusion Measures for MRI from Tensor Basis Analysis" and 
#' Luca Antiga's Insight Journal paper http://hdl.handle.net/1926/576.
#'
#' @param image input image
#' @param objectDimension 0: 'sphere', 1: 'line', or 2: 'plane'.
#' @param isBrightObject Set 'true' for enhancing bright objects and 'false' for dark objects.
#' @param sigmaMin Define scale domain for feature extraction.
#' @param sigmaMax Define scale domain for feature extraction.
#' @param numberOfSigmaSteps Define number of samples for scale space.
#' @param useSigmaLogarithmicSpacing Define sample spacing the for scale space.
#' @param alpha Hessian filter parameter.
#' @param beta Hessian filter parameter.
#' @param gamma Hessian filter parameter.
#' @param setScaleObjectnessMeasure ... 
#' @return hessian objectness image.
#'
#' @author NJ Tustison
#'
#' @examples
#' image <- antsImageRead(getANTsRData("r16"))
#' hessianObjectnessImage <- hessianObjectness(image)
#'
#' @export hessianObjectness

hessianObjectness <- function( image, 
                               objectDimension = 1, 
                               isBrightObject = TRUE,
                               sigmaMin = 0.1,
                               sigmaMax = 10.0,
                               numberOfSigmaSteps = 10,
                               useSigmaLogarithmicSpacing = TRUE,
                               alpha = 0.5, 
                               beta = 0.5, 
                               gamma = 5.0,
                               setScaleObjectnessMeasure = TRUE )
{                                                               
  outputImage <- ANTsRCore::hessianObjectnessR(
    antsImageClone(image), objectDimension, isBrightObject, 
    sigmaMin, sigmaMax, numberOfSigmaSteps, useSigmaLogarithmicSpacing,
    alpha, beta, gamma, setScaleObjectnessMeasure)
  return(outputImage)
}
