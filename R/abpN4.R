#' N4 with previously set parameters based on ants brain processing strategies.
#' 
#' Truncate outlier intensities and bias correct with N4 with or without mask
#' and weight images.
#' 
#' 
#' @param image image to be bias corrected
#' @param intensityTruncation Params to TruncateImageIntensity in ImageMath
#' @param mask optional antsImage mask
#' @param weightimage optional antsImage weighting
#' @param usen3 Use N3 instead of N4
#' @return outputs a bias corrected image. 1 -- Failure
#' @author Tustison N, Avants BB
#' @examples
#' 
#' \dontrun{
#' fn<-getANTsRData('r16') 
#' img<-antsImageRead(fn,2)
#' img2<-abpN4( img )
#' }
#' 
#' @export abpN4
abpN4 <- function(img = NA, intensityTruncation = c(0.025, 0.975, 256), mask = NA, 
  weightimg = NA, usen3 = FALSE) {
  numargs <- nargs()
  if (numargs < 1 | missing(img) | class(img)[1] != "antsImage") {
    cat(" abpN4( img = inimg , intensityTruncation=c( 0.025, 0.975, 256 ), mask=NA, weightimg=NA, usen3=FALSE ) \n")
    return(0)
  }
  if (length(intensityTruncation) != 3) {
    cat("length( intensityTruncation ) should = 3 \n")
    return(1)
  }
  outimg <- antsImageClone(img, "float")
  dim <- as.character(img@dimension)
  ImageMath(dim, outimg, "TruncateImageIntensity", outimg, intensityTruncation[1], 
    intensityTruncation[2], intensityTruncation[3])
  if (usen3 == TRUE) {
    N3BiasFieldCorrection(list(img@dimension, outimg, outimg, "4"))
    N3BiasFieldCorrection(list(img@dimension, outimg, outimg, "2"))
    return(outimg)
  }
  N4_CONVERGENCE_1 <- "[50x50x50x50,0.0000001]"
  N4_CONVERGENCE_2 <- "[20x20x20x20,0.0000001]"
  N4_SHRINK_FACTOR_1 <- "4"
  N4_SHRINK_FACTOR_2 <- "2"
  N4_BSPLINE_PARAMS <- "[200]"
  if (is.na(mask)) {
    N4BiasFieldCorrection(list(d = outimg@dimension, i = outimg, s = N4_SHRINK_FACTOR_1, 
      c = N4_CONVERGENCE_1, b = N4_BSPLINE_PARAMS, o = outimg))
    return(outimg)
  }
  if (is.na(weightimg)) {
    N4BiasFieldCorrection(list(d = outimg@dimension, i = outimg, s = N4_SHRINK_FACTOR_1, 
      c = N4_CONVERGENCE_1, b = N4_BSPLINE_PARAMS, x = mask, o = outimg))
    N4BiasFieldCorrection(list(d = outimg@dimension, i = outimg, s = N4_SHRINK_FACTOR_2, 
      c = N4_CONVERGENCE_2, b = N4_BSPLINE_PARAMS, x = mask, o = outimg))
  }
  if (!is.na(weightimg)) {
    N4BiasFieldCorrection(list(d = outimg@dimension, i = outimg, s = N4_SHRINK_FACTOR_1, 
      c = N4_CONVERGENCE_1, b = N4_BSPLINE_PARAMS, x = mask, w = weightimg, 
      o = outimg))
    N4BiasFieldCorrection(list(d = outimg@dimension, i = outimg, s = N4_SHRINK_FACTOR_2, 
      c = N4_CONVERGENCE_2, b = N4_BSPLINE_PARAMS, x = mask, w = weightimg, 
      o = outimg))
  }
  return(outimg)
} 
