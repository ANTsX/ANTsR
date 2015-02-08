#' N4 with previously set parameters based on ants brain processing strategies.
#'
#' Truncate outlier intensities and bias correct with N4 with or without mask
#' and weight images.
#'
#'
#' @param img image to be bias corrected
#' @param intensityTruncation Params to TruncateImageIntensity in iMath
#' @param mask optional antsImage mask
#' @param weightimg optional antsImage weighting - not implemented yet
#' @param usen3 Use N3 instead of N4
#' @return outputs a bias corrected image. 1 -- Failure
#' @author Tustison N, Avants BB
#' @examples
#'
#' fn<-getANTsRData("r16")
#' img<-antsImageRead(fn,2)
#' img2<-abpN4( img )
#'
#' @export abpN4
abpN4 <- function(img = NA, intensityTruncation = c(0.025, 0.975, 256),
  mask = NA, weightimg = NA, usen3 = FALSE) {
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
  dim <- img@dimension
  ImageMath(dim, outimg, "TruncateImageIntensity", outimg,
    intensityTruncation[1], intensityTruncation[2], intensityTruncation[3])
  if (usen3 == TRUE) {
    outimg<-n3BiasFieldCorrection( outimg, 4 )
    outimg<-n3BiasFieldCorrection( outimg, 2 )
    return(outimg)
  }
  if (is.na(mask)) {
    outimg<-n4BiasFieldCorrection(img)
    return(outimg)
  }
  if (!is.na(mask)) {
    outimg<-n4BiasFieldCorrection(img,mask)
    return(outimg)
  }
  if (!is.na(weightimg)) {
    return(img)
  }
  return(outimg)
}
