#' MR image bias correction based on the N4 algorithm.
#'
#' Truncate outlier intensities and bias correct with the N4 algorithm.
#'
#' @param img image to be bias corrected
#' @param intensityTruncation quantiles for image truncation.
#' @param mask optional antsImage mask
#' @param usen3 Use N3 algorithm instead of N4
#' @return outputs a bias corrected image. 1 indicates failure.
#' @author Tustison N, Avants BB
#' @examples
#'
#' img <- antsImageRead(getANTsRData("r16"))
#' img2 <- abpN4( img )
#'
#' @export abpN4
abpN4 <- function(img, intensityTruncation = c(0.025, 0.975, 256),
  mask = NA,  usen3 = FALSE) {
  numargs <- nargs()
  if (numargs < 1 | missing(img) | class(img)[1] != "antsImage") {
    stop("Missing image.")
  }
  if (length(intensityTruncation) != 3) {
    cat("length( intensityTruncation ) should = 3 \n")
    return(1)
  }
  outimg = iMath(img, "TruncateIntensity", 
                 intensityTruncation[1], intensityTruncation[2], 
                 intensityTruncation[3])
  if (usen3 == TRUE) {
    outimg <- n3BiasFieldCorrection( outimg, 4 )
    outimg <- n3BiasFieldCorrection( outimg, 2 )
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
  return(outimg)
}
