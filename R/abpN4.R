#' MR image bias correction based on the N4 algorithm.
#'
#' Truncate outlier intensities and bias correct with the N4 algorithm.
#'
#' @param img image to be bias corrected
#' @param intensityTruncation quantiles for image truncation.
#' @param mask optional antsImage mask
#' @param usen3 Use N3 algorithm instead of N4
#' @param ... additional arguments to pass to
#' \code{\link{n4BiasFieldCorrection}}
#' @return outputs a bias corrected image. 1 indicates failure.
#' @author Tustison N, Avants BB
#' @examples
#'
#' img <- antsImageRead(getANTsRData("r16"))
#' img2 <- abpN4( img )
#' img3 = abpN4(img)
#' testthat::expect_equal( mean( img2 ), 82.6, tolerance = 1e-2 )
#' testthat::expect_equal( img2, img3 )
#'
#' @export abpN4
abpN4 <- function(img, intensityTruncation = c(0.025, 0.975, 256),
  mask,  usen3 = FALSE,
  ...) {
  numargs <- nargs()
  if (numargs < 1 | missing(img) | class(img)[1] != "antsImage") {
    stop("Missing image.")
  }
  if (length(intensityTruncation) != 3) {
    stop("length( intensityTruncation ) should = 3 \n")
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
  if ( ! missing( mask ) ) {
    outimg<-n4BiasFieldCorrection(img, mask = mask, ...)
  } else {
    outimg<-n4BiasFieldCorrection(img, ...)
  }

  return(outimg)
}
