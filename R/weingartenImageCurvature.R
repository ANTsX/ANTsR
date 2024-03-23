#' image curvature for 2D or 3D
#'
#' uses the weingarten map to estimate image mean or gaussian curvature
#'
#' @param image antsImage
#' @param sigma smoothing parameter
#' @param opt mean by default, otherwise use string gaussian or characterize
#' @param labeled boolean if TRUE then assume image contains labels which denote
#' the surface on which curvature is to be calculated.  in this case, the
#' surface should hold integer value 1; the background should be zero;
#' the interior of the surface should be greater than 1 (e.g. 2).
#' @return image
#' @author Brian B. Avants
#' @references Avants, B, J. Gee, and B. Avants.
#' Shape operator for differential image analysis
#' Information Processing in Medical Imaging, 2003.
#' @keywords curvature
#' @examples
#' img <- makeImage(c(10, 10, 10), rnorm(1000))
#' fik <- weingartenImageCurvature(img)
#' \dontrun{
#' fi <- antsImageRead(getANTsRData("mni"))
#' fik <- weingartenImageCurvature(fi)
#' }
#' @export weingartenImageCurvature
weingartenImageCurvature <- function(image, sigma = 1.0, opt = "mean", labeled = FALSE) {
  if (image@dimension != 3 & image@dimension != 2) {
    stop("input image must be 2D or 3D")
  }
  if (!opt %in% c("mean", "gaussian", "characterize")) {
    stop("choose mean gaussian or characterize as option")
  }
  if (image@dimension == 2) {
    d <- dim(image)
    temp <- as.array(makeImage(c(d, 10)))
    for (k in 2:8) {
      voxvals <- image[1:d[1], 1:d[2]]
      temp[1:d[1], 1:d[2], k] <- voxvals
      # + rnorm(  length( voxvals ), 0, 1.e-5 )
    }
    temp <- as.antsImage(temp)
    myspc <- antsGetSpacing(image)
    myspc <- c(myspc, min(myspc))
    x <- antsSetSpacing(temp, myspc)
  } else {
    temp <- antsImageClone(image)
  }
  optnum <- 0
  if (opt == "gaussian") optnum <- 6
  if (opt == "characterize") optnum <- 5
  mykout <- ANTsRCore::weingartenImageCurvature(
    temp, sigma, optnum, as.integer(labeled)
  )
  if (image@dimension == 3) {
    return(mykout)
  }
  if (image@dimension == 2) {
    subarr <- as.antsImage(as.array(mykout)[, , 5])
    return(antsCopyImageInfo(image, subarr))
  }
}
