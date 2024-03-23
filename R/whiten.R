#' Simple whitening function.
#'
#' Whitens the input matrix using SVD and returns the result.
#'
#' @param x input matrix
#' @param k rank to use
#' @param reducex reduce the input matrix to k-size subspace
#' @return matrix is output
#' @author Avants BB
#' @examples
#'
#' mat <- matrix(rnorm(300), ncol = 50)
#' wmat <- whiten(mat)
#' wmat2 <- whiten(mat, 2, TRUE)
#'
#' @export whiten
whiten <- function(x, k = NULL, reducex = FALSE) {
  if (nargs() == 0) {
    print("Usage:  x_whitened<-whiten( x ) ")
    return(1)
  }
  if (is.null(k)) {
    svdx <- svd(scale(x %*% t(x)))
    dd <- (svdx$d)^(-1 / 2)
    xw <- ((svdx$u %*% diag(dd)) %*% t(svdx$v)) %*% x
  } else {
    n <- nrow(x)
    p <- ncol(x)
    svdx <- svd(scale(x %*% t(x)), nu = min(n, p, k), nv = min(n, p, k))
    dd <- diag(((svdx$d)^(-1 / 2))[1:k])
    xw <- (svdx$u %*% dd) %*% t(svdx$v)
    xw <- (xw) %*% x
  }
  if (reducex) {
    xw <- lowrankRowMatrix(xw, k)
  }
  return(xw)
}





#' Local gyrification index.
#'
#' Estimate the gyrification index at a specific scale.  This will produce images of
#' local sulcification and gyrification as as well as the difference of these
#' two measurements.  The most gyral values are near 1 and most sulcal negative 1.
#' The function is not limited to brain data and should work on any 3D shape.
#' See the paper entitled the shape operator for differential image analysis.
#'
#' @param segmentation - potentially from antsCorticalThickness
#' @param sigma - scale parameter
#' @param k - size of neighborhood
#' @param ksigma - sigma for k-neighborhood
#' @return lGI image
#' @author Avants BB
#' @examples
#' \dontrun{
#' seg <- antsImageRead("segmentation.nii.gz")
#' wm <- thresholdImage(seg, 3, 3) %>% morphology("dilate", 1)
#' lgi <- localGyrificationIndex(wm)
#' }
#'
#' @export localGyrificationIndex
localGyrificationIndex <- function(
    segmentation,
    sigma = 3,
    k = 25,
    ksigma = 3) {
  wmdil <- smoothImage(segmentation, 0.5)
  kapch <- weingartenImageCurvature(wmdil, sigma, "characterize")
  kapmn <- weingartenImageCurvature(wmdil, sigma, "mean")
  gyrsulc <- kapch * segmentation
  gyri <- thresholdImage(gyrsulc, 1, 1) + thresholdImage(gyrsulc, 5, 5)
  sulc <- thresholdImage(gyrsulc, 2, 2) + thresholdImage(gyrsulc, 6, 6)
  gyrsulc <- gyri + sulc * 2
  spatmat <- t(imageDomainToSpatialMatrix(segmentation, segmentation))
  smoothingMatrix <- knnSmoothingMatrix(spatmat, k = k, sigma = ksigma)
  localGyri <- makeImage(segmentation, as.numeric(smoothingMatrix %*% gyri[segmentation == 1]))
  localSulci <- makeImage(segmentation, as.numeric(smoothingMatrix %*% sulc[segmentation == 1]))
  localGyriMinusSulci <- localGyri - localSulci
  return(list(
    localGyri = localGyri,
    localSulci = localSulci,
    localGyriMinusSulci = localGyriMinusSulci
  ))
}
