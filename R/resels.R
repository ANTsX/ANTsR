#' Estimates image resels
#'
#' Utilize the estimated FWHM to find the resels per voxel
#'
#' @param mask statistical value (typically the maxima of a cluster or statistical field)
#' @param fwhm the full width at half maxima measurement
#' @return A vector of resels for dimensions 0:D
#'
#' @details
#'
#' Interprets a given antsImage mask (binarized so as to only contain 0s and 1s) into
#' resolutions elements (as opposed to voxels). Doing so emphasizes the interdependent
#' nature of voxels when undergoing RFT based statistical analyses. Optimized for three
#' dimensions (2D has not yet been tested yet).
#'
#' @references
#' Worlsey K.J., (1996) A Unified Statistical Approach for Determining Significant Signals in Images of Cerebral Activation.
#'
#' @author Zachary P. Christensen
#'
#' @seealso rftPval, euler, rftResults
#' @examples
#' mask <- getMask(antsImageRead(getANTsRData("r16")))
#' myresels <- resels(mask, c(1, 1))
#'
#' @export resels
resels <- function(mask, fwhm) {
  if (class(mask) != "antsImage") {
    stop("mask must be of class antsImage")
  }
  if (max(mask) > 1 | min(mask) < 0) {
    stop("mask must be binarized and only contain 0s and 1s")
  }

  D <- mask@dimension
  if (missing(fwhm)) (fwhm <- rep(1, D))

  resels <- rep(0, 4)
  dimx <- dim(mask)[1]
  x1 <- 2:(dimx + 1)
  x2 <- 3:(dimx + 2)
  rx <- 1 / (fwhm[1])
  if (D > 1) {
    dimy <- dim(mask)[2]
    y1 <- 2:(dimy + 1)
    y2 <- 3:(dimy + 2)
    ry <- 1 / (fwhm[2])
  }
  if (D > 2) {
    dimz <- dim(mask)[3]
    z1 <- 2:(dimz + 1)
    z2 <- 3:(dimz + 2)
    rz <- 1 / (fwhm[3])
  }

  mask <- iMath(mask, "PadImage", 1)
  nvox <- sum(as.array(mask))
  if (D == 2) {
    rz <- 1
    m <- mask[x1, y1]
    xm <- m + mask[x2, y1]
    ym <- m + mask[x1, y2]
    xym <- m + mask[x2, y1] + mask[x1, y2] + mask[x2, y2]

    Ex <- sum(xm[xm == 2]) / 2
    Ey <- sum(ym[ym == 2]) / 2
    Ez <- 1
    Fxy <- sum(xym[xym == 4]) / 4
    Fxz <- 1
    Fyz <- 1
    Fxyz <- Fyz
  } else if (D == 3) {
    m <- mask[x1, y1, z1]
    xm <- m + mask[x2, y1, z1]
    ym <- m + mask[x1, y2, z1]
    zm <- m + mask[x1, y1, z2]
    xym <- m + mask[x2, y1, z1] + mask[x1, y2, z1] + mask[x2, y2, z1]
    xzm <- m + mask[x2, y1, z1] + mask[x1, y1, z2] + mask[x2, y1, z2]
    yzm <- m + mask[x1, y2, z1] + mask[x1, y1, z2] + mask[x1, y2, z2]
    xyzm <- m + mask[x2, y1, z1] + mask[x1, y2, z1] + mask[x1, y1, z2] +
      mask[x2, y2, z1] + mask[x2, y1, z2] + mask[x1, y2, z2] + mask[x2, y2, z2]

    # extract number of voxels that fits each set of parameters (see Worsley 1996 for exact definition of parameters)
    Ex <- sum(xm[xm == 2]) / 2
    Ey <- sum(ym[ym == 2]) / 2
    Ez <- sum(zm[zm == 2]) / 2
    Fxy <- sum(xym[xym == 4]) / 4
    Fxz <- sum(xzm[xzm == 4]) / 4
    Fyz <- sum(yzm[yzm == 4]) / 4
    Fxyz <- sum(xyzm[xyzm == 8]) / 8
  }
  resels[1] <- (nvox - (Ex + Ey + Ez) + (Fyz + Fxz + Fxy) - Fxyz)
  resels[2] <- (Ex - Fxy - Fxz + Fxyz) * rx + (Ey - Fxy - Fyz + Fxyz) * ry + (Ez - Fxz - Fyz + Fxyz) * rz
  resels[3] <- (Fxy - Fxyz) * rx * ry + (Fxz - Fxyz) * rx * rz + (Fyz - Fxyz) * ry * rz
  resels[4] <- Fxyz * rx * ry * rz

  resels
}
