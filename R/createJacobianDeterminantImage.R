#' createJacobianDeterminantImage
#'
#' Compute the jacobian determinant from a transformation file
#'
#' @param domainImg image that defines transformation domain
#' @param tx deformation transformation file name
#' @param doLog return the log jacobian
#' @param geom use the geometric jacobian calculation (boolean)
#' @return jacobianImage
#' @author BB Avants
#' @examples
#' fi <- antsImageRead(getANTsRData("r16"), 2)
#' mi <- antsImageRead(getANTsRData("r64"), 2)
#' fi <- resampleImage(fi, c(128, 128), 1, 0)
#' mi <- resampleImage(mi, c(128, 128), 1, 0)
#' mytx <- antsRegistration(fixed = fi, moving = mi, typeofTransform = c("SyN"))
#' jac <- createJacobianDeterminantImage(fi, mytx$fwdtransforms[[1]], 1)
#' # plot(jac)
#' @export createJacobianDeterminantImage
createJacobianDeterminantImage <- function(
    domainImg,
    tx,
    doLog = FALSE,
    geom = FALSE) {
  dim <- domainImg@dimension
  if (inherits(tx, "antsImage")) {
    txuse <- tempfile(fileext = c(".nii.gz"))
    antsImageWrite(tx, txuse)
  } else {
    txuse <- tx
  }

  outimg <- ANTsRCore::createJacobianDeterminantImageR(
    antsImageClone(domainImg), txuse,
    as.numeric(doLog),
    as.numeric(geom)
  )
  return(outimg)
}
