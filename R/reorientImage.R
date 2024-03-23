#' reorient image by its principal axis
#'
#' align along a specific axis
#'
#'
#' @param img antsImage
#' @param axis1 vector of size dim, might need to play w/axis sign
#' @param axis2 vector of size dim for 3D
#' @param doreflection boolean
#' @param doscale scalar value, 1 allows automated estimate of scaling
#' @param txfn file name for transformation
#' @return reoriented image
#' @author Brian B. Avants
#' @keywords geometry image
#' @examples
#'
#' reofi <- reorientImage(ri(1), c(1, 0))
#'
#' @export reorientImage
reorientImage <- function(
    img, axis1, axis2 = NA,
    doreflection = 0, doscale = 0,
    txfn = NA) {
  if (length(dim(img)) == 1) {
    if (dim(img)[1] == 1) {
      return(NULL)
    }
  }
  if (img@pixeltype != "float") {
    print(args(reorientImage))
    print("input images must have float pixeltype")
    return(NA)
  }
  ax1norm <- sqrt(sum(axis1 * axis1))
  if (ax1norm == 0) ax1norm <- 1
  axis1 <- axis1 / ax1norm * (-1)
  if (is.na(axis2)) {
    axis2 <- rnorm(img@dimension)
    ax2norm <- sqrt(sum(axis2 * axis2))
    if (ax2norm == 0) ax2norm <- 1
    axis2 <- axis2 / ax2norm * (-1)
  }
  axis2 <- axis2 / sqrt(sum(axis2 * axis2)) * (-1)
  if (is.na(txfn)) {
    txfn <- tempfile(fileext = ".mat")
  }
  ANTsRCore::reorientImage(img, txfn, axis1, axis2, doreflection,
    doscale = doscale
  )
  img2 <- antsApplyTransforms(img, img, transformlist = c(txfn))
  return(list(reoimg = img2, txfn = txfn))
}




#' center of mass
#'
#' compute an image center of mass in physical space which is defined as the
#' mean of the intensity weighted voxel coordinate system.
#'
#' @param img antsImage
#' @return vector center of mass
#' @author Brian B. Avants
#' @keywords geometry image
#' @examples
#'
#' fi <- antsImageRead(getANTsRData("r16"))
#' com1 <- getCenterOfMass(fi)
#' fi <- antsImageRead(getANTsRData("r64"))
#' com2 <- getCenterOfMass(fi)
#'
#' @export getCenterOfMass
getCenterOfMass <- function(img) {
  if (length(dim(img)) == 1) {
    if (dim(img)[1] == 1) {
      return(NULL)
    }
  }
  if (img@pixeltype != "float") {
    print("input images must have float pixeltype")
    return(NA)
  }
  mycom <- ANTsRCore::centerOfMass(img)
  return(mycom)
}
