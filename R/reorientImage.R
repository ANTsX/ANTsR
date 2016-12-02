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
#' fi<-antsImageRead( getANTsRData("r16"))
#' reofi<-reorientImage(fi,c(1,0))
#'
#' @export reorientImage
reorientImage <- function(img, axis1, axis2 = NA,
  doreflection = 0, doscale = 0,
  txfn = NA) {
  if (length(dim(img)) == 1)
    if (dim(img)[1] == 1)
      return(NULL)
  if (img@pixeltype != "float") {
    print(args(reorientImage))
    print("input images must have float pixeltype")
    return(NA)
  }
  if (is.na(axis2))
    axis2 <- rep(0, img@dimension)
  # img2<-antsImageClone(img)
  axis1 <- axis1/sqrt(sum(axis1 * axis1)) * (-1)
  axis2 <- axis2/sqrt(sum(axis2 * axis2)) * (-1)
  if (is.na(txfn))
    txfn = tempfile(fileext = ".mat")
  .Call("reorientImage", img, txfn, axis1, axis2, doreflection,
    doscale = doscale,
    PACKAGE = "ANTsR")
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
#' fi<-antsImageRead( getANTsRData("r16"))
#' com<-getCenterOfMass( fi )
#'
#' @export getCenterOfMass
getCenterOfMass <- function( img ) {
  if (length(dim(img)) == 1)
    if (dim(img)[1] == 1)
      return(NULL)
  if ( img@pixeltype != "float" ) {
    print("input images must have float pixeltype")
    return(NA)
  }
  .Call("centerOfMass", img, PACKAGE = "ANTsR")
}
