#' @name reorientImage
#' @title reorient image by its principal axis
#' @description  align along a specific axis
#' @usage  reo<-reorientImage( i1 , axis )
#' @param img antsImage
#' @param axis vector of size dim
#' @param axis2 vector of size dim for 3D
#' @param txfn file name for transformation
#' @return reoriented image
#' @author Brian B. Avants
#' @keywords image geometry
#' @examples
#' fi<-antsImageRead( getANTsRData('r16') ,2)
#' reofi<-reorientImage(fi,c(1,0))
reorientImage <- function( img, axis1, axis2=NA, txfn=NA ) {
  if (length(dim(img)) == 1)
    if (dim(img)[1] == 1)
      return(NULL)
  if ( img@pixeltype != "float"    )
       {
       print(args(reorientImage))
       print("input images must have float pixeltype")
       return(NA)
       }
  if ( is.na(axis2)  ) axis2<-rep(0,img@dimension)
#  img2<-antsImageClone(img)
  if ( is.na(txfn) ) txfn=tempfile(fileext='.mat')
  .Call("reorientImage", img, txfn, axis1, axis2, PACKAGE = "ANTsR")
  img2<-antsApplyTransforms(img,img,transformlist=c(txfn))
  return(list(reoimg=img2,txfn=txfn))
}
