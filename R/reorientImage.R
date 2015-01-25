#' reorient image by its principal axis
#' 
#' align along a specific axis
#' 
#' 
#' @param img antsImage
#' @param axis2 vector of size dim for 3D
#' @param doreflection vector of size dim for 3D
#' @param txfn file name for transformation
#' @param axis vector of size dim, might need to play w/axis sign
#' @return reoriented image
#' @author Brian B. Avants
#' @keywords geometry image
#' @examples
#' 
#' fi<-antsImageRead( getANTsRData('r16') ,2)
#' reofi<-reorientImage(fi,c(1,0))
#' 
#' @export reorientImage
reorientImage <- function( img, axis1, axis2=NA,
  doreflection=0, doscale=0, txfn=NA ) {
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
  axis1<-axis1/sqrt( sum( axis1*axis1 )  )*(-1)
  axis2<-axis2/sqrt( sum( axis2*axis2 )  )*(-1)
  if ( is.na(txfn) ) txfn=tempfile(fileext='.mat')
  .Call("reorientImage", img, txfn, axis1, axis2,
     doreflection, doscale=doscale, PACKAGE = "ANTsR")
  img2<-antsApplyTransforms(img,img,transformlist=c(txfn))
  return(list(reoimg=img2,txfn=txfn))
}
