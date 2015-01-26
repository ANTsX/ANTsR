#' resampleImage
#'
#' Resample image by spacing or number of voxels with various interpolators
#'
#' @param image input antsImage matrix
#' @param resampleParams vector of size dimension with numeric values
#' @param useVoxels true means interpret resample params as voxel counts
#' @param interpType one of 0 (nearest neighbor), 1 (linear), 2 (gaussian), 3 (windowed sinc), 4 (bspline)
#' @return output antsImage
#' @author Avants BB
#' @examples
#'
#' fi<-antsImageRead( getANTsRData('r16') ,2)
#' finn<-resampleImage(fi,c(50,60),1,0)
#' filin<-resampleImage(fi,c(1.5,1.5),0,1)
#'
#' @export resampleImage
resampleImage <- function(image,resampleParams, useVoxels=0, interpType=1 )
  {
  inimg<-antsImageClone( image, 'double' )
  outimg<-antsImageClone( image, 'double' )
  rsampar<-paste(resampleParams,collapse='x')
  args<-list(image@dimension,inimg,outimg,rsampar,useVoxels,interpType)
  k<-int_antsProcessArguments(args)
  retval<-.Call("ResampleImage", k )
  outimg<-antsImageClone( outimg, image@pixeltype )
  return( outimg )
  }
