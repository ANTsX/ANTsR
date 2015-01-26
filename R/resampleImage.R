#' resampleImage
#'
#' Resample image by spacing or other parameters
#'
#' Change an image by its spacing or number of voxels
#'
#' @param image input antsImage matrix
#' @param resampleParams vector of size dimension with numeric values
#' @param useVoxels true means interpret resample params as voxel counts
#' @param interpType one of 0, 1, 2, 3, 4
#' @return output antsImage
#' @author Avants BB
#' @examples
#'
#' fi<-antsImageRead( getANTsRData('r16') ,2)
#' finn<-resampleImage(fi,c(20,20),1,0)
#' filin<-resampleImage(fi,c(1.5,1.5),0,1)
#'
#' @export resampleImage
resampleImage <- function(image,resampleParams, useSpacing=1, interpType=1 )
  {
  outimg<-antsImageClone( image )
  rsampar<-paste(resampleParams,collapse='x')
  args<-list(image@dimension,image,outimg,rsampar,useSpacing,interpType)
  print(args)
  retval<-.Call("ResampleImage", int_antsProcessArguments(args) )
  return( outimg )
  }
