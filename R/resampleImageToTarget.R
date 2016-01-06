#' resampleImageToTarget
#'
#' Resample image by using another image as target reference.
#'
#' @param image input antsImage matrix
#' @param target reference image to use for voxel size and header info
#' @param interpType one of 0 (linear), 1 (nearest neighbor),
#'   2 (gaussian), 3 (windowed sinc), 4 (bspline)
#' @return output antsImage
#' @author Pustina D
#' @examples
#'
#' fi<-antsImageRead( getANTsRData("r16"))
#' finn<-resampleImage(fi,c(50,60),1,0)
#' filin<-resampleImage(fi,c(1.5,1.5),0,1)
#'
#' @export resampleImage
resampleImage <- function(image, target, interpType = 1) {
  inimg <- antsImageClone(image, "double")
  outimg <- antsImageClone(image, "double")
  rsampar <- paste(dim(target), collapse = "x")
  useVoxels <- 1
  args <- list(image@dimension, inimg, outimg, rsampar, useVoxels, interpType)
  k <- .int_antsProcessArguments(args)
  retval <- .Call("ResampleImage", k)
  outimg <- antsImageClone(outimg, image@pixeltype)
  outimg <- antsCopyImageInfo(target, outimg)
  return(outimg)
}
