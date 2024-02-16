#' fsl2antsrTransform
#'
#' Convert an FSL linear transform to an antsrTransform
#'
#' @param reference, target image read in as antsImage
#' @param moving image, read in as antsImage
#' @param matrix  4x4 matrix of parameters
#' @author JT Duda
#' @examples
#' fslMat = matrix(0,4,4)
#' diag(fslMat) = rep(1,4)
#' fslMat[1:3,4] = c(2,3,4)
#' img = antsImageRead(getANTsRData("ch2"))
#' tx = fsl2antsrTransform(fslMat, img, img)
#'
#' @export fsl2antsrTransform
fsl2antsrTransform <- function( matrix, reference, moving ) {
  retval = ANTsRCore::fsl2antsrTransform(matrix, reference, moving, 1)
  return( retval )
}
