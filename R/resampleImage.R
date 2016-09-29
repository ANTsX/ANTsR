#' resampleImage
#'
#' Resample image by spacing or number of voxels with various interpolators.
#' Works with multi-channel images.
#'
#' @param image input antsImage matrix
#' @param resampleParams vector of size dimension with numeric values
#' @param useVoxels true means interpret resample params as voxel counts
#' @param interpType one of 0 (linear), 1 (nearest neighbor),
#'   2 (gaussian), 3 (windowed sinc), 4 (bspline)
#' @return output antsImage
#' @author Avants BB
#' @examples
#'
#' fi<-antsImageRead( getANTsRData("r16"))
#' finn<-resampleImage(fi,c(50,60),TRUE,0)
#' filin<-resampleImage(fi,c(1.5,1.5),FALSE,1)
#'
#' @export resampleImage
resampleImage <- function(image, resampleParams, useVoxels = FALSE, interpType = 1) {
  if ( image@components == 1 )
    {
    inimg <- antsImageClone(image, "double")
    outimg <- antsImageClone(image, "double")
    rsampar <- paste(resampleParams, collapse = "x")
    args <- list(image@dimension, inimg, outimg, rsampar,
      as.numeric(useVoxels), interpType)
    k <- .int_antsProcessArguments(args)
    retval <- .Call("ResampleImage", k)
    outimg <- antsImageClone(outimg, image@pixeltype)
    return(outimg)
    }
  if ( image@components > 1 )
    {
    mychanns = splitChannels( image )
    for ( k in 1:length( mychanns ) )
      {
      inimg <- antsImageClone( mychanns[[k]], "double")
      outimg <- antsImageClone( mychanns[[k]], "double")
      rsampar <- paste(resampleParams, collapse = "x")
      args <- list( image@dimension, inimg, outimg, rsampar,
        as.numeric(useVoxels), interpType)
      temp <- .int_antsProcessArguments(args)
      retval <- .Call("ResampleImage", temp)
      mychanns[[k]] <- antsImageClone(outimg, image@pixeltype)
      }
    return( mergeChannels( mychanns ) )
    }
}
