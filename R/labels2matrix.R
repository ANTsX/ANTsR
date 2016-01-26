#' Convert label image to a matrix
#'
#' Convert a labeled image to an n x m binary matrix where n = number of voxels
#' and m = number of labels. Only includes values inside the provided mask while
#' including background ( img == 0 ) for consistency with timeseries2matrix and
#' other image to matrix operations.
#'
#' @param img input label image
#' @param mask defines domain of interest
#' @return matrix is output
#' @author Avants BB
#' @examples
#'
#' fi = antsImageRead(getANTsRData("r16") ,2) %>% resampleImage(c(60,60),1,0)
#' mask = getMask( fi )
#' labs = kmeansSegmentation( fi, 3 )$segmentation
#' labmat = labels2matrix( labs, mask )
#'
#' @export labels2matrix
labels2matrix <- function( img, mask )
{
  if ( any( dim( img ) != dim( mask ) ) )
    {
    stop("image and mask must be same size")
    }
  vec <- subset( img, mask > 0 )
  theLabels <- sort( unique( vec ) )
  mylabelnames = as.character( theLabels )
  nLabels <- length( theLabels )
  labels <- matrix( 0, nrow = nLabels, ncol = length( vec ) )
  for ( i in 1:nLabels )
    {
    lab = as.numeric( theLabels[ i ] )
    labels[i, ] <- as.numeric( vec == lab )
    }
  return( labels )
}
