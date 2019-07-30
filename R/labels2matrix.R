#' Convert label image to a matrix
#'
#' Convert a labeled image to an n x m binary matrix where n = number of voxels
#' and m = number of labels. Only includes values inside the provided mask while
#' including background ( img == 0 ) for consistency with timeseries2matrix and
#' other image to matrix operations.
#'
#' @param img input label image
#' @param mask defines domain of interest
#' @param targetLabels defines target regions to be returned.  if the target
#' label does not exist in the input label image, then the matrix will contain
#' a constant value of missingVal (default NA) in that row.
#' @param missingVal for missing label values.
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
labels2matrix <- function( img, mask, targetLabels = NULL, missingVal = NA )
{
  if ( any( dim( img ) != dim( mask ) ) )
    {
    stop("image and mask must be same size")
  }
  # vec <- subset( img, mask > 0 )
  vec = img[ mask > 0 ]
  theLabels <- sort( unique( vec ) )
  if ( ! is.null( targetLabels ) )  theLabels = targetLabels
  mylabelnames = as.character( theLabels )
  nLabels <- length( theLabels )
  labels <- matrix( 0, nrow = nLabels, ncol = length( vec ) )
  for ( i in 1:nLabels )
    {
    lab = as.numeric( theLabels[ i ] )
    filler = as.numeric( vec == lab )
    if ( sum( filler ) == 0 ) filler = rep( missingVal, length( vec ) )
    labels[i, ] <- filler
    }
  return( labels )
}
