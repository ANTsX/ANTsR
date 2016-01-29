#' Time-series image to matrix
#' 
#' Extract a matrix from a time-series image after applying a mask.
#' 
#' 
#' @param img Input image of type 'antsImage' or R array.
#' @param mask Input mask of type 'antsImage' or R array. In either case, the
#' number of voxels in the mask may be either equal to that of input image
#' 'img' or equal to number of voxels in one time unit of input image 'img'. In
#' the second case, the mask is reused for every time unit of the 'img'.  A
#' mask of n-labels will cause the function to return a matrix containing the
#' mean time series within each label.
#' @return Success -- an R matrix of dimensions ( dim(img)[length(dim(img))] ,
#' sum(mask==1) )\cr
#' @author Shrinidhi KL
#' @examples
#' 
#' img <- makeImage( c(10,10,10,5) , 0 )
#' # or use antsImageRead ...
#' mask <- array( 1 , dim(img)[1:3] )
#' mat <- timeseries2matrix( img , mask )
#' 
#' @export timeseries2matrix
timeseries2matrix <- function(img, mask) {
  labs <- sort(unique(mask[mask > 0.001]))
  if (length(labs) == 1) 
    logmask <- (mask == 1) else logmask <- (mask > 0)
  mat <- img[logmask]
  dim(mat) <- c(sum(logmask), dim(img)[length(dim(img))])
  mat <- t(mat)
  if (length(labs) == 1) 
    return(mat)
  maskvec <- mask[logmask]
  mmat <- matrix(apply(as.matrix(mat[, maskvec == labs[1]]), FUN = mean, MARGIN = 1), ncol = 1)
  for (i in 2:length(labs)) {
    newmat <- matrix(apply(as.matrix(mat[, maskvec == labs[i]]), FUN = mean, MARGIN = 1), 
      ncol = 1)
    mmat <- cbind(mmat, newmat)
  }
  colnames(mmat) <- paste("L", labs)
  return(mmat)
} 
