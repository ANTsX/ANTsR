#' Compcors the input matrix using SVD and returns the result.
#'
#' @param fmri input fmri image or matrix
#' @param ncompcor n compcor vectors
#' @param variance_extreme high variance threshold e.g 0.95 for 95 percent
#' @param mask optional mask for image
#' @param randomSamples take this many random samples to speed things up
#' @param returnv return the spatial vectors
#' @param returnhighvarmat bool to return the high variance matrix
#' @param returnhighvarmatinds bool to return the high variance matrix indices
#' @param highvarmatinds index list
#' @param scale scale the matrix of high variance voxels, default FALSE. note
#' that you may get slightly different results by scaling the input matrix
#' before passing into this function.
#' @return dataframe of nuisance predictors is output
#' @author Avants BB
#' @examples
#'
#' mat <- matrix( rnorm(50000) ,ncol=500)
#' compcorrdf<-compcor( mat )
#'
#' @export compcor
compcor <- function(fmri, ncompcor = 4,
  variance_extreme = 0.975,
  mask = NA, randomSamples = 1,
  returnv = FALSE, returnhighvarmat = FALSE,
  returnhighvarmatinds = FALSE,
  highvarmatinds = NA,
  scale = TRUE ) {
  if (nargs() == 0) {
    print("Usage:  compcorr_df<-compcor( fmri, mask ) ")
    return(1)
  }
  if (class(fmri)[1] == "antsImage" & is.na(mask)) {
    print("Need to input a mask too")
    print(args(compcor))
    return(NULL)
  }
  if (class(fmri)[1] == "antsImage" & !is.na(mask)) {
    mat <- timeseries2matrix(fmri, mask)
  }
  if ( length(grep("matrix",class(fmri) ) ) > 0 ) {
    mat <- fmri
  }
  if (is.na(highvarmatinds)) {
    temporalvar <- apply(mat, 2, var)
    tvhist <- hist(temporalvar, breaks = c("FD"), plot = F)
    percvar <- variance_extreme  # percentage of high variance data to use
    # get total counts
    totalcounts <- sum(tvhist$counts)
    wh <- (cumsum(tvhist$counts) < (totalcounts * percvar))
    thresh <- max(tvhist$mids[wh])
    highvarmatinds <- which(temporalvar > thresh)
  }
  highvarmat <- mat[, highvarmatinds]
  if ( scale ) highvarmat = scale( highvarmat , scale = FALSE )
  if (returnhighvarmatinds)
    return(highvarmatinds)
  if (!returnv) {
    compcorrsvd <- svd(highvarmat, nu = ncompcor, nv = 0)
    if (ncompcor > 0) {
      compcorr <- (compcorrsvd$u[, 1:ncompcor])
      compcorrnames <- paste("compcorr", c(1:ncol(compcorr)), sep = "")
      nuis <- compcorr
      colnames(nuis) <- c(compcorrnames)
    }
    return(nuis)
  }
  if (returnv) {
    compcorrsvd <- svd(highvarmat, nu = 0, nv = ncompcor)
    if (ncompcor > 0) {
      compcorr <- (compcorrsvd$v[, 1:ncompcor])
      compcorrnames <- paste("compcorr", c(1:ncol(compcorr)), sep = "")
      nuis <- compcorr
      colnames(nuis) <- c(compcorrnames)
    }
    return(nuis)
  }
}
