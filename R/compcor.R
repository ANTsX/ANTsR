#' Simple compcor function.
#'
#' Compcors the input matrix using SVD and returns the result.
#'
#'
#' @param fmri input fmri image or matrix
#' @param ncompcor n compcor vectors
#' @param variance_extreme high variance threshold e.g 0.95 for 95 percent
#' @param mask optional mask for image
#' @param useimagemath use the imagemath implementation instead
#' @param randomSamples take this many random samples to speed things up
#' @param returnv return the spatial vectors
#' @param returnhighvarmat returns the high variance matrix on which svd is
#' @param highvarmatinds index list
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
  mask = NA,
  useimagemath = FALSE, randomSamples = 1,
  returnv = FALSE, returnhighvarmatinds = FALSE,
  highvarmatinds = NA) {
  if (nargs() == 0) {
    print("Usage:  compcorr_df<-compcor( fmri, mask ) ")
    return(1)
  }
  if (useimagemath & !is.na(mask)) {
    myoutfn <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".nii.gz")
    ImageMath(4, myoutfn, "CompCorrAuto", fmri, mask, ncompcor)
    mycsv <- sub(".nii.gz", "_compcorr.csv", myoutfn)
    myvarimg <- sub(".nii.gz", "_variance.nii.gz", myoutfn)
    varimage <- antsImageRead(myvarimg, 3)
    mycompcorrdf <- read.csv(mycsv)
    return(mycompcorrdf)
  }
  if (class(fmri)[1] == "antsImage" & is.na(mask)) {
    print("Need to input a mask too")
    print(args(compcor))
    return(NULL)
  }
  if (class(fmri)[1] == "antsImage" & !is.na(mask)) {
    mat <- timeseries2matrix(fmri, mask)
  }
  if (class(fmri)[1] == "matrix") {
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
