compcor <- function(fmri, ncompcor = 4, variance_extreme = 0.975, mask = NA, useimagemath = FALSE) {
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
  temporalvar <- apply(mat, 2, var)
  tvhist <- hist(temporalvar, breaks = c("FD"), plot = T)
  percvar <- variance_extreme  # percentage of high variance data to use
  # get total counts
  totalcounts <- sum(tvhist$counts)
  wh <- (cumsum(tvhist$counts) < (totalcounts * percvar))
  thresh <- max(tvhist$mids[wh])
  # cumulativesum<-rev( cumsum( tvhist$counts / totalcounts ) ) thresh<-max( tvhist$mids[ ( cumulativesum >
  # percvar ) ] )
  wh <- (temporalvar > thresh)
  wh2 <- (temporalvar <= thresh)
  highvarmat <- mat[, wh]
  compcorrsvd <- svd(highvarmat)
  if (ncompcor > 0) {
    compcorr <- (compcorrsvd$u[, 1:ncompcor])
    compcorrnames <- paste("compcorr", c(1:ncol(compcorr)), sep = "")
    nuis <- compcorr
    colnames(nuis) <- c(compcorrnames)
  }
  return(nuis)
} 
