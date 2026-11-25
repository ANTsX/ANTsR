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
#' mat <- matrix(rnorm(50000), ncol = 500)
#' compcorrdf <- compcor(mat)
#'
#' @export compcor
compcor <- function(
    fmri, ncompcor = 4,
    variance_extreme = 0.975,
    mask = NULL, randomSamples = 1,
    returnv = FALSE, returnhighvarmat = FALSE,
    returnhighvarmatinds = FALSE,
    highvarmatinds = NA,
    scale = TRUE) {
  if (nargs() == 0) {
    print("Usage:  compcorr_df<-compcor( fmri, mask ) ")
    return(1)
  }
  # checked before extracting the time series
  if (!.isNA1(highvarmatinds)) {
    .stopIfNA(highvarmatinds, "highvarmatinds")
  }
  if (is.antsImage(fmri) & is.null(mask)) {
    print("Need to input a mask too")
    print(args(compcor))
    return(NULL)
  }
  if (is.antsImage(fmri) & !is.null(mask)) {
    mask <- check_ants(mask)
    mat <- timeseries2matrix(fmri, mask)
  }
  if (inherits(fmri, "matrix")) {
    mat <- fmri
  }
  if (ncol(mat) == 0) {
    stop("compcor needs at least one voxel")
  }
  if (.isNA1(highvarmatinds)) {
    temporalvar <- apply(mat, 2, var)
    if (length(unique(temporalvar)) < 2) {
      # too few distinct voxel variances for a histogram cut-off
      highvarmatinds <- seq_len(ncol(mat))
    } else {
      tvhist <- hist(temporalvar, breaks = c("FD"), plot = FALSE)
      percvar <- variance_extreme # percentage of high variance data to use
      # get total counts
      totalcounts <- sum(tvhist$counts)
      wh <- (cumsum(tvhist$counts) < (totalcounts * percvar))
      if (any(wh)) {
        thresh <- max(tvhist$mids[wh])
        highvarmatinds <- which(temporalvar > thresh)
      } else {
        highvarmatinds <- seq_len(ncol(mat))
      }
    }
  }
  highvarmat <- mat[, highvarmatinds, drop = FALSE]
  if (scale) highvarmat <- scale(highvarmat, scale = FALSE)
  if (returnhighvarmatinds) {
    return(highvarmatinds)
  }
  # cap at the numerical rank: after centering, constant or duplicated
  # voxels add no information, and extra singular vectors would be
  # arbitrary (e.g. an indicator of the first time point)
  sv <- svd(highvarmat, nu = 0, nv = 0)$d
  tol <- max(dim(highvarmat)) * max(c(sv, 0)) * .Machine$double.eps
  maxcomp <- if (length(sv) == 0 || max(sv) == 0) 0L else sum(sv > tol)
  if (ncompcor > maxcomp) {
    warning("Only ", maxcomp, " CompCor components can be estimated (rank of ",
      ncol(highvarmat), " high-variance voxels over ", nrow(highvarmat),
      " time points); using ", maxcomp, " instead of ", ncompcor, ".",
      call. = FALSE
    )
    ncompcor <- maxcomp
  }
  if (ncompcor == 0) {
    return(matrix(numeric(0), nrow = if (returnv) ncol(highvarmat) else nrow(highvarmat), ncol = 0))
  }
  if (!returnv) {
    compcorrsvd <- svd(highvarmat, nu = ncompcor, nv = 0)
    nuis <- compcorrsvd$u[, seq_len(ncompcor), drop = FALSE]
  } else {
    compcorrsvd <- svd(highvarmat, nu = 0, nv = ncompcor)
    nuis <- compcorrsvd$v[, seq_len(ncompcor), drop = FALSE]
  }
  colnames(nuis) <- paste("compcorr", seq_len(ncompcor), sep = "")
  nuis
}
