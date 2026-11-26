#' Simple taskFMRI function.
#'
#' Input 4D time series matrix. (Perform slice timing correction externally).
#' Estimate hemodynamicRF from block design. Compute brain mask on average bold
#' image.  Get nuisance variables : motion , compcor , globalsignal.
#' High-frequency filter the time series ( externally ). Correct for
#' autocorrelation using bullmore 1996 MRM and AR(2) model with parameters
#' derived from global residual signal. Estimate final glm.
#'
#'
#' @param mat input matrix (time by voxels)
#' @param hrf input hrf
#' @param myvars output of getfMRInuisanceVariables: a list with
#' \code{nuisancevariables} (matrix of nuisance predictors, e.g. the
#' \code{denoisingParams1}, \code{denoisingParams2}, ... columns produced by
#' \code{getfMRInuisanceVariables}), \code{globalsignal} and, optionally,
#' \code{motion} (matrix of motion predictors, named \code{motion1}, ... if
#' unnamed).
#' @param correctautocorr correct for temporal autocorrelation.  An AR(2)
#' model (Burg) is estimated from the mean residual of the full model
#' (task + nuisance) and the same AR filter is applied to the response and to
#' every column of the design before the full model is refit (iterated, as in
#' Bullmore et al., 1996).
#' @param residualizedesignmatrix boolean, residualize the nuisance
#' predictors against \code{hrf} before fitting
#' @param myformula right hand side (without \code{hrf}) of the model fitted
#' at each voxel, as a character string.  By default all supplied nuisance
#' predictors are used: \code{globalsignal}, every column of
#' \code{myvars$nuisancevariables} and every column of \code{myvars$motion}.
#' @return list with entries
#' \describe{
#' \item{beta}{the t-statistic of the \code{hrf} term at each voxel (kept
#' under this name for backward compatibility; identical to \code{tstat})}
#' \item{tstat}{the t-statistic of the \code{hrf} term at each voxel}
#' \item{coef}{the estimated \code{hrf} coefficient at each voxel}
#' \item{fmrimat}{the (filtered, if \code{correctautocorr}) response}
#' \item{adesmat}{the (filtered, if \code{correctautocorr}) design}
#' }
#' @author Avants BB
#' @references Bullmore E., et al. (1996) Statistical methods of estimation
#' and inference for functional MR image analysis. Magn Reson Med 35:261-277.
#' @examples
#' \dontrun{
#' # read the fmri image in and maybe do slice timing correction
#' fmri <- getANTsRData("pcasl")
#' fmri <- antsImageRead(fmri)
#' #  fmri<-iMath(fmri,"SliceTimingCorrection","bspline") # optional
#' myvars <- getfMRInuisanceVariables(fmri, moreaccurate = 0, maskThresh = 100)
#' mat <- myvars$matrixTimeSeries
#' mat <- frequencyFilterfMRI(mat, 2.5, freqLo = 0.01, freqHi = 0.1, opt = "butt")
#' blockfing <- c(0, 36, 72)
#' hrf <- hemodynamicRF(
#'   scans = dim(fmri)[4], onsets = blockfing,
#'   durations = rep(12, length(blockfing)), rt = 2.5, mean = TRUE
#' )
#' activationT <- taskFMRI(mat, hrf, myvars)$tstat
#' }
#'
#' @export taskFMRI
taskFMRI <- function(
    mat, hrf, myvars,
    correctautocorr = FALSE, residualizedesignmatrix = FALSE,
    myformula = NA) {
  if (nargs() == 0) {
    print("Usage:  betas<-taskFMRI( x , hrf ) ")
    return(1)
  }
  mat <- as.matrix(mat)
  nt <- nrow(mat)
  .asPredictors <- function(x, prefix) {
    if (is.null(x) || all(is.na(x))) {
      return(NULL)
    }
    x <- as.matrix(x)
    if (nrow(x) != nt && ncol(x) == nt) {
      x <- t(x)
    }
    if (nrow(x) != nt) {
      stop(prefix, " predictors must have one row per time point")
    }
    if (is.null(colnames(x))) {
      colnames(x) <- paste0(prefix, seq_len(ncol(x)))
    }
    x
  }
  # name the columns explicitly so that the formula terms (hrf,
  # globalsignal, ...) are found in the design data frame
  desmat <- cbind(
    globalsignal = as.numeric(myvars$globalsignal),
    .asPredictors(myvars$nuisancevariables, "nuisance"),
    .asPredictors(myvars$motion, "motion")
  )
  if (all(is.na(myformula))) {
    myformula <- paste(paste0("`", colnames(desmat), "`"), collapse = " + ")
  }
  if (residualizedesignmatrix) {
    desmat <- residuals(lm(desmat ~ as.numeric(hrf)))
  }
  desmat <- cbind(hrf = as.numeric(hrf), desmat)
  myformhrf <- as.formula(paste("amat ~ hrf + ", myformula))
  amat <- mat
  adesmat <- desmat
  if (correctautocorr) {
    # Bullmore et al. (1996): estimate AR coefficients from the residual of
    # the full model, prewhiten the response and the full design with the
    # same filter, and iterate.  Nuisance terms are never removed from the
    # response separately, so task variance shared with them is kept.
    initialarcoefs <- NULL
    for (i in 1:11) {
      residmat <- residuals(lm(myformhrf, data = data.frame(adesmat, check.names = FALSE)))
      residsig <- rowMeans(as.matrix(residmat))
      arcoefs <- ar(residsig, FALSE, 2, method = "burg")$ar
      if (is.null(initialarcoefs)) {
        initialarcoefs <- arcoefs
      }
      if (length(arcoefs) == 0 || abs(arcoefs[1]) <= 0.05) {
        break
      }
      amat <- .taskARfilter(amat, arcoefs)
      adesmat <- .taskARfilter(adesmat, arcoefs)
    }
    residmat <- residuals(lm(myformhrf, data = data.frame(adesmat, check.names = FALSE)))
    arcoefs <- ar(rowMeans(as.matrix(residmat)), FALSE, 2, method = "burg")$ar
    message("initial AR coefficients: ", paste(signif(initialarcoefs, 3), collapse = ", "),
      "; final: ", paste(signif(arcoefs, 3), collapse = ", "))
  }
  # final glm: (filtered) response on the full (filtered) design
  fit <- lm(myformhrf, data = data.frame(adesmat, check.names = FALSE))
  sm <- summary(fit)
  if (ncol(amat) == 1) {
    sm <- list(sm)
  }
  coefs <- vapply(sm, function(x) coefficients(x)["hrf", 1], numeric(1))
  tstat <- vapply(sm, function(x) coefficients(x)["hrf", 3], numeric(1))
  names(coefs) <- names(tstat) <- NULL
  tstat[is.na(tstat)] <- 0
  coefs[is.na(coefs)] <- 0
  return(list(
    beta = tstat, tstat = tstat, coef = coefs,
    fmrimat = amat, adesmat = adesmat
  ))
}

# apply the AR whitening operator x_t - sum_k a_k x_{t-k} to every column;
# the first rows, which lack some lags, reuse the earliest available value
# (as in the original implementation)
.taskARfilter <- function(x, arcoefs) {
  x <- as.matrix(x)
  out <- x
  n <- nrow(x)
  for (k in seq_along(arcoefs)) {
    lagged <- x[c(rep(1, min(k, n)), seq_len(max(n - k, 0))), , drop = FALSE]
    out <- out - arcoefs[k] * lagged
  }
  out
}



.arCorrection <- function(boldmatin, loops = 3, armethod = "burg") {
  boldmat <- boldmatin
  for (i in 1:loops) {
    arcoefs <- ar(boldmat, FALSE, 2, method = armethod)$ar # use something similar to  SPM's  autoregression estimate
    if (i == 1) {
      initialarcoefs <- arcoefs
    }
    if (abs(arcoefs[1]) > 0.05) {
      mat1 <- magic::ashift(boldmat, c(1, 0))
      mat1[1, ] <- boldmat[1, ]
      mat2 <- magic::ashift(boldmat, c(2, 0))
      mat2[1, ] <- boldmat[1, ]
      mat2[2, ] <- boldmat[2, ]
      if (length(arcoefs) == 2) {
        boldmat <- boldmat - mat1 * arcoefs[1] - mat2 * arcoefs[2]
      }
      if (length(arcoefs) == 1) {
        boldmat <- boldmat - mat1 * arcoefs[1]
      }
    }
  }
  # new regression
  arcoefs <- ar(boldmat, FALSE, length(arcoefs), method = armethod)$ar # use something similar to  SPM's  autoregression estimate
  if (mean(abs(sum(arcoefs))) > mean(abs(sum(initialarcoefs)))) {
    print(paste("final arcoefs", arcoefs))
    print(paste("initi arcoefs", initialarcoefs))
  }
  return(list(outmat = boldmat, initcoefs = initialarcoefs, arcoefs = arcoefs))
}
