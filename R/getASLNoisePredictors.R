#' Get nuisance predictors from ASL images
#' 
#' Get nuisance predictors from ASL images
#' 
#' 
#' @param aslmat ASL input matrix.
#' @param tc Tag-control sawtooth pattern vector.
#' @param noisefrac Fraction of data to include in noise pool.
#' @param polydegree Degree of polynomial for detrending. A value of 0
#' indicates no detrending.
#' @param k Number of cross-validation folds.
#' @param npreds Number of predictors to output.
#' @param method Method of selecting noisy voxels.  One of 'compcor' or
#' 'noisepool'. See \code{Details}.
#' @param covariates Covariates to be considered when assessing prediction of
#' tc pattern.
#' @param noisepoolfun Function used for aggregating R^2 values.
#' @return Matrix of size \code{nrow(aslmat)} by \code{npreds}, containing a
#' timeseries of all the nuisance predictors.
#' @author Brian B. Avants, Benjamin M. Kandel
#' @examples
#' 
#' moco <- antsMotionCalculation(getANTsRData('pcasl'))
#' aslmat <- timeseries2matrix(moco$moco_img, moco$moco_mask)
#' tc <- rep(c(0.5, -0.5), length.out=nrow(aslmat))
#' noise <- getASLNoisePredictors(aslmat, tc)
#' 
#' @export getASLNoisePredictors
getASLNoisePredictors <- function(aslmat, tc, noisefrac = 0.1, polydegree = 3, k = 5, 
  npreds = 12, method = "noisepool", covariates = NA, noisepoolfun = max) {
  getnoisepool <- function(x, frac = noisefrac) {
    xord <- sort(x)
    l <- round(length(x) * frac)
    val <- xord[l]
    return(x < val & x < 0)
  }
  
  if (polydegree > 0) {
    timevals <- 1:nrow(aslmat)
    p <- stats::poly(timevals, degree = polydegree)
    aslmat <- residuals(lm(aslmat ~ 0 + p))
    if (!all(is.na(covariates))) {
      covariates <- cbind(data.matrix(covariates), p)
    } else covariates <- p
  }
  
  R2base <- crossvalidatedR2(aslmat, tc, k, covariates = covariates)
  R2base <- apply(R2base, FUN = noisepoolfun, MARGIN = 2)
  noisepool <- getnoisepool(R2base)
  if (all(noisepool == TRUE)) {
    stop("all voxels meet your pvalthresh - try increasing the value")
  }
  if (all(noisepool == FALSE)) {
    stop("zero voxels meet your pvalthresh - try decreasing the value")
  }
  if (method == "compcor") {
    noiseu <- compcor(aslmat, npreds)
  } else if (method == "noisepool") {
    noiseu <- svd(aslmat[, noisepool], nv = 0, nu = npreds)$u
  }
  noiseu
} 
