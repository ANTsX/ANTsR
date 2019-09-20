#' Get nuisance predictors from ASL images
#'
#' Get nuisance predictors from ASL images
#'
#'
#' @param aslmat ASL input matrix.
#' @param tc Tag-control sawtooth pattern vector.
#' @param noisefrac Fraction of data to include in noise pool.
#' @param polydegree Degree of polynomial for detrending, with a value of 0
#' indicating no detrending, or \code{'loess'} for LOESS-based estimation of
#' global time-series trends.
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
#' # for real data do img<-antsImageRead(getANTsRData("pcasl"),4)
#' set.seed(120)
#' img<-makeImage( c(10,10,10,20), rnorm(1000*20)+1 )
#' mask = getMask( getAverageOfTimeSeries( img ) )
#' aslmat <- timeseries2matrix( img, mask )
#' tc <- rep(c(0.5, -0.5), length.out=nrow(aslmat))
#' noise <- getASLNoisePredictors(aslmat, tc, k=2, npreds=2, noisefrac=0.5 )
#' cm = colMeans(noise)
#' if (getRversion() < "3.5.0") {
#'     testthat::expect_equal(cm, c(-0.223292128499263, 0.00434481670243642))
#' } else {
#'     testthat::expect_equal(cm, c(-0.223377249912075, 0.0012754214030999))
#' }
#'
#' 
#' @export getASLNoisePredictors
getASLNoisePredictors <- function(aslmat, tc, noisefrac = 0.1, polydegree = 'loess', k = 5,
  npreds = 12, method = "noisepool", covariates = NA, noisepoolfun = max) {
  getnoisepool <- function(x, frac = noisefrac) {
    xord <- sort(x)
    l <- round(length(x) * frac)
    val <- xord[l]
    return(x < val & x < 0)
  }

  if (is.numeric(polydegree)) {
    if (polydegree > 0) {
      timevals <- 1:nrow(aslmat)
      p <- stats::poly(timevals, degree = polydegree)
      aslmat <- residuals(lm(aslmat ~ 0 + p))
      if (!all(is.na(covariates))) {
        covariates <- cbind(data.matrix(covariates), p)
      } else covariates <- p
    }
  } else if (polydegree == 'loess') {
    timevals <- 1:nrow(aslmat)
    mean.ts <- apply(aslmat, 1, mean)
    myloess <- loess(mean.ts ~ timevals)
    p <- myloess$fitted
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
