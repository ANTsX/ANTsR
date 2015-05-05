#' @name combineNuisancePredictors
#' @title Combine and reduce dimensionality of nuisance predictors.
#' @description Combine and select nuisance predictors to maximize
#' correlation between \code{inmat} and \code{target}.
#' @usage combineNuisancePredictors(inmat, target, globalpredictors=NA,
#'   maxpreds=4, localpredictors=NA, method="cv", k=5, covariates=NA, ordered=F)
#' @param inmat Input predictor matrix.
#' @param target Target outcome matrix.
#' @param globalpredictors Global predictors of size \code{nrow(inmat)}
#' by \code{n}, where \code{n} is the number of global predictors.
#' @param maxpreds Maximum number of predictors to output.
#' @param localpredictors Local predictor array of size \code{nrow(inmat)}
#' by \code{ncol(inmat)} by \code{m}, where \code{m} is the number of
#' local predictors.
#' @param k Number of cross-validation folds.
#' @param method Method of selecting noisy voxels.  One of 'svd' or 'cv'.
#' See \code{Details}.
#' @param covariates Covariates to be considered when assessing prediction
#' of \code{target}.
#' @param ordered Can the predictors be assumed to be ordered from most
#' important to least important, as in output from PCA?
#' Computation is much faster if so.
#' @return Array of size \code{nrow(aslmat)} by \code{npreds},
#' containing a timeseries of all the nuisance predictors.
#' If \code{localpredictors} is not NA, array is of size \code{nrow(aslmat)}
#' by \code{ncol(aslmat)} by \code{npreds}.
#' @author Benjamin M. Kandel, Brian B. Avants
#' @examples
#' set.seed(120)
#' simimg<-makeImage( c(10,10,10,20), rnorm(1000*20) )
#' moco <- antsMotionCalculation( simimg , moreaccurate=0)
#' # for real data use below
#' # moco <- antsMotionCalculation(getANTsRData("pcasl"))
#' aslmat <- timeseries2matrix(moco$moco_img, moco$moco_mask)
#' tc <- rep(c(0.5, -0.5), length.out=nrow(aslmat))
#' noise <- getASLNoisePredictors(aslmat, tc, 0.5 )
#' noise.sub <- combineNuisancePredictors(aslmat, tc, noise, 2)
#' @export combineNuisancePredictors
combineNuisancePredictors <- function(inmat, target, globalpredictors = NA, maxpreds = 4,
  localpredictors = NA, method = "cv", k = 5, covariates = NA, ordered = F) {
  avgR2 <- function(inmat, target, k, covariates) {
    r2 <- crossvalidatedR2(inmat, target, k, covariates, fast = T)
    r2max <- apply(r2, FUN = median, MARGIN = 2)
    r2pos <- r2max[r2max > 0]
    median(r2pos)
  }
  bestPredictors <- function(inmat, noisemat, target = target, maxpreds = maxpreds,
    method = method, k = k, covariates = covariates, ordered = ordered) {
    if (method == "svd") {
      noise <- svd(noisemat, nv = 0, nu = maxpreds)$u
    } else if (method == "cv") {
      r2sum <- rep(0, length(maxpreds))
      bestpreds <- NULL
      if (ordered) {
        for (ii in 1:ncol(globalpredictors)) {
          residmat <- residuals(lm(inmat ~ globalpredictors[, ii]))
          r2sum[ii] <- avgR2(residmat, target, k, covariates)
        }
        scl <- 0.95
        if (max(r2sum, na.rm = T) < 0)
          scl <- 1.05
        mxt <- scl * max(r2sum, na.rm = T)
        bestpreds <- (1:maxpreds)[which(r2sum > mxt)[1]]
      } else {
        r2sum <- matrix(rep(0, ncol(globalpredictors) * maxpreds), ncol = maxpreds)
        for (ii in 1:maxpreds) {
          if (is.null(bestpreds)) {
          preds.remain <- 1:ncol(globalpredictors)
          } else {
          preds.remain <- (1:ncol(globalpredictors))[-bestpreds]
          }
          for (jj in preds.remain) {
          residmat <- residuals(lm(inmat ~ globalpredictors[, c(bestpreds,
            jj)]))
          r2sum[jj, ii] <- avgR2(residmat, target, k, covariates)
          }
          bestpreds <- c(bestpreds, which.max(r2sum[, ii]))
        }
      }
      noise <- noisemat[, bestpreds]
    }
    noise
  }

  noise <- bestPredictors(inmat, globalpredictors, target, maxpreds, method, k,
    covariates, ordered)
  noise
}
