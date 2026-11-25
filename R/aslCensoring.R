#' Censor bad volumes from ASL data.
#'
#' @param asl input asl image
#' @param mask mask for calculating perfusion
#' @param nuis fixed nuisance parameters
#' @param method one of 'outlier', 'robust', or 'scor'.  See \code{Details}.
#' @param reject.pairs whether to reject only tag-control pairs of images,
#' as opposed to single images.  Rejecting pairs of images is necessary for
#' non-regression-based ASL averaging methods.
#' @param ... Additional arguments to pass to censoring method.  See \code{Details.}
#' @details \code{aslCensoring} is an interface to ASL timepoint censoring algorithms.
#' Three options are currently provided, with different additional arguments:
#' \describe{
#'  \item{\code{outlier}}{ Outlier rejection from Tan et al.  This method rejects
#'    volumes that are either far from the mean of the time-series or whose
#'    standard deviation is far from the standard deviations of the individual volumes.
#'    Accepts two additional arguments:
#'      \describe{
#'         \item{\code{sigma.mean}: }{how many standard
#'         deviations the mean of the volume can be from the
#'         mean of all the volumes before
#'         being thrown out.}
#'         \item{\code{sigma.sd}: }{how many standard deviations from the
#'         mean of standard deviations can the standard deviation of the volume be
#'         before being thrown out.}
#'      }
#' }
#'  \item{\code{robust}}{ Uses a robust regression approach to estimate volumes
#'    with high leverage.  Accepts three arguments:
#'    \describe{
#'      \item{\code{nuis}:}{ Nuisance regressors to use as covariates.}
#'      \item{\code{robthresh}:}{ Threshold for weights on leverage estimates.  Points
#'         with weights under this value will be thrown out; defaults to 0.95.}
#'      \item{\code{skip}:}{ Proportion of points to skip when estimating leverage.
#'        Defaults to 20 (1/20 of the image is used).}
#'     }
#'   }
#'   \item{\code{scor}}{ A simplified version of the SCORE method of Dolui
#'     et al.: frames most correlated with the mean are removed while the
#'     spatial variance of the mean decreases.  Unlike the published method it
#'     operates on the frames given rather than pairwise CBF maps, uses the
#'     total rather than the pooled within-tissue variance, and never removes
#'     the last frame.  No parameters.}
#' }
#' @return vector of the same length as number of timepoints in \code{asl}, with
#'  1 indicating the corresponding timepoint is included and 0 indicating exclusion.
#' @author Kandel BM
#' @examples
#' set.seed(1)
#' nvox <- 5 * 5 * 5 * 30
#' dims <- c(5, 5, 5, 30)
#' voxvals <- array(rnorm(nvox) + 500, dim = dims)
#' voxvals[, , , 5] <- voxvals[, , , 5] + 600
#' asl <- makeImage(dims, voxvals)
#' censored <- aslCensoring(asl)
#'
#' @references Tan H. et al., ``A Fast, Effective Filtering Method
#' for Improving Clinical Pulsed Arterial Spin Labeling MRI,'' JMRI 2009.
#' @export aslCensoring

aslCensoring <- function(asl, mask = NULL, nuis = NA, method = "outlier",
                         reject.pairs = FALSE, ...) {
  # Supporting functions for censoring data: robSelection and scor.
  robSelection <- function(mat, xideal, mask, nuis = NA, robthresh = 0.95, skip = 20) {
    cbfform <- formula(mat ~ xideal)
    rcbfform <- formula(mat[, vox] ~ xideal)
    if (!all(is.na(nuis))) {
      rmat <- residuals(lm(mat ~ nuis))
      cbfform <- formula(mat ~ xideal + nuis)
      rcbfform <- formula(rmat[, vox] ~ xideal)
    }
    if (!all(is.na(nuis))) {
      rmat <- residuals(lm(mat ~ nuis))
      cbfform <- formula(mat ~ xideal + nuis)
      rcbfform <- formula(rmat[, vox] ~ xideal)
    }
    mycbfmodel <- lm(cbfform) # standard regression
    betaideal <- ((mycbfmodel$coeff)[2, ])
    if (mean(betaideal) < 0) {
      betaideal <- (betaideal) * (-1)
    }
    cbfi <- antsImageClone(mask)
    cbfi[mask == 1] <- betaideal # standard results
    indstozero <- NULL
    ctl <- robustbase::lmrob.control("KS2011", max.it = 1000)
    regweights <- rep(0, nrow(mat))
    rbetaideal <- rep(0, ncol(mat))
    robvals <- mat * 0
    vox <- 1
    ct <- 0
    visitvals <- (skip:floor((ncol(mat) - 1) / skip)) * skip
    if (skip == 1) {
      visitvals <- 1:ncol(mat)
    }
    rgw <- regweights
    myct <- 0
    thisct <- 1
    for (vox in visitvals) {
      try(mycbfmodel <- robustbase::lmrob(rcbfform, control = ctl), silent = TRUE)
      rbetaideal[vox] <- mycbfmodel$coeff[2]
      if (!is.null(mycbfmodel$rweights)) {
        rgw <- rgw + mycbfmodel$rweights
        myct <- myct + 1
        robvals[, myct] <- mycbfmodel$rweights
      }
      thisct <- thisct + 1
    }
    regweights <- (rgw / myct)
    if (is.na(mean(regweights))) {
      regweights[] <- 1
    }
    # check if the robustness selects the blank part of the time series now use the
    # weights in a weighted regression
    indstozero <- which(regweights < (robthresh * max(regweights)))
    keepinds <- which(regweights > (robthresh * max(regweights)))
    if (length(keepinds) < 20) {
      indstozero <- which(regweights < (0.95 * robthresh * max(regweights)))
      keepinds <- which(regweights > (0.95 * robthresh * max(regweights)))
    }
    if (length(keepinds) < 20) {
      indstozero <- which(regweights < (0.5 * robthresh * max(regweights)))
      keepinds <- which(regweights > (0.5 * robthresh * max(regweights)))
    }
    regweights[indstozero] <- 0 # hard thresholding
    if (robthresh < 1 & robthresh > 0) {
      mycbfmodel <- lm(cbfform, weights = regweights)
    }
    betaideal <- ((mycbfmodel$coeff)[2, ])
    if (mean(betaideal) < 0) {
      betaideal <- (betaideal) * (-1)
    }
    indstozero
  }

  aslOutlierRejection <- function(
      asl, mask = NULL, centralTendency = median,
      sigma.mean = 2.5, sigma.sd = 2) {
    if (is.null(mask)) {
      avg <- getAverageOfTimeSeries(asl)
      avg <- n3BiasFieldCorrection(avg, 2)
      avg <- n3BiasFieldCorrection(avg, 2)
      mask <- getMask(avg, mean(avg), Inf)
    }
    nvox <- sum(mask[mask > 0])
    npairs <- dim(asl)[4] / 2
    tc <- rep(c(1, 2), npairs)
    aslmat <- timeseries2matrix(asl, mask)
    diffs <- matrix(rep(NA, nrow(aslmat) / 2 * ncol(aslmat)),
      nrow = nrow(aslmat) / 2
    )
    for (ii in 1:nrow(diffs)) {
      diffs[ii, ] <- aslmat[ii * 2, ] - aslmat[ii * 2 - 1, ]
    }

    if (mean(diffs) < 0) {
      diffs <- -diffs
    }

    ts.diff <- diffs
    centers <- apply(ts.diff, 1, centralTendency)
    mean.centers <- centralTendency(centers)
    sd.centers <- sd(centers)
    sds <- apply(ts.diff, 1, sd)
    mean.sds <- mean(sds)
    sd.sds <- sd(sds)
    which.outlierpairs <- which((abs(centers - mean.centers) > sigma.mean * sd.centers) |
      (abs(sds - mean.sds) > sigma.sd * sd.sds))
    which.outliers <- rep(which.outlierpairs, each = 2)
    tc.outliers <- rep(c(1, 2), length(which.outlierpairs))
    which.outliers[tc.outliers == 1] <- which.outliers[tc.outliers == 1] * 2 - 1
    which.outliers[tc.outliers == 2] <- which.outliers[tc.outliers == 2] * 2
    which.outliers
  }

  scor <- function(asl) {
    # Simplified SCORE (Dolui et al., 2017): repeatedly drop the frame most
    # correlated with the current mean and keep the removal while the
    # spatial variance of the mean decreases.  Unlike the published
    # method this works on the frames given (not pairwise CBF maps) and
    # uses the total spatial variance rather than the pooled within-tissue
    # variance.
    # asl is a time x voxel matrix (from timeseries2matrix): rows are frames
    asl <- t(asl) # voxel x time, so columns index timepoints below
    nframes <- dim(asl)[2]
    keep <- rep(TRUE, nframes)
    meancbf <- rowMeans(asl)
    var.best <- var(meancbf)
    # never remove the last remaining frame
    while (sum(keep) > 1) {
      cc <- rep(NA_real_, nframes)
      for (timepoint in which(keep)) {
        cc[timepoint] <- suppressWarnings(cor(meancbf, asl[, timepoint]))
      }
      # no frame has a valid correlation with the mean: stop
      if (all(is.na(cc))) {
        break
      }
      removed <- which.max(cc)
      candidate <- keep
      candidate[removed] <- FALSE
      meancand <- rowMeans(asl[, candidate, drop = FALSE])
      var.cand <- var(meancand)
      # stop (keeping the frame) when the variance does not improve
      if (!is.finite(var.cand) || !is.finite(var.best) || var.cand >= var.best) {
        break
      }
      keep <- candidate
      meancbf <- meancand
      var.best <- var.cand
    }
    which(!keep)
  }

  if (is.null(mask)) {
    myar <- apply(as.array(asl), c(1, 2, 3), mean)
    img <- makeImage(dim(myar), myar)
    antsSetSpacing(img, antsGetSpacing(asl)[1:3])
    antsSetOrigin(img, antsGetOrigin(asl)[1:3])
    antsSetDirection(img, antsGetDirection(asl)[1:3, 1:3])
    mask <- getMask(img)
  } else {
    mask <- check_ants(mask)
  }
  ts <- timeseries2matrix(asl, mask)

  if (method == "robust") {
    if (!usePkg("robust")) {
      print("Need robust package")
      return(NULL)
    }
    xideal <- (rep(
      c(1, 0),
      dim(ts)[1]
    )[1:dim(ts)[1]] - 0.5) # control minus tag
    which.outliers <- robSelection(ts, xideal, mask, nuis, ...)
  } else if (method == "outlier") {
    which.outliers <- aslOutlierRejection(asl, mask, ...)
  } else if (method == "scor") {
    which.outliers <- scor(ts)
  }

  if (reject.pairs) {
    odds <- which.outliers[which((which.outliers %% 2) == 1)]
    evens <- which.outliers[which((which.outliers %% 2) == 0)]
    odds.additional <- odds + 1
    evens.additional <- evens - 1
    which.outliers <- sort(unique(c(
      which.outliers,
      odds.additional, evens.additional
    )))
  }

  if (length(which.outliers) > 0) {
    aslmat.inlier <- ts[-which.outliers, , drop = FALSE]
  } else {
    aslmat.inlier <- ts
  }
  asl.inlier <- matrix2timeseries(asl, mask, aslmat.inlier)
  list(which.outliers = which.outliers, asl.inlier = asl.inlier)
}
