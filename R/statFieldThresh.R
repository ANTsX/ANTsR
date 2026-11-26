#' Produces a threshold value based on cluster or voxel level statistics
#'
#' @param x statistical map of class antsImage
#' @param pval p-value for determining threshold
#' @param nvox minimum desired cluster size (in voxels)
#' @param n number of images in conjunction
#' @param fwhm full width at half maxima
#' @param resels resolution elements in search region
#' @param df degrees of freedom expressed as df = c(degrees of interest, degrees of error).
#' Following \code{\link{euler}}, T- and chi-square fields use \code{df[2]},
#' F-fields use \code{df[1]} and \code{df[2]}.
#' @param fieldType
#' \describe{
#' \item{T: }{T-field}
#' \item{F: }{F-field}
#' \item{X: }{Chi-square field'}
#' \item{Z: }{Gaussian field}
#' }
#' @param threshType
#' \describe{
#' 	\item{cRFT: }{computes a threshold per expected cluster level probability}
#' 	\item{pRFT: }{uses the mask and pval calculates the minimum statistical threshold}
#' 	\item{cFDR: }{cluster-extent FDR at the primary height \code{pp} (as \code{spm_uc_clusterFDR})}
#' 	\item{pFDR: }{topological peak FDR on local maxima above the primary height \code{pp} (as \code{spm_uc_peakFDR})}
#' }
#' @param pp primary (initial) p-value threshold used in FDR methods
#' @param verbose enables verbose output
#' @return Outputs a statistical value to be used for threshold a SPM.
#' For \code{"cFDR"} this is the primary height, with the FDR cluster-extent
#' threshold (in voxels, at least \code{nvox}) in \code{attr(u, "k")}.
#' @details
#'
#' A statistical threshold level is determined using the estimated p-value \code{pval}
#' given the provided parameters. "cRFT" and "pRFT" specify the method of estimation
#' should use RFT cluster and peak statistic estimates respectively.
#'
#' In addition to RFT based thresholds the user may choose to utilize
#' topological false-discovery rate (FDR) thresholds (Chumbley et al., 2010).
#' Both use a primary p-value threshold (default \code{pp = .001}) to define a
#' primary height. "pFDR" applies Benjamini-Hochberg at \code{pval} to the
#' conditional p-values of the local maxima above that height,
#' \eqn{E[EC](z_i) / E[EC](u)}, and returns the observed height of the last
#' peak that passes, so the threshold is never below the primary height.
#' "cFDR" keeps the primary height and applies Benjamini-Hochberg to the
#' uncorrected cluster-extent p-values of the clusters at that height; the
#' smallest passing extent (in voxels) is returned as \code{attr(u, "k")}.
#'
#' It is important that the user recognize that when statistical analyses are computed
#' using RFT that the threshold level plays a role in predicting the p-value. This is
#' because the probability of obtaining results is weighted against the probability of
#' any occurence given the threshold. For example, if two clusters where exactly the
#' same but had one was obtained using a lower threshold it would have a lower p-value.
#' This has been shown with a power analysis using similar parameters to those presented
#' herein with the additional variable of signal characteristics to demonstrate the effect
#' of image modality on analysis (Friston et al., 1996). Therefore, parameters should be
#' chosen according to the type of analysis being performed (fMRI, PET, or VBM) and the
#' hypothesis being tested. Due to widely varying oppinions on appropriate thresholding
#' procedures no specific recommendations are made here. This function simply facilitates
#' the use of several approaches that users may utilize after consulting available literature.
#'
#' @references
#' Chumbley J., (2010) Topological FDR for neuroimaging
#'
#' Friston K.J., (1994) Assessing the Significance of Focal Activations Using Their Spatial Extent
#'
#' Friston K.J., (1996) Detecting Activations in PET and fMRI: Levels of Inference and Power
#' @author Zachary P. Christensen
#' @examples
#' \dontrun{
#' # threshold according to estimated peak height
#' thresh_image <- statFieldThresh(timg, .05, 10, 1, mysmooth$fwhm,
#'   c(1, fit$df.residual),
#'   fieldType = "T",
#'   threshType = pRFT
#' )
#' }
#' @export statFieldThresh
statFieldThresh <- function(x, pval, nvox, n, fwhm, resels, df,
                            fieldType, threshType, pp = .001, verbose = FALSE) {
  D <- x@dimension
  vox2res <- 1 / prod(fwhm)
  k <- nvox * vox2res
  # RFT based thresholding ------------------------------------------------------------------------------------------
  if (threshType == "cRFT" | threshType == "pRFT") {
    u <- max(x)
    if (threshType == "cRFT") {
      alpha <- rftPval(D = D, c = 1, k = k, u = u, n = n, resels = resels, df = df, fieldType = fieldType)$Pcor
    } else if (threshType == "pRFT") {
      alpha <- rftPval(D = D, c = 1, k = 0, u = u, n = n, resels = resels, df = df, fieldType = fieldType)$Pcor
    }
    if (alpha > pval) {
      stop("No voxels survive threshold given the parameters")
    }
    while (alpha < pval) {
      u <- u - .01
      if (threshType == "cRFT") {
        alpha <- rftPval(D = D, c = 1, k = k, u = u, n = n, resels = resels, df = df, fieldType = fieldType)$Pcor
      } else if (threshType == "pRFT") {
        alpha <- rftPval(D = D, c = 1, k = 0, u = u, n = n, resels = resels, df = df, fieldType = fieldType)$Pcor
      }
    }

    # FDR based thresholding ------------------------------------------------------------------------------------------
  } else if (threshType == "cFDR" | threshType == "pFDR") {
    # primary height for the given fieldType; degrees of freedom follow
    # the conventions used in euler() (Worsley et al., 1996): T and
    # chi-square fields use df[2], F-fields use df[1] and df[2]
    stat <- .int_upperTailQuantile(pp, df, fieldType)
    xarr <- as.array(x)
    if (threshType == "pFDR") {
      # topological peak FDR (Chumbley et al., 2010; spm_uc_peakFDR): BH on
      # the conditional p-values of local maxima above the primary height,
      # p_i = E[EC](peak_i) / E[EC](stat).  The threshold is the observed
      # height of the last peak that passes.
      peakvals <- sort(.int_localMaxima(xarr, stat), decreasing = TRUE)
      if (length(peakvals) == 0) {
        stop("No voxels survive the primary threshold given the parameters")
      }
      Eu <- rftPval(D = D, c = 1, k = 0, u = stat, n = n, resels = resels, df = df, fieldType = fieldType)$Ec
      if (isTRUE(verbose)) {
        progress <- txtProgressBar(min = 0, max = length(peakvals), style = 3)
      }
      ps <- numeric(length(peakvals))
      for (i in seq_along(peakvals)) {
        ps[i] <- rftPval(
          D = D, c = 1, k = 0, u = peakvals[i], n = n, resels = resels,
          df = df, fieldType = fieldType
        )$Ec / Eu
        if (isTRUE(verbose)) {
          setTxtProgressBar(progress, i)
        }
      }
      if (isTRUE(verbose)) {
        close(progress)
      }
      ps <- pmin(ps, 1)
      j <- .int_bhLastPass(ps, pval)
      if (is.na(j)) {
        stop("No voxels survive FDR threshold given the parameters")
      }
      u <- max(peakvals[order(ps)][j], stat)
    } else {
      # cluster FDR (spm_uc_clusterFDR): clusters at the fixed primary
      # height, BH on their uncorrected extent p-values P(extent >= k | stat).
      # The height stays at the primary threshold; the FDR extent threshold
      # (in voxels) is returned as attr(u, "k").
      fdrclust <- as.array(labelClusters(x, minClusterSize = 1, minThresh = stat, maxThresh = Inf))
      fdrlabs <- sort(unique(fdrclust[fdrclust > 0]))
      if (length(fdrlabs) == 0) {
        stop("No voxels survive the primary threshold given the parameters")
      }
      extents <- vapply(fdrlabs, function(lab) sum(fdrclust == lab), numeric(1))
      if (isTRUE(verbose)) {
        progress <- txtProgressBar(min = 0, max = length(extents), style = 3)
      }
      ps <- numeric(length(extents))
      for (i in seq_along(extents)) {
        ps[i] <- rftPval(
          D = D, c = 1, k = extents[i] * vox2res, u = stat, n = n,
          resels = resels, df = df, fieldType = fieldType
        )$Punc
        if (isTRUE(verbose)) {
          setTxtProgressBar(progress, i)
        }
      }
      if (isTRUE(verbose)) {
        close(progress)
      }
      j <- .int_bhLastPass(ps, pval)
      if (is.na(j)) {
        stop("No voxels survive FDR threshold given the parameters")
      }
      u <- stat
      attr(u, "k") <- max(extents[order(ps)][j], nvox)
    }
  }
  u
}

# quantile with upper-tail probability p for the given field type
.int_upperTailQuantile <- function(p, df, fieldType) {
  switch(fieldType,
    Z = qnorm(p, lower.tail = FALSE),
    T = qt(p, df[2], lower.tail = FALSE),
    F = qf(p, df[1], df[2], lower.tail = FALSE),
    X = qchisq(p, df[2], lower.tail = FALSE),
    stop("fieldType must be one of Z, T, F or X")
  )
}

# rank (in increasing p order) of the last p-value passing Benjamini-Hochberg
# at level q, inclusive (p_(j) <= j q / m); NA if none pass
.int_bhLastPass <- function(p, q) {
  ps <- sort(p)
  pass <- which(ps <= seq_along(ps) / length(ps) * q)
  if (length(pass) == 0) NA_integer_ else max(pass)
}

# values of the local maxima (>= every neighbour, full connectivity) that are
# at or above height u
.int_localMaxima <- function(arr, u) {
  d <- dim(arr)
  if (is.null(d)) d <- length(arr)
  D <- length(d)
  padded <- array(-Inf, d + 2)
  inner <- as.matrix(expand.grid(lapply(d, function(n) seq_len(n) + 1)))
  padded[inner] <- arr
  ismax <- arr >= u
  offsets <- as.matrix(expand.grid(rep(list(-1:1), D)))
  offsets <- offsets[rowSums(abs(offsets)) > 0, , drop = FALSE]
  for (o in seq_len(nrow(offsets))) {
    nb <- padded[sweep(inner, 2, offsets[o, ], "+")]
    ismax <- ismax & (as.vector(arr) >= nb)
  }
  as.vector(arr)[as.vector(ismax)]
}

