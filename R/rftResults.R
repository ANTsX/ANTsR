#' RFT Statistical Results
#'
#' Returns RFT based statistical results for a single statistical image
#'
#' @param x statistical field image of class antsImage
#' @param resels resel values for the mask
#' @param fwhm full width at half maxima
#' @param df degrees of freedom expressed as df = c(degrees of
#' interest, degrees of error)
#' @param fieldType
#' \describe{
#' \item{T: }{T-field}
#' \item{F: }{F-field}
#' \item{X: }{Chi-square field'}
#' \item{Z: }{Gaussian field}
#' }
#' @param RPVImg resels per voxel image
#' @param k minimum desired cluster size (default = 1)
#' @param threshType a numeric value to threshTypeold the statistical
#' field or a character of the following methods:
#' \describe{
#' 	\item{cRFT: }{computes a threshTypeold per expected cluster level
#' 	probability }
#' 	\item{pRFT: }{uses the mask and pval calculates the minimum statistical
#' 	threshTypeold}
#' 	\item{cFDR: }{cluster-extent FDR at the primary height \code{pp}; clusters
#' 	smaller than the FDR extent threshold (and \code{k}) are dropped}
#' 	\item{pFDR: }{topological peak FDR on the local maxima above the
#' 	primary height \code{pp}}
#' }
#' @param pval the p-value for estimating the threshTypeold (default = .05)
#' @param pp the primary (initial) p-value for threshTypeolding (only used for
#'  FDR methods; default = .001)
#' @param n number of images in conjunction
#' @param statdir directory where output is saved (if not specified
#' images are not saved)
#' @param verbose enables verbose output
#'
#' @return Outputs a statistical value to be used for threshTypeold a
#' statistical field image
#' \describe{
#' \item{SetStats: }{set-level statistics and number of clusters}
#' \item{ClusterStats: }{cluster-level statistics and descriptors}
#' \item{PeakStats: }{peak-level statistics and descriptor"}
#' \item{LabeledClusters: }{image of labeled clusters}
#' \item{threshTypeold: }{the threshTypeold used}
#' }
#'
#' @details
#'
#' \code{rftPval} is used to compute all family-wise error (FWE) corrected
#' statistics while \code{p.adjust} is used to compute all false-discovery rate
#' based statistics. All statistics herein involve implementation of random
#' field theory (RFT) to some extent.
#'
#' Both cluster-level and peak-level statistics are described by the
#' uncorrected
#'  p-value along with the FDR and FWE corrected p-values for each cluster.
#' Peak-level statistics are described by the maximum statistical value in each
#' cluster and the comparable Z statistic. The peak-level FDR values are
#' Benjamini-Hochberg adjusted over one maximum per reported cluster, unlike
#' SPM's peak table, which can list several local maxima within a cluster. The ClusterStats table also contains
#' coordinates for each cluster and the number of voxels therein. By default
#' \code{threshType = "pRFT"} and pval=.05. Alternatively, the user may use a
#' specific numeric value for threshTypeolding the statistical field.
#' \code{statFieldThresh} more fully describes using appropriate threshTypeolds
#' for statistical fields and how \code{pp} plays a role in FDR
#' threshTypeolding.
#'
#' @references
#' Chumbley J., (2010) Topological FDR for neuroimaging
#'
#' Friston K.J., (1996) Detecting Activations in PET and
#' fMRI: Levels of Inference and Power
#'
#' Worsley K.J., (1992) A Three-Dimensional Statistical Analysis for
#' CBF Activation Studies in Human Brain.
#'
#' @author Zachary P. Christensen
#' @examples
#' \dontrun{
#' mnit1 <- antsImageRead(getANTsRData("mni"))
#' mask <- getMask(mnit1)
#' ilist <- list()
#' for (i in 1:10) {
#'   ilist <- lappend(ilist, antsImageClone(mnit1) * rnorm(1))
#' }
#' response <- rnorm(10)
#' imat <- imageListToMatrix(ilist, mask)
#' residuals <- matrix(nrow = nrow(imat), ncol = ncol(imat))
#' tvals <- matrix(nrow = nrow(imat), ncol = ncol(imat))
#' for (i in 1:ncol(imat)) {
#'   fit <- lm(response ~ imat[, i])
#'   tvals <- coefficients(fit)[2]
#'   residuals[, i] <- residuals(fit)
#' }
#' myfwhm <- estSmooth(residuals, mask, fit$df.residual)
#' res <- resels(mask, myfwhm$fwhm)
#' timg <- makeImage(mask, tvals)
#'
#' # threshold to create peak values with p-value of .05 (default)
#' results1 <- rftResults(timg, res, myfwhm$fwhm, df,
#'   fieldType = "T",
#'   threshType = "pRFT", pval = .05
#' )
#'
#' # threshold to create clusters with p-value of .05
#' results2 <- rftResults(timg, res, myfwhm$fwhm, df,
#'   fieldType = "T",
#'   threshType = "cRFT", pval = .05
#' )
#'
#' # initial threshold at p-value of .001 followed by peak FDR threshTypeold at
#' # p-value of .05
#' results3 <- rftResults(timg, res, myfwhm$fwhm, df,
#'   fieldType = "T",
#'   threshType = "pFDR", pval = .05, pp = .01
#' )
#'
#' # initial threshold at p-value of .001 followed by cluster FDR threshold at
#' # p-value of .05
#' results4 <- rftResults(timg, res, myfwhm$fwhm, df,
#'   fieldType = "T",
#'   threshType = "cFDR", pval = .05, pp = .01
#' )
#'
#' # correcting for non-isotropic
#' results5 <- rftResults(timg, res, myfwhm$fwhm, df,
#'   fieldType = "T",
#'   fwhm$RPVImg
#' )
#' }
#' @export rftResults
rftResults <- function(x, resels, fwhm, df, fieldType,
                       RPVImg = NULL, k = 1, threshType = "pRFT", pval = .05,
                       pp = .001, n = 1, statdir = NULL, verbose = FALSE) {
  if (missing(x)) {
    stop("Must specify x")
  }
  if (missing(resels)) {
    stop("Must specify resels")
  }
  if (missing(fwhm)) {
    stop("Must specify fwhm")
  }
  if (missing(df)) {
    stop("Must specify degrees of freedom (df)")
  }
  if (missing(fieldType)) {
    stop("Must specify fieldType")
  }

  if (is.character(threshType)) {
    if (isTRUE(verbose)) {
      cat("Calculating threshTypeold \n")
    }
    u <- statFieldThresh(
      x = x, pval = pval, nvox = k, n = n, fwhm = fwhm, resels = resels,
      df = df, fieldType = fieldType, threshType = threshType, pp = pp,
      verbose = verbose
    )
  } else if (is.numeric(threshType)) {
    u <- threshType
  } else {
    stop(paste0(
      "threshType must be a numeric value or a character ",
      "specifying the chosen method for calculating a threshold"
    ))
  }
  # cluster FDR keeps the primary height and returns an extent threshold
  if (!is.null(attr(u, "k"))) {
    k <- max(k, attr(u, "k"))
  }
  u <- as.numeric(u)
  D <- x@dimension
  vox2res <- 1 / prod(fwhm)
  nvox <- length(as.vector(x[x != 0]))
  mask <- getMask(x)
  # extract clusters at threshold
  clust <- labelClusters(x, minClusterSize = k, minThresh = u, maxThresh = Inf)
  if (!is.null(statdir)) {
    antsImageWrite(clust,
      filename = file.path(statdir, "ClusterImg.nii.gz")
    )
  }
  # sort so that rows line up with getCentroids(), which is in label order
  labs <- sort(unique(clust[clust > 0]))
  nclus <- length(labs) # number of clusters
  if (nclus == 0) {
    warning("No clusters survive threshold given the parameters")
    return(list(
      "SetStats" = NA, "ClusterStats" = NULL, "PeakStats" = NULL,
      "LabeledClusters" = 0, "threshold" = u, "ClusterImage" = clust
    ))
  }
  cnames <- paste("Cluster", seq_len(nclus), sep = "") # create name for each cluster
  k <- k * vox2res # convert voxel number to resels


  # create table for cluster-level statistics---------------
  ClusterStats <- as.table(matrix(nrow = nclus, ncol = 7))
  dimnames(ClusterStats)[[1]] <- cnames
  dimnames(ClusterStats)[[2]] <- c(
    "Pfwe", "Pfdr", "P",
    "Voxels", "xc", "yc", "zc"
  )
  # getCentroids() always returns x, y, z, t columns (z = 0 for 2D images)
  ClusterStats[, 5:7] <- getCentroids(clust)[seq_len(nclus), 1:3, drop = FALSE]
  # get locations of clusters

  # create table for peak-level statistics----------------------
  PeakStats <- as.table(matrix(nrow = nclus, ncol = 5))
  dimnames(PeakStats)[[1]] <- cnames
  dimnames(PeakStats)[[2]] <- c("Pfwe", "Pfdr", "P", "MaxStat", "Z")

  # Set-Level----------------------------------------------------
  Pset <- rftPval(D = D, c = nclus, k = k, u = u, n = n, resels = resels, df = df, fieldType = fieldType)$Pcor
  Ez <- rftPval(D = D, c = 1, k = 0, u = u, n = n, resels = resels, df = df, fieldType = fieldType)$Ec

  # Cluster-Level------------------------------------------------
  ClusterStats[, 4] <- sapply(labs, function(tmp) {
    (
      sum(as.array(clust[clust == tmp])) / tmp)
  })
  K <- sapply(seq_len(nclus), function(i) {
    tmp <- ClusterStats[i, 4] # number of voxels in cluster
    (
      if (is.null(RPVImg)) {
        # follows isotropic image assumptions
        K <- tmp * vox2res
      } else {
        RPVImg <- check_ants(RPVImg)
        # extract resels per voxel in cluster (for non-isotropic image)
        cmask <- thresholdImage(clust, labs[i], labs[i])
        rkc <- RPVImg[cmask == 1]
        lkc <- sum(rkc) / tmp
        iv <- matrix(resels(cmask, c(1, 1, 1)), nrow = 1)
        iv <- iv %*% matrix(c(1 / 2, 2 / 3, 2 / 3, 1), ncol = 1)
        K <- iv * lkc
      })
  })
  ClusterStats[, 1] <- sapply(K, function(tmp) {
    (
      rftPval(D = D, c = 1, k = tmp, u = u, n = n, resels = resels, df = df, fieldType = fieldType)$Pcor)
  })
  # fwe p-value (cluster)
  ClusterStats[, 3] <- sapply(K, function(tmp) {
    (
      rftPval(D = D, c = 1, k = tmp, u = u, n = n, resels = resels, df = df, fieldType = fieldType)$Punc)
  })
  # uncorrected p-value (cluster)
  ClusterStats[, 2] <- p.adjust(ClusterStats[, 3], "BH") # FDR (cluster)

  # Peak-Level-----------------------------------------------------
  PeakStats[, 4] <- sapply(labs, function(tmp) {
    (
      max(x[clust == tmp]))
  }) # max value for each cluster
  PeakStats[, 1] <- sapply(
    PeakStats[, 4],
    function(tmp) {
      (
        rftPval(D = D, c = 1, k = 0, u = tmp, n = n, resels = resels, df = df, fieldType = fieldType)$Pcor)
    }
  ) # fwe p-value (peak)
  PeakStats[, 2] <- p.adjust(sapply(PeakStats[, 4], function(tmp) {
    (
      rftPval(D = D, c = 1, k = 0, u = tmp, n = n, resels = resels, df = df, fieldType = fieldType)$Ec / Ez)
  }), "BH") # FDR (peak)
  # uncorrected p-value (peak), from the upper tail so that strong peaks
  # keep their precision; df[2] for chi-square as in euler()
  PeakStats[, 3] <- sapply(PeakStats[, 4], function(tmp) {
    switch(fieldType,
      Z = pnorm(tmp, lower.tail = FALSE),
      T = pt(tmp, df[2], lower.tail = FALSE),
      F = pf(tmp, df[1], df[2], lower.tail = FALSE),
      X = pchisq(tmp, df[2], lower.tail = FALSE)
    )
  })
  # comparable Z-statistic (from uncorrected peak p-value)
  PeakStats[, 5] <- qnorm(PeakStats[, 3], lower.tail = FALSE)

  # prepare output-------------------------------------------
  ClusterStats <- round(ClusterStats, 4)
  PeakStats <- round(PeakStats, 4)
  results <- list(
    "SetStats" = Pset,
    "ClusterStats" = ClusterStats, "PeakStats" = PeakStats,
    "LabeledClusters" = nclus, "threshold" = u,
    "ClusterImage" = clust
  )

  if (!is.null(statdir)) {
    write.csv(ClusterStats, file = file.path(statdir, "ClusterStats"))
    write.csv(PeakStats, file = file.path(statdir, "PeakStats"))
  }
  results
}
