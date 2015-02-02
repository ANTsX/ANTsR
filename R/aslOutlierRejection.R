#' @name aslOutlierRejection
#' @title Pair-wise subtraction based outlier rejection.
#' @description Performs outlier rejection based on divergence from mean difference.
#' @usage aslOutlierRejection(asl, mask=NA, centralTendency=median, sigma.mean=2.5, sigma.sd=2.0)
#' @details Computes pair-wise differences in ASL time-series data and removes outliers.  One of two criteria is sufficient to classify an image as an outlier: 1) A difference image for which the mean is higher than the mean of all the images plus \code{sigma.mean} times the standard deviation of the whole time-series; 2) A difference image which has a standard deviation greater than sigam.sd times the mean standard deviation of each difference image.
#' @param asl ASL time-series image.
#' @param mask Mask to compute average over. If not supplied, estimated from data.
#' @param centralTendency Function to compute central tendency.  Defaults to median, but mean may also work.
#' @param sigma.mean Scalar defining how many standard deviations away from the mean a volume must be to be rejected.
#' @param sigma.sd Scalar defining how many standard deviations away from mean of standard deviations the volume must be to be rejected.
#' @return List of ASL time-series image including only inlier pairs and vector of outlier pair numbers (numbers correspond to *input* ASL time-series).
#' @author Benjamin M. Kandel
#' @examples \dontrun{WIP}
#' @references Tan H. et al., ``A Fast, Effective Filtering Method for Improving Clinical Pulsed Arterial Spin Labeling MRI,'' JMRI 2009.
#' @export aslOutlierRejection
aslOutlierRejection <- function(asl, mask = NA, centralTendency = median,
  sigma.mean = 2.5, sigma.sd = 2) {
  if (is.na(asl))
    stop("ASL must be provided.")
  if (is.na(mask)) {
    avg <- getAverageOfTimeSeries(asl)
    N3BiasFieldCorrection(3, avg, avg, 2)
    N3BiasFieldCorrection(3, avg, avg, 2)
    mask <- getMask(avg, mean(avg), Inf)
  }
  nvox <- sum(mask[mask > 0])
  npairs <- dim(asl)[4]/2
  tc <- rep(c(1, 2), npairs)
  diffs <- matrix(rep(NA, npairs * nvox), ncol = npairs)
  aslmat <- timeseries2matrix(asl, mask)
  for (ii in 1:npairs) {
    diffs[, ii] <- aslmat[ii * 2, ] - aslmat[ii * 2 - 1, ]
  }
  if (mean(diffs) < 0)
    diffs <- -diffs
  centers <- apply(diffs, 2, centralTendency)
  mean.centers <- mean(centers)
  sd.centers <- sd(centers)
  sds <- apply(diffs, 2, sd)
  mean.sds <- mean(sds)
  sd.sds <- sd(sds)
  which.outlierpairs <- which((abs(centers - mean.centers) > sigma.mean * sd.centers) |
    (abs(sds - mean.sds) > sigma.sd * sd.centers))
  which.outliers <- rep(which.outlierpairs, each = 2)
  tc.outliers <- rep(c(1, 2), length(which.outlierpairs))
  which.outliers[tc.outliers == 1] <- which.outliers[tc.outliers == 1] * 2 - 1
  which.outliers[tc.outliers == 2] <- which.outliers[tc.outliers == 2] * 2
  aslmat.inlier <- aslmat[-which.outliers, ]
  asl.inlier <- matrix2timeseries(asl, mask, aslmat.inlier)
  list(asl.inliers = asl.inlier, outliers = which.outliers)

}
