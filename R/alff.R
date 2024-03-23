#' alffmap
#'
#' Amplitude of Low Frequency Fluctuations (ALFF; Zang et al., 2007) and
#' fractional Amplitude of Low Frequency Fluctuations (f/ALFF; Zou et al., 2008)
#' are related measures that quantify the amplitude of low frequency
#' oscillations (LFOs).  This function outputs ALFF and fALFF for the input.
#'
#' @param x input vector for the time series of interest
#' @param flo low frequency, typically 0.01
#' @param fhi high frequency, typically 0.1
#' @param tr the period associated with the vector x (inverse of frequency)
#' @param detrend detrend the input time series
#' @param takesqrt take the sqrt of the computed values
#' @param kernel smoothing kernel in estimate, see \code{spec.pgram}
#' @return vector is output showing ALFF and fALFF values
#' @author Avants BB
#' @examples
#'
#'
#' set.seed(1)
#' mat <- matrix(rnorm(3000), ncol = 50)
#' fallf <- apply(mat, FUN = alffmap, MARGIN = 2)
#' k <- kernel("daniell", rep(2, 3))
#' fallf2 <- apply(mat, FUN = alffmap, MARGIN = 2, kernel = k)
#' testthat::expect_equal(mean(fallf2), 3.00367017972746)
#'
#' @export alffmap
alffmap <- function(x, flo = 0.01, fhi = 0.1, tr = 1,
                    detrend = TRUE,
                    takesqrt = FALSE,
                    kernel) {
  args <- list(
    x = stats::ts(x, frequency = 1.0 / tr),
    taper = 0, fast = TRUE,
    detrend = detrend, demean = FALSE, log = "n", plot = FALSE
  )
  if (!missing(kernel)) {
    args$kernel <- kernel
  }
  temp <- do.call(stats::spec.pgram, args)
  # if ( missing( "kernel") ) {
  #   temp = stats::spec.pgram(
  #     stats::ts( x, frequency = 1.0 / tr ), taper = 0, fast = TRUE,
  #     detrend = detrend, demean = FALSE, log = "n", plot = FALSE )
  # } else {
  #   temp = stats::spec.pgram(
  #     stats::ts( x, frequency = 1.0 / tr ), taper = 0, fast = TRUE,
  #     detrend = detrend, demean = FALSE, log = "n", plot = FALSE,
  #     kernel = kernel )
  # }
  fselect <- (temp$freq >= flo & temp$freq <= fhi)
  denom <- sum(temp$spec)
  numer <- sum(temp$spec[fselect])

  if (takesqrt) {
    denom <- sqrt(denom)
    numer <- sqrt(numer)
  }
  return(c(numer, numer / denom))
}
