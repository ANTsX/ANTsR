#' quantifyCBF
#'
#' Computes CBF from ASL - pasl or pcasl
#'
#' @param perfusion input asl matrix
#' @param mask 3D image mask (antsImage)
#' @param parameters list with entries for sequence and m0 (at minimimum).
#' For pcasl, \code{T1blood} is the longitudinal relaxation rate of blood
#' in 1/sec (default 0.67, i.e. T1 = 1.49 s); \code{omega} (post-labeling
#' delay), \code{slicetime} and \code{tau} (label duration) are in seconds.
#' For pasl, times are in msec, \code{T1blood} is T1 of blood (default
#' 1150), and \code{TI2} is the inversion time of the first slice; when
#' omitted it is \code{TI1 + delaytime}.
#' @param M0val baseline M0 value (optional)
#' @param outlierValue trim outliers by this fractional value (optional)
#' @return a list is output with 3 types of cbf images
#' @author Avants BB, Kandel B, Duda JT
#' @examples
#' \dontrun{
#' if (!exists("fn")) {
#'   fn <- getANTsRData("pcasl")
#' }
#' # PEDS029_20101110_pcasl_1.nii.gz # high motion subject
#' asl <- antsImageRead(fn)
#' # image available at http://files.figshare.com/1701182/PEDS012_20131101.zip
#' pcasl.bayesian <- aslPerfusion(asl,
#'   dorobust = 0., useDenoiser = 4, skip = 11, useBayesian = 1000,
#'   moreaccurate = 0, verbose = TRUE, maskThresh = 0.5
#' ) # throw away lots of data
#' # user might compare to useDenoiser=FALSE
#' pcasl.parameters <- list(sequence = "pcasl", m0 = pcasl.bayesian$m0)
#' cbf <- quantifyCBF(
#'   pcasl.bayesian$perfusion, pcasl.bayesian$mask,
#'   pcasl.parameters
#' )
#' meancbf <- cbf$kmeancbf
#' print(mean(meancbf[pcasl.bayesian$mask == 1]))
#' antsImageWrite(meancbf, "temp.nii.gz")
#' pcasl.processing <- aslPerfusion(asl,
#'   moreaccurate = 0,
#'   dorobust = 0.95, useDenoiser = NULL, skip = 5, useBayesian = 0
#' )
#' # user might compare to useDenoiser=FALSE
#' pcasl.parameters <- list(sequence = "pcasl", m0 = pcasl.processing$m0)
#' cbf <- quantifyCBF(pcasl.processing$perfusion, pcasl.processing$mask, pcasl.parameters)
#' meancbf <- cbf$kmeancbf
#' print(mean(meancbf[pcasl.processing$mask == 1]))
#' antsImageWrite(meancbf, "temp2.nii.gz")
#' plot(meancbf, slices = "1x50x1")
#' }
#'
#' @export
quantifyCBF <- function(
    perfusion,
    mask, parameters, M0val = NA, outlierValue = 0.02) {
  if (is.null(parameters$sequence)) {
    stop("Parameter list must specify a sequence type: pasl, pcasl, or casl")
  }

  if ((parameters$sequence != "pcasl") && (parameters$sequence != "pasl")) {
    stop("Only pcasl and pasl supported for now. casl in development")
  }

  if (is.null(parameters$m0)) {
    stop("Must pass in an M0 image: mean of the control images or externally acquired m0")
  }

  # Is perfusion a time-signal?
  hasTime <- FALSE
  nTimePoints <- 0
  if (length(dim(perfusion)) == (length(dim(mask)) + 1)) {
    hasTime <- TRUE
    nTimePoints <- dim(perfusion)[length(dim(perfusion))]
  }

  if (parameters$sequence == "pcasl") {
    M0 <- as.array(parameters$m0)
    perf <- as.array(perfusion)

    lambda <- 0.9
    if (!is.null(parameters$lambda)) {
      lambda <- parameters$lambda
    }

    alpha <- 0.85 # ASLtbx says 0.68 for 3T and 0.71 for 1.5T
    if (!is.null(parameters$alpha)) {
      alpha <- parameters$alpha
    }

    # T1blood is a relaxation RATE (1/sec), R1 = 1/T1.  With R1 the
    # consensus equation (Alsop et al., 2015)
    #   6000 lambda dM exp(PLD / T1) / (2 alpha T1 M0 (1 - exp(-tau / T1)))
    # equals 6000 lambda dM R1 / (2 alpha M0 (exp(-PLD R1) - exp(-(tau + PLD) R1))),
    # which is what is computed below.
    T1b <- 0.67 # 1/sec as per ASLtbx for 3T, ASLtbx suggests 0.83 for 1.5T
    if (!is.null(parameters$T1blood)) {
      T1b <- parameters$T1blood
    }

    # delay time
    omega <- 1
    if (!is.null(parameters$omega)) {
      omega <- parameters$omega
    }

    # slice delay time
    slicetime <- 0.0505 # 50.5 ms value from ASLtbx
    if (!is.null(parameters$slicetime)) {
      slicetime <- parameters$slicetime
    }

    tau <- 1.5
    if (!is.null(parameters$tau)) {
      tau <- parameters$tau
    }

    sliceTimeMat <- rep(c(1:dim(M0)[3]), each = dim(M0)[1] * dim(M0)[2])
    dim(sliceTimeMat) <- dim(M0)

    # Expand for time-series
    if (hasTime) {
      sliceTimeMat <- rep(as.array(sliceTimeMat), nTimePoints)
      dim(sliceTimeMat) <- dim(perfusion)
      M0 <- rep(as.array(M0), nTimePoints)
      dim(M0) <- dim(perfusion)
    }
    omegaMat <- slicetime * sliceTimeMat + omega

    if (is.na(M0val)) {
      M0val <- M0
    }

    # 60 for seconds to minutes, 100 for 100g (standard units)
    cbf <- perf * 60 * 100 * (lambda * T1b) / (2 * alpha * M0val * (exp(-omegaMat *
      T1b) - exp(-(tau + omegaMat) * T1b)))
    cbf[!is.finite(cbf)] <- 0

    if (hasTime) {
      # cbf is a plain array here, so average over the last (time) axis
      meancbf <- apply(cbf, seq_len(length(dim(cbf)) - 1), mean)
    } else {
      meancbf <- cbf
    }
  } else if (parameters$sequence == "pasl") {
    print("PASL")
    M0 <- as.array(parameters$m0)
    perf <- as.array(perfusion)

    # From Chen 2011
    TI1 <- 700
    if (!is.null(parameters$TI1)) {
      TI1 <- parameters$TI1
    }

    # TI2 (inversion to readout of the first slice, msec).  The QUIPSS II
    # equation (Alsop et al., 2015; Chen et al., 2011) uses
    # exp(-TI2 / T1b); when TI2 is not given it is TI1 + delaytime.
    TI2 <- parameters$TI2

    # From Chen 2011
    lambda <- 0.9
    if (!is.null(parameters$lambda)) {
      lambda <- parameters$lambda
    }

    # From Chen 2011
    alpha <- 0.95 # ASLtbx says 0.68 for 3T and 0.71 for 1.5T
    if (!is.null(parameters$alpha)) {
      alpha <- parameters$alpha
    }

    T1b <- 1150 # msec as per ASLtbx for 3T, ASLtbx suggests 0.83 for 1.5T
    if (!is.null(parameters$T1blood)) {
      T1b <- parameters$T1blood
    }

    # slice delay time
    slicetime <- 45 # from ASLtbx
    if (!is.null(parameters$slicetime)) {
      slicetime <- parameters$slicetime
    }

    A <- 1.06
    if (!is.null(parameters$A)) {
      A <- parameters$A
    }

    T2wm <- 40
    if (!is.null(parameters$T2wm)) {
      T2wm <- parameters$T2wm
    }

    T2b <- 80
    if (!is.null(parameters$T2b)) {
      T2b <- parameters$T2b
    }

    TE <- 20
    if (!is.null(parameters$TE)) {
      TE <- parameters$TE
    }

    delaytime <- 800 # from ASLtbx
    if (!is.null(parameters$delaytime)) {
      delaytime <- parameters$delaytime
    }

    sliceTimeMat <- rep(c(1:dim(M0)[3]), each = dim(M0)[1] * dim(M0)[2])
    dim(sliceTimeMat) <- dim(M0)

    # Expand for time-series
    if (hasTime) {
      sliceTimeMat <- rep(as.array(sliceTimeMat), nTimePoints)
      dim(sliceTimeMat) <- dim(perfusion)
      M0 <- rep(as.array(M0), nTimePoints)
      dim(M0) <- dim(perfusion)
    }
    if (is.null(TI2)) {
      TI2 <- TI1 + delaytime
    }
    TI <- slicetime * sliceTimeMat + TI2

    # T2 correction of the white-matter-referenced M0 to blood (ASLtbx;
    # Wang et al. 2003 / Chen et al. 2011): exp((1/T2wm - 1/T2b) * TE)
    Aprim <- A * exp(((1 / T2wm) - (1 / T2b)) * TE)
    cbf <- (3000 * 1000 * perf) / (Aprim * M0 * exp(-TI / T1b) * TI1 * alpha)
    cbf[!is.finite(cbf)] <- 0

    if (hasTime) {
      # cbf is a plain array here, so average over the last (time) axis
      meancbf <- apply(cbf, seq_len(length(dim(cbf)) - 1), mean)
    } else {
      meancbf <- cbf
    }
  }



  # apply mask to cbf time series
  if (hasTime) {
    timecbfimg <- antsImageClone(perfusion)
    timeMask <- rep(as.array(mask), nTimePoints)
    dim(timeMask) <- dim(perfusion)

    timecbfimg[(timeMask < 1)] <- 0
    timecbfimg[(timeMask == 1)] <- cbf[(timeMask == 1)]
  }

  # appy mask to mean cbf image
  meancbfimg <- meancbf * mask

  kcbf <- NA

  if (!hasTime) {
    timecbfimg <- meancbfimg
  }

  return(list(meancbf = meancbfimg, kmeancbf = kcbf, timecbf = timecbfimg))
}
