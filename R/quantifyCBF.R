#' quantifyCBF
#'
#' Computes CBF from ASL - pasl or pcasl
#'
#'
#' @param aslmat input asl matrix
#' @param aslmask 3D image mask (antsImage)
#' @param parameters list with entries for sequence and m0 (at minimimum)
#' @return a list is output with 3 types of cbf images
#' @author Avants BB, Kandel B, Duda JT
#' @examples
#'
#'   \dontrun{
#'   if (!exists("fn") ) fn<-"PEDS012_20131101_pcasl_1.nii.gz"
#'   # PEDS029_20101110_pcasl_1.nii.gz # high motion subject
#'   asl<-antsImageRead(fn,4)
#' # image available at http://files.figshare.com/1701182/PEDS012_20131101.zip
#'   pcasl.bayesian <- aslPerfusion( asl, interpolation="linear",
#'         dorobust=0., useDenoiser=4, skip=11, useBayesian=1000,
#'         moreaccurate=0, verbose=T, maskThresh=0.5 ) # throw away lots of data
#' # user might compare to useDenoiser=FALSE
#'   pcasl.parameters <- list( sequence="pcasl", m0=pcasl.bayesian$m0 )
#'   cbf <- quantifyCBF( pcasl.bayesian$perfusion, pcasl.bayesian$mask,
#'      pcasl.parameters )
#'   meancbf <- cbf$kmeancbf
#'   print(mean(meancbf[ pcasl.bayesian$mask==1 ]))
#'   antsImageWrite( meancbf , 'temp.nii.gz')
#'   pcasl.processing <- aslPerfusion( asl, moreaccurate=0,
#'     interpolation="linear",
#'     dorobust=0.95, useDenoiser=NA, skip=5,  useBayesian=0 )
#'   # user might compare to useDenoiser=FALSE
#'   pcasl.parameters <- list( sequence="pcasl", m0=pcasl.processing$m0 )
#'   cbf <- quantifyCBF( pcasl.processing$perfusion, pcasl.processing$mask, pcasl.parameters )
#'   meancbf <- cbf$kmeancbf
#'   print(mean(meancbf[ pcasl.processing$mask==1 ]))
#'   antsImageWrite( meancbf , 'temp2.nii.gz')
#'   plotANTsImage(  meancbf, slices='1x50x1')
#'   }
#'
#' @export quantifyCBF
quantifyCBF <- function(perfusion, mask, parameters,
   M0val = NA, outlierValue = 0.02) {

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

    alpha <- 0.85  # ASLtbx says 0.68 for 3T and 0.71 for 1.5T
    if (!is.null(parameters$alpha)) {
      alpha <- parameters$alpha
    }

    T1b <- 0.67  # 1/sec as per ASLtbx for 3T, ASLtbx suggests 0.83 for 1.5T
    if (!is.null(parameters$T1blood)) {
      T1b <- parameters$T1blood
    }

    # delay time
    omega <- 1
    if (!is.null(parameters$omega)) {
      omega <- parameters$omega
    }

    # slice delay time
    slicetime <- 0.0505  # 50.5 ms value from ASLtbx
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

    if ( is.na(M0val) ) M0val <- M0

    # 60 for seconds to minutes, 100 for 100g (standard units)
    cbf <- perf * 60 * 100 * (lambda * T1b) /
      (2 * alpha * M0val * (exp(-omegaMat *
      T1b) - exp(-(tau + omegaMat) * T1b)))
    cbf[!is.finite(cbf)] <- 0

    if (hasTime) {
      meancbf <- apply(cbf, c(1, 2, 3), mean)
      dim(meancbf) <- dim(mask)
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

    # From Chen 2011
    TI2 <- 1700
    if (!is.null(parameters$TI2)) {
      TI2 <- parameters$TI2
    }

    # From Chen 2011
    lambda <- 0.9
    if (!is.null(parameters$lambda)) {
      lambda <- parameters$lambda
    }

    # From Chen 2011
    alpha <- 0.95  # ASLtbx says 0.68 for 3T and 0.71 for 1.5T
    if (!is.null(parameters$alpha)) {
      alpha <- parameters$alpha
    }

    T1b <- 1150  # msec as per ASLtbx for 3T, ASLtbx suggests 0.83 for 1.5T
    if (!is.null(parameters$T1blood)) {
      T1b <- parameters$T1blood
    }

    # slice delay time
    slicetime <- 45  # from ASLtbx
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

    delaytime <- 800  # from ASLtbx
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
    TI <- slicetime * sliceTimeMat + delaytime + TI1

    Aprim <- A * exp(((1/T2wm) - (1/T2b) * TE))
    cbf <- (3000 * 1000 * perf)/(Aprim * M0 * exp(-TI/T1b) * TI1 * alpha)
    cbf[!is.finite(cbf)] <- 0

    if (hasTime) {
      meancbf <- apply(cbf, c(1, 2, 3), mean)
      dim(meancbf) <- dim(mask)
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
  meancbfimg <- antsImageClone(mask)
  meancbfimg[(mask < 1)] <- 0
  meancbfimg[(mask == 1)] <- meancbf[(mask == 1)]

  epckg <- try(require(extremevalues))
  if (!epckg) {
#    getPckg("extremevalues")
  }
  if ( epckg )
    {
    usePkg("extremevalues")
    cbfvals <- meancbfimg[(mask == 1)]
    K <- getOutliers(cbfvals, method = "I", distribution = "normal", FLim = c(outlierValue,
      1 - outlierValue))
    kcbf <- antsImageClone(meancbfimg)
    kcbf[meancbfimg < K$yMin] <- 0
    kcbf[meancbfimg > K$yMax] <- K$yMax
    } else kcbf<-NA

  if (!hasTime) {
    timecbfimg <- meancbfimg
  }

  return(list(meancbf = meancbfimg, kmeancbf = kcbf, timecbf = timecbfimg))
}
