# quantifyCBF
# 
# chen 2011 paper pCASL
# --------------------------------------------------------------------------------------
quantifyCBF <- function( perfusion, mask, parameters , outlierValue = 0.02 )
{

  # FIXME - for now assuming mean perfusion image passed in, not time-course
  
  if ( is.null(parameters$sequence) ) {
    stop( "Parameter list must specify a sequence type: pasl, pcasl, or casl" )
  }

  if ( parameters$sequence != "pcasl" ) {
    stop( "Only pcasl supported for now. pasl and casl in development" );
  }

  if (  is.null(parameters$m0) ) {
   stop( "Must pass in an M0 image: mean of the control images or externally acquired m0" );
   }

  # Is perfusion a time-signal?
  hasTime <- FALSE
  nTimePoints <- 0
  if ( length(dim(perfusion)) == ( length(dim(mask)) + 1 ) ) {
    hasTime <- TRUE
    nTimePoints <- dim(perfusion)[length(dim(perfusion))]
  }
  
  if ( parameters$sequence == "pcasl" ) {
    M0 <- as.array(parameters$m0)
    perf <- as.array(perfusion)
  
    lambda <- 0.9
    if ( ! is.null(parameters$lambda) ) {
      lambda <- parameters$lambda
    }

    alpha <- 0.85    # ASLtbx says 0.68 for 3T and 0.71 for 1.5T
    if ( ! is.null(parameters$alpha) ) {
      alpha <- parameters$alpha
    }
    
    T1b <- 0.67 # 1/sec as per ASLtbx for 3T, ASLtbx suggests 0.83 for 1.5T
    if ( ! is.null(parameters$T1blood) ) {
      T1b <- parameters$T1blood
    }

    # delay time
    omega <- 1
    if ( ! is.null(parameters$omega) ) {
      omega <- parameters$omega
    }

    # slice delay time
    slicetime <- 0.0505   # 50.5 ms value from ASLtbx
    if ( ! is.null(parameters$slicetime) ) {
      slicetime <- parameters$slicetime
    }
    
    tau <- 1.5
    if ( ! is.null(parameters$tau) ) {
      tau <- parameters$tau
    }

    sliceTimeMat <- rep(c(1:dim(M0)[3]), each=dim(M0)[1]*dim(M0)[2] )
    dim(sliceTimeMat) <- dim(M0)

    # Expand for time-series
    if ( hasTime ) {
      sliceTimeMat <- rep( as.array(sliceTimeMat), nTimePoints )
      dim(sliceTimeMat) <- dim(perfusion)
      M0 <- rep( as.array(M0), nTimePoints )
      dim(M0) <- dim(perfusion)
    }
    omegaMat <- slicetime * sliceTimeMat + omega
   
    cbf <- perf*60*100*( lambda * T1b ) / ( 2 * alpha * M0 * ( exp( -omegaMat * T1b ) - exp( -( tau + omegaMat ) * T1b ) ) )
    cbf[ !is.finite(cbf) ] <- 0  
    
    if ( hasTime ) {
      meancbf <- apply( cbf, c(1,2,3), mean )
      dim(meancbf) <- dim(mask)
    }
    else {
      meancbf <- cbf
    }
    
  }
 
  # apply mask to cbf time series
  if ( hasTime ) {
    timecbfimg <- antsImageClone( perfusion )
    timeMask <- rep( as.array(mask), nTimePoints )
    dim(timeMask) <- dim(perfusion)
    
    timecbfimg[ (timeMask < 1) ] <- 0
    timecbfimg[ (timeMask == 1) ] <- cbf[ (timeMask == 1) ]
  }
  
  # appy mask to mean cbf image
  meancbfimg <- antsImageClone( mask )
  meancbfimg[ (mask < 1 ) ] <- 0
  meancbfimg[ (mask == 1) ] <- meancbf[ (mask == 1) ]

  pckg = try(require(extremevalues))
  if(!pckg) {
    getPckg("extremevalues")
  }
  library(extremevalues)
  cbfvals<-meancbfimg[ (mask == 1) ]
  K <- getOutliers(cbfvals,method="I",distribution="normal",FLim=c( outlierValue , 1 - outlierValue ))
  kcbf<-antsImageClone( meancbfimg )
  kcbf[ meancbfimg < K$yMin ]<-0
  kcbf[ meancbfimg > K$yMax ]<-K$yMax

  if ( !hasTime ) {
    timecbfimg <- meancbfimg
  }

  return( list(meancbf=meancbfimg, kmeancbf=kcbf, timecbf=timecbfimg) )
}
