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

  M0 <- as.array(parameters$m0)
  #deltaM <- as.array(perfusion)
  perf <- as.array(perfusion)
  
  lambda <- 0.9
  if ( ! is.null(parameters$lambda) ) {
    lambda <- parameters$lambda
  }

  alpha <- 0.85
  # ASLtbx says 0.68 for 3T and 0.71 for 1.5T
  if ( ! is.null(parameters$alpha) ) {
    alpha <- parameters$alpha
  }

  T1b <- 0.67 # 1/sec as per ASLtbx for 3T
  # ASLtbx suggests 0.83 for 1.5T
  if ( ! is.null(parameters$T1blood) ) {
    T1b <- parameters$T1blood
  }

  omega <- 1
  if ( ! is.null(parameters$omega) ) {
    omega <- parameters$omega
    }

  tau <- 1.5
  if ( ! is.null(parameters$tau) ) {
    tau <- parameters$tau
  }

  scaling <- 60*100*( lambda * T1b ) / ( 2 * alpha * M0 * ( exp( -omega * T1b ) - exp( -( tau + omega ) * T1b ) ) )
  cbf <- scaling*perf
  
  cbf[ is.nan(cbf) ] <- 0

  # Get mean from time-series data
  #meanvalues <- array( 0 , dim(M0)[1:3] )
  # Fix this using apply
  #for( x in 1:(dim(M0)[1]) )
  #  for( y in 1:(dim(M0)[2]) )
  #    for( z in 1:(dim(M0)[3]) )
  #      {
  #       meanvalues[ x , y , z ] <- 5400.0*mean( cbf[ x , y , z , ] )
  #      }
  cbf[ (mask < 1) ] <- 0

  cbfimg <- antsImageClone( mask )
  cbfimg[ (mask < 1 ) ] <- 0
  cbfimg[ (mask == 1) ] <- cbf[ (mask == 1) ]
  pckg = try(require(extremevalues))
  if(!pckg) {
    getPckg("extremevalues")
  }
  library(extremevalues)
  cbfvals<-cbfimg[ (mask == 1) ]
  K <- getOutliers(cbfvals,method="I",distribution="normal",FLim=c( outlierValue , 1 - outlierValue ))
  kcbf<-antsImageClone( cbfimg )
  kcbf[ cbfimg < K$yMin ]<-0
  kcbf[ cbfimg > K$yMax ]<-K$yMax

  return( list(meancbf=cbfimg, kmeancbf=kcbf) )
}
