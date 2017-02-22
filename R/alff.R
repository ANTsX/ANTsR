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
#' mat <- matrix(rnorm(3000),ncol=50)
#' fallf = apply( mat, FUN=alffmap, MARGIN=2 )
#' k = kernel("daniell", rep( 2, 3 ) )
#' fallf2 = apply( mat, FUN=alffmap, MARGIN=2, kernel=k )
#'
#' @export alffmap
alffmap <- function( x, flo=0.01, fhi=0.1, tr=1,
  detrend = TRUE,
  takesqrt = FALSE,
  kernel )
  {
  if ( missing( "kernel") )
    temp = spec.pgram( ts( x, frequency = 1.0 / tr ), taper = 0, fast = TRUE,
      detrend = detrend, demean = FALSE, log = "n", plot = FALSE )
  if ( ! missing( "kernel") )
    temp = spec.pgram( ts( x, frequency = 1.0 / tr ), taper = 0, fast = TRUE,
      detrend = detrend, demean = FALSE, log = "n", plot = FALSE, kernel = kernel )
  fselect = ( temp$freq >= flo & temp$freq <= fhi )
  if ( takesqrt ) denom = sqrt( sum( temp$spec ) ) else denom = sum( temp$spec )
  if ( takesqrt )
    numer = sqrt( sum( temp$spec[ fselect ] ) ) else numer = sum( temp$spec[ fselect ] )
  return( c( numer, numer/denom ) )
  }
