#' Band pass filtering for BOLD image.
#'
#' This function works for a BOLD time-series data
#'
#'
#' @param boldmat Time series matrix for bold image
#' @param tr The sequence's TR value , typically 3 or 4.
#' @param freqLo The lower frequency limit, e.g. 0.01 in band-pass
#' filter
#' @param freqHi The higher frequency limit, e.g. 0.1 in band-pass
#' filter
#' @param opt  one of 'trig','butt','stl' Type of filter to use: butterworth,
#' trigonometric, stl.
#' @return output is the filtered time series.
#' @author Avants BB
#' @examples
#'
#' fmat<-replicate(1000, rnorm(200))
#' k<-1
#' tr = 4
#' for ( ftype in c("butt","stl","trig") ) {
#'   myres <- frequencyFilterfMRI( fmat, tr = tr, freqLo = 0.01, freqHi = 0.05, opt = ftype )
#'   comparemat <- cbind( fmat[,k], myres[,k] )
#'   plot( ts( comparemat ), main=ftype )
#'   Sys.sleep(0.3)
#'   temp = spectrum( ts(fmat[,k], frequency=1/tr ) )
#'   plot( temp$freq, temp$spec, type='l' )
#'   temp = spectrum( ts(myres[,k], frequency=1/tr ) )
#'   plot( temp$freq, temp$spec, type='l' )
#' }
#'
#' @export frequencyFilterfMRI
frequencyFilterfMRI <- function(boldmat, tr, freqLo = 0.01,
  freqHi = 0.1, opt = "butt") {
  pixtype <- "float"
  if (nargs() == 0) {
    return(NULL)
  }
  if (!is.numeric(tr) | missing(tr)) {
    print("TR parameter is missing or not numeric type - is typically between 2 and 4 , depending on your fMRI acquisition")
    return(NULL)
  }
  if (!is.numeric(freqLo) | !is.numeric(freqHi)) {
    print("freqLo/Hi is not numeric type")
    return(NULL)
  }
  if (missing(boldmat)) {
    print("Missing first (image) parameter")
    return(NULL)
  }
  freqLo <- freqLo * tr
  freqHi <- freqHi * tr
  voxLo <- round((1/freqLo))  # remove anything below this (high-pass)
  voxHi <- round((1/freqHi))  # keep anything above this
  myTimeSeries <- ts(boldmat, frequency = 1/tr)
  if (opt == "stl") {
    trendfrequencyL <- 1/freqLo
    trendfrequencyH <- 1/freqHi
    for (i in 1:ncol(boldmat)) {
      if (!is.na(trendfrequencyH))
        boldmat[, i] <- data.frame(stl(ts(boldmat[, i], frequency = trendfrequencyH),
          "per")$time.series)$trend
      if (!is.na(trendfrequencyL)) {
        temp <- data.frame(stl(ts(boldmat[, i], frequency = trendfrequencyL),
          "per")$time.series)$trend
        boldmat[, i] <- boldmat[, i] - temp
      }
    }
    return(boldmat)
  }
  if (opt == "wav") {
    if ( !usePkg("wmtsa") ) { print("Need wmtsa package"); return(NULL) }
    dnz <- myTimeSeries * 0
    for (i in 1:ncol(myTimeSeries)) {
      dnz[, i] <- wmtsa::wavShrink(myTimeSeries[, i], thresh.fun = "adaptive", thresh.scale = 0.1)
    }
    filteredTimeSeries <- ts(myTimeSeries - dnz)
    return(filteredTimeSeries)
  }
  if (opt == "butt") {
    if ( !usePkg("signal") ) { print("Need signal package"); return(NULL) }
    bf <- signal::butter(2, c(freqLo, freqHi), type = "pass")
    filteredTimeSeries <- matrix(signal::filter(bf, myTimeSeries), nrow = nrow(myTimeSeries))
    return(filteredTimeSeries)
  }
  if (opt == "trig") {
    if ( !usePkg("mFilter") ) { print("Need mFilter package"); return(NULL) }
    if (nrow(myTimeSeries)%%2 > 0) {
      firsttime = myTimeSeries[1, ]
      myTimeSeries <- rbind( firsttime, myTimeSeries ) # pad the time-series
      filteredTimeSeries <- residuals(mFilter::cffilter(myTimeSeries, pl = voxHi, pu = voxLo,
        drift = FALSE, root = FALSE, type = c("trigonometric")))
      filteredTimeSeries <- filteredTimeSeries[2:nrow(filteredTimeSeries), ] # depad
      filteredTimeSeries <- ts(filteredTimeSeries, frequency = 1/tr)
    } else {
      filteredTimeSeries <- residuals(mFilter::cffilter(myTimeSeries, pl = voxHi, pu = voxLo,
        drift = FALSE, root = FALSE, type = c("trigonometric")))
    }
    temporalvar <- apply(filteredTimeSeries, 2, var)
    wh <- which(temporalvar == 0)
    for (x in wh) {
      filteredTimeSeries[, x] <- sample(filteredTimeSeries, nrow(filteredTimeSeries))
    }
    return(filteredTimeSeries)
  }


}
