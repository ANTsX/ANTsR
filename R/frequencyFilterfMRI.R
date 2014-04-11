frequencyFilterfMRI <- function( boldmat, tr, freqLo = 0.01, freqHi = 0.1 ) {
  pixtype <- "float"
  if (nargs() == 0) {
    print(myusage)
    return(NULL)
  }
  if (!is.numeric(tr) | missing(tr)) {
    print("TR parameter is missing or not numeric type - is typically between 2 and 4 , depending on your fMRI acquisition")
    print(myusage)
    return(NULL)
  }
  if (!is.numeric(freqLo) | !is.numeric(freqHi)) {
    print("freqLo/Hi is not numeric type")
    print(myusage)
    return(NULL)
  }
  if (missing(boldmat)) {
    print("Missing first (image) parameter")
    print(myusage)
    return(NULL)
  }
  freqLo <- freqLo * tr
  freqHi <- freqHi * tr
  voxLo <- round((1/freqLo))  # remove anything below this (high-pass)
  voxHi <- round((1/freqHi))  # keep anything above this
  myTimeSeries <- ts(boldmat, frequency = 1/tr)
  if (nrow(myTimeSeries)%%2 > 0) {
    firsttime <- myTimeSeries[1, ]
    myTimeSeries <- myTimeSeries[2:nrow(myTimeSeries), ]
    filteredTimeSeries <- residuals(cffilter(myTimeSeries, pl = voxHi, pu = voxLo, drift = FALSE, root = FALSE, 
      type = c("trigonometric")))
    filteredTimeSeries <- rbind(firsttime, filteredTimeSeries)
    filteredTimeSeries <- ts(filteredTimeSeries, frequency = 1/tr)
  } else {
    filteredTimeSeries <- residuals(cffilter(myTimeSeries, pl = voxHi, pu = voxLo, drift = FALSE, root = FALSE, 
      type = c("trigonometric")))
  }
  temporalvar <- apply(filteredTimeSeries, 2, var)
  wh <- which(temporalvar == 0)
  for (x in wh) {
    filteredTimeSeries[, x] <- sample(filteredTimeSeries, nrow(filteredTimeSeries))
  }
  return(filteredTimeSeries)
}
