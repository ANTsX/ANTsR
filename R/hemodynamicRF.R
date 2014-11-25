hemodynamicRF <- function(scans = 1, onsets = c(1), durations = c(1), rt = 3, times = NULL, 
  mean = TRUE, a1 = 6, a2 = 12, b1 = 0.9, b2 = 0.9, cc = 0.35) {
  
  mygamma <- function(x, a1, a2, b1, b2, c) {
    d1 <- a1 * b1
    d2 <- a2 * b2
    c1 <- (x/d1)^a1
    c2 <- c * (x/d2)^a2
    res <- c1 * exp(-(x - d1)/b1) - c2 * exp(-(x - d2)/b2)
    res
  }
  
  
  
  if (is.null(times)) {
    scale <- 1
  } else {
    scale <- 100
    onsets <- times/rt * scale
    durations <- durations/rt * scale
    rt <- rt/scale
    scans <- scans * scale
  }
  numberofonsets <- length(onsets)
  
  if (length(durations) == 1) {
    durations <- rep(durations, numberofonsets)
  } else if (length(durations) != numberofonsets) {
    stop("Length of duration vector does not match the number of onsets!")
  }
  stimulus <- rep(0, scans)
  
  for (i in 1:numberofonsets) {
    for (j in onsets[i]:(onsets[i] + durations[i] - 1)) {
      stimulus[j] <- 1
    }
  }
  stimulus <- c(rep(0, 20 * scale), stimulus, rep(0, 20 * scale))
  # just fill with zeros to avoid bounding effects in convolve
  hrf <- convolve(stimulus, mygamma(((40 * scale) + scans):1, a1, a2, b1/rt, b2/rt, 
    cc))/scale
  hrf <- hrf[-(1:(20 * scale))][1:scans]
  hrf <- hrf[unique((scale:scans)%/%scale) * scale]
  
  dim(hrf) <- c(scans/scale, 1)
  
  if (mean) {
    hrf - mean(hrf)
  } else {
    hrf
  }
  hrf<-hrf-min(hrf)
  hrf<-hrf/sum(hrf)
} 
