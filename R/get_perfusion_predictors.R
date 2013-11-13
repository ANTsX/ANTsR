get_perfusion_predictors <- function(mat, motionparams, xideal = NULL, labelfirst = 1, ncompcorparameters = 3) {
  myusage <- "usage: get_perfusion_predictors(  mat , motionparams , xideal = NULL , labelfirst = 1 , ncompcorparameters = 3 ) "
  if (nargs() == 0) {
    print(myusage)
    return(NULL)
  }
  if (missing(mat) | missing(motionparams)) {
    print("Missing one or more input parameter(s).")
    print(myusage)
    return(NULL)
  }
  if (is.null(xideal)) {
    if (!labelfirst) {
      xideal <- (rep(c(1, 0), dim(mat)[1])[1:dim(mat)[1]] - 0.5)  # control minus tag
    } else {
      xideal <- (rep(c(0, 1), dim(mat)[1])[1:dim(mat)[1]] - 0.5)  # tag minus control
    }
  } else if (length(xideal) != dim(mat)[1]) {
    print("'xideal' must have length equal to dim(mat)[1]")
    return(NULL)
  }
  # get nuisance variables : motion, compcor, etc motionparams <- as.data.frame( moco_params )
  motionnuis <- t(motionparams)[2:ncol(motionparams), ]  # matrix elements
  metricnuis <- motionnuis[1, ]
  globalsignal <- rowMeans(mat)
  globalsignalASL <- residuals(lm(globalsignal ~ xideal))
  
  # here is a 2nd (new) way to deal with motion nuisance vars - svd - just keep top 3 components
  msvd <- svd(t(motionnuis[2:nrow(motionnuis), ]))
  nsvdcomp <- 3
  motionnuis <- (msvd$u[, 1:nsvdcomp])
  print(paste(" % var of motion ", (sum(msvd$d[1:nsvdcomp])/sum(msvd$d))))
  motionnuis <- t(motionnuis)
  motnames <- paste("motion", c(1:nrow(motionnuis)), sep = "")
  nuis <- t(rbind(metricnuis, (motionnuis)))
  colnames(nuis) <- c("metricnuis", motnames)
  # compute temporal variance of each column and apply CompCor
  temporalvar <- apply(mat, 2, var)
  tvhist <- hist(temporalvar, breaks = c("FD"), plot = T)
  percvar <- 0.975  # percentage of high variance data to use
  # get total counts
  totalcounts <- sum(tvhist$counts)
  wh <- (cumsum(tvhist$counts) < (totalcounts * percvar))
  thresh <- max(tvhist$mids[wh])
  # cumulativesum<-rev( cumsum( tvhist$counts / totalcounts ) ) thresh<-max( tvhist$mids[ ( cumulativesum >
  # percvar ) ] )
  wh <- (temporalvar > thresh)
  wh2 <- (temporalvar <= thresh)
  print(paste(percvar, sum(wh), sum(wh2)))
  highvarmat <- mat[, wh]
  compcorrsvd <- svd(highvarmat %*% t(highvarmat))
  if (ncompcorparameters > 0) {
    compcorr <- t(compcorrsvd$u[1:ncompcorparameters, ])
    compcorrnames <- paste("compcorr", c(1:ncol(compcorr)), sep = "")
    nuis <- cbind(nuis, compcorr)
    colnames(nuis) <- c("metricnuis", motnames, compcorrnames)
  }
  return(list(xideal = xideal, nuis = nuis, globalsignal = globalsignal, globalsignalASL = globalsignalASL))
} 
