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
  if (ncompcorparameters > 0) {
    pcompcorr <-compcor( mat,  ncompcorparameters )
    dnz<-aslDenoiseR( mat, xideal, motionparams=NA, selectionthresh=0.1,
      maxnoisepreds=ncompcorparameters, polydegree=4 , crossvalidationgroups=6,
      scalemat=F, noisepoolfun=max )
    pcompcorr<-dnz$noiseu
    compcorrnames <- paste("compcorr", c(1:ncol(pcompcorr)), sep = "")
    nuis <- cbind(nuis, pcompcorr)
    colnames(nuis) <- c("metricnuis", motnames, compcorrnames)
  }
  return(list(xideal = xideal, nuis = nuis, globalsignal = globalsignal, globalsignalASL = globalsignalASL))
}
