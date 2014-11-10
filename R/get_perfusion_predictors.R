get_perfusion_predictors <- function(mat,
  motionparams, xideal = NULL, labelfirst = 1,
  ncompcorparameters = 3, useDenoiser = NA) {
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
  # get nuisance variables : motion, compcor, etc motionparams <- as.data.frame(
  # moco_params )
  motionnuis <- t(motionparams)[2:ncol(motionparams), ]  # matrix elements
  metricnuis <- motionnuis[1, ]
  globalsignal <- rowMeans(mat)
  globalsignalASL <- residuals(lm(globalsignal ~ xideal))

  # here is a 2nd (new) way to deal with motion nuisance vars - svd - just keep top
  # 3 components
  msvd <- svd(t(motionnuis[2:nrow(motionnuis), ]))
  nsvdcomp <- 3
  motionnuis <- (msvd$u[, 1:nsvdcomp])
  motionnuis <- residuals(lm(motionnuis~xideal))
  print(paste(" % var of motion ", (sum(msvd$d[1:nsvdcomp])/sum(msvd$d))))
  motionnuis <- t(motionnuis)
  motnames <- paste("motion", c(1:nrow(motionnuis)), sep = "")
  nuis <- t(rbind(metricnuis, (motionnuis)))
  colnames(nuis) <- c("metricnuis", motnames)
  nuis <- t( motionnuis )
  colnames(nuis) <- motnames
  dnz<-NA
  if ( ncompcorparameters > 0 | !(all(is.na(useDenoiser))) ) {
    if (  ncompcorparameters > 0 )
      denoisingParams <- compcor(mat, ncompcorparameters)
    if (!(all(is.na(useDenoiser)))) {
      # include t(motionnuis) if you want to model motion
      DVARS <- computeDVARS(mat)
#      dnz <- aslDenoiseR(mat, xideal, motionparams = NA,
#        selectionthresh = 0.2,
#        maxnoisepreds = useDenoiser, polydegree = 8,
#        crossvalidationgroups = 6,
#        scalemat = F, noisepoolfun = max)
      clustasl<-4 # sample(clusterTimeSeries( mat,  6 )$clusters)
      dnz<-aslDenoiseR( mat, xideal, # motionparams=DVARS,
        selectionthresh=0.1, crossvalidationgroups=clustasl,
        maxnoisepreds=useDenoiser, debug=FALSE, polydegree=4 )
      denoisingParams <- dnz$noiseu
      dnz<-dnz$R2final
    }
    if ( exists("denoisingParams") )
      {
      compcorrnames <- paste("denoisingParams",
        c(1:ncol(denoisingParams)), sep = "")
      colnames(denoisingParams) <- c(compcorrnames)
      }
  } else denoisingParams<-NA
  return( list(xideal = xideal, nuis = denoisingParams,
    motion = t( motionnuis ),
    globalsignal = globalsignal,
    globalsignalASL = globalsignalASL, dnz=dnz ))
}
