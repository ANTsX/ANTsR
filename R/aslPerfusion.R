aslPerfusion <- function(asl, maskThresh = 0.75,
  moreaccurate = 1, dorobust = 0.92,
  m0 = NA, skip = 20, mask = NA,
  interpolation = "linear", checkmeansignal = 100,
  moco_results = NULL, regweights = NULL,
  useDenoiser = NA, useBayesian=0, verbose=FALSE,
  ncompcor=0, N3=FALSE ) {
  pixtype <- "float"
  myusage <- args(aslPerfusion)
  if (nargs() == 0) {
    print(myusage)
    return(NULL)
  }
  if (!is.numeric(maskThresh)) {
    print("maskThresh is not numeric type")
    print(myusage)
    return(NULL)
  }
  if (is.character(asl)) {
    if (length(asl) != 1) {
      print("'asl' should be only one filename")
      return(NULL)
    }
    asl <- antsImageRead(asl, 4)
  } else if (class(asl) == "antsImage") {
    if (asl@pixeltype != pixtype) {
      asl <- antsImageClone(asl, pixtype)
    }
    if (asl@dimension != 4) {
      print(paste("'asl' must have pixeltype ", pixtype, " and dimension '4'"))
      return(NULL)
    }
  } else {
    print("'asl' must be a filename or an 'antsImage'")
    return(NULL)
  }
  if (missing(asl)) {
    print("Missing first (image) parameter")
    print(myusage)
    return(NULL)
  }
  n <- length(dim(asl))
  if (n != 4) {
    print("input image must have dimension 4 ")
    return(NULL)
  }
  if (is.null(moco_results))
    moco_results <- motion_correction(asl, moreaccurate = moreaccurate)
  motionparams <- as.data.frame(moco_results$moco_params)
  moco_mask_img <- getMask(moco_results$moco_avg_img,
    lowThresh = mean(moco_results$moco_avg_img) *
    maskThresh, highThresh = Inf, cleanup = TRUE)
  if (!is.na(mask))
    moco_mask_img <- mask
  mat<-timeseries2matrix( asl, moco_mask_img )
  if ( N3 )
    {
    asl<-timeseriesN3( asl, moco_mask_img )
    }
  if (is.na(m0)) {
    print("Estimating m0 image from the mean of the control values - might be wrong for your data! please check!")
    ctllabs<-c(1:(dim(asl)[4]/2)) * 2 # TC - jj data
    taglabs<-ctllabs-1
    mvals2 <- apply(mat[ctllabs, ], 2, mean)
    mvals1 <- apply(mat[taglabs, ], 2, mean)
    if (verbose) print(paste("Mean-1st-label",mean(mvals1)))
    if (verbose) print(paste("Mean-2nd-label",mean(mvals2)))
    # mean control should exceed mean tag
    if ( mean(mvals2) > mean(mvals1) )
    {
      m0vals<-mvals2
      m1vals<-mvals1
    } else {
      m0vals<-mvals1
      m1vals<-mvals2
    }
    if (verbose) print(paste("ctl",mean(m0vals),'tag',mean(m1vals)))
    m0 <- antsImageClone(moco_mask_img)
    m0[moco_mask_img == 0] <- 0
    m0[moco_mask_img == 1] <- m0vals
    N3BiasFieldCorrection(3,m0,m0,4)
    N3BiasFieldCorrection(3,m0,m0,2)
    # Get average tagged image
    m1 <- antsImageClone(moco_mask_img)
    m1[moco_mask_img == 0] <- 0
    m1[moco_mask_img == 1] <- m1vals
  }
  if (!is.na(mask))
    moco_mask_img <- mask
  mat <- timeseries2matrix(moco_results$moco_img, moco_mask_img)
  # mat <- timeseries2matrix( asl, moco_mask_img)
  if (checkmeansignal > 0) {
    if ( verbose )
      print("Check the mean signal to eliminate frames with high drop out rate")
    imgmeans <- apply(mat, FUN = mean, MARGIN = 1)
    if ( verbose ) plot(ts(imgmeans))
    if ( sum( imgmeans > checkmeansignal ) < (nrow(mat)/2) )
    {
    print("imgmeans suggests data is likely bad - return NA")
    return(NA)
    }
    mat <- subset(mat, imgmeans > checkmeansignal)
    motionparams <- subset(motionparams, imgmeans > checkmeansignal)
    imgmeans <- apply(mat, FUN = mean, MARGIN = 1)
  }
  # Get perfusion time series
  perfusionTimeSeries <- #antsImageClone(moco_results$moco_img)
    new("antsImage", "float", 4)
  ImageMath(4, perfusionTimeSeries,
    "TimeSeriesInterpolationSubtraction", asl,
    interpolation)
  perfusionTimeSeries[!is.finite(as.array(perfusionTimeSeries))]<- 0
  perfusionTimeSeries[is.finite(as.array(perfusionTimeSeries))]<- -1 * perfusionTimeSeries[is.finite(as.array(perfusionTimeSeries))]

  # mat <- antsr_frequency_filter( mat , freqHi = 0.5 , freqLo = 0.01, tr = 4 )
  predictors <- get_perfusion_predictors( mat,
    motionparams, NULL, 1, ncompcor, useDenoiser )
  if ( verbose ) print( names(predictors) )
  # predictors$nuis<-cbind( predictors$globalsignalASL, predictors$nuis )
  mynuis <- data.frame( predictors$nuis, predictors$motion )
#    motionparams[,3:ncol(motionparams)] )
#  if ( !all(is.na(useDenoiser)))
#    mynuis <- data.frame( predictors$nuis )
  if ( verbose ) {
    cat("Nuisance variables\n")
    print( colnames(mynuis) )
  }
  perfusion <- perfusionregression(mask_img = moco_mask_img,
      mat = mat,
      xideal = predictors$xideal,
      nuis = data.matrix(mynuis), dorobust = dorobust,
      skip = skip, selectionValsForRegweights = predictors$dnz,
      useBayesian=useBayesian )
  return(list(perfusion = perfusion$cbfi,
    perfusionTimeSeries = perfusionTimeSeries,
    aslTimeSeries = mat, xideal = predictors$xideal,
    nuisancevariables = mynuis,
    mask = moco_mask_img, m0 = m0, m1 = m1,
    globalsignal = predictors$globalsignalASL,
    indstozero = perfusion$indstozero,
    regweights = perfusion$regweights))
}
