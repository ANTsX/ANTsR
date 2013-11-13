aslPerfusion <- function(asl, maskThresh = 500, moreaccurate = TRUE, dorobust = 0.92, m0 = NA, skip = 20, mask = NA, 
  interpolation = "linear", checkmeansignal = 100) {
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
      print(paste("'asl' must have pixeltype  ", pixtype))
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
  moco_results <- motion_correction(asl, moreaccurate = moreaccurate)
  motionparams <- as.data.frame(moco_results$moco_params)
  moco_mask_img <- getMask(moco_results$moco_avg_img, lowThresh = maskThresh, highThresh = 1e+09, cleanup = TRUE)
  if (!is.na(mask)) 
    moco_mask_img <- mask
  mat <- timeseries2matrix(moco_results$moco_img, moco_mask_img)
  if (checkmeansignal > 0) {
    print("Check the mean signal to eliminate frames with high drop out rate")
    imgmeans <- apply(mat, FUN = mean, MARGIN = 1)
    mat <- subset(mat, imgmeans > checkmeansignal)
    motionparams <- subset(motionparams, imgmeans > checkmeansignal)
    imgmeans <- apply(mat, FUN = mean, MARGIN = 1)
    print(imgmeans)
  }
  if (is.na(m0)) {
    print("Estimating m0 image from the mean of the control values - might be wrong for your data! please check!")
    m0vals <- apply(mat[c(1:(nrow(mat)/2)) * 2, ], 2, mean)  # for T C T C , JJ data
    m0 <- antsImageClone(moco_mask_img)
    m0[moco_mask_img == 0] <- 0
    m0[moco_mask_img == 1] <- m0vals
  }
  # mat <- antsr_frequency_filter( mat , freqHi = 0.5 , freqLo = 0.01, tr = 4 )
  predictors <- get_perfusion_predictors(mat, motionparams, NULL, 1, 3)
  
  # Get average tagged image
  m1vals <- apply(mat[c(1:(nrow(mat)/2)) * 2 - 1, ], 2, mean)  # for T C T C , JJ data
  m1 <- antsImageClone(moco_mask_img)
  m1[moco_mask_img == 0] <- 0
  m1[moco_mask_img == 1] <- m1vals
  # predictors$nuis<-cbind( predictors$globalsignalASL, predictors$nuis )
  mynuis <- as.data.frame(as.data.frame(predictors$nuis[, 2:7]))
  print(colnames(mynuis))
  perfusion <- perfusionregression(mask_img = moco_mask_img, mat = mat, xideal = predictors$xideal, nuis = as.matrix(mynuis), 
    dorobust = dorobust, skip = skip)
  
  # Get perfusion time series
  perfusionTimeSeries <- new("antsImage", "float", 4)
  ImageMath(4, perfusionTimeSeries, "TimeSeriesInterpolationSubtraction", moco_results$moco_img, interpolation)
  
  perfusionTimeSeries[!is.finite(as.array(perfusionTimeSeries))] <- 0
  perfusionTimeSeries[is.finite(as.array(perfusionTimeSeries))] <- -1 * perfusionTimeSeries[is.finite(as.array(perfusionTimeSeries))]
  
  return(list(perfusion = perfusion, perfusionTimeSeries = perfusionTimeSeries, aslTimeSeries = mat, xideal = predictors$xideal, 
    nuisancevariables = predictors$nuis, mask = moco_mask_img, m0 = m0, m1 = m1, globalsignal = predictors$globalsignalASL))
} 
