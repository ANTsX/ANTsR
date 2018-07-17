#' ASL-based Perfusion from PASL, CASL or pCASL.
#'
#' This function will estimate perfusion from an ASL time series using a
#' (robust) regression approach. It will do motion correction, compcorr, factor
#' out nuisance variables and use regression to estimate the perfusion itself.
#' It will mask the image too based on a simple procedure ( should fix this in
#' the future by allowing the user to optionally pass a mask in from the
#' outside ).  WARNING: the function will estimate the m0 image from the mean
#' of the control tags assuming that the data is acquired T C T C as is most of
#' JJ's data.  Quantitative CBF can be obtained by mutiplying the output of
#' this function by a scalar.
#'
#' @param asl input asl image
#' @param maskThresh for estimating a brain mask
#' @param moreaccurate zero, one or two with the last being the most accurate
#' @param dorobust robustness parameter, lower value keeps more data
#' @param m0 known M0 if any
#' @param skip stride to speed up robust regression weight estimates
#' @param mask known brain mask
#' @param checkmeansignal throw out volumes with mean lower than this thresh
#' @param moco_results passes prior motion results so moco does not get repeated
#' @param regweights known temporal weights on regression solution, if any
#' @param useDenoiser use the aslDenoiser if this value is a range gt than zero
#' @param useBayesian strength of bayesian prior
#' @param verbose bool
#' @param ncompcor number of compcor parameters
#' @param N3 bool correct target image before motion corr
#' @return output a list of relevant objects
#' @author Avants BB
#' @examples
#'
#' # image available at http://files.figshare.com/1701182/PEDS012_20131101.zip
#' # fn<-'PEDS012_20131101_pcasl_1.nii.gz'
#' # asl<-antsImageRead(fn,4)
#' set.seed(1)
#' nvox <- 5*5*5*10
#' dims <- c(5,5,5,10)
#' asl <- makeImage( dims , rnorm( nvox )+500 ) %>% iMath("PadImage" , 2 )
#' aslmean <- getAverageOfTimeSeries( asl )
#' aslmask <- getMask( aslmean , 0.001 , Inf )
#' aslmat<-timeseries2matrix( asl, aslmask )
#' for ( i in 1:10 ) aslmat[,i*2]<-aslmat[,i*2]*2
#' asl<-matrix2timeseries( asl, aslmask, aslmat )
#' # NOT WORKING
#' \dontrun{
#' pcasl.processing <- aslPerfusion( asl, moreaccurate=1, dorobust=0 )
#' testthat::expect_equal(mean(pcasl.processing$m1), 62.2115522470984)
#' pcasl.processing <- aslPerfusion( asl, moreaccurate=1, ncompcor=2 )
#' # allow some rejection
#' pcasl.processing <- aslPerfusion( asl, moreaccurate=1, dorobust=0.925 )
#' }
#' @export aslPerfusion
aslPerfusion <- function(
  asl,
  maskThresh = 0.75,
  moreaccurate = 1,
  dorobust = 0.92,
  m0 = NA,
  skip = 20,
  mask = NA,
  checkmeansignal = 100,
  moco_results = NULL,
  regweights = NULL,
  useDenoiser = NA,
  useBayesian=0,
  verbose=FALSE,
  ncompcor=0,
  N3=FALSE ) {
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
      stop("'asl' should be only one filename")
    }
    asl <- antsImageRead(asl, 4)
  } else if (class(asl) == "antsImage") {
    if (asl@pixeltype != pixtype) {
      asl <- antsImageClone(asl, pixtype)
    }
    if (asl@dimension != 4) {
      stop(paste("'asl' must have pixeltype ", pixtype, " and dimension '4'"))
    }
  } else {
    stop("'asl' must be a filename or an 'antsImage'")
  }
  if (missing(asl)) {
    print("Missing first (image) parameter")
    print(myusage)
    return(NULL)
  }
  n <- length(dim(asl))
  if (n != 4) {
    stop("input image must have dimension 4 ")
  }
  if (is.null(moco_results))
    moco_results <- .motion_correction(asl, moreaccurate = moreaccurate)
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
    warning(paste("Estimating m0 image from the mean of the control values.", 
     "Might be wrong for your data! Please check!"))
    ctllabs<-c(1:(dim(asl)[4]/2)) * 2 # TC - jj data
    taglabs<-ctllabs-1
    mvals2 <- apply(mat[ctllabs, , drop = FALSE], 2, mean)
    mvals1 <- apply(mat[taglabs, , drop = FALSE], 2, mean)
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
    m0<-n3BiasFieldCorrection(m0,4)
    m0<-n3BiasFieldCorrection(m0,2)
    # Get average tagged image
    m1 <- antsImageClone(moco_mask_img)
    m1[moco_mask_img == 0] <- 0
    m1[moco_mask_img == 1] <- m1vals
  }
  if (!is.na(mask))
    moco_mask_img <- mask
  mat <- timeseries2matrix(moco_results$moco_img, moco_mask_img)
  if (checkmeansignal > 0) {
    if ( verbose )
      print("Check the mean signal to eliminate frames with high drop out rate")
    imgmeans <- apply(mat, FUN = mean, MARGIN = 1)
    if ( verbose ) plot(ts(imgmeans))
    if ( sum( imgmeans > checkmeansignal ) < (nrow(mat)/2) )
    {
    warning("imgmeans suggests data is likely bad - returning NA")
    return(NA)
    }
    mat <- subset(mat, imgmeans > checkmeansignal)
    motionparams <- subset(motionparams, imgmeans > checkmeansignal)
    imgmeans <- apply(mat, FUN = mean, MARGIN = 1)
  }

  predictors <- .get_perfusion_predictors( mat,
    motionparams, NULL, 1, ncompcor, useDenoiser )
  if (verbose) { 
    print( names(predictors) )
  }
  if ( ! all( is.na(predictors$nuis) ) ) {
    mynuis <- data.frame( predictors$nuis, predictors$motion )
  } else {
    mynuis <- data.frame( predictors$motion )
  }
  if ( verbose ) {
    cat("Nuisance variables\n")
    print( colnames(mynuis) )
  }
  if ( ! all( is.na(mynuis)) ) {
    rmat <- residuals( lm( mat ~ 0 + data.matrix(mynuis) )  )
  } else {
    rmat <- mat
  }
  perfusion <- perfusionregression(mask_img = moco_mask_img,
      mat = mat, # or rmat?
      xideal = predictors$xideal,
      nuis = data.matrix(mynuis), dorobust = dorobust,
      skip = skip, selectionValsForRegweights = predictors$dnz,
      useBayesian=useBayesian )
  return(list(perfusion = perfusion$cbfi,
    aslTimeSeries = mat, xideal = predictors$xideal,
    nuisancevariables = mynuis,
    mask = moco_mask_img, m0 = m0, m1 = m1,
    globalsignal = predictors$globalsignalASL,
    indstozero = perfusion$indstozero,
    regweights = perfusion$regweights))
}
