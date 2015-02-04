#' Extract generic fMRI nuisance variables for ASL or BOLD
#'
#' Will motion correct, run compcorr and estimate global signal. Outputs a list
#' with the time series data matrix (time by voxels), motion and other nuisance
#' variables, global signal (for BOLD or ASL), the mask and the average time
#' series image.  Meant to be used before filterfMRIforNetworkAnalysis or any
#' other results-oriented processing.
#'
#'
#' @param boldImageOrFileName input antsImage or filename
#' @param maskThresh will use this intensity threshold to estimate a mask
#' otherwise will use the mask passed in
#' @param mask binary image masking the intput image
#' @return outputs list described above.
#' @author Avants BB
#' @examples
#'
#' \dontrun{
#' # set moreaccurate=T for final results
#' dd<-getfMRInuisanceVariables( bold, maskThresh=100 , moreaccurate=F )
#' tsResid<-residuals( lm( dd$matrixTimeSeries ~
#'     dd$globalsignal + dd$nuisancevariables ))
#' mynetwork<-filterfMRIforNetworkAnalysis( tsResid , tr=4,
#'    mask=dd$mask ,cbfnetwork = 'BOLD',
#'   labels = aalImageLabels , graphdensity = 0.25 )
#' # use colnames( dd$nuisancevariables ) to see nuisance variables
#'
#' ee<-getfMRInuisanceVariables( pcasl, mask = pcaslmask ,
#'    moreaccurate=F )
#' tsResid<-residuals( lm( ee$matrixTimeSeries ~
#'     ee$globalsignalASL + ee$nuisancevariables ))
#' myASLnetwork<-filterfMRIforNetworkAnalysis( tsResid , tr=4,
#'    mask=ee$mask ,cbfnetwork = 'ASLCBF', labels = aal2asl ,
#'    graphdensity = 0.25 )
#' # use colnames( dd$nuisancevariables ) to see nuisance variables
#' }
#'
#' @export getfMRInuisanceVariables
getfMRInuisanceVariables <- function(fmri, maskThresh = 500, moreaccurate = TRUE,
  mask = NA) {
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
  if (is.character(fmri)) {
    if (length(fmri) != 1) {
      print("'fmri' should be only one filename")
      return(NULL)
    }
    fmri <- antsImageRead(fmri, 4)
  } else if (class(fmri) == "antsImage") {
    if (fmri@pixeltype != pixtype) {
      print(paste("'fmri' must have pixeltype  ", pixtype))
      fmri <- antsImageClone(fmri, pixtype)
    }
    if (fmri@dimension != 4) {
      print(paste("'fmri' must have pixeltype ", pixtype, " and dimension '4'"))
      return(NULL)
    }
  } else {
    print("'fmri' must be a filename or an 'antsImage'")
    return(NULL)
  }
  if (missing(fmri)) {
    print("Missing first (image) parameter")
    print(myusage)
    return(NULL)
  }
  n <- length(dim(fmri))
  if (n != 4) {
    print("input image must have dimension 4 ")
    return(NULL)
  }
  moco_results <- motion_correction(fmri, moreaccurate = moreaccurate)
  moco_mask_img <- getMask(moco_results$moco_avg_img, lowThresh = maskThresh, highThresh = 1e+09,
    cleanup = TRUE)
  if (!is.na(mask))
    moco_mask_img <- mask
  mat <- timeseries2matrix(moco_results$moco_img, moco_mask_img)
  motionparams <- as.data.frame(moco_results$moco_params)
  predictors <- get_perfusion_predictors(mat, motionparams, NULL, 1, 3)
  globalsignal <- predictors$globalsignal
  return(list(matrixTimeSeries = mat, nuisancevariables = predictors$nuis, mask = moco_mask_img,
    avgImage = moco_results$moco_avg_img, globalsignal = globalsignal, globalsignalASL = predictors$globalsignalASL,
    moco_img = moco_results$moco_img))
}
