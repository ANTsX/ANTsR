#' Extract generic fMRI nuisance variables for ASL or BOLD
#'
#' Will motion correct, run compcorr and estimate global signal. Outputs a list
#' with the time series data matrix (time by voxels), motion and other nuisance
#' variables, global signal (for BOLD or ASL), the mask and the average time
#' series image.  Meant to be used before filterfMRIforNetworkAnalysis or any
#' other results-oriented processing.
#'
#'
#' @param fmri input antsImage or filename
#' @param maskThresh will use this intensity threshold to estimate a mask
#' otherwise will use the mask passed in
#' @param moreaccurate zero, one or two with increasing accuracy/computation
#' @param mask binary image masking the intput image, precedent over mask thresh
#' @return outputs list described above.
#' @author Avants BB
#' @examples
#'
#' \dontrun{
#' if (!exists("fn") ) fn<-getANTsRData("pcasl")
#' pcasl<-antsImageRead( fn )
#' aslmean<-getAverageOfTimeSeries( pcasl )
#' aslmask<-getMask(aslmean)
#' ee<-getfMRInuisanceVariables( pcasl, mask = aslmask ,
#'         moreaccurate=F )
#' }
#'
#' @export getfMRInuisanceVariables
getfMRInuisanceVariables <- function(
  fmri, maskThresh = 500, moreaccurate = 1,
  mask = NULL) {
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
  moco_results <- .motion_correction(fmri, moreaccurate = moreaccurate)
  moco_mask_img <- getMask(moco_results$moco_avg_img, lowThresh = maskThresh, highThresh = 1e+09,
    cleanup = TRUE)
  if (!is.null(mask)) {
    moco_mask_img <- mask
  } 
  mat <- timeseries2matrix(moco_results$moco_img, moco_mask_img)
  motionparams <- as.data.frame(moco_results$moco_params)
  predictors <- .get_perfusion_predictors(mat, motionparams, NULL, 1, 3)
  globalsignal <- predictors$globalsignal
  return(list(matrixTimeSeries = mat, nuisancevariables = predictors$nuis, mask = moco_mask_img,
    avgImage = moco_results$moco_avg_img, globalsignal = globalsignal, globalsignalASL = predictors$globalsignalASL,
    moco_img = moco_results$moco_img))
}
