#' Preprocess BOLD fMRI image data.
#'
#' Preprocess fMRI data by performing compcor/motion correction, nuisance
#' regression, band-pass filtering, and spatial smoothing.
#'
#'
#' @param boldImage 4-D ANTs image fMRI data.
#' @param meanBoldFixedImageForMotionCorrection Optional target fixed image for
#' motion correction.
#' @param maskImage 3-D ANTs image defining the region of interest.
#' @param maskingMeanRatioThreshold If mask image is not specified,
#' a mask image is
#' created using the specified threshold which is in terms of the mean of the
#' average image ie 0.8 means threshold at 0.8 of the mean.
#' @param initialNuisanceVariables Optional initial nuisance variables.
#' @param numberOfCompCorComponents Numer of CompCor nuisance components.
#' @param doMotionCorrection Boolean indicating whether motion correction
#' should be performed and used in nuisance regression.
#' @param useMotionCorrectedImage Boolean indicating whether or not the motion
#' corrected image should be used in the rest of the pipeline.  This is off by
#' default to avoid additional interpolation.
#' @param motionCorrectionAccuracyLevel Accuracy for the motion correcting
#' registration: 0 = fast/debug parameters, 1 = intrasession parameters, or 2 =
#' intersession/intersubject parameters.
#' @param frequencyLowThreshold Lower threshold for bandpass filtering.
#' @param frequencyHighThreshold Upper threshold for bandpass filtering.
#' @param spatialSmoothingType Either \code{none}, \code{gaussian} (isotropic) or
#' \code{perona-malik} (anisotropic) smoothing.
#' @param spatialSmoothingParameters For gaussian smoothing, this is a single
#' scalar designating the smoothing sigma (in mm).  For perona-malik, a vector
#' needs to be specified with the conductance parameter and the number of
#' iterations, e.g. \code{c(0.25, 5)}.
#' @param residualizeMatrix boolean
#' @return List of:
#' \itemize{
#'   \item{cleanBOLDImage: }{Cleaned BOLD image.}
#'   \item{maskImage: }{mask image.}
#'   \item{DVARS: }{Framewise change in BOLD signal, as in Powers et al.}
#'   \item{DVARSPostCleaning: }{DVARS after cleaning image.}
#'   \item{FD: }{Framewise displacement.}
#'   \item{globalSignal: }{Global signal.}
#'   \item{nuisanceVariables: }{Nuisance variables used in denoising.}
#' }
#' @references Power et al. 2012, "Spurious but systematic correlations
#' in functional connectivity MRI networks arise from subject motion."
#' NeuroImage 59, 2142-2154.
#' @author Tustison NJ, Avants BB
#' @examples
#' set.seed(123)
#' n=16
#' nvox <- n*n*n*12
#' dims <- c(n,n,n,12)
#' boldImage <- makeImage(dims, rnorm(nvox) + 500) %>% iMath("PadImage", 2)
#' # for real data: boldImage <- antsImageRead(getANTsRData('pcasl'))
#' cleanfMRI <- preprocessfMRI(boldImage)
#' @export preprocessfMRI
preprocessfMRI <- function(boldImage,
  maskImage = NA,
  maskingMeanRatioThreshold = 0.75,
  initialNuisanceVariables = NA,
  numberOfCompCorComponents = 6,
  doMotionCorrection = TRUE,
  useMotionCorrectedImage = FALSE,
  motionCorrectionAccuracyLevel = 1,
  meanBoldFixedImageForMotionCorrection = NA,
  frequencyLowThreshold = NA,
  frequencyHighThreshold = NA,
  spatialSmoothingType = "none",
  spatialSmoothingParameters = 0,
  residualizeMatrix = TRUE) {

  nuisanceVariables <- initialNuisanceVariables

  numberOfTimePoints <- dim(boldImage)[4]

  # do motion correction http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3254728/

  framewiseDisplacement <- rep(0, numberOfTimePoints)
  if (doMotionCorrection) {
    motionCorrectionResults <- .motion_correction(boldImage, fixed = meanBoldFixedImageForMotionCorrection,
      moreaccurate = motionCorrectionAccuracyLevel)
    motionCorrectionParameters <- motionCorrectionResults$moco_params
    nuisanceVariables <- as.matrix(motionCorrectionParameters)[, 3:ncol(motionCorrectionParameters)]
    for (i in 2:numberOfTimePoints) {
      motionCorrectionParametersAtTime1 <- c(motionCorrectionParameters[i,
        3:14])
      rotationMatrixAtTime1 <- matrix(as.numeric(motionCorrectionParametersAtTime1[1:9]),
        ncol = 3, nrow = 3)
      translationAtTime1 <- as.numeric(motionCorrectionParametersAtTime1[10:12])
      motionCorrectionParametersAtTime2 <- c(motionCorrectionParameters[i -
        1, 3:14])
      rotationMatrixAtTime2 <- matrix(as.numeric(motionCorrectionParametersAtTime2[1:9]),
        ncol = 3, nrow = 3)
      translationAtTime2 <- as.numeric(motionCorrectionParametersAtTime2[10:12])

      # pick a point 10 mm from the center

      samplePoint <- data.matrix(t(matrix(rep(10, 3), nrow = 1)))

      # calculate the transformed point at time point i and ( i - 1 )

      transformedPointAtTime1 <- data.matrix(rotationMatrixAtTime1) %*% samplePoint +
        translationAtTime1
      transformedPointAtTime2 <- data.matrix(rotationMatrixAtTime2) %*% samplePoint +
        translationAtTime2
      framewiseDisplacement[i] <- sum(abs(transformedPointAtTime2 - transformedPointAtTime1))
    }
    framewiseDisplacement[1] <- mean(framewiseDisplacement[2:numberOfTimePoints])

    if (useMotionCorrectedImage) {
      boldImage <- motionCorrectionResults$moco_img
    }
  } else {
    if (useMotionCorrectedImage) {
      cat("Warning:  if motion correction is not performed then the motion corrected image is unavailable for use.\n")
      useMotionCorrectedImage <- FALSE
    }
  }

  averageImage <- new("antsImage", "float", 3)
  antsMotionCorr(list(d = 3, a = boldImage, o = averageImage))
  # Calculate the mask, if not supplied.

  if (is.na(maskImage)) {
    maskImage <- getMask( averageImage )
  }
  averageImage[maskImage == 0] <- 0

  # do nuisance regression then bandpass filtering
  # http://blogs.discovermagazine.com/neuroskeptic/2013/06/12/when-cleaning-fmri-data-is-a-nuisance/
  boldMatrix <- timeseries2matrix(boldImage, maskImage)
  DVARS <- computeDVARS(boldMatrix)


  # Calculate CompCor nuisance variables
  # http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2214855/
  if (numberOfCompCorComponents > 0) {
    compCorNuisanceVariables <- compcor(boldImage, maskImage, ncompcor = numberOfCompCorComponents,
      variance_extreme = 0.975)
    if (is.na(nuisanceVariables) || is.null(dim(nuisanceVariables))) {
      nuisanceVariables <- compCorNuisanceVariables
    } else {
      nuisanceVariables <- cbind(nuisanceVariables, compCorNuisanceVariables)
    }
  }

  # replace boldMatrix in place with residualized version
  if (!is.na(nuisanceVariables[1]) & residualizeMatrix) {
    boldMatrix <- residuals(lm(boldMatrix ~ scale(nuisanceVariables)))
  }
  # replace boldMatrix in place with frequency filtered version
  if (!is.na(frequencyHighThreshold) & !is.na(frequencyHighThreshold) & (frequencyLowThreshold !=
    frequencyHighThreshold)) {
    boldMatrix <- frequencyFilterfMRI(boldMatrix, tr = antsGetSpacing(boldImage)[4],
      freqLo = frequencyLowThreshold, freqHi = frequencyHighThreshold, opt = "trig")
  }
  DVARSpostCleaning <- computeDVARS(boldMatrix)

  # Convert the cleaned matrix back to a 4-D image
  globalSignal <- apply(boldMatrix, FUN = mean, MARGIN = 2)
  cleanBoldImage <- matrix2timeseries(boldImage, maskImage, boldMatrix)

  # anisotropically smooth the 4-D image, if desired

  smoothCleanBoldImage <- antsImageClone(cleanBoldImage, "float")
  if (spatialSmoothingType == "gaussian") {
    if (length(spatialSmoothingParameters) == 1) {
      sigmaVector <- paste0(spatialSmoothingParameters[1], "x", spatialSmoothingParameters[1],
        "x", spatialSmoothingParameters[1], "x0")
      imageMath(4, smoothCleanBoldImage, "G", smoothCleanBoldImage, sigmaVector)
    } else {
      stop("Expecting a single scalar parameter.")
    }
  } else if (spatialSmoothingType == "perona-malik") {
    if (length(spatialSmoothingParameters) == 2) {
      imageMath(4, smoothCleanBoldImage, "PeronaMalik", cleanBoldImage, spatialSmoothingParameters[1],
        spatialSmoothingParameters[2])
    } else {
      stop("Expecting a two element vector.")
      return
    }
  } else if (spatialSmoothingType != "none") {
    stop("Unrecognized smoothing option.")
  }
  #####################################################################
  return(list(cleanBoldImage = smoothCleanBoldImage, maskImage = maskImage, DVARS = DVARS,
    DVARSpostCleaning = DVARSpostCleaning, FD = framewiseDisplacement, globalSignal = globalSignal,
    nuisanceVariables = nuisanceVariables))
}


#' computeDVARS
#'
#' compute the DVARS quality control metric
#'
#'
#' @param boldMatrix matrix of bold signal
#' @return DVARS vector.
#' @author Tustison NJ, Avants BB
#' @examples
#'
#'  mat <- matrix(c(0,1,2,0,0,1,2,2,2),ncol=3)
#'  dv<-computeDVARS(mat)
#'
#' @export computeDVARS
computeDVARS <- function(boldMatrix) {
  # For quality assurance measures, we calculate the temporal derivative of the RMS
  # variance over voxels (DVARS as in
  # http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3254728/)
  DVARS <- rep(0, nrow(boldMatrix))
  for (i in 2:nrow(boldMatrix)) {
    DVARS[i] <- sqrt(mean((boldMatrix[i, ] - boldMatrix[i - 1, ])^2))
  }
  DVARS[1] <- mean(DVARS)
  return(DVARS)
}
