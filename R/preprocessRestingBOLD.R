#' Preprocess resting BOLD fMRI image data.
#'
#' Preprocess resting fMRI by performing compcor/motion correction, nuisance
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
#' @param motionCorrectionIterations number of iteration for motion correction
#' @param frequencyLowThreshold Lower threshold for bandpass filtering.
#' @param frequencyHighThreshold Upper threshold for bandpass filtering.
#' @param spatialSmoothingType Either \code{none}, \code{gaussian} (isotropic) or
#' \code{perona-malik} (anisotropic) smoothing.
#' @param spatialSmoothingParameters For gaussian smoothing, this is a single
#' scalar designating the smoothing sigma (in mm).  For perona-malik, a vector
#' needs to be specified with the conductance parameter and the number of
#' iterations, e.g. \code{c(0.25, 5)}.
#' @param residualizeMatrix boolean
#' @param denseFramewise ignored; framewise displacement always uses all
#' brain-mask voxels (kept for backward compatibility)
#'
#' @return List of:
#' \describe{
#'   \item{cleanBOLDImage: }{Cleaned BOLD image.}
#'   \item{maskImage: }{mask image.}
#'   \item{DVARS: }{Framewise change in BOLD signal, as in Powers et al.}
#'   \item{DVARSPostCleaning: }{DVARS after cleaning image.}
#'   \item{FD: }{Framewise displacement: mean Euclidean displacement (in mm)
#'   of brain-mask points between consecutive frames, computed from the full
#'   motion-correction transforms.  This is not the FD of Power et al. (2012),
#'   which sums absolute changes in translations and radius-scaled rotations.
#'   The first value is the mean of the others.}
#'   \item{globalSignal: }{Global signal.}
#'   \item{nuisanceVariables: }{Nuisance variables used in denoising.}
#' }
#' @references Power et al. 2012, "Spurious but systematic correlations
#' in functional connectivity MRI networks arise from subject motion."
#' NeuroImage 59, 2142-2154.
#' @author Tustison NJ, Avants BB
#' @export
preprocessRestingBOLD <- function(
    boldImage,
    maskImage = NULL,
    maskingMeanRatioThreshold = 0.75,
    initialNuisanceVariables,
    denseFramewise = FALSE,
    numberOfCompCorComponents = 6,
    doMotionCorrection = TRUE,
    useMotionCorrectedImage = FALSE,
    motionCorrectionAccuracyLevel = 1,
    motionCorrectionIterations = 1,
    meanBoldFixedImageForMotionCorrection = NULL,
    frequencyLowThreshold = NA,
    frequencyHighThreshold = NA,
    spatialSmoothingType = "none",
    spatialSmoothingParameters = 0,
    residualizeMatrix = TRUE) {
  nuisanceVariables <- NULL
  if (!missing(initialNuisanceVariables)) {
    nuisanceVariables <- initialNuisanceVariables
  }

  numberOfTimePoints <- dim(boldImage)[4]

  # do motion correction
  framewiseDisplacement <- rep(0, numberOfTimePoints)

  if (doMotionCorrection) {
    # Reference image for motion correction
    motionCorrectionResults <- NA
    if (is.null(meanBoldFixedImageForMotionCorrection)) {
      meanBoldFixedImageForMotionCorrection <- getAverageOfTimeSeries(boldImage)
    } else {
      meanBoldFixedImageForMotionCorrection <- check_ants(meanBoldFixedImageForMotionCorrection)
    }

    # Iterative motion correction
    for (iter in 1:motionCorrectionIterations) {
      motionCorrectionResults <- .motion_correction(boldImage,
        fixed = meanBoldFixedImageForMotionCorrection,
        moreaccurate = motionCorrectionAccuracyLevel
      )
      # refine the reference with the motion-corrected average; otherwise
      # every iteration repeats exactly the same registration
      if (iter < motionCorrectionIterations) {
        meanBoldFixedImageForMotionCorrection <- motionCorrectionResults$moco_avg_img
      }
    }

    # Store motion correction parameters
    motionCorrectionParameters <- motionCorrectionResults$moco_params
    nuisanceVariables <- cbind(
      nuisanceVariables,
      as.matrix(motionCorrectionParameters)[, 3:ncol(motionCorrectionParameters)]
    )

    # Framewise displacement: mean Euclidean displacement of brain-mask points
    # between consecutive frames, using the full ITK transforms (row-major
    # matrix, translation and the center antsMotionCorr used, i.e. the center
    # of gravity of the fixed image of the last iteration)
    framewiseDisplacement <- .mocoFramewiseDisplacement(
      motionCorrectionParameters, meanBoldFixedImageForMotionCorrection,
      maskImage
    )$MeanDisplacement
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

  averageImage <- getAverageOfTimeSeries(boldImage)

  # Calculate the mask, if not supplied.
  if (is.null(maskImage)) {
    maskImage <- getMask(averageImage)
  } else {
    maskImage <- check_ants(maskImage)
  }
  averageImage[maskImage == 0] <- 0

  # do nuisance regression then bandpass filtering
  # http://blogs.discovermagazine.com/neuroskeptic/2013/06/12/when-cleaning-fmri-data-is-a-nuisance/
  boldMatrix <- timeseries2matrix(boldImage, maskImage)
  DVARS <- computeDVARS(boldMatrix)


  # Calculate CompCor nuisance variables
  # https://pmc.ncbi.nlm.nih.gov/articles/PMC2214855/
  if (numberOfCompCorComponents > 0) {
    compCorNuisanceVariables <- compcor(boldImage, maskImage,
      ncompcor = numberOfCompCorComponents,
      variance_extreme = 0.975
    )
    nuisanceVariables <- cbind(nuisanceVariables, compCorNuisanceVariables)
  }

  # replace boldMatrix in place with residualized version
  if (!is.na(nuisanceVariables[1]) & residualizeMatrix) {
    boldMatrix <- residuals(lm(boldMatrix ~ scale(nuisanceVariables)))
  }
  # replace boldMatrix in place with frequency filtered version
  if (!is.na(frequencyLowThreshold) & !is.na(frequencyHighThreshold) & (frequencyLowThreshold !=
    frequencyHighThreshold)) {
    boldMatrix <- frequencyFilterfMRI(boldMatrix,
      tr = antsGetSpacing(boldImage)[4],
      freqLo = frequencyLowThreshold, freqHi = frequencyHighThreshold, opt = "trig"
    )
  }
  DVARSpostCleaning <- computeDVARS(boldMatrix)

  # Convert the cleaned matrix back to a 4-D image
  globalSignal <- apply(boldMatrix, FUN = mean, MARGIN = 1)
  cleanBoldImage <- matrix2timeseries(boldImage, maskImage, boldMatrix)

  # anisotropically smooth the 4-D image, if desired
  smoothCleanBoldImage <- cleanBoldImage * 1

  if (spatialSmoothingType == "gaussian") {
    if (length(spatialSmoothingParameters) == 1) {
      sigmaVector <- c(rep(spatialSmoothingParameters[1], 3, ), 0)
      smoothCleanBoldImage <- smoothImage(cleanBoldImage, sigmaVector)
    } else {
      stop("Expecting a single scalar parameter.")
    }
  } else if (spatialSmoothingType == "perona-malik") {
    if (length(spatialSmoothingParameters) == 2) {
      smoothCleanBoldImage <- iMath(
        cleanBoldImage, "PeronaMalik", spatialSmoothingParameters[1],
        spatialSmoothingParameters[2]
      )
    } else {
      stop("Expecting a two element vector.")
      return()
    }
  } else if (spatialSmoothingType != "none") {
    stop("Unrecognized smoothing option.")
  }
  #####################################################################
  return(list(
    cleanBoldImage = smoothCleanBoldImage, maskImage = maskImage, DVARS = DVARS,
    DVARSpostCleaning = DVARSpostCleaning, FD = framewiseDisplacement, globalSignal = globalSignal,
    nuisanceVariables = nuisanceVariables
  ))
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
#' mat <- matrix(c(0, 1, 2, 0, 0, 1, 2, 2, 2), ncol = 3)
#' dv <- computeDVARS(mat)
#'
#' @export computeDVARS
computeDVARS <- function(boldMatrix) {
  # For quality assurance measures, we calculate the temporal derivative of the RMS
  # variance over voxels (DVARS as in
  # https://pmc.ncbi.nlm.nih.gov/articles/PMC3254728/)
  DVARS <- rep(0, nrow(boldMatrix))
  for (i in 2:nrow(boldMatrix)) {
    DVARS[i] <- sqrt(mean((boldMatrix[i, ] - boldMatrix[i - 1, ])^2))
  }
  DVARS[1] <- mean(DVARS)
  return(DVARS)
}



#' computeDVARSspatialMap
#'
#' compute the DVARS quality control metric at every voxel
#'
#'
#' @param boldMatrix matrix of bold signal
#' @return DVARS spatial vector.
#' @author Tustison NJ, Avants BB
#' @examples
#'
#' mat <- matrix(c(0, 1, 2, 0, 0, 1, 2, 2, 2), ncol = 3)
#' dv <- computeDVARSspatialMap(mat)
#'
#' @export computeDVARSspatialMap
computeDVARSspatialMap <- function(boldMatrix) {
  # For quality assurance measures, we calculate the temporal derivative of the RMS
  # variance over voxels (DVARS as in
  # https://pmc.ncbi.nlm.nih.gov/articles/PMC3254728/)
  dvmat <- boldMatrix * 0
  for (i in 2:nrow(boldMatrix)) {
    dvmat[i, ] <- sqrt((boldMatrix[i, ] - boldMatrix[i - 1, ])^2)
  }
  return(colMeans(dvmat, na.rm = TRUE))
}
