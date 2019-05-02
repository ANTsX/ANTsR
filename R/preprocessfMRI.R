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
#' @param num_threads will execute 
#' \code{Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = num_threads)} before
#' running to attempt a more reproducible result.  See
#' \url{https://github.com/ANTsX/ANTs/wiki/antsRegistration-reproducibility-issues}
#' for discussion.  If \code{NULL}, will not set anything. 
#' @param seed will execute 
#' \code{Sys.setenv(ANTS_RANDOM_SEED = seed)} before
#' running to attempt a more reproducible result.  See
#' \url{https://github.com/ANTsX/ANTs/wiki/antsRegistration-reproducibility-issues}
#' for discussion.  If \code{NULL}, will not set anything.  
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
#' n=8
#' nvox <- n*n*n*6
#' dims <- c(n,n,n,6)
#' boldImage <- makeImage(dims, rnorm(nvox) + 500) %>% iMath("PadImage", 2)
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
  residualizeMatrix = TRUE,
  num_threads = 1,
  seed = NULL  ) {

  ###################################
  # set for reproducibility
  ###################################
  ants_random_seed = itk_threads = NULL
  if (!is.null(seed)) {
    ants_random_seed = Sys.getenv("ANTS_RANDOM_SEED")
    Sys.setenv(ANTS_RANDOM_SEED = seed)    
  }
  if (!is.null(num_threads)) {
    itk_threads = Sys.getenv("ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS")
    Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = num_threads)
  }  
  on.exit({
    if (!is.null(ants_random_seed)) {
      Sys.setenv(ANTS_RANDOM_SEED = ants_random_seed)
    }
    if (!is.null(itk_threads)) {
      Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = itk_threads)
    }    
  })
  
  
  
  nuisanceVariables <- initialNuisanceVariables

  numberOfTimePoints <- dim(boldImage)[4]

  # do motion correction http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3254728/

  framewiseDisplacement <- rep(0, numberOfTimePoints)
  if (doMotionCorrection) {
    motionCorrectionResults <- .motion_correction(boldImage, fixed = meanBoldFixedImageForMotionCorrection,
      moreaccurate = motionCorrectionAccuracyLevel,
      num_threads = num_threads,
      seed = seed)
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
      framewiseDisplacement[i] <- dist( rbind(transformedPointAtTime2,transformedPointAtTime1 ))[[1]]
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
    if (all(is.na(nuisanceVariables)) || is.null(dim(nuisanceVariables))) {
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
  globalSignal <- apply(boldMatrix, FUN = mean, MARGIN = 1)
  cleanBoldImage <- matrix2timeseries(boldImage, maskImage, boldMatrix)

  # anisotropically smooth the 4-D image, if desired
  smoothCleanBoldImage = cleanBoldImage*1

  if (spatialSmoothingType == "gaussian") {
    if (length(spatialSmoothingParameters) == 1) {
      sigmaVector <- c( rep( spatialSmoothingParameters[1], 3, ), 0 )
      smoothCleanBoldImage <- smoothImage(cleanBoldImage, sigmaVector)
    } else {
      stop("Expecting a single scalar parameter.")
    }
  } else if (spatialSmoothingType == "perona-malik") {
    if (length(spatialSmoothingParameters) == 2) {
      smoothCleanBoldImage <- iMath(cleanBoldImage, "PeronaMalik", spatialSmoothingParameters[1],
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
