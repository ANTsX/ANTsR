\name{antsPreprocessfMRI}
\alias{antsPreprocessfMRI}

\title{
Preprocess BOLD fMRI image data.
}

\description{
Preprocess fMRI data which includes compcor/motion correction, nuisance regression,
band-pass filtering, and spatial smoothing.
}

\usage{
antsPreprocessfMRI <- function( boldImage,
  meanBoldFixedImageForMotionCorrection = NA,
  maskImage = NA,
  maskingThreshold = 0.75,
  initialNuisanceVariables = NA,
  numberOfCompCorComponents = 6,
  doMotionCorrection = TRUE,
  useMotionCorrectedImage = FALSE,
  spatialSmoothingType = c( "none", "gaussian", "perona-malik" ),
  spatialSmoothingParameters = 0.0,
  frequencyLowThreshold = 0.01,
  frequencyHighThreshold = 0.1,
  motionCorrectionAccuracyLevel = 1 )
}

\arguments{
\item{boldImage}{4-D ANTs image fMRI data.}
\item{meanBoldFixedImageForMotionCorrection}{Optional target fixed image for motion correction.}
\item{maskImage}{3-D ANTs image defining the region of interest.}
\item{maskingThreshold}{If mask image is not specified, a mask image is created using
                        the specified threshold which is in terms of the mean of the
			average image ie 0.8 means threshold at 0.8 of the mean.}
\item{initialNuisanceVariables}{Optional initial nuisance variables.}
\item{numberOfCompCorComponents}{Numer of CompCor nuisance components (default = 6).}
\item{doMotionCorrection}{Booelan indicating whether motion correction should be performed
                          and used in nuisance regression.}
\item{useMotionCorrectedImage}{Boolean indicating whether or not the motion corrected
                               image should be used in the rest of the pipeline.  This is
                               off by default to avoid additional interpolation.}
\item{spatialSmoothingType}{Either "none", "gaussian" (isotropic) or "perona-malik" (anisotropic)
                            smoothing.}
\item{spatialSmoothingParameters}{For gaussian smoothing, this is a single scalar designating
                                  the smoothing sigma (in mm).  For perona-malik, a vector
                                  needs to be specified with the conductance parameter and the
                                  number of iterations, e.g. c( 0.25, 5 ).}
\item{frequencyLowThreshold}{Lower threshold for bandpass filtering.}
\item{frequencyHighThreshold}{Upper threshold for bandpass filtering.}
\item{motionCorrectionAccuracyLevel}{for motion correcting registration, 0 (fast/debug parameters), 1 (intrasession parameters) or 2 (intersession/intersubect parameters).}
}

\value{
Output is the "clean" fMRI bold image and mask.  Quality assurance output includes
the framewise displacement (FD) and DVARS.  "DVARS is the root mean squared (RMS) change in
BOLD signal from volume to volume (D referring to temporal derivative of time courses
and VARS referring to RMS variance over voxels.)" --- Power et. al 2012, Spurious but
systematic correlations in functional connectivity MRI networks arise from subject motion.
Neuroimage 59, 2142-2154.
}

\author{
Tustison NJ, Avants BB
}

\examples{
\dontrun{
boldImage <- antsImageRead( "fmri.nii.gz", dim = 4, pixeltype = "float" )
cleanfMRI <- antsPreprocessfMRI( boldImage )
cleanBoldImage <- cleanfMRI$cleanBoldImage
maskImage <- cleanfMRI$maskImage
framewiseDisplacement <- cleanfMRI$FD
dtRMS <- cleanfMRI$DVARS
}
}
