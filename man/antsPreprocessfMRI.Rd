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
  maskImage = NA, maskingThreshold = 100,
  initialNuisanceVariables = NA,
  doCompCor = 6,
  doMotionCorrection = TRUE,
  useMotionCorrectedImage = FALSE,
  spatialSmoothingParameter = 0.0,
  spatialSmoothingNumberOfIterations = 5,
  frequencyLowThreshold = 0.01,
  frequencyHighThreshold = 0.1 )
}

\arguments{
\item{boldImage}{4-D ANTs image fMRI data.}
\item{maskImage}{3-D ANTs image defining the region of interest.}
\item{maskingThreshold}{If mask image is not specified, a mask image is created using
                        the specified threshold.}
\item{initialNuisanceVariables}{Optional initial nuisance variables.}
\item{doCompCor}{Numer of compcor nuisance components e.g. 6.}
\item{useMotionCorrectedImage}{Boolean indicating whether or not the motion corrected
                               image should be used in the rest of the pipeline.  This is
                               off by default to avoid additional interpolation.}
\item{spatialSmoothingParameter}{Conductance value for anisotropic smoothing.  Typical values
                                 range from 0.5 to 2.0.}
\item{spatialSmoothingNumberOfIterations}{Number of iterations for anisotropic smoothing.}
\item{frequencyLowThreshold}{Lower threshold for bandpass filtering.}
\item{frequencyHighThreshold}{Upper threshold for bandpass filtering.}
}

\value{
Output is the "clean" fMRI bold image and mask.
}

\author{
Tustison NJ
}

\examples{
\dontrun{
boldImage <- antsImageRead( "fmri.nii.gz", dim = 4, pixeltype = "float" )
cleanfMRI <- antsPreprocessfMRI( boldImage )
cleanBoldImage <- cleanfMRI$cleanBoldImage
maskImage <- cleanfMRI$maskImage
}
}
