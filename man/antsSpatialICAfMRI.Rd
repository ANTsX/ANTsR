\name{antsSpatialICAfMRI}
\alias{antsSpatialICAfMRI}

\title{
Perform spatial ICA on fMRI bold data.
}

\description{
Perform spatial ICA on group or individual fMRI data.  Preprocessing is incorporated
into this function (cf antsPreprocessfMRI.R).
}

\usage{
antsSpatialICAfMRI <- function( boldImages,
  maskImage = NA, numberOfICAComponents = 40,
  frequencyLowThreshold = 0.01, frequencyHighThreshold = 0.1,
  normalizeComponentImages = TRUE )
}

\arguments{
\item{boldImages}{a list of 4-D ANTs image fMRI data.}
\item{maskImage}{3-D ANTs image defining the region of interest.  If not specified,
                 one is automatically calculated.}
\item{numberOfICAComponents}{Number of estimated observers (components).}
\item{frequencyLowThreshold}{Lower threshold for bandpass filtering.}
\item{frequencyHighThreshold}{Upper threshold for bandpass filtering.}
\item{normalizeComponentImages}{Boolean to specify whether each component vector element
                                is normalized to its z-score.}
}

\value{
 Output list includes standard ICA matrices from the fastICA algorithm:

  X	= pre-processed data matrix

  K = pre-whitening matrix that projects data onto the first n.comp principal components

  W	= estimated un-mixing matrix (see definition in details)

  A	= estimated mixing matrix

  S	= estimated source matrix


 We also include the list of component images, mask image, and the following
 QA numbers (cf antsPreprocessingfMRI.R):

  maxDVARS = vector where each element is the maximum DVARS from each bold image

  meanDVARS = vector where each element is the mean DVARS from each bold image

  maxFD = vector where each element is the maximum framewise displacement from each bold image

  meanFD = vector where each element is the mean framewise displacement from each bold image
}

\author{
Tustison NJ
}

\examples{
\dontrun{
boldImages <- list()
boldImages[[1]] <- antsImageRead( "subject1.nii.gz", dim = 4, pixeltype = "float" )
boldImages[[2]] <- antsImageRead( "subject2.nii.gz", dim = 4, pixeltype = "float" )
boldImages[[3]] <- antsImageRead( "subject3.nii.gz", dim = 4, pixeltype = "float" )
boldImages[[4]] <- antsImageRead( "subject4.nii.gz", dim = 4, pixeltype = "float" )

icaResults <- antsSpatialICAfMRI( boldImages, numberOfICAComponents = 20 )
componentImages <- icaResults$componentImages

# write out the component images
for( i in 1:length( icaResults$componentImages ) )
  {
  componentFileName <- paste0( "componentImage", i, ".nii.gz" )
  cat( "Writing ", componentFileName, ".\n" )
  antsImageWrite( componentImages[[i]], componentFileName )
  }
}
}
