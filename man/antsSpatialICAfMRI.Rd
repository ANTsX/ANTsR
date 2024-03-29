% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/antsSpatialICAfMRI.R
\name{antsSpatialICAfMRI}
\alias{antsSpatialICAfMRI}
\title{Perform spatial ICA on fMRI bold data.}
\usage{
antsSpatialICAfMRI(
  boldImages,
  maskImage = NULL,
  numberOfICAComponents = 20,
  normalizeComponentImages = TRUE,
  verbose = FALSE
)
}
\arguments{
\item{boldImages}{a list of 4-D ANTs image fMRI data.}

\item{maskImage}{A 3-D ANTs image defining the region of interest.  This
must be specified.}

\item{numberOfICAComponents}{Number of estimated observers (components).}

\item{normalizeComponentImages}{Boolean to specify whether each component
vector element is normalized to its z-score.}

\item{verbose}{boolean setting verbosity level.}
}
\value{
Output list includes standard ICA matrices from the fastICA
algorithm:

X = pre-processed data matrix

K = pre-whitening matrix that projects data onto the first n.comp principal
components

W = estimated un-mixing matrix (see definition in details)

A = estimated mixing matrix

S = estimated source matrix

and the component images.
}
\description{
Perform spatial ICA on group or individual fMRI data.  Preprocessing should
be performed prior to calling this function (cf preprocessfMRI.R).
}
\examples{

set.seed(2017)
boldImages <- list()
n <- 16
nvox <- n * n * n * 12
dims <- c(n, n, n, 12)
boldImages[[1]] <- makeImage(dims, rnorm(nvox) + 500)
boldImages[[2]] <- makeImage(dims, rnorm(nvox) + 500)
boldImages[[3]] <- makeImage(dims, rnorm(nvox) + 500)
maskImage <- getAverageOfTimeSeries(boldImages[[1]]) * 0 + 1
icaResults <- antsSpatialICAfMRI(boldImages, maskImage,
  numberOfICAComponents = 2
)

}
\author{
Tustison NJ, Avants BB
}
