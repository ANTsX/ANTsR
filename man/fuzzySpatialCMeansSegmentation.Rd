% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fuzzySpatialCMeansSegmentation.R
\name{fuzzySpatialCMeansSegmentation}
\alias{fuzzySpatialCMeansSegmentation}
\title{fuzzySpatialCMeansSegmentation}
\usage{
fuzzySpatialCMeansSegmentation(
  image,
  mask = NULL,
  numberOfClusters = 4,
  m = 2,
  p = 1,
  q = 1,
  radius = 2,
  maxNumberOfIterations = 20,
  convergenceThreshold = 0.02,
  verbose = FALSE
)
}
\arguments{
\item{image}{image to be segmented.}

\item{mask}{optional mask image.  Otherwise, the entire image is used.}

\item{numberOfClusters}{number of segmentation clusters}

\item{m}{fuzziness parameter (default = 2).}

\item{p}{membership importance parameter (default = 1).}

\item{q}{spatial constraint importance parameter (default = 1).
\code{q = 0} is equivalent to conventional fuzzy c-means.}

\item{radius}{neighborhood radius (scalar or array) for spatial
constraint.}

\item{maxNumberOfIterations}{iteration limit (default = 20).}

\item{convergenceThreshold}{Convergence between iterations is measured
using the Dice coefficient (default = 0.02).}

\item{verbose}{print progress.}
}
\value{
list containing segmentation and probability images
}
\description{
Fuzzy spatial c-means for image segmentation.
}
\details{
Image segmentation using fuzzy spatial c-means as described in

Chuang et al., Fuzzy c-means clustering with spatial information for image
segmentation.  CMIG: 30:9-15, 2006.
}
\examples{
image <- antsImageRead(getANTsRData("r16"))
mask <- getMask(image)
fuzzy <- fuzzySpatialCMeansSegmentation(image, mask)

}
\author{
NJ Tustison
}
