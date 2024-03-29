% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addNoiseToImage.R
\name{addNoiseToImage}
\alias{addNoiseToImage}
\title{addNoiseToImage}
\usage{
addNoiseToImage(
  image,
  noiseModel = c("additivegaussian", "saltandpepper", "shot", "speckle"),
  noiseParameters
)
}
\arguments{
\item{image}{input image}

\item{noiseModel}{either "additivegaussian", "saltandpepper", "shot",
or "speckle".}

\item{noiseParameters}{vector defining the noise models.
\code{additivegaussian}: (mean, standardDeviation),
\code{saltandpepper}: (probability, saltValue, pepperValue),
\code{shot}: (scale),
\code{speckle}: (standardDeviation),}
}
\value{
noise corrupted image.
}
\description{
Add noise to image using additive Guassian, salt-and-pepper,
shot, or speckle noise.
}
\examples{
image <- antsImageRead(getANTsRData("r16"))
noiseImage <- addNoiseToImage(image, "additivegaussian", c(0, 1))
noiseImage <- addNoiseToImage(image, "saltandpepper", c(0.1, 0, 100))
noiseImage <- addNoiseToImage(image, "shot", c(1.0))
noiseImage <- addNoiseToImage(image, "speckle", c(1.0))

}
\author{
NJ Tustison
}
