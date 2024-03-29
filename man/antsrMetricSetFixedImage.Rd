% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/antsrMetric_class.R
\name{antsrMetricSetFixedImage}
\alias{antsrMetricSetFixedImage}
\title{antsrMetricSetFixedImage}
\usage{
antsrMetricSetFixedImage(metric, image)
}
\arguments{
\item{metric}{an 'antsrMetric'}

\item{image}{the fixed 'antsImage'}
}
\description{
set fixed image for image to image metric
}
\note{
After calling this, must call antsrMetricInitialize(metric)
}
\examples{
x <- antsImageRead(getANTsRData("r16"))
y <- antsImageRead(getANTsRData("r30"))
metric <- antsrMetricCreate(x, y, type = "MeanSquares")
z <- x * 2
antsrMetricSetFixedImage(metric, z)
}
