% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/antsrMetric_class.R
\name{antsrMetricSetFixedImageMask}
\alias{antsrMetricSetFixedImageMask}
\title{antsrMetricSetFixedImageMask}
\usage{
antsrMetricSetFixedImageMask(metric, image)
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
z <- getMask(x)
antsrMetricSetFixedImageMask(metric, z)
}
