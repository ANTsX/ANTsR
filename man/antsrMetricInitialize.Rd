% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/antsrMetric_class.R
\name{antsrMetricInitialize}
\alias{antsrMetricInitialize}
\title{antsrMetricInitialize}
\usage{
antsrMetricInitialize(metric)
}
\arguments{
\item{metric}{an 'antsrMetric'}
}
\description{
prepare to return values
}
\note{
must call this after setting up object, before getting values back
}
\examples{
x <- antsImageRead(getANTsRData("r16"))
y <- antsImageRead(getANTsRData("r30"))
z <- getMask(y)
metric <- antsrMetricCreate(x, y, type = "MeanSquares")
antsrMetricSetMovingImageMask(metric, z)
antsrMetricInitialize(metric)
}
