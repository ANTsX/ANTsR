% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/antsrTransform_class.R
\name{getAntsrTransformParameters}
\alias{getAntsrTransformParameters}
\title{getAntsrTransformParameters}
\usage{
getAntsrTransformParameters(transform)
}
\arguments{
\item{transform}{antsrTransform}
}
\value{
array of parameters'
}
\description{
Get parameters of transform
}
\examples{
tx <- new("antsrTransform")
params <- getAntsrTransformParameters(tx)
}
