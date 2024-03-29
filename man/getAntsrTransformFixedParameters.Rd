% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/antsrTransform_class.R
\name{getAntsrTransformFixedParameters}
\alias{getAntsrTransformFixedParameters}
\title{getAntsrTransformFixedParameters}
\usage{
getAntsrTransformFixedParameters(transform)
}
\arguments{
\item{transform}{antsrTransform}
}
\value{
array of fixed parameters
}
\description{
Get fixed parameters of transform
}
\examples{
tx <- new("antsrTransform")
params <- getAntsrTransformFixedParameters(tx)
}
