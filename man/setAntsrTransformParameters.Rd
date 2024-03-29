% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/antsrTransform_class.R
\name{setAntsrTransformParameters}
\alias{setAntsrTransformParameters}
\title{setAntsrTransformParameters}
\usage{
setAntsrTransformParameters(transform, parameters)
}
\arguments{
\item{transform}{antsrTransform}

\item{parameters}{array of parameters}
}
\value{
TRUE
}
\description{
Set parameters of transform
}
\examples{
tx <- new("antsrTransform")
params <- getAntsrTransformParameters(tx)
setAntsrTransformParameters(tx, params * 2)
}
