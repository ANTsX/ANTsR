% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/antsrTransform_class.R
\name{applyAntsrTransformToVector}
\alias{applyAntsrTransformToVector}
\title{applyAntsrTransformToVector}
\usage{
applyAntsrTransformToVector(transform, vectors)
}
\arguments{
\item{transform}{antsrTransform}

\item{vectors}{a matrix where each row is a vector to transform}
}
\value{
array of coordinates
}
\description{
Apply transform to spatial vector
}
\examples{
transform <- new("antsrTransform",
  precision = "float",
  type = "AffineTransform", dimension = 2
)
vec2 <- applyAntsrTransformToVector(transform, c(1, 2, 3))
}
