% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiscaleSVDxpts.R
\name{measure_orthogonality_gradient}
\alias{measure_orthogonality_gradient}
\title{Measure the Orthogonality Gradient of a Non-Square Matrix}
\usage{
measure_orthogonality_gradient(mat)
}
\arguments{
\item{mat}{A numeric matrix.}
}
\value{
A matrix representing the gradient of the orthogonality measure.
}
\description{
Computes the gradient of the orthogonality measure for a non-square matrix.
}
\examples{
A <- matrix(c(1, 0, 0, 1, 0, 1), nrow = 3, ncol = 2)
measure_orthogonality_gradient(A)
B <- matrix(c(1, 1, 0, 1, 0, 1), nrow = 3, ncol = 2)
measure_orthogonality_gradient(B)
}
