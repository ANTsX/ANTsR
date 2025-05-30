% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiscaleSVDxpts.R
\name{orthogonalizeAndQSparsify}
\alias{orthogonalizeAndQSparsify}
\title{Sparsify and optionally orthogonalize a matrix}
\usage{
orthogonalizeAndQSparsify(
  v,
  sparsenessQuantile = 0.5,
  positivity = "either",
  orthogonalize = TRUE,
  softThresholding = FALSE,
  unitNorm = FALSE,
  sparsenessAlg = NA
)
}
\arguments{
\item{v}{Input matrix}

\item{sparsenessQuantile}{Quantile to control sparseness - higher is sparser}

\item{positivity}{Restrict to positive or negative solution (beta) weights. Choices are "positive", "negative", or "either".}

\item{orthogonalize}{Run Gram-Schmidt if TRUE.}

\item{softThresholding}{Use soft thresholding if TRUE.}

\item{unitNorm}{Normalize each vector to unit norm if TRUE.}

\item{sparsenessAlg}{If specified, use a matrix partition algorithm ("orthorank", "spmp", "sum_preserving_matrix_partition" or "basic").}
}
\value{
A sparsified and optionally orthogonalized matrix.
}
\description{
This function implements a quantile-based sparsification operation.
}
\examples{
mat <- replicate(100, rnorm(20))
mat <- orthogonalizeAndQSparsify(mat)
}
