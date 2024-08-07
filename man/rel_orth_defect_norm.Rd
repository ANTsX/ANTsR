% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiscaleSVDxpts.R
\name{rel_orth_defect_norm}
\alias{rel_orth_defect_norm}
\title{Normalized Relative Orthogonality Defect}
\usage{
rel_orth_defect_norm(A)
}
\arguments{
\item{A}{A numeric matrix.}
}
\value{
A numeric scalar representing the normalized relative orthogonality defect.
}
\description{
Computes the normalized relative orthogonality defect of a matrix A.
}
\examples{
A <- matrix(rnorm(100), nrow = 10)
rel_orth_defect_norm(A)
}
