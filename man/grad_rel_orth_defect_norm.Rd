% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiscaleSVDxpts.R
\name{grad_rel_orth_defect_norm}
\alias{grad_rel_orth_defect_norm}
\title{Gradient of Normalized Relative Orthogonality Defect}
\usage{
grad_rel_orth_defect_norm(A)
}
\arguments{
\item{A}{A numeric matrix.}
}
\value{
A numeric matrix representing the gradient of the normalized relative orthogonality defect.
}
\description{
Computes the gradient of the normalized relative orthogonality defect with respect to A.
}
\examples{
A <- matrix(rnorm(100), nrow = 10)
grad_rel_orth_defect_norm(A)
}
