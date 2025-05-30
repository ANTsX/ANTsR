% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiscaleSVDxpts.R
\name{gradient_invariant_orthogonality_defect}
\alias{gradient_invariant_orthogonality_defect}
\title{Compute the Gradient of the Orthogonality Defect}
\usage{
gradient_invariant_orthogonality_defect(A)
}
\arguments{
\item{A}{A numeric matrix.}
}
\value{
A matrix representing the gradient of the orthogonality defect with respect to \code{Ap}.
}
\description{
This function computes the gradient of the orthogonality defect for a matrix \code{A},
The orthogonality defect is defined as the sum of the squared off-diagonal elements 
of \code{t(A) \%*\% A}.
}
