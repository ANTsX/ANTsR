% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/randsccan.R
\name{randwhiten}
\alias{randwhiten}
\title{randomized whitening function.}
\usage{
randwhiten(A, k, seed = NA)
}
\arguments{
\item{A}{input matrix}

\item{k}{rank to use}

\item{seed}{for testing}
}
\value{
whitened matrix is output
}
\description{
uses random matrix to whiten a matrix
}
\examples{
A <- matrix(rnorm(3000), ncol=50 )
k=10
Aw=randwhiten(A,k)
# (Aw) \%*\% t(Aw)
}
\author{
Avants BB
}
\references{
N. Halko, P.G. Martinsson, J. Tropp "Finding structure with randomness: Stochastic algorithms for constructing approximate matrix decompositions" arXiv 0909.4061
}
\keyword{whiten}
