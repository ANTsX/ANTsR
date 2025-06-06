% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/randsccan.R
\name{randcca}
\alias{randcca}
\title{randomized cca function.}
\usage{
randcca(x, y, k, seed = NA)
}
\arguments{
\item{x}{input matrix}

\item{y}{input matrix}

\item{k}{rank to use}

\item{seed}{for testing}
}
\value{
outputs a list containing:
\describe{
  \item{whitened: }{low-rank whitened joint matrix}
  \item{svd: }{low-rank svd of joint matrix}
}
}
\description{
uses random matrix to estimate cca results
}
\examples{
set.seed(13)
x <- matrix(rnorm(3000), nrow = 50)
y <- x \%*\% matrix(rnorm(60 * 100), nrow = 60)
k <- 10
dr <- randcca(x, y, k, 1)

}
\author{
Avants BB
}
