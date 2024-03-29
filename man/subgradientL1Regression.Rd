% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subgradientL1Regression.R
\name{subgradientL1Regression}
\alias{subgradientL1Regression}
\title{Simple subgradientL1Regression function.}
\usage{
subgradientL1Regression(
  y,
  x,
  s = 0.01,
  percentvals = 0.1,
  nits = 100,
  betas = NA,
  sparval = NA
)
}
\arguments{
\item{y}{outcome variable}

\item{x}{predictor matrix}

\item{s}{gradient descent parameter}

\item{percentvals}{percent of values to use each iteration}

\item{nits}{number of iterations}

\item{betas}{initial guess at solution}

\item{sparval}{sparseness}
}
\value{
output has a list of summary items
}
\description{
SubgradientL1Regression solves y approx x beta
}
\examples{

mat <- replicate(1000, rnorm(200))
y <- rnorm(200)
wmat <- subgradientL1Regression(y, mat, percentvals = 0.05)
print(wmat$resultcorr)

}
\author{
Avants BB
}
