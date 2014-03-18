\name{subgradientL1Regression}
\alias{subgradientL1Regression}

\title{
Simple subgradientL1Regressioning function.
}

\description{
SubgradientL1Regression solves y approx x beta 
}

\usage{
wmat<-subgradientL1Regression( y, x, s=0.01, percentvals=0.1 , nits=100, betas=NA   )
}

\arguments{
\item{y}{
  outcome variable
}
\item{x}{
  predictor matrix
}
}

\value{
output has a list of summary items
}

\author{
Avants BB
}

\examples{
mat <- matrix(c(rep(1,1000),rep(0,200)),ncol=50)
y<-rnorm(24)
wmat<-subgradientL1Regression( y, mat, percentvals=1 ) 
}
